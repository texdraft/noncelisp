;;;; Didactic, over-engineered interpreter for a simple but nontrivial Lisp.

(defstruct environment
  "Represents a lexical environment, containing bindings of variables, blocks,
and GO tags, with a separate association list for each namespace. Entries in the
VARIABLES slot have the form
        (name . value)
with the natural meaning. Entries in the BLOCKS slot have the form
        (block-name . x),
where X is NIL during the block's dynamic extent but becomes :INVALID after the
block has been exited. Entries in the TAGS slot have the form
        (name marker . forms),
where MARKER is initially (CONS NIL NIL) but will have its cdr replaced by
:INVALID (as with block names) when the TAGBODY is exited, and FORMS is the
sequence of forms following the tag in the TAGBODY."
  (variables nil :type list)
  (blocks nil :type list)
  (tags nil :type list))

(defvar *global-environment* (make-environment)
  "The global lexical environment, where all the predefined functions and
macros are to be found and where top-level DEFINE/DEFINE-MACRO forms stash
definitions.")

(defvar *values* nil
  "List of secondary values returned by last form executed. This list is
invalidated at the beginning of every call to EVALUATE and is only modified by
the interpreted function VALUES.")

(defstruct (closure (:print-object print-closure))
  "The runtime representation of an interpreted function, containing its code
and the surrounding lexical environment. If the function was defined by DEFINE
then its name is also stored for better diagnostics. If the function is defined
as a macro then the MACROP slot is T.

The FUNCTION slot normally holds a lambda expression, but it can also hold a CL
function object for “native” functions."
  (function nil :type (or list function))
  (name nil :type symbol)
  (macrop nil :type boolean)
  (environment nil :type environment))

(defun print-closure (closure stream)
  "Print a CLOSURE object in a more opaque way than the default."
  (print-unreadable-object (closure stream :identity t :type nil)
    ;; Arbitrary limits to avoid huge lambda expressions
    (let ((*print-length* 5)
          (*print-level* 2))
      (format stream "~:[Function~;Macro~] ~W"
                     (closure-macrop closure)
                     (or (closure-name closure)
                         (closure-function closure))))))

;;; Conditions for control flow

(define-condition control-transfer ()
  ()
  (:documentation
    "Abstract base class of conditions signaled to transfer control."))

(define-condition go-condition (control-transfer)
  ((entry :type list
          :initarg :entry
          :reader go-condition-entry))
  (:documentation
    "Condition signaled to implement GO. The ENTRY slot holds the entry in an
ENVIRONMENT for the tag; GO checks to make sure the transfer is valid."))

(define-condition return-from-condition (control-transfer)
  ((entry :type cons
          :initarg :entry
          :reader return-from-condition-entry)
   (value :initarg :value ; the value given to return
          :reader return-from-condition-value))
  (:documentation
    "Condition signaled to implement RETURN-FROM. The ENTRY slot holds the entry
in an ENVIRONMENT for the block name; RETURN-FROM checks to make sure the
transfer is valid."))

;;; Conditions for exceptional situations

(define-condition interpreter-error (error)
  ()
  (:documentation
    "Base class of errors signaled during evaluation."))

(define-condition interpreter-unbound (unbound-variable interpreter-error)
  ())

(define-condition unbound-function (unbound-variable interpreter-error)
  ()
  (:documentation
    "Signaled when a variable in a function call is unbound.")
  (:report
    (lambda (condition stream)
      (format stream "Unbound function ~A" (cell-error-name condition)))))

(define-condition not-a-function (type-error interpreter-error)
  ()
  (:default-initargs
    :expected-type 'closure)
  (:documentation
    "Signaled when code attempts to apply something that isn't a function.")
  (:report
    (lambda (condition stream)
      (format stream "The value ~A (of type ~A) is not a function."
                     (type-error-datum condition)
                     (type-of (type-error-datum condition))))))

(define-condition special-operator-syntax-error (parse-error interpreter-error)
  ((name :type symbol
         :initarg :name
         :reader special-operator-syntax-error-name))
  (:documentation
    "Signaled if a special operator is used with incorrect syntax.")
  (:report
    (lambda (condition stream)
      (format stream "Invalid syntax for ~A."
                     (special-operator-syntax-error-name condition)))))

(define-condition unseen-go-tag (control-error interpreter-error)
  ((tag :type symbol
        :initarg :tag
        :reader unseen-go-tag-tag))
  (:documentation
    "Signaled when user code attempts to GO to a tag that isn't bound.")
  (:report
    (lambda (condition stream)
      (format stream "Attempt to go to nonexistent tag ~A."
                     (unseen-go-tag-tag condition)))))

(define-condition invalid-go (control-error interpreter-error)
  ((tag :type symbol
        :initarg :tag
        :reader invalid-go-tag))
  (:documentation
    "Signaled when user code attempts to GO to a tag bound by a TAGBODY whose
dynamic extent has ended.")
  (:report
    (lambda (condition stream)
      (format stream "Attempt to go to tag ~A outside of its dynamic extent."
                     (invalid-go-tag condition)))))

(define-condition bad-go-tag (interpreter-error)
  ((given :initarg :given
          :reader bad-go-tag-given))
  (:documentation
    "Signaled when something other than a symbol is given to GO.")
  (:report
    (lambda (condition stream)
      (format stream "GO tags must be symbols (~A given)."
                     (bad-go-tag-given condition)))))

(define-condition unseen-block (control-error interpreter-error)
  ((name :type symbol
         :initarg :name
         :reader unseen-block-name))
  (:documentation
    "Signaled when user code attempts to RETURN-FROM a block that isn't in scope.")
  (:report
    (lambda (condition stream)
      (format stream "Attempt to return from nonexistent block ~A"
                     (unseen-block-name condition)))))

(define-condition invalid-block (control-error interpreter-error)
  ((name :type symbol
         :initarg :name
         :reader invalid-block-name))
  (:documentation
    "Signaled when user code attempts to RETURN-FROM a block whose dynamic
extent has ended.")
  (:report
    (lambda (condition stream)
      (format stream "Attempt to return from block ~A outside of its dynamic extent."
                     (invalid-block-name condition)))))

(define-condition bad-block-name (interpreter-error)
  ((given :initarg :given
          :reader bad-block-name-given))
  (:documentation
    "Signaled when something other than a symbol is given as a block name.")
  (:report
    (lambda (condition stream)
      (format stream "Block names must be symbols (~A given)."
                     (bad-block-name-given condition)))))

(define-condition invalid-lambda (parse-error interpreter-error)
  ((form :initarg :form
         :reader invalid-lambda-form)
   (why :type string
        :initarg :why
        :reader invalid-lambda-why))
  (:documentation
    "Signaled when a list of the form (LAMBDA …) is not a valid lambda expression.")
  (:report
    (lambda (condition stream)
      (format stream "This is not a lambda expression (~A): ~W"
                     (invalid-lambda-why condition)
                     (invalid-lambda-form condition)))))

(define-condition argument-mismatch (program-error interpreter-error)
  ((given :type integer
          :initarg :given
          :reader argument-mismatch-given)
   (expected :type integer
             :initarg :expected
             :reader argument-mismatch-expected)
   (name :type symbol
         :initarg :name
         :reader argument-mismatch-name))
  (:documentation
    "Signaled when use of function doesn't match its definition.")
  (:report
    (lambda (condition stream)
      (format stream "Invalid number of arguments given for function ~A; ~D ~
                      expected, ~D given"
                     (argument-mismatch-name condition)
                     (argument-mismatch-expected condition)
                     (argument-mismatch-given condition)))))

;;; Environment manipulation

(defun normalize-lambda (expression environment) ; environment for error handling
  "Return the given expression, a list beginning with LAMBDA, if it is
a syntactically valid lambda expression. Otherwise signal an error and set up
restarts. If the expression is valid and contains multiple body forms, they are
wrapped in a PROGN."
  (destructuring-bind (l parameters &rest body) expression
    (restart-case (cond ((null body)
                         (error 'invalid-lambda
                                :form expression
                                :why "at least one body form is required"))
                        ((not (typep parameters '(or symbol cons)))
                         (error 'invalid-lambda
                                :form expression
                                :why "parameters must be a single symbol or a list of symbols"))
                        ((and (listp parameters) ; if it's a parameter list
                              (not (null parameters)) ; and there is at least one parameter
                              (not (every #'symbolp parameters))) ; they must all be symbols
                          (error 'invalid-lambda :form expression
                                                 :why "malformed parameter list"))
                        ((null (rest body)) ; one form
                         expression)
                        (t
                         (list l
                               parameters
                               (cons 'progn body))))
      (use-value (value)
        :interactive (lambda ()
                       (prompt-evaluate-form environment))
        (normalize-lambda value environment)))))

(defun enclose (name macrop expression environment)
  "Make a closure from a lambda expression and an environment. The environment's
slots must be copied to enforce lexical scope rules."
  (make-closure :function (normalize-lambda expression environment)
                :name name
                :macrop macrop
                :environment (copy-structure environment)))

(defun bind-variable (name value environment)
  "Add a variable binding to an environment."
  (push (cons name value) (environment-variables environment)))

(defun lookup-variable (name environment)
  "Find the (NAME . VALUE) cons cell for a given name in a given environment."
  (or (assoc name (environment-variables environment))
      (assoc name (environment-variables *global-environment*))))

(defun mutate-cell (cell value)
  "Mutate a variable binding, returning the new value."
  (setf (cdr cell) value))

(defun cell-value (cell)
  "Extract the value in a variable binding."
  (cdr cell))

(defun assign-variable (name value environment)
  "Assign a value to a variable or error if no such variable is bound. The value
stored is returned."
  (let ((found (lookup-variable name environment)))
    (if found
        (mutate-cell found value)
        (restart-case (error 'interpreter-unbound :name name)
          (globally-bind (value)
            :report (lambda (stream)
                      (format stream "Bind ~A to ~A in the global environment."
                                     name
                                     value))
            :interactive (lambda ()
                           (prompt-evaluate-form environment))
            (bind-variable name value *global-environment*)
            value)))))

(defun define (name value environment)
  "Bind NAME to VALUE in the given lexical environment, warning the user if they
are shadowing an existing binding."
  (when (lookup-variable name environment)
    (warn "Redefining ~A." name))
  (when (closure-p value)
    ;; Enable recursive calls.
    (bind-variable name value (closure-environment value))
    (when (not (closure-name value))
      (setf (closure-name value) name)))
  (bind-variable name value environment)
  value)

(defun augment-environment (names values environment)
  "Bind NAMES to VALUES in the given environment; used to implement LET."
  (loop for name in names as value in values do
    (bind-variable name value environment)))

(defun bind-block (name environment)
  "Add a block name to a given environment, returning the cons cell used for the
binding."
  (let ((cell (list name)))
    (push cell (environment-blocks environment))
    cell))

(defun lookup-block (name environment)
  "Find the block entry for NAME in the given environment."
  (assoc name (environment-blocks environment)))

(defun bind-tag (tag marker forms environment)
  "Add a go tag to a given environment."
  (push (list* tag marker forms) (environment-tags environment)))

(defun lookup-tag (tag environment)
  "Find the tag entry for TAG in the given environment."
  (assoc tag (environment-tags environment)))

(let ((initializedp nil))
  (defun initialize-global-environment ()
    "Set up the global environment to contain a few useful CL bindings, unless
this function has already been called (to avoid redefinitions)."
    (unless initializedp
      (setf initializedp t)
      (flet ((f (name &optional (function (symbol-function name)))
               (bind-variable name
                              (make-closure :function function
                                            :name name
                                            :environment *global-environment*)
                              *global-environment*))
             (v (name value)
               (bind-variable name value *global-environment*)))
        (f '+)
        (f '-)
        (f '*)
        (f '/)
        (f '=)
        (f '<)
        (f '<=)
        (f '>)
        (f '>=)
        (f 'cons)
        (f 'first)
        (f 'rest)
        (f 'second)
        (f 'third)
        (f 'fourth)
        (f 'fifth)
        (f 'sixth)
        (f 'seventh)
        (f 'eighth)
        (f 'ninth)
        (f 'tenth)
        (f 'nth)
        (f 'random)
        (f 'eq? #'eq)
        (f 'eql? #'eql)
        (f 'equal? #'equal)
        (f 'cons? #'consp)
        (f 'atom? #'atom)
        (f 'list? #'listp)
        (f 'print)
        (f 'read)
        (f 'read-line)
        (f 'gensym)
        (f 'error)
        (f 'cerror)
        (f 'apply (lambda (function arguments)
                    (interpreter-apply function arguments)))
        (f 'values (lambda (&rest arguments)
                     (setf *values* (rest arguments))
                     (first arguments)))
        (v 't t)
        (v 'nil nil)
        (evaluate (read-file "prelude.nl") *global-environment*)))))

(defun read-file (name)
  "Repeatedly read forms from the file named NAME, returning them enclosed in a
PROGN."
  (let ((forms nil))
    (with-open-file (input name)
      (loop for form := (read input nil) while form do
        (push form forms)))
    (cons 'progn (nreverse forms))))

;;; Evaluation

(defun interpreter-special-operator-p (name)
  "Is the given name defined as a special operator?"
  (and (symbolp name) (get name :special-operator)))

(defun evaluate (expression environment)
  "Evaluate an expression in the given lexical environment."
  (setf *values* nil) ; invalidate values left by intervening computations
  (cond ((symbolp expression)
         (evaluate-symbol expression environment))
        ((atom expression) ; self-evaluating
         expression)
        ;; It must be a cons.
        ((eq (first expression) 'lambda)
         (enclose nil nil expression environment))
        (t
         (evaluate-cons expression environment))))

(defun evaluate-symbol (symbol environment)
  "Evaluate a symbol in the given environment."
  (restart-case (let ((entry (lookup-variable symbol environment)))
                  (if (null entry)
                      (error 'interpreter-unbound :name symbol))
                      (cell-value entry))
    (use-value (value)
      :interactive (lambda ()
                     (prompt-evaluate-form environment))
      value)
    (store-value (value)
      :interactive (lambda ()
                     (prompt-evaluate-form environment))
      (bind-variable symbol value environment)
      value)))

(defun evaluate-cons (expression environment)
  "Evaluate an expression of the form (OPERATOR . REST)."
  (let ((operator (first expression)))
    (cond ((interpreter-special-operator-p operator)
           (funcall (get operator :special-operator)
                    expression
                    environment))
          ;; We do the lookup here instead of by calling EVALUATE-SYMBOL so that
          ;; we can signal and trap UNBOUND-FUNCTION instead of the less precise
          ;; UNBOUND-VARIABLE.
          ((symbolp operator)
           (let ((found (lookup-variable operator environment)))
             (if found
                 (evaluate-call (cell-value found)
                                (rest expression)
                                environment)
                 (restart-case (error 'unbound-function :name operator)
                   (use-value (value)
                     (evaluate-call value
                                    (rest expression)
                                    environment))
                   (store-value (value)
                     (bind-variable operator value environment)
                     (evaluate expression environment))))))
          (t
           (evaluate-call (evaluate operator environment)
                          (rest expression)
                          environment)))))

(defun evaluate-list (list environment)
  "Evaluate each item of a list in a given lexical environment, returning a list
of the results of evaluation."
  (mapcar (lambda (e)
            (evaluate e environment))
          list))

(defun evaluate-call (closure rest environment)
  "Evaluate a call to a function or a macro."
  (cond ((not (typep closure 'closure))
         (restart-case (error 'not-a-function :datum closure)
           (use-value (value)
             :interactive (lambda ()
                            (prompt-evaluate-form environment))
             (evaluate-call value
                            rest
                            environment))))
        ((closure-macrop closure) ; macro; give arguments unevaluated
         (evaluate (interpreter-apply closure rest)
                   environment))
        (t
         (interpreter-apply closure
                            (evaluate-list rest environment)))))

(defun interpreter-apply (closure arguments)
  "Apply a function, checking for mismatched parameters and arguments."
  (let ((function (closure-function closure)))
    (if (typep function 'function) ; native CL function
        (apply function arguments) ; this should do any checking we need
        ;; Now we must have a lambda expression
        (apply-lambda function
                      arguments
                      (closure-name closure)
                      (copy-structure (closure-environment closure))))))

(defun apply-lambda (function arguments name environment)
  (let ((parameters (second function))
        (argument-count 0))
    (cond ((and (symbolp parameters) ; single symbol will be bound to all arguments
                (not (null parameters))) ; but avoid doing this for no-argument functions
           (bind-variable parameters arguments environment)
           (evaluate (third function) environment))
          (t
           (if (null parameters)
               (setf argument-count (list-length arguments))
               (loop for name := (pop parameters) as argument := (pop arguments)
                     do (bind-variable name argument environment)
                        (incf argument-count)
                     until (or (null parameters) (null arguments))))
           (if (and (endp arguments) (endp parameters)) ; got to the ends of both lists
               ;; NORMALIZE-LAMBDA ensures that the lambda has only one body form.
               (evaluate (third function) environment)
               (restart-case (error 'argument-mismatch
                                    :name name
                                    :expected (list-length parameters)
                                    :given argument-count)
                 (replace-function (f) ; taken from SBCL
                   :report (lambda (stream)
                             (format stream "Call a different function ~
                                             with the same arguments"))
                   :interactive (lambda ()
                                  (prompt-evaluate-form environment))
                   (evaluate-call f arguments environment))))))))

;;; Special operators.

;; This kludge is here to provide slightly better error reporting when a
;; special operator is used with incorrect syntax. All the spec says is that
;;         If the result of evaluating the expression does not match the
;;         destructuring pattern, an error of type error should be signaled,
;; which is not very helpful. So we want to trap an error that occurs while
;; destructuring, without also concealing errors that happen in the body.
(defmacro estructuring-bind (name lambda-list expression &body body)
  (let ((tag (gensym))
        (condition (gensym)))
    `(let ((,tag (cons nil nil))) ; avoid stepping on anyone's toes
      (block nil
        (error (catch ,tag
                  (handler-case (destructuring-bind ,lambda-list ,expression
                                  (handler-case (return (progn ,@body))
                                    (error (,condition)
                                      (throw ,tag ,condition))))
                    (error (,condition)
                      (declare (ignore ,condition))
                      (error 'special-operator-syntax-error :name ',name)))))))))

(defmacro define-special-operator (name lambda-list environment &body body)
  "Create a function for evaluating a special operator and store it under the
:SPECIAL-OPERATOR property on NAME. This function will be called with two
arguments: the special form (excluding the operator) and the lexical
environment."
  (let ((form (gensym)))
    `(setf (get ',name :special-operator)
           (lambda (,form ,environment)
             (declare (ignorable environment))
             (estructuring-bind ,name ,lambda-list (rest ,form)
               ,@body)))))

(defun evaluate-progn (forms environment)
  "Evaluate a list of forms in the given environment, returning the result of
the last one."
  (loop for form in forms
        as result := (evaluate form environment)
        finally (return result)))

(defmacro with-environment ((v) environment &body body)
  "Make it easier for special operators to introduce new levels of scope."
  `(let ((,v (copy-structure ,environment)))
     ,@body))

;; (define f (lambda (x) …))
;; (define (f x) …)   ≡ (define f (lambda (x) …))
;; (define (f . x) …) ≡ (define f (lambda x …))
(define-special-operator define (name &rest rest) environment
  (cond ((symbolp name)
         (unless (null (rest (rest rest)))
           (error "Extra stuff in DEFINE form: ~A." (rest (rest rest))))
         (define name (evaluate (first rest) environment) environment))
        ((consp name)
         (unless (symbolp (first name))
           (error "A cons after DEFINE must be of the form (NAME . PARAMETERS)."))
         (define (first name)
                 (evaluate (list* 'lambda (cdr name) rest)
                           environment)
                 environment))
        (t
         (error "DEFINE can define only symbols (~A given)" (type-of name)))))

(define-special-operator define-macro ((name parameter) &rest definition) environment
  (define name
          (enclose name t (list* 'lambda parameter definition) environment)
          environment))

(define-special-operator quote (form) environment
  form)

(define-special-operator progn (&rest forms) environment
  (evaluate-progn forms environment))

(define-special-operator if (test then &optional else) environment
  (if (evaluate test environment)
      (evaluate then environment)
      (evaluate else environment)))

(define-special-operator setq (name value) environment
  (assign-variable name
                   (evaluate value environment)
                   environment))

;; Just for fun, we support the full range of CL binding styles.
(define-special-operator let (bindings &rest forms) environment
  (let ((old environment))
    (with-environment (environment) environment
      (loop for binding in bindings do
        (cond ((symbolp binding)
               (bind-variable binding nil environment))
              ((null (rest binding))
               (bind-variable (first binding) nil environment))
              (t
               (bind-variable (first binding)
                              (evaluate (second binding) old)
                              environment))))
      (evaluate-progn forms environment))))

(define-special-operator let* (bindings &rest forms) environment
  (with-environment (environment) environment
    (loop for binding in bindings do
      (cond ((symbolp binding)
             (bind-variable binding nil environment))
            ((null (rest binding))
             (bind-variable (first binding) nil environment))
            (t
             (bind-variable (first binding)
                            (evaluate (second binding) environment)
                            environment))))
    (evaluate-progn forms environment)))

(define-special-operator block (name &rest forms) environment
  (if (symbolp name)
      (with-environment (environment) environment
        (let ((entry (bind-block name environment)))
          (block exit
            (unwind-protect (handler-case (evaluate-progn forms environment)
                              (return-from-condition (condition)
                                (if (eq (return-from-condition-entry condition) entry)
                                    (return-from exit (return-from-condition-value condition))
                                    (signal condition))))
              (setf (cdr entry) :invalid)))))
      (error 'bad-block-name :given (type-of name))))

(define-special-operator return-from (name &optional value) environment
  (if (symbolp name)
      (let ((entry (lookup-block name environment)))
        (cond ((not entry)
               (error 'unseen-block :name name))
              ((eq (cdr entry) :invalid)
               (error 'invalid-block :name name))
              (t
               (signal 'return-from-condition :entry entry
                                              :value (evaluate value environment)))))
      (error 'bad-block-name :given (type-of name))))

;; Unlike CL we allow only symbols for tags.
(define-special-operator tagbody (&rest body) environment
  (let ((marker (list nil)))
    (with-environment (environment) environment
      ;; First collect the tags and the tail of the TAGBODY that they label.
      (loop for (first . rest) on body do
        (cond ((symbolp first)
               (bind-tag first marker rest environment))
              ((atom first)
               (warn "Non-symbol atom ~A in TAGBODY" first))))
      ;; The outer loop runs until a control transfer happens or until the end
      ;; of the TAGBODY is reached. The inner loop runs for each tail of the
      ;; form being evaluated.
      (unwind-protect (block nil
                        (loop with segment := body do
                          (handler-case (progn (loop for item in segment do
                                                 (unless (symbolp item)
                                                   (evaluate item environment)))
                                               ;; This RETURN is reached only if
                                               ;; we fall off of the end of the
                                               ;; TAGBODY.
                                               (return))
                            (go-condition (condition)
                              (let ((entry (go-condition-entry condition)))
                                (if (eq (second entry) marker)
                                    (setf segment (rest (rest entry)))
                                    (signal condition)))))))
        ;; Here we are performing a transfer of control out of the TAGBODY,
        ;; meaning that its dynamic extent has ended.
        (setf (cdr marker) :invalid))))
  nil)

(define-special-operator go (tag) environment
  (if (symbolp tag)
      (let ((entry (lookup-tag tag environment)))
        (cond ((null entry)
               (error 'unseen-go-tag :tag tag))
              ((eq (cdr (second entry)) :invalid)
               (error 'invalid-go :tag tag))
              (t
               (signal 'go-condition :entry entry))))
      (error 'bad-go-tag :given (type-of tag))))

(define-special-operator unwind-protect (protected-form &rest cleanup-forms) environment
  (handler-case (evaluate protected-form environment)
    (control-transfer (condition)
      (evaluate-progn cleanup-forms environment)
      (signal condition))))

;; Based on SBCL's full-eval.lisp
(define-special-operator multiple-value-call (function &rest forms) environment
  (interpreter-apply (evaluate function environment)
                     (loop for form in forms
                           nconcing (cons (evaluate form environment) *values*))))

(define-special-operator test (form result-form) environment
  (let* ((result (evaluate result-form environment))
         (test (evaluate form environment))
         (success (equal test result)))
    (if success
        (format t "Test: ~A = ~A? PASS.~%" form result)
        (error "Test: ~A = ~A? FAIL (got ~A)." form result test))))

(define-special-operator test-error (form) environment
  (handler-case (evaluate form environment)
    (interpreter-error (condition)
      (format t "Test: ~A signaled ~A: " form (type-of condition))
      (write condition :escape nil)
      (terpri))))

;;; The top level

(defun prompt-evaluate-form (environment)
  "Ask the user for a form to be evaluated, then return a singleton list of the
result of evaluating that form in the given environment."
  (write-string "Enter a form to be evaluated: ")
  (finish-output)
  (list (evaluate (read) environment)))

(defun top-level (&optional (input *standard-input*) (output *standard-output*))
  "A nice read-evaluate-print-loop for this Lisp dialect."
  (initialize-global-environment)
  (let ((environment *global-environment*))
    (handler-case (let ((counter 0))
                    (loop (incf counter)
                          (format output "~&[~D]: " counter)
                          (finish-output output)
                          (let ((value (evaluate (read input) environment)))
                            (bind-variable (intern (format nil "$~D" counter))
                                           value
                                           environment)
                            (format output "$~D = ~A~%" counter value))))
      (end-of-file (condition)
        (declare (ignore condition))
        (write-line "Bye." output)))))

(defun run-file (file)
  (initialize-global-environment)
  (evaluate (read-file file) *global-environment*))

(run-file "tests.nl")