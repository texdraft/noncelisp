;; Simple library for the Lisp dialect

(define (null? l)
  (eq? l nil))

(define (list . elements)
  (if (null? elements)
        nil
        (cons (first elements)
              (apply list (rest elements)))))

(define list*
  (lambda elements
    (if (null? (rest elements))
        (first elements)
        (if (null? (rest (rest elements)))
            (cons (first elements) (second elements))
            (cons (first elements) (apply list* (rest elements)))))))

(define (map f l)
  (if (null? l)
      nil
      (cons (f (first l))
            (map f (rest l)))))

(define-macro (cond b)
  (define (expand clauses)
    (if (null? clauses)
        nil
        (let ((clause (first clauses)))
          (list 'if
                (first clause)
                (cons 'progn (rest clause))
                (expand (rest clauses))))))
  (expand b))

(define-macro (when b)
  (list 'if (first b) (cons 'progn (rest b)) nil))

(define-macro (unless b)
  (list 'if (first b) nil (cons 'progn (rest b))))

(define-macro (and b)
  (cond ((null? b)
         t)
        ((null? (rest b))
         (first b))
        (t
         (list 'if (first b) (cons 'and (rest b))))))

(define-macro (or b)
  (cond ((null? b)
         nil)
        ((null? (rest b))
         (first b))
        (t
         (let ((result (gensym)))
           (list 'let (list (list result (first b)))
             (list 'if result result (cons 'or (rest b))))))))

;; (while test . body)
(define-macro (while b)
  (define (expand b)
    (let ((test (first b))
          (body (cons 'progn (rest b)))
          (again (gensym))
          (end (gensym)))
      (list 'block
            nil
            (list 'tagbody
                  (list 'go end)
                  again
                  body
                  end
                  (list 'if test (list 'go again) '(return-from nil))))))
  (expand b))

(define-macro (prog b)
  (list 'block nil
        (list 'let (first b) (cons 'tagbody (rest b)))))

(define-macro (return b)
  (list* 'return-from nil b))