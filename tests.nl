;; Non-exhaustive test suite showing off what the interpreter can do.

(test-error unbound)
(test-error (unbound))

(test-error (go))

(test (let ((variable 1))
        (let ((variable 2))
          variable))
      2)

(test (let* ((variable 1)
             (c (lambda () variable)))
        (let ((variable 2))
          (c)))
      1)

(test (list* 1 2 3 4) (cons 1 (cons 2 (cons 3 4))))
(test (list* 1 2 3 (list 'a 'b)) (list 1 2 3 'a 'b))

(test (let ((x 0))
        (let ((x 1)
              (y x))
          y))
      0)

(test (let ((x 0))
        (let* ((x 1)
               (y x))
          y))
      1)

(define variable 'value)
(test variable 'value)
(setq variable 'value2)
(test variable 'value2)

(define function (lambda (x) (cons x x)))
(define alias function)
(test (function 1) (alias 1))

(test (map (lambda (x) (+ x 1)) (list 1 2 3 4 5))
      (list 2 3 4 5 6))

(define (make-counter)
  (let ((count -1))
    (lambda ()
      (setq count (+ count 1))
      count)))
(define counter1 (make-counter))
(define counter2 (make-counter))
(test (progn (counter1)
             (counter1)
             (counter1))
      2)
(test (progn (counter2)
             (counter2))
      1)
(test (counter1) 3)

(define increment-it nil)
(define get-value nil)
(let ((closed-over 1))
  (setq increment-it (lambda ()
                       (setq closed-over (+ closed-over 1))))
  (setq get-value (lambda ()
                    closed-over)))
(test (get-value) 1)
(test (increment-it) 2)
(test (get-value) 2)

(define (factorial-recursive n)
  (if (= n 0)
      1
      (* n (factorial-recursive (- n 1)))))
(test (factorial-recursive 5) 120)
(define (factorial-iterative n)
  (prog (f)
        (setq f 1)
      a (when (= n 0)
          (return f))
        (setq f (* f n))
        (setq n (- n 1))
        (go a)))
(test (factorial-iterative 5) 120)

(define (man-or-boy k x1 x2 x3 x4 x5)
  (define (B)
    (setq k (- k 1))
    (man-or-boy k B x1 x2 x3 x4))
  (if (<= k 0)
      (+ (x4) (x5))
      (B)))
(test (man-or-boy 10 (lambda () 1)
                     (lambda () -1)
                     (lambda () -1)
                     (lambda () 1)
                     (lambda () 0))
      -67)

(define (block-test)
  (block a
    (block b
      (block c
        (block d
          (block e
            (return-from b 5)))))))
(test (block-test) 5)

(define (block-test2)
  (block nil
    (let ((rf (lambda (x) (return x))))
      (block nil
        (block nil
          (rf 1))
        2))))
(test (block-test2) 1)

(define (block-test3)
  (block nil
    (block nil
      (block nil
        (return 1))
      2)
    3))
(test (block-test3) 3)

(define (block-test4)
  (block a
    (block b
      (block c
        (block d
          (block e
            1))))))
(test (block-test4) 1)

(test-error (block '(what am i)))
(test-error (return-from nonexistent))
(test-error (block nil
              (return-from '(not a symbol))))

(define exit nil)
(define (do-something-else)
  (exit 10))
(define (block-closure)
  (block nil
    (setq exit (lambda (x) (return x)))
    (do-something-else)))
(test (block-closure) 10)
(test-error (exit))

(define (tagbody-test)
  (block nil
    (tagbody
      a (go b)
      b (go d)
      c (go e)
      d (go c)
      e (go g)
      f (go h)
      g (go f)
      h (return 10))))
(test (tagbody-test) 10)

(test-error (go nonexistent))

(define go-to nil)
(define (do-go-to)
  (go-to))
(define (tagbody-closure)
  (block nil
    (tagbody (setq go-to (lambda () (go done)))
             (go start)
        done (return t)
       start (go-to))))
(test (tagbody-closure) t)
(test-error (go-to))

(test-error (go '(not tag)))

(test (block nil
        (tagbody (go start)
             out (return 1)
           start (let ((v1 1))
                   (let ((v2 2))
                     (let ((v3 3))
                       ((lambda ()
                          (go out))))))))
      1)

;; Not used in an actual TEST form but it's fun.
(define (spaghetti)
  (block nil
    (tagbody
        (define labels (list (lambda () (go a))
                             (lambda () (go b))
                             (lambda () (go c))
                             (lambda () (go d))
                             (lambda () (return))))
        (define (transfer)
          ((nth (random 5) labels)))
        (print 'testing-spaghetti)
        (transfer)
      a (print 'a)
        (transfer)
      b (print 'b)
        (transfer)
      c (print 'c)
        (transfer)
      d (print 'd)
        (transfer))))
(spaghetti)
(spaghetti)
(spaghetti)
(spaghetti)
(spaghetti)
(spaghetti)

(define (test-while n)
  (while (> n 0)
    (setq n (- n 1)))
  n)
(test (test-while 10) 0)

(let ((f (lambda () t)))
  (test-error (f 1)))

(test (and t nil) nil)
(test (and t t) t)
(test (and nil t) nil)
(define unchanged 10)
(test (and t t t t t t t t t nil (setq unchanged 11)) nil)
(test unchanged 10)

(test (or t nil) t)
(test (or nil t) t)
(test (or nil nil) nil)
(test (or nil nil nil nil nil nil t (setq unchanged 11)) t)
(test unchanged 10)

(define cleaned-up 0)
(define (unwind-test)
  (block nil
    (unwind-protect (return 5)
      (setq cleaned-up 1))))
(test (unwind-test) 5)
(test cleaned-up 1)