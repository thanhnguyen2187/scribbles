(load "./sdf/manager/load")
(manage 'new-environment 'combinators)

;; quoter:begin:compose
(define (compose f g)
  (let* ((n (get-arity g))
         (m (get-arity f))
         (the-composition
           (lambda args
             (assert (= n (length args)))
             (assert (= m 1))
             (f (g args)))))
    (restrict-arity the-composition n)
    the-composition))
;; quoter:end:compose

((compose (lambda (x) (list 'foo x))
          (lambda (x) (list 'bar x)))
 'z)

;; quoter:begin:parallel-combine
(define (parallel-combine h f g)
  (define (the-combination . args)
    (let ((n (length args)))
      (assert (= (get-arity f) n))
      (assert (= (get-arity g) n))
      (assert (= (get-arity h) 2)))
    (h (apply f args)
       (apply g args)))
  (restrict-arity the-combination n)
  the-combination)
;; quoter:end:parallel-combine

((parallel-combine (lambda (a b) (list a b))
                   (lambda (x y z) (list 'foo x y z))
                   (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c)

; (manage 'run-tests)

