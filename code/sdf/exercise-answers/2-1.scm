(load "./sdf/manager/load")
(manage 'new-environment 'combinators)

;; quoter:begin:compose
(define (compose f g)
  (define (the-composition . args)
    (assert (= (get-arity g)
               (length args)))
    (restrict-arity the-composition (length args))
    (f (apply g args)))
  the-composition)
;; quoter:end:compose

((compose (lambda (x)
            (list 'foo x))
          (lambda (x)
            (list 'bar x)))
 'z)

;Value: (foo (bar z))

;; quoter:begin:parallel-combine
(define (parallel-combine h f g)
  (define (the-combination . args)
    (h (apply f args)
       (apply g args)))
  (let ((n1 (get-arity f))
        (n2 (get-arity g)))
    (assert (= n1 n2))
    (restrict-arity the-combination n1))
  the-combination)
;; quoter:end:parallel-combine

((parallel-combine list
                   (lambda (x y z)
                     (list 'foo x y z))
                   (lambda (u v w)
                     (list 'bar u v w)))
 'a 'b 'c)

;; run tests if possible
; (manage 'run-tests)
