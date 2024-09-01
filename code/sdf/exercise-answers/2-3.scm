(load "./sdf/manager/load")
(manage 'new-environment 'combinators)

;; quoter:begin:combine-apply
(define (combine-apply f g)
  (let ((m (get-arity f))
        (n (get-arity g)))
    (assert (= n m))
    (define (the-combination . args)
      (assert (= n (length args)))
      (let-values ((fv (apply f args))
                   (gv (apply g args)))
        (apply values (append fv gv))))
    (restrict-arity the-combination n)))
;; quoter:end:combine-apply

((combine-apply (lambda (x y z) (list 'foo x y z))
                (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c)

;Value: (foo a b c)
;Value: (bar a b c)

;; quoter:begin:parallel-combine
(define (parallel-combine h f g)
  (compose h (combine-apply f g)))
;; quoter:end:parallel-combine

((parallel-combine (lambda (x y) (- x y))
                   (lambda (a b c) (+ a b c))
                   (lambda (d e f) (+ d e f)))
 1 2 3)

;Value: 0

;; run tests if possible
; (manage 'run-tests)

