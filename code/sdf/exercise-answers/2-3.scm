(load "./sdf/manager/load")
(manage 'new-environment 'combinators)

;; quoter:begin:function-1
('write 'implementation 'here)
;; quoter:end:function-1

;; run tests if possible
; (manage 'run-tests)

(values (list 1 2 3) (list 4 5 6))

(define (spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (values (apply f (list-head args n))
                (apply g (list-tail args n))))
      (restrict-arity the-combination t))))

(define (spread-apply f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (let-values ((fv (apply f (list-head args n)))
                     (gv (apply g (list-tail args n))))
          (apply values (append fv gv))))
      (restrict-arity the-combination t))))

(define (spread-combine h f g)
  (compose h (spread-apply f g)))

(define (compose f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args))
                      f))
  (restrict-arity the-composition (get-arity g)))
