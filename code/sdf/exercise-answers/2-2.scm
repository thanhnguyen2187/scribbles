(load "./sdf/manager/load")
(manage 'new-environment 'combinators)

;; quoter:begin:compose
(define (compose f g)
  (let ((g-arity-min (procedure-arity-min (procedure-arity g)))
        (g-arity-max (procedure-arity-max (procedure-arity g)))
        (f-arity-min (procedure-arity-min (procedure-arity f)))
        (f-arity-max (procedure-arity-max (procedure-arity f))))
    (define (the-composition . args)
      (let ((n (length args)))
        (assert (<= g-arity-min n))
        (assert (or (not g-arity-max)
                    (<= n g-arity-max)))
        (assert (or (not f-arity-max)
                    (>= f-arity-min 1)))
        (f (g args))))
    the-composition))
;; quoter:end:compose

((compose list
          (lambda (x) (list 'bar x)))
 'z)
;Value: ((bar (z)))

;; quoter:begin:parallel-combine
(define (parallel-combine h f g)
  (let ((g-arity-min (procedure-arity-min (procedure-arity g)))
        (g-arity-max (procedure-arity-max (procedure-arity g)))
        (f-arity-min (procedure-arity-min (procedure-arity f)))
        (f-arity-max (procedure-arity-max (procedure-arity f)))
        (h-arity-min (procedure-arity-min (procedure-arity h)))
        (h-arity-max (procedure-arity-max (procedure-arity h))))
    (assert (or (not h-arity-max)
                (>= h-arity-min 2)))
    (define (the-combination . args)
      (let ((n (length args)))
        (assert (>= n g-arity-min))
        (assert (>= n f-arity-min))
        (assert (or (not g-arity-max)
                    (<= n g-arity-max)))
        (assert (or (not f-arity-max)
                    (<= n f-arity-max))))
      (h (apply f args)
         (apply g args)))
    the-combination))
;; quoter:end:parallel-combine

((parallel-combine list list list) 1 2 3 4 5)
;Value: ((1 2 3 4 5) (1 2 3 4 5))

;; quoter:begin:spread-combine
(define (spread-combine h f g)
  (let ((f-arity (get-arity f))
        (g-arity-min (procedure-arity-min (procedure-arity g)))
        (g-arity-max (procedure-arity-max (procedure-arity g)))
        (h-arity-min (procedure-arity-min (procedure-arity h)))
        (h-arity-max (procedure-arity-max (procedure-arity h))))
    (define (the-combination . args)
      (let* ((n (length args))
             (n-remains (- n f-arity)))
        (assert (<= g-arity-min n-remains))
        (assert (<= h-arity-min 2))
        (h (apply f (list-head args f-arity))
           (apply g (list-tail args f-arity)))))
    the-combination))
;; quoter:end:spread-combine

((spread-combine list
                 (lambda (x y)
                   (list x y))
                 (lambda (u v w)
                   (list w v u)))
 'a 'b 'c 'd 'e)
;Value: ((a b) (e d c))

((spread-combine list
                 (lambda (x y) (list x y))
                 list)
 1 2 3 4)
;Value: ((1) (2 3 4))

