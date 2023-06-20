(define name '("Franco" "Damian"))
(define netID "dfranco24@unm.edu")

;; Example 1
(define fact
  (lambda (x)
    (letrec
        ((loop
          (lambda (x acc)
            (if (= x 0)
                acc
                (loop (sub1 x) (* x acc))
                )
            )
          )
         )
      (loop x 1)
)))

;; Example 2
(define reverse
  (lambda (x)
    (letrec
        ((loop
          (lambda (x acc)
            (if (null? x)
                acc
                (loop (cdr x) (cons (car x) acc))
                )
            )
          )
         )
      (loop x '())
)))

;; Example 3
(define iota
  (lambda (x)
    (letrec
        ((loop
          (lambda (x acc)
            (if (= x 0)
                acc
                (loop (sub1 x) (cons x acc))
                )
            )
          )
         )
      (loop x '())
)))

;;#1
(define tail-recur
  (lambda (bpred xproc aproc acc0)
    (lambda (x)
      (letrec
          ((loop
            (lambda (x acc)
              (if (bpred x)
                  acc
                  (loop (xproc x) (aproc x acc))
                  )
              )
            ))
        (loop x acc0)
          )
      )
    )
)

;;#2
(define length-tr
  (tail-recur null? cdr (lambda (x y) (+ y 1)) 0)
)

;;#3
(define foldr-tr
  (lambda (proc init ls)
    (letrec
        ((loop
          (tail-recur null? cdr (lambda (x y) (proc (car x) y)) init)
          ))
      (loop ls)
        )
    )
)

#|
;;Testing:
(length-tr '(a b c d e f))
(length-tr '(1 2 3))
(length-tr '(1 2 c d))
(foldr-tr + 0 '(1 2 2))
(foldr-tr - 0 '(1 2 3 4 5))
(foldr-tr * 0 '(2 4 6 8))
|#




