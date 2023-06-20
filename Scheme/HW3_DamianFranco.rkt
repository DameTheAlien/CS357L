;;PART III
(define select
  (lambda (pred)
    (lambda (ls0 ls1)
      (map cdr
           (filter
            (lambda (x) (pred (car x)))
            (map cons ls0 ls1))))))

(define outer-product
  (lambda (proc)
    (lambda (us vs)
      (map (lambda (u)
             (map (lambda (v) (proc u v)) vs)) us))))

(define iota
  (lambda (n)
    (letrec
        ((loop
          (lambda (n acc)
            (if (= n 0)
                acc
                (loop (sub1 n)
                      (cons n acc))))))
      (loop n '()))))

;;CAN USE apply, select, map, filter, outer-prod, AND iota.

;;1
#|
(define length-m
  (lambda (ls)
    (apply +
           (map (lambda (x) (if (null? x) 0 1)) ls)
     )
   )
 )
(length-m '(1 2 3 4 5))
(length-m '(a b c e d f g))
|#

;;2
#|
(define sum-of-squares
  (lambda args
    (apply + (map (lambda (x) (* x x)) args))
   )
)
(sum-of-squares 1 2 3 4 5)
(sum-of-squares 3 4 5 6) ;;86
|#


;;avg
;;3
#|
(define avg
  (lambda args
    (/ (apply + args)
       (apply +
           (map (lambda (x) (if (null? x) 0 1)) args)
        )
     )
   )
 )
(avg 1 2 3 4 5 6)
(avg 4 5 5 5 5 5) ;; 29/6
|#

;;avg-odd
;;4
#|
(define avg-odd
  (lambda args
    (letrec
        ((help-plz
          (lambda (newLs)
            (/ (apply + newLs)
               (length-m newLs)
              )
            )
          ))
      (help-plz ((select odd?) (iota (length-m args)) args))
      )
    )
  )
(avg-odd 1 2 3 4 5 6)
(avg-odd 4 5 5 5 5 5 5) ;;5
|#

;;shortest
;;5
#|
(define shortest
  (lambda (ls0 ls1)
    (cond
      ((< (length-m ls0) (length-m ls1)) ls0)
      ((> (length-m ls0) (length-m ls1)) ls1)
      (else (display "BOTH ARE EQUAL"))
     )
    )
  )
(shortest '(1 2 3 4 5 6) '(1 2 3))
|#

;;avg-fact
;;6
(define avg-fact
  (lambda args
    (letrec
        ((help-plz
          (lambda (fact-ls)
            (/ (apply + fact-ls)
               (length-m fact-ls)
               )
            )
          ))
      (help-plz (map (lambda (x) (apply * (iota x))) args))
      )
   )
 )
(avg-fact 1 2 3 4 5 6)
(avg-fact 2 3 4) ;;32/3

;;tally
;;7
#|
(define tally
  (lambda (pred)
    (lambda (ls)
      (apply +
           (map (lambda (x) (if (pred x) 1 0)) ls)
         )
      )
   )
 )
((tally odd?) '(1 2 3 4 5 6 7))
((tally even?) '(1 2 3 4 5 6 7))
|#

;;list-ref
;;8
#|
(define list-ref-d
  (lambda (ls n)
    ((select (lambda (x) (if (= n (- x 1)) #t #f)))
     (iota (length-m ls))
     ls
     )
   )
 )
(list-ref-d 1 '(9 4 6 7 5))
|#
 