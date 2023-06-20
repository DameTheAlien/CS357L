;;(and (equal? (car ls) (cdr ls)) #T (null? ls) #f 'dog);;only evaluates true if all are true

;;(and 't 'jgh 'mei)
;;(and #t #f #f)


;;(cdr (cons (list 1 2 3) 4)
     
;;(car (cdr (list 'cat 'dog)))

;;(or #f #f #t #f 'yes)

;;(null? (cdr (list 1)))

;;(equal? (cons 1 2) (cons 1 2))

;;((lambda (x y) x) 8 2)

;;(append '(1) '(2 3))

;;(list? (cons 1 '(2)))

;;(if #f 'dog 'mouse)

;;(append '(1 (2 3)) '(4 (5 6)))

;;(equal? '(1 (2 3)) '(1 (2 3)))

;;(list? (cons (list 1 2 3) 4))
;;(cons (list 1 2 3) 4)

;;((lambda (x) 7) 8)


;;(cons 'a '(b))
;;(cons '(a) '(b))
;;(cons '(a) 'b)
;;(cons '(a) '())
;;(cons (cons 'a 'b) '())


;;(let ((x 2) (y 3))
  ;;(let ((x 4) (y 5))
    ;;(+ x y)))


;;(let ((x 2) (y 3))
  ;;(let* ((y 4) (y x))
    ;;(list x y)))


;;(let ((car cdr) (cdr car))
  ;;(cdr (car '(a b))))


;;(let* ((car cdr) (cdr car))
  ;;(cdr (car '(a b))))

(define mystery
  (lambda (proc init) ;;1 2
    (letrec
        ((loop
          (lambda (n acc)
            (if (= n 0)
                acc
                (loop (- n 1) (proc n acc)) 
             )
            )
          ))
      (loop 3 init)
      )
    )
  )

;;(mystery + 0)
;;(mystery cons '())
;;(mystery (lambda (x y) y) 1)
;;(mystery (lambda (x y) (cdr y)) '(it was the best of times))

(define powers-of-two
  (lambda (n)
    (letrec
        ((loop
          (lambda (iter ls)
            (cond
              ((< iter 0) ls)
              (else (loop (- iter 1) (append ls (cons (expt 2 iter) '()))))
              )
            )
          ))
      (loop (- n 1) '())
      )
    )
  )

;;(powers-of-two 6)



(define append-m
  (lambda (ls1 ls2)
    (letrec
      ((loop
	(lambda (ls appended-ls)
	  (cond
            ((null? ls) appended-ls)
	    (else (loop (cdr ls) (cons (car ls) appended-ls)))
           )
          )
        ))
      (loop (loop ls1 '()) ls2)
      )
    )
  )

;;(append-m '(1 2 3) '(4 5 6))




(define swap
  (lambda (x y ls)
    (cond
      ((null? ls) '())
      ((list? (car ls))
       (cons (swap x y (car ls)) (swap x y (cdr ls)))
       )
      ((equal? (car ls) x) (append (cons y '()) (swap x y (cdr ls))))
      ((equal? (car ls) y) (append (cons x '()) (swap x y (cdr ls))))
      (else (append (list (car ls)) (swap x y (cdr ls))))
      )
    )
  )


;;(swap 1 2 '(1 2 3 4 8 6 1 2))
;;(swap 'foo 'bar '(foo ((bar)) foo))




    