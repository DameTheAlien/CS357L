;;4.4
(define deepen-1
  (lambda (ls)
    (cond
      ((null? ls) '())
      (else (cons (list (car ls)) (deepen-1 (cdr ls))))
      )
    )
  )

(deepen-1 '(a b c)) ;; => '((a) (b) (c))

;;4.6
(define insert-left-all
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls)) (cons (insert-left-all new old (car ls)) (insert-left-all new old (cdr ls))))
      ((equal? (car ls) old) (cons new (cons old (insert-left-all new old (cdr ls)))))
      (else (cons (car ls) (insert-left-all new old (cdr ls))))
      )
    )
  )

(insert-left-all 'z 'a '(a ( (b a) ((a (c)))))) ;; => '(z a ((b z a) ((z a (c)))))

;;4.10
(define leftmost
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? (car ls)) (leftmost (car ls)))
      (else (car ls))
      )
    )
  )

(leftmost '((a b) (c (d e)))) ;; => 'a

;;4.11
(define rightmost
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((and (pair? (car ls)) (null? (cdr ls))) (rightmost (car ls)))
      ((null? (cdr ls)) (car ls))
      (else (rightmost (cdr ls)))
      )
    )
  )

(rightmost '((a b) (d (c d (f (g h) i) m n) u) v)) ;; => 'v

;;4.18
(define length-it
  (lambda (ls)
    (letrec
        ((loop
          (lambda (ls iter)
            (cond
              ((null? ls) iter);
              (else (loop (cdr ls) (+ iter 1)))
              )
            )
          ))
       (loop ls 0)
     )
   )
 )

(length-it '(1 2 3)) ;; => 3

;;4.19
(define mk-asc-list-of-ints
  (lambda (n)
    (letrec
        ((loop
          (lambda (ls n)
            (cond
              ((equal? n 0) ls)
              (else (loop (cons n ls) (- n 1)))
             )
            )
          ))
         (loop '() n)
        )
    )
  )

(mk-asc-list-of-ints 10) ;; => '(1 2 3 4 5 6 7 8 9 10)

(define mk-desc-list-of-ints
  (lambda (n)
    (letrec
        ((loop
          (lambda (ls n iter)
            (cond
              ((> iter n) ls)
              (else (loop (cons iter ls) n (+ iter 1)))
             )
           )
         ))
      (loop '() n 1)
   )
  )
)

(mk-desc-list-of-ints 10) ;; => '(10 9 8 7 6 5 4 3 2 1)

;;4.20
(define occurs
  (lambda (item ls)
    (cond
      ((null? ls) 0)
      ((equal? (car ls) item) (+ (occurs item (cdr ls)) 1))
      (else (occurs item (cdr ls)))
    )
  )
)

(occurs 'a '(a b a c a d)) ;; => 3

(define occurs-it
  (lambda (item ls)
    (letrec
        ((loop
          (lambda (item ls iter)
            (cond
              ((null? ls) iter)
              ((equal? (car ls) item) (loop item (cdr ls) (+ iter 1)))
              (else (loop item (cdr ls) iter))
              )
            )
          ))
        (loop item ls 0)
        )
    )
  )

(occurs-it 'a '(a b a c a d)) ;; => 3

;;#2
(define calculator
  (lambda (ls)
    (cond
      ((null? ls) '())   
      ((list? ls)                           
       (let ((num1 (car ls))
             (num2 (caddr ls))
             (oper (cadr ls)))
         ((case oper
            ((+) +)
            ((-) -)
            ((*) *)
            ((/) /))
          (calculator num1) (calculator num2))
         )
       )
      (else ls)
      )                   
    )
  )

(calculator '(1 + (2 * 8))) ;; => 17


;;#3
(define infix->prefix
  (lambda (ls)
    (cond
      ((list? ls)
       (let ((num1 (car ls))
             (num2 (caddr ls))
             (oper (cadr ls)))
         (cons oper (cons (infix->prefix num1) (cons (infix->prefix num2) '())))
         )
       )
      (else ls)
      )
    )
  )

(infix->prefix '(1 + (2 * 8))) ;; => '(+ 1 (* 2 8))


;;#4
(define iota-iota
  (lambda (n)
    (letrec
      ((loop 
	(lambda (m acc)
          (cond
            ((equal? m 0) acc)
            (else (loop (- m 1) (cons (cons m m) acc)))
            )
          )
        )
       )
      (loop n '())
      )
    )
  )

(iota-iota 5) ;; => '((1 . 1) (1 . 2) (1 . 3) (1 . 4) (1 . 5) (2 . 1) (2 . 2) (2 . 3) (2 . 4) (2 . 5) (3 . 1) (3 . 2) (3 . 3) (3 . 4) (3 . 5) (4 . 1) (4 . 2) (4 . 3) (4 . 4) (4 . 5) (5 . 1) (5 . 2) (5 . 3) (5 . 4) (5 . 5))

;;#5
(define digits->number
  (lambda (ls)
    (letrec
        ((loop
          (lambda (n rest)
            (cond
              ((null? rest) n)
              (else (loop (+ (* 10 n) (car rest)) (cdr rest)))
            )
          )
         )
        )
      (loop 0 ls)
     )
    )
  )

(digits->number '(7 6 1 5)) ;; => 7615

;;#6
(define cond->if
  (lambda (ls)
    (letrec
        ((loop
          (lambda (ls iter)
            (cond
              ((null? ls) ls)
              ((equal? (car ls) 'cond) (cons 'if (loop (cdr ls) (+ iter 1))))
              ((equal? (caar ls) 'else) (cons (car ls) (loop (cdr ls) (+ iter 1))))
              ((> iter 1) (cons (cons 'if (cons (car ls) (loop (cdr ls) (+ iter 1)))) '()))
              (else (cons (car ls) (loop (cdr ls) (+ iter 1))))
             )
          )
          ))
         (loop ls 0)
         )
    )
  )

(cond->if '(cond ((> x y) (- x y)) ((< x y) (- y x)) (else 0))) ;; => '(if (> x y) (- x y) (if (< x y) (- y x) 0))

;;#7 isn't tail-recursive but it works
(define cos-m
  (lambda (x)
    (letrec
        ((loop-1
          (lambda (x n)
            (letrec
                ((loop-2
                  (lambda (iter prev out)
                    (cond
                      ((equal? iter (* 2 n)) (+ prev out))
                      (else (loop-2 (+ iter 2)
                                    (/ (* prev (* x x -1)) (+ iter 1) (+ iter 2))
                                    (+ prev out)
                                    )
                            )
                      )
                   )
                  )
                 )
              (loop-2 0 1 0)
              )
            )
          )
         )
      (loop-1 x 100)
      )
    )
  )
    
(cos 3.14159) ;; => -0.9999999999964797
