(define name '("Franco" "Damian"))
(define netID "dfranco24@unm.edu")


;; === Part 1, Points: 10, Weight: 1/3 ===
(define compose
  (lambda (f g)
    (lambda (x)
      (f (g x))
      )
    )
)

(define id (lambda (x) x))

;; problem1-1-7.2    p234
;; Points: 1/10
;; ((compose3 id id id) 4) => 4
(define compose3
  (lambda (f g h)
    (lambda (x)
      (f (g (h x)))
      )
    )
)


;; problem1-1-7.3    p234
;; Points: 1/10
;; ((compose-many id id id id) 4) => 4
(define compose-many
  (lambda args
    (cond
      ((null? args) x)
      (else
       ((compose (car args)
                 (apply compose-many (cdr args))) x))
      )
    )
  )
)


;; problem1-1-7.6    p235
;; Points: 1/10
;; (map-first-two + '(2 3 4 5 7)) => '(5 7 9 12)
(define map-first-two
  (lambda (proc ls)
    (cond
      ((null? (cdr ls)) '())
      (else (cons (proc (car ls) (car (cdr ls)))
                  (map-first-two proc (cdr ls))))
      )
    )
)


;; problem1-1-7.7    p235
;; Points: 1/10
;; (reduce + '(2 3 4 5 7)) => 21
(define reduce
  (lambda (proc ls)
    (cond
      ((null? (cdr ls)) (car ls))
      (else (reduce proc (cons (proc (car ls) (car (cdr ls))) (cddr ls))))
     )
   )
)


;; problem1-1-7.8    p236
;; Points: 1/10
;; (andmap positive? '(3 4 6 9)) => #t
(define andmap
  (lambda (pred ls)
    (cond
      ((null? ls) #t)
      ((pred (car ls)) (andmap pred (cdr ls)))
      (else #f)
      )
    )
)


;; problem1-1-7.12-a    p243
;; Points: 0.5/10
;; ((curried* 5) 25) => 125
(define curried*
  (lambda (m)
    (lambda (n)
      (* m n)
      )
    )
)


;; problem1-1-7.12-b    p243
;; Points: 0.5/10
;; (times10 580) => 5800
(define times10
  (lambda (x)
    ((curried* x) 10)
    )
)


;; problem1-1-7.18-a    p244
;; Points: 0.5/10
;; (between? 4 5 6) => #t
(define between?
  (lambda (x y z)
    (if (< x y)
        (if (< y z)
            #t
            #f)
        #f
        )
    )
)


;; problem1-1-7.18-b    p244
;; Points: 0.5/10
;; (((between?-c 4) 5) 6) => #t
(define between?-c
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (if (< x y)
            (if (< y z)
                #t
                #f)
            #f
            )
        )
      )
   )
)


;; problem1-1-7.22    p250
;; Points: 1/10
;; ((mult-by-scalar 3) '(1 -2 3 -4)) => '(3 -6 9 -12)
(define mult-by-scalar
  (lambda (c)
    (flat-recur
     '()
     (lambda (x y)
       (cons (* c x) y)
       )
     )
    )
)

(define deep-recur
  (lambda (seed item-proc list-proc)
    (letrec
        (
         (helper
          (lambda (ls)
            (if (null? ls)
                seed
                (let ((a (car ls)))
                  (if (or (pair? a) (null? a))
                      (list-proc (helper a) (helper (cdr ls)))
                      (item-proc a (helper (cdr ls)))
                      )
                  )
                )
            )
          )
         )
      helper
      )
    )
)

;; problem1-1-7.30    p
;; Points: 1/10
;; (reverse-all '(1 (2 3) 4)) => '(4 (3 2) 1)
(define reverse-all
  (deep-recur
   '()
   (lambda (x y)
     (append y (cons x '()))
     )
   (lambda (x y)
     (append y (cons x '()))
     )
   )
)


;; problem1-1-7.31    p
;; Points: 1/10
;; ((flat-recur 0 +) '(1 2 3)) => 6
(define flat-recur
  (lambda (seed list-proc)
    (deep-recur
     seed
     list-proc
     list-proc
     )
    )
)


;; === Part 2, Points: 10, Weight: 1/3 ===

;; problem2-1-a
;; Points: 1/8
;; ((tail-recur zero? sub1 * 1) 5) => 120
(define tail-recur
  (lambda (bpred xproc aproc acc0)
    (letrec
        ((outer
          (lambda (n)
            (letrec
                ((loop
                  (lambda (x acc)
                    (if (list? x)
                        (if
                         (bpred x)
                         acc
                         (loop (xproc x) (aproc (car x) acc))
                         )
                        (if
                         (bpred x)
                         acc
                         (loop (xproc x) (aproc x acc))
                         )
                      )
                    )
                  ))
              (loop n acc0)
              )
            )
          ))
      outer
    )
  )
)

;; problem2-1-b
;; Points: 1/8
;; (reverse '(1 2 3)) => '(3 2 1)
(define reverse
  (tail-recur null? cdr cons '()) 
)

;; problem2-1-c
;; Points: 1/8
;; (iota 3) => '(1 2 3)
(define iota
  (tail-recur zero? (lambda (x) (- x 1)) cons '())
)

;; problem2-2-
;; Points: 1/8
;; ((disjunction2 symbol? procedure?) +) => #t
(define disjunction2
    (lambda (pred1 pred2)
      (lambda (arg)
        (cond
          ((and (pred1 arg) (pred2 arg))
           (if (or (procedure? arg) (symbol? arg))
               #t
               arg
               )
           )
          ((pred1 arg)
           (if (or (procedure? arg) (symbol? arg))
               #t
               arg
               )
           )
          ((pred2 arg)
           (if (or (procedure? arg) (symbol? arg))
               #t
               arg
               )
           )
          (else #f)
          )
        )
      )
  )

;; problem2-3-
;; Points: 1/8
;; ((disjunction procedure?) +) => #t
(define disjunction
  (lambda preds
    (lambda (arg)
      (cond
        (((car preds) arg)
         (if (or (procedure? arg) (symbol? arg))
             #t
             (apply disjunction (cdr preds))
             )
         )
        (else (apply disjunction (cdr preds))))
      )
    )
)

;; problem2-4-
;; Points: 1/8
;; (matrix-map even? '((1 2) (3 4))) => '((#f #t) (#f #t))
(define matrix-map
  (lambda (f A)
    (cons (map f (car A)) (cons (map f (cadr A)) '()))
    )
)

;; problem2-5
(define fold
  (lambda (seed proc)
    (letrec
        ((pattern
          (lambda (ls)
            (if (null? ls)
                seed
                (proc (car ls) (pattern (cdr ls)))
                )
            )
          )
         )
      pattern
      )
    )
)


;; problem2-5-a
;; Points: 1/8
;; (delete-duplicates '(a b a b a b a b)) => '(a b)
(define delete-duplicates
  (fold
   '()
   (lambda (x y)
     (if ((lambda (last y)
            (sequence-ormap
             (lambda (x)
               (equal? last x))
             y))
            x y)
         y
         (cons x y)
         )
     )
   )
)

;; problem2-5-b
;; Points: 1/8
;; (assoc 'b '((a 1) (b 2))) => '(b 2)
(define assoc
  (lambda (item ls)
    ((fold
      #f
      (lambda (x y)
        (cond
          ((equal? (car x) item) x)
          (else y)
          )
        )
      )
     ls
     )
    )
)


;; === Part 3, Points: 8, Weight: 1/3 ===

;; problem3-1-
;; Points: 1/8
;; (length '(1 2 3 4)) => 4
(define length
  (lambda (ls)
    (apply +
           (map
            (lambda (x) (if (null? x) 0 1))
            ls)
           )
    )
)

;; problem3-2-
;; Points: 1/8
;; (sum-of-squares 3 4 5 6) => 86
(define sum-of-squares
  (lambda args
    (apply + (map (lambda (x) (* x x)) args))
    )
)

;; problem3-3-
;; Points: 1/8
;; (avg 4 5 5 5 5 5) => 29/6
(define avg
  (lambda args
    (/ (apply + args)
       (apply +
              (map (lambda (x) (if (null? x) 0 1)) args)
              )
       )
    )
)

;; problem3-4-
;; Points: 1/8
;; (avg-odd 4 5 5 5 5 5) => 5
(define avg-odd
  (lambda args
    (letrec
        ((help-plz
          (lambda (newLs)
            (/ (apply + newLs)
               (length newLs)
               )
            )
          ))
      (help-plz ((select odd?) (iota (length args)) args))
      )
    )
)

;; problem3-5-
;; Points: 1/8
;; (shortest '(1 2) '(2 3 4) '(4) '(5 6 7 8)) => '(4)
(define shortest
  (lambda lists
    (map length lists)
    )
)

;; problem3-6-
;; Points: 1/8
;; (avg-fact 2 3 4) => 32/3
(define avg-fact
  (lambda args
    (letrec
        ((help-plz
          (lambda (fact-ls)
            (/ (apply + fact-ls)
               (length fact-ls)
               )
            )
          ))
      (help-plz (map (lambda (x) (apply * (iota x))) args))
      )
    )
)

;; problem3-7-
;; Points: 1/8
;; ((tally even?) '(1 2 3 4 5)) => 2
(define tally
  (lambda (pred)
    (lambda (ls)
      (apply +
             (map
              (lambda (x) (if (pred x) 1 0))
              ls)
             )
      )
    )
)

;; problem3-8-
;; Points: 1/8
;; (list-ref '(5 7 60 98) 0) => 5
(define list-ref
  (lambda (ls n)
    ((select (lambda (x) (if (= n (- x 1)) #t #f)))
     (iota (length ls))
     ls
     )
    )
)
