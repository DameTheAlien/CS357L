
;; Fill in your name and net ID below
;;  replacing Last with your last name(s)
;;  replacing First with your first name(s)
;;  and replacing netID@unm.edu with your unm email address
(define name '("Franco" "Damian"))
(define netID "dfranco24@unm.edu")

;; If you accept the academic honesty pledge, replace the "" below with
;;      "I accept and agree to the academic honesty pledge"
(define academic-honesty-pledge
    "I accept and agree to the academic honesty pledge")


;; Fill out the following defintions in wichever order you choose
;;  your final submission should evaluate without errors.
;; Each defintion comes pre-filled as '(), delete the '() prior to
;;  defining your function. Do not replace it with another define:
;;  i.e. The following defintion is WRONG:
;;      (define func
;;          (define func
;;              (lambda ...
;;
;; Do NOT submit your answer as a quoted list, i.e. the following 
;;  definition is also WRONG, as the quote was not removed:
;;      (define func
;;          '(lambda ...


;; ======= Variadic Functions =======

;; Using the instructions provided on LEARN, give a defintion for number
(define number
    (lambda args
      (letrec
          ((loop
            (lambda (ls iter)
              (cond
                ((null? ls) iter)
                (else (loop (cdr ls) (+ iter 1)))
              )
             )
            ))
        (loop args 0)
          )
      )
)


;; ============ Currying ============

;; Using the instructions provided on LEARN, give a defintion for sum-of-squares-c
(define sum-of-squares-c
    (lambda (a)
      (lambda (b)
        (lambda (c)
          (lambda (d)
            (+ (* a a) (* b b) (* c c) (* d d))
            )
          )
        )
      )
)

;; Using the instructions provided on LEARN, give a defintion for curry4
(define curry4
    (lambda (proc)
      (lambda (a)
        (lambda (b)
          (lambda (c)
            (lambda (d)
              (+ (* a a) (* b b) (* c c) (* d d))
              )
            )
          )
        )
      )
)

;; ============== Fold ==============

;; You may use the definition of fold provided
(define fold
  (lambda (proc seed)
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
      pattern)
    )
  )

;; Using the instructions provided on LEARN, give a defintion for snoc
(define snoc
  (lambda (ls item)
    (letrec
        ((proc-call
          (fold
           (lambda (x y)
             (cons x y)
             )
           item
           )
          ))
      (proc-call ls)
        )
    )
)

;; Using the instructions provided on LEARN, give a defintion for take-while
(define take-while
    (lambda (pred ls)
      (letrec
          ((proc-call
            (fold
             (lambda (x y)
               (cond
                ((pred x) (cons x y))
                (else '())
                )
               )
             '()
             )
            ))
        (proc-call ls)
        )
      )
)

;; ===== Procedural Abstraction =====

;; Using the instructions provided on LEARN, give a defintion for fold-tree
(define fold-tree
  (lambda (proc seed spred)
    (letrec
        ((loop
          (lambda (ls)
            (cond
              ((null? ls) 0)
              ((pair? ls)
               (proc (loop (car ls))
                     (loop (cdr ls))
                     ) 
               )
              (else spred seed)
              )
            )
          ))
         loop
        )
    )
)

;; Using the instructions provided on LEARN, give a defintion for depth
(define depth
  (fold-tree
   (lambda (x y)
     (add1 (max x y))
     )
   0
   (lambda (x) x)
   )
)


;; Using the instructions provided on LEARN, give a defintion for count
(define count-m
    (lambda (pred sexpr)
        (cond
          ((null? sexpr) 0)
          ((pair? sexpr)
           (+ (count-m pred (car sexpr))
              (count-m pred (cdr sexpr))))
          ((pred sexpr) 1)
          (else 0)
          )
      )
  )

(define count
    (lambda (pred ls)
      ((count-c pred) ls)
     )
)

(define count-c
  (lambda (pred)
    (lambda (ls)
      (letrec
          ((proc-call
            (fold-tree
             (lambda (x y)
               (+ x y)
               )
             (list ls)
             (lambda (x) (if (pred x) 1 0))
             )
            ))
        (proc-call ls)
      )
      )
    )
  )
  (count even? '((1 . 2) . (3 . 4)))

;; ========== Flatten Tree ==========

;; Using the instructions provided on LEARN, give a defintion for flatten-tree
(define flatten-tree
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((pair? ls)
       (append (flatten-tree (car ls)) 
               (flatten-tree (cdr ls))
             ) 
       )
      (else (list ls))
      )
    )
)

;; Using the instructions provided on LEARN, give a defintion for map-tree
(define map-tree
    '()
)
