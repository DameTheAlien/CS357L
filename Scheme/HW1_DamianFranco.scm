(define name '("Damian" "Franco"))
(define netID "dfranco24")

;; For prompts providing double quotes, fill in the exact answer in the quotes:
;;      e.g. for problem n.n,
;;          with a prompt: "what is the result of running the code ''(+ 1 2)"
;;          and the skeleton: (define problemn.n "")
;;          you should fill in: (define problemn.n "''(+ 1 2)")

;; For prompts providing the empty list, replace the empty list with the exact result or function contents:
;;      e.g. for problem n.n,
;;          with a prompt: "give code which produces ''(+ 1 2)"
;;          and the skeleton: (define problemn.n '())
;;          you should fill in: (define problemn.n ''(+ 1 2))
;;      e.g. for problem n.n,
;;          with a prompt: "what is the result of running the code (/ 1 2)"
;;          and the skeleton: (define problemn.n '())
;;          you should fill in: (define problemn.n 1/2)
;;
;;      Do NOT, when required to provide an exact answer, merely give the code you are required to evaluate
;;      e.g. for problem n.n,
;;          with a prompt: "what is the result of running the code (/ 1 2)"
;;          and the skeleton: (define problemn.n '())
;;          you should NOT fill in: (define problemn.n (/ 1 2))


;; problem 1.2, page 13
(define problem1.2
    '(
        ('a "10500900")
        ('b "2.5e-007")
        ('c "'big-number")
        ('d "'cat")
        ('e "'cheshire")
        ('f "10500900")
        ('g "'big-number")
        ('h "'number1")
    )
)


;; problem 1.3, page 13
(define problem1.3
    '(
        ('a 4)
        ('b 2/5)
        ('c 2/3)
        ('d 0.6666666666666667)
    )
)


;; problem 1.4, page 13
(define problem1.4
    '(
        ('a '(- (* 4 7) (+ 13 5)))
        ('b '((* 3 (+ 4 (- -5 -3)))))
        ('c '((/ 2.5 (* 5 (/ 1 10)))))
        ('d '((* 5(+ 255(* 537(+ 98.3(- 375(* 2.5 153))))))))
    )
)


;; problem 1.5, page 14
(define problem1.5
    '(
        ;;changed '() to "" because it just makes more sense in this context.
        ('a "(a + (a - (b + c))") ;;((a + (a - (b + c)))
        ('b "(a * b) + (c * b)") ;;((a * b) + (c * b))
        ('c "(a - b) / (a - c)") ;;((a - b) / (a - c))
     )
)


;; problem 1.6, page 19
(define problem1.6
    '(
        ('a '((cons 'one (cons 'two (cons 'three (cons 'four '()))))))
        ('b '((cons 'one (cons '(two three four) '()))))
        ('c '((cons 'one (cons '(two three) '(four)))))
        ('d '((cons '(one two) (cons '(three four) '()))))
        ('e '((cons (cons '(one) '())'())))
    )
)


;; problem 1.10, page 25
(define problem1.10
    '(
        ('a #f)
        ('b #t)
        ('c #f)
        ('d #t)
    )
)


;; problem 1.14, page 30
(define problem1.14
    '(
        ('a #t)
        ('b #f)
        ('c #f)
        ('d #t)
        ('e #f)
        ('f #t)
    )
)


;; problem 2.1, page 39
(define second
  (lambda(list)
    (car (cdr list)))
)


;; problem 2.3, page 39
(define make-list-of-one
  (lambda (item)
    (cons item '()))
)
(define make-list-of-two
  (lambda (item1 item2)
    (cons item1 (make-list-of-one item2)))
)
(define firsts-of-both
  (lambda (list-1 list-2)
    (make-list-of-two (car list-1) (car list-2)))
)
(define problem2.3
    '(
        ('a '(1 2))
        ('b '((a b) (e f)))
    )
)


;; problem 2.4, page 39
(define juggle
  (lambda(sexpr)
    (cons (car (cdr (cdr sexpr)))
          (cons (car sexpr)
                (cons (car (cdr sexpr)) '()))))
)


;; problem 2.6, page 45
(define a #t)
(define b #t)
(define c #t)
(define e #f)
(define f #f)
(define problem2.6
    '(
        ('a #t)
        ('b #t)
        ('c #t)
        ('d #f)
    )
)


;; problem 2.7, page 45
(define expr #f)
(define problem2.7
    '(
        ('a #t)
        ('b #f)
        ('c #t)
        ('d #f)
    )
)

(if (equal? 1 2)
    (cons '(YES) '())
    (cons '(No) '())


;; problem 2.10, page 53
(define last-item
  (lambda (ls)
    (if (null? (cdr ls)) (car ls)
        (last-item (cdr ls))))
)
(define member?
  (lambda (item ls)
    (if (null? ls) #f
        (or (equal? (car ls) item)
                (member? item (cdr ls)))))
)
(define remove-1st
  (lambda (item ls)
    (if (null? ls) '()
      (if (equal? (car ls) item) (cdr ls)
          (cons (car ls) (remove-1st item (cdr ls))))))
)


;; problem 2.12, page 54
(define mystery
  (lambda (ls)
    (if (null? (cddr ls))
        (cons (car ls) '())
        (cons (car ls) (mystery (cdr ls)))))
)
(define problem2.12
    '(
        ('a '(1 2 3 4))
        ('b "The general behavior of the procedure is that the user inputs a list and removes the last item.") ;; BEHAVIOR
        ('b "remove-last") ;; REASONABLE NAME
    )
)


;; problem 2.13, page 54
(define subst-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) old) (cons new (subst-1st new old (cdr ls))))
      (else (cons (car ls) (subst-1st new old (cdr ls))))))
)

;; problem 2.14, page 55
(define insert-left-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) old)
       (cons new (cons old (cdr ls))))
      (else (cons (car ls)
                  (insert-left-1st new old (cdr ls))))))
)


;; problem 2.15, page 55
(define list-of-first-items
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((cons (car (car ls))
             (list-of-first-items (cdr ls))))
      (else (cons '() '()))))
)


;; problem 2.16, page 56
(define replace
  (lambda (new ls)
    (cond
      ((null? ls) '())
      ((equal? new (car ls)) '())
      (else (cons new (replace new (cdr ls))))))
)


;; problem 2.18, page 56
(define remove-1st
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (cdr ls))
      (else (reverse (cons (car ls) (remove-1st item (cdr ls))))
            ))))
(define remove-last
    '((lambda (item ls)
    (cond
      ((null? ls) '())
      ((remove-1st item (reverse ls)))
      (else '()))))
)