;;Damian Franco
;;CS-357L(Section 002)
;;Homework 1

;;1.2
(define big-number 10500900)
(define small-number 0.00000025)
(define cheshire 'cat)
(define number1 big-number)
(define number2 'big-number)
;;a (Output = 10500900)
big-number
;;b (Output = 2.5e-007)
small-number
;;c (Output = 'big-number)
'big-number
;;d (Output = 'cat)
cheshire
;;e (Output = 'cheshire)
'cheshire
;;f (Output = 10500900)
number1
;;g (Output = 'big-number)
number2
;;h (Output = 'number1)
'number1

;;1.3
;;a (output = 4)
(- 10 (- 8 (- 6 4)))
;;b (output = 2/5)
(/ 40 (* 5 20))
;;c (output = 2/3)
(/ 2 3)
;;d (output = 0.6666666666666667)
(+ (* 0.1 20) (/ 4 -3))

;;1.4
;;a (Result = 10)
(- (* 4 7) (+ 13 5))
;;b (Result = 6)
(* 3 (+ 4 (- -5 -3)))
;;c (Result = 5.0)
(/ 2.5 (* 5 (/ 1 10)))
;;d (Result = 245073.0)
(* 5(+ 255(* 537(+ 98.3(- 375(* 2.5 153))))))

;;1.5
;;a
;;Given Scheme expression: (+ a (- (+ b c) a)
;;My Arithmetic Expression: (a + (a - (b + c))
;;b
;;Given Scheme Expression: (+ (* a b) (* c b))
;;My Arithmetic Expression: (a * b) + (c * b)
;;c
;;Given Scheme Expression: (/ (- a b) (- a c))
;;My Arithmetic Expression: (a - b) / (a - c)

;;1.6
;;a (Output = (one two three four))
(cons 'one (cons 'two (cons 'three (cons 'four '()))))
;;b (Output = (one (two three four)))
(cons 'one (cons '(two three four) '()))
;;c (Output = (one (two three) four))
(cons 'one (cons '(two three) '(four)))
;;d (Output = ((one two) (three four)))
(cons '(one two) (cons '(three four) '()))
;;e (Output = (((one)))
(cons (cons '(one) '())'())

;;1.10
;;a
;;(symbol? (a b))
;;Output: #f
;;b
;;(pair? (a b))
;;Output: #t
;;c
;;(? (a b))
;;Output: #f
;;d
;;(null? (cdr (cons a '())))
;;Output: #t

;;1.14
;;a (Result: #t)
(symbol? (car '(cat mouse)))
;;b (Result: #f)
(symbol? (cdr '((cat mouse))))
;;c (Result: #f)
(symbol? (cdr '(cat mouse)))
;;d (Result: #t)
(pair? (cons 'hound 'dog))
;;e (Result: #f)
(pair? (car '(cheshire cat)))
;;f (Result: #t)
(pair? (cons '() '()))     

;;2.1 (Output: Second item in list)
(define second
  (lambda(list)
          (car (cdr list))))
(second '(1 7 3 0 4))  

;;2.3
(define make-list-of-one
  (lambda (item)
    (cons item '())))

(define make-list-of-two
  (lambda (item1 item2)
    (cons item1 (make-list-of-one item2))))
  
(define firsts-of-both
  (lambda (list-1 list-2)
    (make-list-of-two (car list-1) (car list-2))))

;;a (Output: '(1 2))
(firsts-of-both '(1 3 5 7) '(2 4 6))
;;b (Output: '((a b) (e f))
(firsts-of-both '((a b) (c d)) '((e f) (g h)))

;;2.4
(define juggle
  (lambda(sexpr)
    (cons (car (cdr (cdr sexpr)))
          (cons (car sexpr)
                (cons (car (cdr sexpr)) '())))))
(juggle '(jump quick spot)) ;;for testing
(juggle '(dog bites man))   ;;for testing

;;2.6
(define a #t)
(define b #t)
(define c #t)
(define e #f)
(define f #f)
;;a (Output: #t)
(and a(or b e))
;;b (Output: #t)
(or e (and (not f) a c))
;;c (Output: #t)
(not (or (not a) (not b)))
;;d (Output: #f)
(and (or a f) (not (or b e)))  

;;2.7
(define expr #t)
;;a (Output: #t)
(or (symbol? expr) (not (symbol? expr)))
;;b (Output: #f)
(and (null? expr) (not (null? expr)))
;;c (Output: #t)
(not (and (or expr #f) (not expr)))
;;d (Output: #f)                   
(not (or expr #t))       

;;2.10
(define last-item
  (lambda (ls)
    (if (null? (cdr ls)) (car ls)
      (last-item (cdr ls)))))

(define member?
  (lambda (item ls)
    (if (null? ls) #f
      (or (equal? (car ls) item)
                (member? item (cdr ls))))))

(define remove-1st
  (lambda (item ls)
    (if (null? ls) '()
      (if (equal? (car ls) item) (cdr ls)
          (cons (car ls) (remove-1st item (cdr ls)))))))

;;Testing
(define x '(1 2 3 4 10))
(last-item x)
(member? 5 x)
(remove-1st 2 x)  

;;2.12
;;The general behavior of the procedure is that the
;;user inputs a list and removes the last item in
;;the list and returns the list without the last item.
;;I would rename it to remove-last
(define mystery ;;or (define remove-last
  (lambda (ls)
    (if (null? (cddr ls))
        (cons (car ls) '())
        (cons (car ls) (mystery (cdr ls))))))
;;Output: '(1 2 3 4)
(mystery '(1 2 3 4 5)) ;;or (remove-last '(1 2 3 4 5))

;;2.13
;;with equal?
(define subst-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) old) (cons new (subst-1st new old (cdr ls))))
      (else (cons (car ls) (subst-1st new old (cdr ls)))))))
;;with eq?
(define substq-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((eq? (car ls) old) (cons new (subst-1st new old (cdr ls))))
      (else (cons (car ls) (subst-1st new old (cdr ls)))))))
;;with eqv?
(define substv-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((eqv? (car ls) old) (cons new (subst-1st new old (cdr ls))))
      (else (cons (car ls) (subst-1st new old (cdr ls)))))))
;;Testing
;;subst-1st
(subst-1st 'dog 'cat '(my cat is clever))
(subst-1st 'b 'a '(c a b a c))
(subst-1st '(0) '(*) '((x) (1) (*) (2)))
(subst-1st 'two 'one '())
;;substq-1st
(substq-1st 'dog 'cat '(my cat is clever))
(substq-1st 'b 'a '(c a b a c))
(substq-1st '(0) '(*) '((x) (1) (*) (2)))
(substq-1st 'two 'one '())
;;substv-1st
(substv-1st 'dog 'cat '(my cat is clever))
(substv-1st 'b 'a '(c a b a c))
(substv-1st '(0) '(*) '((x) (1) (*) (2)))
(substv-1st 'two 'one '())

;;2.14
;;Example procedure
(define insert-right-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) old)
       (cons old (cons new (cdr ls))))
      (else (cons (car ls)
                  (insert-right-1st new old (cdr ls)))))))
;;testing
(insert-right-1st 'not 'does '(my dog does have fleas))
;;My insert-left-1st procedure
(define insert-left-1st
  (lambda (new old ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) old)
       (cons new (cons old (cdr ls))))
      (else (cons (car ls)
                  (insert-left-1st new old (cdr ls)))))))
;;testing
(insert-left-1st 'hot 'dogs '(I eat dogs))
(insert-left-1st 'fun 'games '(some fun))
(insert-left-1st 'a 'b '(a b c a b c))
(insert-left-1st 'a 'b '())

;;2.15
(define list-of-first-items
  (lambda (ls)
    (cond
      ((null? ls) '())
      ((cons (car (car ls)) (list-of-first-items (cdr ls))))
      (else (cons '() '()))
      )))
;;testing
(list-of-first-items '((a) (b c d) (e f)))
(list-of-first-items '((1 2 3) (4 5 6))) 
(list-of-first-items '((one)))
(list-of-first-items '()) 

;;2.16
(define replace
  (lambda (new ls)
    (cond
      ((null? ls) '())
      ((equal? new (car ls)) '())
      (else (cons new (replace new (cdr ls))))
      )))
;;testing
(replace 'no '(will you do me a favor))
(replace 'yes '(do you like ice cream))
(replace 'why '(not))
(replace 'maybe '())

;;2.18
;;NOTE: These procedures work for most tests but does
;;not work for the second test case given to us, and
;;other test case so this is janky but works 80% of
;;the time.
;;
;;The remove-1st procedure given to us in the book
;;but I modified it to reverse the list to return
;;the right output aka removes the last occurance.
(define remove-1st
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((equal? (car ls) item) (cdr ls))
      (else (reverse (cons (car ls) (remove-1st item (cdr ls))))
            ))))
;;My remove-last procedure which calls my remove-1st
;;procedure for the list in reverse.
(define remove-last
  (lambda (item ls)
    (cond
      ((null? ls) '())
      ((remove-1st item (reverse ls)))
      (else '()))))
;;testing
(remove-last 'a '(b a n a n a s))
(remove-last 'a '(b a n a n a))
(remove-last 'a '())                                                 
