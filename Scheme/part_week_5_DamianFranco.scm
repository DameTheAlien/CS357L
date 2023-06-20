(define name '("Franco" "Damian"))
(define netID "dfranco24@unm.edu")
;; Define the following functions
;;   each using a single line consisting
;;   of a single call to reduce (foldr)
;;      for an internal lambda,
;;      use (x ls) as the parameter set

(define test '(1 2 3 4 5))
(define list-reduce
    (lambda args
        (foldr cons '() args)
    )
)
(equal? (apply list-reduce test) test)

(define +-reduce
    (lambda args
        (foldr + 0 args)
    )
)
(equal? (apply +-reduce test) (apply + test))


(define filter-reduce
    (lambda (pred? . args)
        (foldr 
		(lambda (x ls) (if (pred? x) (cons x ls) ls)) 
		'() 
		args)
    )
)
(equal? (apply filter-reduce (cons even? test)) (filter even? test))


(define map-reduce
    (lambda (f ls)
        (foldr 
		(lambda (x ls) (if (f x) (cons #t ls) (cons #f ls))) 
		'() 
		ls)
    )
)
(equal? (map-reduce odd? test) (map odd? test))