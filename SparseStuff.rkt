#lang racket

#|
    contains code that defines checks to see if inputed polynomial is sparse.
    also conatins code to convert it to being spare if not
|#

#|defiens how to check if a polynomial is sparse|#

(define(is-sparse polynomial)
    ; if current element is sublist then 
    ; it is a Sparse polynomial
    (if (list? (car polynomial))
        #true
        #false
    )
)

#|defiens how to convert a polynomal into being sparse
    x is the polynomal being converted, and y is the currnet index
|#
(define(to-sparse polynomial index)

   (cond
        ;check if it is empty, and return an empty list
        ((empty? polynomial) '())

        ;check if  element is equail to 0
        ((= (car polynomial) 0) (to-sparse (cdr polynomial) (add1 index)))
        ;otherwise run throuh the rest
        (else 
        (cons (list (car polynomial) index)
            (to-sparse (cdr polynomial) (add1 index)))
        )        
   )
)

;testing to see if checking if sparse works

(display(is-sparse '(1 2 3 0 0)))
(newline)
(display(is-sparse '((1 0) (2 1) (3 2) (9 8))))
(newline)

;testing to see if checking converting to sparse works

(display(to-sparse '(1 2 3 0 0) 0))
(newline)
(display(is-sparse (to-sparse '(1 2 3 0 0) 0)))
(newline)