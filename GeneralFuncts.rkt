
#lang racket

#|check if a polynomial is sparse|#
(define(is-sparse polynomial)
    ; if current element is sublist then 
    ; it is a Sparse polynomial
    (if (list? (car polynomial))
        #true
        #false
    )
)

#|
    convert a polynomal into being sparse
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


#|
    check if the input is the zero polynomial
    works for both types of polynomials
|#
(define(is-zero? polynomial)
    
    (if (is-sparse polynomial)
        ; polynomial is sparse, get inner list
        (is-zero? (car polynomial ))

        ; polynomial isn't sparse so just check if current element (first) is 0
        (if (zero?(car polynomial) )
            #true
            #false
        )
    )
)

(display(is-zero? '(0)))
(newline)
(display(is-zero? '((0 0))))
(newline)

#| 
    returns the degree of the polynomial (degree of the zero polynomial is negative
    infinity: -inf.0)
    converts any dense polynomials following a chekc into sparse sense
    this only works for sparse
|#
(define(degree polynomial)

    (cond
        ;checking if input polynomial is empty
        ((is-zero? polynomial) -inf.0)
        ; read check if the polynomial is dense, if so convert to sparse
        ((not (is-sparse polynomial)) 
        ; read the last element in a spare polynomail which is a list
        ; then reads the last element in that list to get the degree
           (last (last (to-sparse polynomial 0)))
        )
        (else
        ; read the last element in a spare polynomail which is a list
        ; then reads the last element in that list to get the degree
           (last (last polynomial))
        )
   
    )
)
;x^0 + 2x^1 + 3x^2 should return degree 2
(display(degree '(1 2 3 0 0)))
(newline)
;x^0 + 2x^1 + 3x^2 + 9x^ 8 should return degree 8
(display(degree '((1 0) (2 1) (3 2) (9 8))))
(newline)



