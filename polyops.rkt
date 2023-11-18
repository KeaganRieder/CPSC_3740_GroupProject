#lang racket

; checks if polynomial is sparse (has sub list)
(define(is-sparse polynomial)

    (if (empty? polynomial)
        #false
        ; if current element is sublist then 
        ; it is a Sparse polynomial
        (if (list? (car polynomial))
            #true
            #false
        )
    )
)

; convert a polynomal into being sparse

(define(to-sparse polynomial index)

   (cond
        ; checking if polynomial is already sparse
        ((is-sparse polynomial) polynomial)
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


; takes a polynomial p(x) and a value k, returns the result of p(k).
; only works for sparse
(define (eval polynomial k)

    (cond
        ; check if empty
        ((empty? polynomial) '(0))
        ; check if 0
        ((is-zero? polynomial ) '(0))
        ; check if  not sparse and then converting
        ((not (is-sparse polynomial)) (eval (to-sparse polynomial 0) k))

        ; evaluating
        (else
            (apply + (map (lambda (term)
                (* (first term) (expt k (second term))))
                polynomial
            ))
        )
    )
)
    
;testing eval
(display(eval  '(1 2 3 0 0) 2)) ; (2)^0 + 2(2)^1 + 3(2)^2 = 17
(newline)
(display(eval  '((1 0) (2 1) (3 2) (9 8)) 2)) ; (2)^0 + 2(2)^1 + 3(2)^2 + 9(2)^ 8 = 2321
(newline)

; adds two polynomials and returns the sum