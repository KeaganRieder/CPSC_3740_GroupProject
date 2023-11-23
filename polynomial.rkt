#lang racket
#|
###########################################################################
    About
    3740 group Project
    by Keagan Rieder and Justin Wolfenden

    a collection of functions to perform operations on polynomail repsent 
    as either sparse or dense. the following is summary of the operation
    that this program is capable of:
    > check and convert type of polynomial 
    > perform mathmatical operation on between polyniomals.
    > find different part of polynomials, like the
        >> find the degree and coeeficent
        >> if it's a zero polynoimal
############################################################################
|#

#|
############################################################################
    defining functions to check if it's dense, sparse, or zero
    as well as convert between the dense or sparse
############################################################################
|#

; check if the input is the zero polynomial
; works for both types of polynomials
(define(is-sparse? polynomial)

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
    ; checking if polynomial is already sparse
    ( if (is-sparse? polynomial) 
        polynomial
        ; it isn't so convert
        (make-sparse polynomial index)
    )
    
)

;helper function for to-sparse
(define (make-sparse polynomial index)

    (cond
        ;check if it is empty, and return an empty list
        ((empty? polynomial) '())

        ;check if  element is equail to 0
        ((= (car polynomial) 0) (make-sparse (cdr polynomial) (add1 index)))
        ;otherwise run throuh the rest
        (else 
        (cons (list (car polynomial) index)
            (make-sparse (cdr polynomial) (add1 index)))
        )        
   )
)

; x is a list, returns false if list is sparse and true if list is dense
(define (is-dense? x)
  ;is first elment a sub list in a list?
  (if (list? (car x))
      false
      true))

; converts from sparse to dense  x must be a sparse list or it will error, set y to 0 for recursion
(define (to-dense x y)
    ; check if x is dense
    (if (is-dense? x)
      ; x is dense so just retunr it
      x
      ; x isn't dense so convert
      (make-dense x y))
)

; helper function for to dense, that converts th epolynomial to a dense type
(define (make-dense x y)
    
    ; is it empty
    (if (null? x)
        null

        ;its not empty, lets assume it is sparse, is the second value of the first list the value we are curently looking for?
        (if (equal? y (car (cdr (car x))))
            ;it is, add the value to the list
            (append (list (list-ref (car x) 0)) (make-dense (cdr x) (+ 1 y)))
            ;its not add 0 to the list and look for the next value
            (append (list 0) (make-dense x (+ 1 y)))))  
)

; check if the input is the zero polynomial
; works for both types of polynomials
(define(is-zero? polynomial)
    
    (if (is-sparse? polynomial)
        ; polynomial is sparse, get inner list
        (is-zero? (car polynomial ))

        ; polynomial isn't sparse so just check if current element (first) is 0
        (if (zero?(car polynomial) )
            #true
            #false
        )
    )
)

#|
############################################################################
    defining function to check the degree and coefficient
############################################################################
|#

; returns the coefficent of x^y converts all lists to dense for ease of implementation negative y will break this
(define (coeff x y)
  ;round about way of asking is y less then or equal to length x
  (if (negative? (- y (length x)))
              (list-ref (to-dense x) y)
              ;returns false if y will not work
              false))


; returns the degree of the polynomial (degree of the zero polynomial is negative
; infinity: -inf.0)
; converts any dense polynomials following a chekc into sparse sense
; this only works for sparse

(define(degree polynomial)

    (cond
        ;checking if input polynomial is empty
        ((is-zero? polynomial) -inf.0)
        ; read check if the polynomial is sparse, if so convert to sparse
        ((is-sparse? polynomial) 
        ; read the last element in a spare polynomail which is a list
        ; then reads the last element in that list to get the degree
           (last (last polynomial ))
        )
        ; it's not sparse meaning it's dense so convert to sparse
        (else
            (degree (to-sparse polynomial 0))
        )
   
    )
)

#|
############################################################################
    defining function to perform mathmatical operation between polynomials
############################################################################
|#

; takes a polynomial p(x) and a value k, returns the result of p(k).
; only works for sparse
(define (eval polynomial k)

    (cond
        ; check if empty
        ((empty? polynomial) '(0))
        ; check if 0
        ((is-zero? polynomial ) '(0))
        ; check if  not sparse and then converting
        ((not (is-sparse? polynomial)) (eval (to-sparse polynomial 0) k))

        ; evaluating
        (else
            (apply + (map (lambda (term)
                (* (first term) (expt k (second term))))
                polynomial
            ))
        )
    )
)

; takes two polynomials and adds them broken atm need to fix

(define (add x y)
    ; check if both polynomials are sparse
    (if (and (is-sparse? x) (is-sparse? y))
        ; both are sparse polynomials
        (to-sparse(add-Poly (to-dense x 0) (to-dense y 0)) 0)
    
        ; one of them isn't convert which ever one is 
        ; sparse to be dense and call helper function to calcualte addtion
        (add-Poly (to-dense x 0) (to-dense y 0)))
)

; helper function to handle adding to polynomials together
(define (add-Poly x y) 
    (cond
        ;check if both lists are empty
        ((and (empty? x) (empty? y) ) '())

        ;x and y are both not empty, see if x is
        ((empty? x) (append (list (car x)) ( add-Poly x (cdr y))))

        ;x isn't empty, so see check if y is empty
        ((empty? y) 
        
            (append (list (car  x)) (add-Poly (cdr x)  y))
        )

        ; otherwise add
        (else 

            (append (list (+ (car x ) (car y)))  (add-Poly (cdr x) (cdr y)))
        )
    )
)

; helper function to make a ploynomials coefficent negative
; primarly allows for the add-Poly fucntion to be reused
(define (invert-coef polynoimal)
    (map (lambda (x) (* -1 x)) polynoimal)
)

; subtracts polynomial y from polynomial X
; first checks the types of poly nomials
(define (subtract x y)
    ; check if both polynomials are sparse
    (if (and (is-sparse? x) (is-sparse? y))
        ; both are sparse polynomials
        (to-sparse (add-Poly (to-dense x 0) (invert-coef (to-dense y 0))) 0)
    
        ; one of them isn't convert which ever one is 
        ; sparse to be dense and call helper function to calcualte addtion
        (add-Poly (to-dense x 0) (invert-coef (to-dense y 0)))
    )
)

; helper function to multiply polynomial term coef x by term coef y
(define (multiply-coeffs x y)
    (*  x  y)
)
; helper function to  polynomial increase polynomials terms x power by y
(define (increase-power x y)
    (+  x  y)
)
; helper function to multiply polynomial term x by term y
(define (multiply-terms x y)
   
    (list (list
        (multiply-coeffs (car x) (car y))
        ;cadrmeans get character of the next element form current point
        (increase-power (cadr x) (cadr y)) )
    )
)

; helper function to apply polynomials y term to polynomial x
(define (apply-y-to-x x y)
    ; check if the end of the polynomial x ahs been reached
    (cond
        ; check if y is empty
        ((empty? x) '())
        ;still not at end of polynomial so still able to apply y to x
        (else
           (append  (multiply-terms (car x) y) (apply-y-to-x (cdr x) y))
        ))
)

; helper function to move through the terms in polynomial y 
(define (multiply-poly x y)
   
    (cond
        ; check if y is empty
        ((empty? y)  '())

        ;still not at end of polynomial so still able to apply y to x
        (else (append(apply-y-to-x x (car y)) (multiply-poly x (cdr y))))
    )
)

; function used to check types of polynomials pased in and convert the
; to the correct type following multipling
(define (multiply x y)

    (cond
        ; check if polynomials are zero polynomials
        ((or(is-zero? x) (is-zero? y)) '(0))
        ; check if both polynmoials are sparse
        ((and (is-sparse? x) (is-sparse? y))
            (multiply-poly x y)
        )
        ; polynomials are both not zero or sparse, meaning ones dense so output result
        ; as dense
        (else
            (to-dense (multiply-poly (to-sparse x 0) (to-sparse y 0)) 0)
        )
    )
)

;todo make it so like terms are combined

#| 
############################################################################
    Test Cases
############################################################################
|#

; sparse poly used for testing: ((1 0) (2 1) (3 2) (9 8))
; dense poly used for testing:  (1 2 3 0 0)

;testing is-sparse?
(display "test case for is-sparse?")
(newline)
(display(is-sparse?  '(1 2 3 0 0))) ; return false
(newline)
(display(is-sparse?  '((1 0) (2 1) (3 2) (9 8)))) ; return true
(newline)

; testing to-sparse
(display "test case for to-sparse")
(newline)
(display(to-sparse  '((1 0) (2 1) (3 2) (9 8)) 0)) ; just returns  ((1 0) (2 1) (3 2) (9 8))
(newline)
(display(to-sparse  '(1 2 3 0 0) 0)) ; convertss
(newline)
(display(to-sparse  '(0 0 1 4 6 7 0) 0)) ; convertss
(newline)
(display(is-sparse? (to-sparse '(1 2 3 0 0) 0))) ; returns t
(newline)

; testing is-dense
; todo add test cases

; testing to-dense
; todo add test cases

;testing is-zero?
(display "test case for is-zero?")
(newline)
(display(is-zero? `(0))) ; true
(newline)
(display(is-zero? '((0 0)))) ; true
(newline)
(display(is-zero? '((1 0) (2 1) (3 2) (9 8)))) ; false
(newline)
(display(is-zero? '(1 2 3 0 0))) ; false
(newline)

; testing degree
(display "test case for degree")
(newline)
(display(degree '(1 2 3 0 0))) ; x^0 + 2x^1 + 3x^2 should return degree 2
(newline)

(display(degree '((1 0) (2 1) (3 2) (9 8)))) ; x^0 + 2x^1 + 3x^2 + 9x^ 8 should return degree 8
(newline)

; testing coeff
(display "test case for coeff")
(newline)
(coeff (list (list 1 0) (list 2 1) (list 3 2) (list 4 3)) 4) ; need a check 

;testing eval
(display "test case for eval")
(newline)
(display(eval  '(1 2 3 0 0) 2)) ; (2)^0 + 2(2)^1 + 3(2)^2 = 17
(newline)
(display(eval  '((1 0) (2 1) (3 2) (9 8)) 2)) ; (2)^0 + 2(2)^1 + 3(2)^2 + 9(2)^ 8 = 2321
(newline)

;testing add
(display "test case for add")
(newline)
#|
    x^0 = 1 + 3 =  4
    x^1 = 2 + 2 = 4
    x^2 = 3 + 1 = 4
    = (4 4 4)
|#
(display(add '(1 2 3) '(3 2 1)) )
(newline)
#|
    x^0 = 1 + 0 =  1
    x^1 = 1 + 2 = 3
    x^2 = 3 + 2 = 3
    x^3 = 3 + 0 = 3
    = ((1, 0) (3 1) (5 2) (3 0))
|#
(display(add '((1 1) (2 2) (3 3)) '((1 0) (2 1) (3 2))) )
(newline)
#|
    x^0 = 3 + 0 =  3
    x^1 = 1 + 2 = 3
    x^2 = 2 + 1 = 3
    x^3 = 3 + 0 = 3
    x^4 = 0
    x^5 = 6
    = ((3 3 3 3 0 6 )
|#
(display(add '((1 1) (2 2) (3 3) (6 5)) '(3 2 1)))
(newline)
;testing sub
(display "test case for sub")
(newline)
#|
    x^0 = 1 - 3 =  -2
    x^1 = 2 - 2 = 0
    x^2 = 3 - 1 = -2
    = (-2 0 -2)
|#
(display(subtract '(1 2 3) '(3 2 1)) )
(newline)
#|
    x^0 = 1 - 0 =  1
    x^1 = 1 - 2 = -2
    x^2 = 3 - 2 = 1
    x^3 = 3 - 0 = 3
    = ((1, 0) (-2 1) (1 2) (3 0))
|#
(display(subtract '((1 1) (2 2) (3 3)) '((1 0) (2 1) (3 2))))
(newline)
#|
    x^0 = 0 - 3 =  3
    x^1 = 1 - 2 = -1
    x^2 = 3 - 1 = 2
    x^3 = 3 - 0 = 3
    x^4 = 0
    x^5 = 6
    = (-3 -1 1 3 0 6 )
|#
(display(subtract '((1 1) (2 2) (3 3) (6 5)) '(3 2 1)) )
(newline)
; testing multiply-polys
(display "test case for multiply?")
(newline)

#|   
   (2x + 2x^3) * 4x^2 = 8x^3+8x^5 
|#
(display(multiply  '((2  1) (2  3)) '((4  2))))
(newline)

#|
   
   (2x + 2x^3) * (4x^2 + 3x^4) = 8x^3+ 14x^5 + 6x^7
|#
(display(multiply  '((2  1) (2  3)) '((4  2) (3 4))))
(newline)