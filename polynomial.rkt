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

; x is a list, returns false if list is sparse and true if list is dense
(define (is-dense? x)
  ;is it a list of lists?
  (if (list? (car x))
      false
      true))

; converts from sparse to dense  x must be a sparse list or it will error, set y to 0 for recursion
(define (to-dense x y)
  ;is it empty
 (if (null? x)
     null

     ;its not empty, lets assume it is sparse, is the second value of the first list the value we are curently looking for?
     (if (equal? y (car (cdr (car x))))
         ;it is, add the value to the list
         (append (list (list-ref (car x) 0)) (to-dense (cdr x) (+ 1 y)))
         ;its not add 0 to the list and look for the next value
         (append (list 0) (to-dense x (+ 1 y))))))

; checks if its dense then makes it dense if it is not already dense
(define (make-dense x)
  (if (is-dense? x)
      x
      (to-dense x 0)))

; check if the input is the zero polynomial
; works for both types of polynomials
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

#|
############################################################################
    defining function to check the degree and coefficient
############################################################################
|#

; returns the coefficent of x^y converts all lists to dense for ease of implementation negative y will break this
(define (coeff x y)
  ;round about way of asking is y less then or equal to length x
  (if (negative? (- y (length x)))
              (list-ref (make-dense x) y)
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

#|
############################################################################
    defining function to perform mathmatical operation between polynomials
############################################################################
|#

#| 
############################################################################
    Test Cases
############################################################################
|#

; sparse poly used for testing: ((1 0) (2 1) (3 2) (9 8))
; dense poly used for testing:  (1 2 3 0 0)

;testing is-sparse
(display(is-sparse  '(1 2 3 0 0))) ; return false
(newline)
(display(is-sparse  '((1 0) (2 1) (3 2) (9 8)))) ; return true
(newline)

; testing to-sparse
(display(to-sparse  '((1 0) (2 1) (3 2) (9 8)) 0)) ; just returns  ((1 0) (2 1) (3 2) (9 8))
(newline)
(display(to-sparse  '(1 2 3 0 0) 0)) ; convertss
(newline)
(display(is-sparse (to-sparse '(1 2 3 0 0) 0))) ; returns t
(newline)

; testing is-dense
; todo add test cases

; testing to-dense
; todo add test cases

;testing is-zero?
(display(is-zero? '(0))) ; true
(newline)
(display(is-zero? '((0 0)))) ; true
(newline)
(display(is-zero? '((1 0) (2 1) (3 2) (9 8)))) ; false
(newline)
(display(is-zero? '(1 2 3 0 0))) ; false
(newline)

; testing degree
(display(degree '(1 2 3 0 0))) ; x^0 + 2x^1 + 3x^2 should return degree 2
(newline)

(display(degree '((1 0) (2 1) (3 2) (9 8)))) ; x^0 + 2x^1 + 3x^2 + 9x^ 8 should return degree 8
(newline)

; testing coeff
(coeff (list (list 1 0) (list 2 1) (list 3 2) (list 4 3)) 4) 
