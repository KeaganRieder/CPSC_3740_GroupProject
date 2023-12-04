#lang racket
#|
###########################################################################
    About
    conatins function the define 
    - how to check if a polynomial is zero
    - evaluate it based on value k passed in 
    - methods of getting the degree and coef
############################################################################
|#

; polynoimal is a list, returns false if list is sparse and true if list is dense
(define (is-dense? polynoimal)
  ;is first elment a sub list in a list?
  (if (list? (car polynoimal))
      #false
      #true))
; converts from sparse to dense polynoimal 
(define (to-dense polynoimal)
    ; check if polynoimal is dense
    (if (is-dense? polynoimal)
      ; polynoimal is dense so just return it
      polynoimal
      ; polynoimal isn't dense so convert
      (make-dense polynoimal 0)))
; helper function for to dense, that converts the polynomial to a dense type
(define (make-dense polynoimal degree)
    ; is it empty
    (if (null? polynoimal)
        null
        ;its not empty, lets assume it is sparse, is the second value of the first list the value we are curently looking for?
        (if (equal? degree (car (cdr (car polynoimal))))
            ;it is, add the value to the list
            (append (list (list-ref (car polynoimal) 0)) (make-dense (cdr polynoimal) (+ 1 degree)))
            ;its not add 0 to the list and look for the nepolynoimalt value
            (append (list 0) (make-dense polynoimal (+ 1 degree))))) )
;checks if polunomial is sparse type
(define(is-sparse? polynomial)
    (if (empty? polynomial)
        #false
        ; if current element is sublist then
        ; it is a Sparse polynomial
        (if (list? (car polynomial))
            #true
            #false
        )))
; convert a polynomal into being sparse
(define(to-sparse polynomial)
    ; checking if polynomial is already sparse
    ( if (is-sparse? polynomial)
        polynomial
        ; it isn't so convert and start at degree 0
        (make-sparse polynomial 0)
    ))
;helper function for to-sparse
(define (make-sparse polynomial degree)
    (cond
        ;check if it is empty, and return an empty list
        ((empty? polynomial) '())

        ;check if  element is equail to 0
        ((= (car polynomial) 0) (make-sparse (cdr polynomial) (add1 degree)))
        ;otherwise run throuh the rest
        (else
        (cons (list (car polynomial) degree)
            (make-sparse (cdr polynomial) (add1 degree)))
        )))
    
#|
###########################################################################
    function in file definition
############################################################################
|#
; check if the input is the zero polynomial
(define(is-zero? polynomial)
    (if (empty? polynomial)
        ;it's empty meaning zero
        #t
        ;not empty
        (if (is-sparse? polynomial)
            ; polynomial is sparse, get inner list
            (is-zero? (car polynomial))

            ; polynomial isn't sparse so just check if current element (first) is 0
            (if (zero?(car polynomial))
                #t
                #f
            ))))

; returns the coefficent of polynoimal^y converts all lists to dense for ease of implementation negative y will break this
(define (coeff polynoimal degree)
    (cond
        ;check if end of polynomial has been reached
        ((empty? polynoimal) '(0))
        ;checking if list is sparse
        ((is-sparse? polynoimal)
            ;check if current element is the power being searched for
            (if ( = (last (car polynoimal))  degree)
                ; it is so return it
                (car (car polynoimal))
                ;it's not
                (coeff (cdr polynoimal) degree)
            )
        )
        ;otherwise it's dense so convert
        (else
            (coeff (to-sparse polynoimal) degree)
        )
    )
)

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
            (degree (to-sparse polynomial))
        )))


#|
############################################################################
    Test Cases
############################################################################
|#
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
(display(is-zero? '())) ;
(newline)

;testing degree
(display "test case for degree")
(newline)
(display(degree '(1 2 3 0 0))) ; polynoimal^0 + 2polynoimal^1 + 3polynoimal^2 should return degree 2
(newline)

(display(degree '((1 0) (2 1) (3 2) (9 8)))) ; polynoimal^0 + 2polynoimal^1 + 3polynoimal^2 + 9polynoimal^ 8 should return degree 8
(newline)

;testing coeff
(display "test case for coeff")
(newline)

(display (coeff `((24 0) (10 1) (6 2)) 2)) ; returns 6
(newline)
(display (coeff `(2 3 6) 2)) ; returns 6
(newline)
(display (coeff `(2 3 6) 1)) ; returns 3
(newline)
(display (coeff `(2 3 6) (degree `(2 3 6)))) ; returns 6
(newline)