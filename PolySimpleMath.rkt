#lang racket
#|
###########################################################################
    About
    conatins function the define 
    - evaulating polynomials based on vaule k passed in
    - adding and subtracting polynomials
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
                (coeff (cdr polynoimal) degree)))
        ;otherwise it's dense so convert
        (else
            (coeff (to-sparse polynoimal) degree))))

#|
###########################################################################
    function in file definition
############################################################################
|#

; takes a polynomial p(l) and a value k, returns the result of p(k).
; only works for sparse
(define (eval polynomial k)
    (cond
        ; check if empty
        ((empty? polynomial) '(0))
        ; check if 0
        ((is-zero? polynomial ) '(0))
        ; check if  not sparse and then converting
        ((not (is-sparse? polynomial)) (eval (to-sparse polynomial) k))

        ; evaluating
        (else
            (apply + (map (lambda (term)
                (* (first term) (expt k (second term))))
                polynomial
            ))))
)

; takes two polynomials and adds them broken atm need to fipoly1
(define (add poly1 poly2)
    (cond
        ; check if both list is empty
        ((and (empty? poly1) (empty? poly2))
            '()
        )
        ;checking either list is empty
        ((or (empty? poly1) (empty? poly2))
            (if (empty? poly1)
                poly2 ; poly1 is empty so return poly2 as result
                poly1 ; poly2 is empty so return poly1 as result
            ))
        ;neither are so check type

        ((and (is-sparse? poly1) (is-sparse? poly2))
            ; both are sparse polynomials
            (to-sparse(add-Poly (to-dense poly1) (to-dense poly2))))

        (else
            ; one of them isn't convert which ever one is
            ; sparse to be dense and call helper function to calcualte addtion
            (add-Poly (to-dense poly1) (to-dense poly2)))))

; helper function to handle adding to polynomials together
(define (add-Poly poly1 poly2)
    (cond
        ;check if both lists are empty
        ((and (empty? poly1) (empty? poly2) ) '())

        ;poly1 and y are both not empty, see if poly1 is
        ((empty? poly1) (append (list (car poly2)) ( add-Poly poly1 (cdr poly2))))
        ;poly1 isn't empty, so see check if y is empty
        ((empty? poly2)  (append (list (car  poly1)) (add-Poly (cdr poly1)  poly2)))
        ; otherwise add
        (else  (append (list (+ (car poly1 ) (car poly2)))  (add-Poly (cdr poly1) (cdr poly2))))))

; helper function to make a ploynomials coefficent negative
; primarly allows for the add-Poly fucntion to be reused
(define (invert-coef polynoimal)
    (map (lambda (poly1) (* -1 poly1)) polynoimal))

; subtracts polynomial y from polynomial poly1
; first checks the types of polynomials
(define (subtract poly1 poly2)
    ; check if both polynomials are sparse
    (if (and (is-sparse? poly1) (is-sparse? poly2))
        ; both are sparse polynomials
        (to-sparse (add-Poly (to-dense poly1) (invert-coef (to-dense poly2))))

        ; one of them isn't convert which ever one is
        ; sparse to be dense and call helper function to calcualte addtion
        (add-Poly (to-dense poly1) (invert-coef (to-dense poly2)))))

#|
############################################################################
    Test Cases
############################################################################
|#

;testing eval
(display "test case for eval")
(newline)
(display(eval  '(1 2 3 0 0) 2)) ;  = 17
(newline)
(display(eval  '((1 0) (2 1) (3 2) (9 8)) 2)) ; = 2321
(newline)

;testing add
(display "test case for add")
(newline)
(display(add '(1 2 3) '(3 2 1)) );= (4 4 4)
(newline)
(display(add '((1 1) (2 2) (3 3)) '((1 0) (2 1) (3 2))) );= ((1, 0) (3 1) (5 2) (3 0))
(newline)
(display(add '((1 1) (2 2) (3 3) (6 5)) '(3 2 1))) ;= ((3 3 3 3 0 6 )
(newline)
(display(add '((1 1) (2 2) (3 3) (6 5)) '())) ; = empty list
(newline)

;testing sub
(display "test case for sub")
(newline)
(display(subtract '(1 2 3) '(3 2 1)) ) ; = (-2 0 -2)
(newline)
(display(subtract '((1 1) (2 2) (3 3)) '((1 0) (2 1) (3 2)))); = ((1, 0) (-2 1) (1 2) (3 0))
(newline)
(display(subtract '((1 1) (2 2) (3 3) (6 5)) '(3 2 1)) ) ;= (-3 -1 1 3 0 6 )
(newline)