#lang racket
#|
###########################################################################
    About
    conatins functions the define how to check weatehr a polynomial
    is sparse, or dense as well as methods of converting between
    the types
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
############################################################################
    Test Cases
############################################################################
|#

;testing is-sparse?
(display "test case for is-sparse?")
(newline)
(display(is-sparse?  '(1 2 3 0 0))) ; return false
(newline)
(display(is-sparse?  '((1 0) (2 1) (3 2) (9 8)))) ; return true
(newline)

;testing to-sparse
(display "test case for to-sparse")
(newline)
(display(to-sparse  '((1 0) (2 1) (3 2) (9 8)))) ; just returns  ((1 0) (2 1) (3 2) (9 8))
(newline)
(display(to-sparse  '(1 2 3 0 0))) ; converts
(newline)
(display(to-sparse  '(0 0 1 4 6 7 0))) ; convertss
(newline)
(display(is-sparse? (to-sparse '(1 2 3 0 0)))) ; returns t
(newline)

;testing is-dense
(display "test case for is-dense")
(newline)
(display(is-sparse?  '(1 2 3 0 0))) ; return True
(newline)
(display(is-sparse?  '((1 0) (2 1) (3 2) (9 8)))) ; return False
(newline)

;testing to-dense
(display "test case for to-dense")
(newline)
(display (to-dense '(1 2 3 4 5))) ; just returns
(newline)
(display (to-dense '((1 0) (2 1) (3 2) (9 8)))) ; converts with a lot of 0's between 3 and 9
(newline)
(display (to-dense '((3 2) (5 5)))) ; converts, should add 2 zeros before 3 and 2 after
(newline)