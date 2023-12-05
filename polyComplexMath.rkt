#lang racket
#|
###########################################################################
    About
    conatins function the define 
    - multiply polynomials
    - divide them and get the remainder/derivative
    - get derivative
    - get the gcd of two polynomials
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
        )))
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

; subtracts polynomial poly2 from polynomial poly1
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
###########################################################################
    function in file definition
############################################################################
|#
; helper function to multiply polynomial term poly1 by term y
(define (multiply-terms poly1 poly2)
    (list (list
        (* (car poly1) (car poly2))
        ;cadr means get character of the next element form current point
        (+ (cadr poly1) (cadr poly2)))))

; helper function to apply polynomials y term to polynomial poly1
(define (apply-poly2-to-p1term poly1 poly2)
    ; check if the end of the polynomial poly1 ahs been reached
    (cond
        ; check if y is empty
        ((empty? poly1) '())
        ;still not at end of polynomial so still able to apply y to poly1
        (else
           (append  (multiply-terms (car poly1) poly2) (apply-poly2-to-p1term (cdr poly1) poly2)))))

; helper function to move through the terms in polynomial y
(define (multiply-poly poly1 poly2)
    (if (> (length poly2) 1)
        ( if (> (length poly2) 2)
            (append  (add (apply-poly2-to-p1term poly1 (car poly2)) (multiply-poly poly1 (cdr poly2))))
            (append  (add (apply-poly2-to-p1term poly1 (car poly2)) (apply-poly2-to-p1term poly1 (cadr poly2))))
        )
        (append(apply-poly2-to-p1term poly1 (car poly2)))
    ))

; function used to check types of polynomials pased in and convert the
; to the correct type following multipling
(define (multiply poly1 poly2)
    (cond
        ((or (empty? poly1)(empty? poly2))
            '(0)
        )
        ((or (is-zero? poly1)(is-zero? poly2))
            '(0)
        )
        ; check if both polynmoials are sparse
        ((and (is-sparse? poly1) (is-sparse? poly2))
            ;check if output's empty
            (multiply-poly poly1 poly2)
        )
        ; polynomials are both not zero or sparse, meaning ones dense so output result
        ; as dense
        (else
            (to-dense (multiply-poly (to-sparse poly1) (to-sparse poly2)))
        )))

; helper function to divide terms
; this uses the smae logic as the helper function
; multiply-terms for multiply
(define (div-terms term1 term2)
    (list (list
        (/ (car term1) (car term2)) ; divide term1 coef by term2 coef
        (- (cadr term1) (cadr term2)))) ; subtract term2 power from term1 power
)

; helper function to manage divison of poly1 by poly2 
; returns a liast that conatins the remainder and quotent
(define (poly-div poly1 poly2)
    (cond
        ; check if degree of poly1 is less then poly2
        ; meaning that division is done
        ((< (degree (reverse poly1)) (degree (reverse poly2))) ; breaks in some cases 
            ; return the quotient (quot) and remainder (poly1)
            '()) 
        ;otherwise continue dvision
        (else
            ; apply disivon to p1
            ; divide terms  (append (div-terms (car poly1) (car poly2)))
            (append (div-terms (car poly1) (car poly2)) 
            (poly-div (reverse (subtract (reverse poly1) (multiply (reverse poly2) (div-terms (car poly1) (car poly2))))) poly2))
        )))


;helper function to get the remainder
(define(get-remainder poly1 poly2)
    (if (is-zero? (subtract (multiply (reverse (poly-div (reverse poly1) (reverse poly2)))  poly2) poly1))
    '(0)
    (subtract poly1 (multiply (reverse (poly-div (reverse poly1) (reverse poly2)))  poly2)))
)

; handles divison of polynomials
; returns a list conatining the remainder and quoent of a divison 
; between polynomials
(define (division poly1 poly2)
     (cond
        ((empty? poly1)
            (cons '(0) '((0)))
        )
        ; check if either are 0
        ((or (is-zero?  poly2) (empty? poly2)) -inf.0)
        ;making sure both polys are sparse types
        ((or (is-dense?  poly1) (is-dense?  poly2))
            ; one poly was dense so just convert both
            (division (to-sparse poly1) (to-sparse poly2))
        )
        ;it's valid so proceed with divison
        (else 
            (cons 
                ; find quotenet
                (reverse(poly-div (reverse poly1) (reverse poly2)))
                ;finding remainder
                (get-remainder poly1 poly2)
            )
        )
    )
)

;divide a polynomial poly1(x) by poly2(x) and returns the quotient. You may assume
;q(x) ̸= 0.
(define (quotient poly1 poly2)
    (car (division poly1 poly2))
)

; divide a polynomial p(x) by q(x) and returns the remainder. You may assume
; q(x) ̸= 0
(define (remainder poly1 poly2 )
    (cdr (division poly1 poly2))   
)

; helper function of derivative that applys the power rule of a polynomial
; passed into it
(define (power-rule poly1)
    ;making sure to not pass back negatives
    (cond
        ; if power is less then 0 becomes zero then just return empty list
        ((< (- (cadr poly1) 1) 0) '() )
        (else
            (list(list (* (car poly1) (cadr poly1))
                (- (cadr poly1) 1))))))

; finds the dervative of a polynomial  poly1
; works for both sparse and dense representations of polynomials
(define (derivative poly1)
    (cond
        ;check if polynomial is empty and or at it's end
        ((empty? poly1) '())
        ; check if sparse
        ((is-sparse? poly1)

           (append (power-rule (car poly1)) (derivative (cdr poly1)))
        )
        ; not empty, or sparse meaning it is dense
        (else
            (to-dense (derivative (to-sparse poly1))))))

;helper function to calcuatle gcd inorder to return proper
; type

(define (find-gcd poly1 poly2)
    ; (display "gcd")(display poly1) (display " | ") (display poly1)(newline)
    ; checking if poly 2 is 0 or they both equil
    ; which means that the gcd has been found
    (cond
        ((or (is-zero? poly2)) poly1 )
        ; ((empty? (cdr (division poly1 poly2))) poly1)
        (else (find-gcd poly2 (cdr (division poly1 poly2))))
    )
)

;given two polynomials, returns their greatest common divisor. If the greatest common
; divisor is non-zero, ensures that the leading coefficient (coefficient corresponding to
; highest power of x) is 1 by multiplying by an appropriate constant
(define (gcd poly1 poly2)
    ;checking type
    (cond
        ; check if both polynmoials are sparse
        ((and (is-sparse? poly1) (is-sparse? poly2))
            ;check if output's empty
            (find-gcd poly1 poly2)
        )
        ; polynomials are both not zero or sparse, meaning ones dense so output result
        ; as dense
        (else
            (to-dense (find-gcd (to-sparse poly1) (to-sparse poly2)))
        )))

#|
############################################################################
    Test Cases
############################################################################
|#
;testing multiply-polys
(display "test case for multiply")
(newline)
(display(multiply  '((2  1) (2  3)) '((4  2)))) ; = 8x^3+8x^5
(newline)
(display(multiply  '((2  1) (2  3)) '((4  2) (3 4)))) ; = 8x^3+ 14x^5 + 6x^7
(newline)
(display(multiply  '(2 1 2 3) '(4  2 3 4))) ; = 12x^8 + 12x^7 + 12x^6 + 23x^5 + 22x^4 +20x^3 + 12x^2 + 6x + 8
(newline)
(display(multiply  '(0) '(2 1 2))) ; 0
(newline)
(display(multiply  '(2) '(2 1 2))) ; left side is doubled
(newline)
(display(multiply  '() '())) ; 0
(newline)

; ;divison only works for sparse rep so no need to test dense
(display "test case for division")
(newline)
(display (car (division '((64 0) (0 1) (0 2) (27 3) )'((4 0) (3 1))))) ; quotient = 9x^2 + -12x +16 ;
(newline)
(display (cdr (division '((64 0) (0 1) (0 2) (27 3) )'((4 0) (3 1))))) ; remainder = 0
(newline)
(display (car (division '((3 0) (-5 1) (6 2) ) '((-1 0) (2 1) )))) ; quotient = 3x-1 ;
(newline)
(display (cdr (division '((3 0) (-5 1) (6 2) ) '((-1 0) (2 1) )))) ; remainder = 2
(newline)
(display  (cdr (division '((4 0) (-8 1) (6 2) (7 3)) '((-2 0) (1 1) (1 3))))) ; =7x^2 - 13x + 11 currently maybe failing
(newline)

; (display(division '() '(5 -2 1 ))) ; 0,0
; (newline)
; (display(division '(5 -2 1 )'() )) ; error
; (newline)

; ;quotient testing
; (display "test case for quotient")
;remainder and quote need work still 


; (display "test case for derivative")
; (newline)
; (display (derivative '((5 0) (-2 1) (3 2)) )); = ((-2 0) (6 1))
; (newline)
; (display (derivative '((5 0) (-2 1) (3 2) (4 5)) )); = ((-2 0) (6 1) (20 4))
; (newline)
; (display (derivative '(5 -2 3 0 0 4 ))) ; = (-2 6  0  0 20)
; (newline)
; (display (derivative '())) ; = empty list
; (newline)




(display "test case for gcd")
(newline)
(display (gcd '((4 0) (-8 1) (6 2) (7 3)) '((-2 0) (1 1) (1 3)))) ; 
(newline)
; (display (gcd '(0 4 6 2) '( 0 2 1 ) )) ; = x^2 - 2x
; (newline)
; (display (gcd '((2 0) (-12 1) (18 2) (-12 3) (3 4)) '((4 0) (-4 1) (1 2)))) ; = x^2 - 4x + 4 2 -12x+18x^2-12x^3+
; (newline)

;7x^3 + 6x^2 - 8x + 4 and q(x) = x^3 + x - 2.