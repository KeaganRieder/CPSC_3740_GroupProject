#lang racket

; ###########################################################################
;   About
;   3740 group Project
;   by Keagan Rieder and Justin Wolfenden
;
;   The goal of this project is using rakcet define a set of functions 
;   which perfom theoperation on polynomails which are either represnted as
;   dense or sparse. 
;   
;   more detailed description of polynomials types can be found in the  
;   'projct' pdf in /docs
;
;   some assumption assumptions have been made:
;   an empty list is consider a zero polynomial that isn't consdered
;   to be any particuler type of polynomial
; ############################################################################

; ###############################################################################################
;     type checking and converting functions 
; ###############################################################################################


; takes a polynomial and determines if it is currenty represnt
; as a dense (returns #true) or if it's sparse (returns #flase)
(define (is-dense? polynomial)
    ;check if polynomial is empty
    (if (empty? polynomial)
        #false
        ; check if first element in given polynomial is sublist 
        (if (list? (car polynomial))
        #false
        #true)
    )
  
 )

; takes a polynomial, first checking if it's already dense otherwise
; it calls the helper function make-dense and converts it to dense
(define (to-dense polynomial)
    ; check if polynomial is dense
    (if (is-dense? polynomial)
      ; polynomial is dense so return it
      polynomial
      ; polynomial isn't dense so convert 
      ; making sure to start at degree 0 (starting degree)
      (make-dense polynomial 0)))

; helper function used by to to-dense after checking type of polynomial
; inorder to properly convert polynomial from sparse to dense
(define (make-dense polynomial degree)
    ; is it empty
    (if (null? polynomial)
        null
        ; its not empty, lets assume it is sparse, is the second value of the
        ; first list the value we are curently looking for?
        (if (equal? degree (car (cdr (car polynomial))))
            ; it is, add the value to the list
            (append (list (list-ref (car polynomial) 0)) (make-dense (cdr polynomial) (+ 1 degree)))
            ; its not add 0 to the list and look for the nepolynoimalt value
            (append (list 0) (make-dense polynomial (+ 1 degree))))) )

; takes a polynomial and determines if it is currenty represnt
; as a sparse (returns #true) or if it's dense (returns #false)
(define(is-sparse? polynomial)
    (if (empty? polynomial)
        #false
        ; if current element is sublist then
        ; it is a Sparse polynomial
        (if (list? (car polynomial))
            #true
            #false
        )))

; takes a polynomial, first checking if it's already sparse otherwise
; it calls the helper function make-sparse and converts it to sparse
(define(to-sparse polynomial)
    ; checking if polynomial is already sparse
    ( if (is-sparse? polynomial)
        polynomial
        ; it isn't so convert and start at degree 0
        (make-sparse polynomial 0)
    ))

; helper function used by to to-sparse after checking type of polynomial
; inorder to properly convert polynomial from dense to sparse
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

; checks if providied polynomial is zero or empty. converts provided polynomial
; to dense representation if sparse localy by calling itself it then runs 
; through th epolynomial to see if it's zero
(define(is-zero? polynomial)
    ;checking type and if empty
    (cond
        ;it's empty meaning zero
        ((empty? polynomial) #t)
        ;polynomials is sparse convert to dense
        ((is-sparse? polynomial) (is-zero? (to-dense polynomial)))
        
        ;check last charcter of list, to see if zero
        (else
            (if (zero? (last polynomial))
                #t
                #f
            )
        )
    ))

; ###############################################################################################
;     math functions
; ###############################################################################################

; takes a polynomial and desired degree to check and returns the coeeficent of 
; polynomial^degree, following converting to spasre representation, returns 0
; if provided degree doesn't exsit
(define (coeff polynomial degree)
    (cond
        ;check if end of polynomial has been reached
        ((empty? polynomial) '(0))
        ;checking if list is sparse
        ((is-sparse? polynomial)
            ;check if current element is the power being searched for
            (if ( = (last (car polynomial))  degree)
                ; it is so return it
                (car (car polynomial))
                ;it's not
                (coeff (cdr polynomial) degree)))
        ;otherwise it's dense so convert
        (else
            (coeff (to-sparse polynomial) degree))))

; takes a polynomial and returns it's degree of the polynomial following 
; converting it to sparse representation
(define(degree polynomial)

    (cond
        ;checking if input polynomial is empty
        ((is-zero? polynomial) -inf.0)
        ; read check if the polynomial is sparse, if so convert to sparse
        ((is-sparse? polynomial)
        ; read the last element in a sparse polynomial which is a list
        ; then reads the last element in that list to get the degree
           (last (last polynomial ))
        )
        ; it's not sparse meaning it's dense so convert to sparse
        (else
            (degree (to-sparse polynomial))
        )))

; takes a polynomial and a value k, which it then uses to evaluate and return the value of
; p(K), following converting the polynomial sparse
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
            )))))

; takes two polynomials checking what there representation is, then it calls the helper-function
; add-Poly, passing both polynomials in following converting both polynomials to dense. then converts
; there sum to the peorper represnation
(define (add poly1 poly2)
    (cond
        ; check if both list is empty
        ((and (empty? poly1) (empty? poly2))
            '(0)
        )
        ;checking either list is empty
        ((or (empty? poly1) (empty? poly2))
            (if (empty? poly1)
                poly2 ; poly1 is empty so return poly2 as result
                poly1 ; poly2 is empty so return poly1 as result
            ))
        ;neither are so check type
        ((and (is-sparse? poly1) (is-sparse? poly2))
            ; both are sparse polynomials, so convert to dense then convert
            ; return to sparse
            (to-sparse(add-Poly (to-dense poly1) (to-dense poly2))))

        (else
            ; both aren't dense so make sure both are dense and call helper function
            ; but no need to convert return type to dense
            (add-Poly (to-dense poly1) (to-dense poly2)))))

; helper function used by add inorder to add poly1 & poly2,
; well mainatining the proper sum represntation 
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

; helper function used by subtartcion to invert a polynomial coefficents
(define (invert-coef polynomial)
    (map (lambda (poly1) (* -1 poly1)) polynomial))

; takes two polynomials checking what there representation is, then it calls the helper-function
; add-Poly, passing both polynomials in following inverting and converting both polynomials to dense. 
; then converts there sum to the peorper represnation
(define (subtract poly1 poly2)
    ; check if both polynomials are sparse
    (if (and (is-sparse? poly1) (is-sparse? poly2))
        ; both are sparse polynomials
        (to-sparse (add-Poly (to-dense poly1) (invert-coef (to-dense poly2))))

        ; one of them isn't convert which ever one is
        ; sparse to be dense and call helper function to calcualte addtion
        (add-Poly (to-dense poly1) (invert-coef (to-dense poly2)))))

; helepr function used by apply-poly1-to-p2term to multiply a poly1 term by poly2 term
(define (multiply-terms poly1 poly2)
    (list (list
        (* (car poly1) (car poly2))
        ;cadr means get character of the next element form current point
        (+ (cadr poly1) (cadr poly2)))))

; helper function used by multiply-poly to multiply poly1 by the current poly2 term 
(define (apply-poly1-to-p2term poly1 poly2)
    ; check if the end of the polynomial poly1 ahs been reached
    (cond
        ; check if poly1 is empty
        ((empty? poly1) '())
        ;still not at end of polynomial so still able to apply y to poly1
        (else
           (append  (multiply-terms (car poly1) poly2) (apply-poly1-to-p2term (cdr poly1) poly2)))))

; helper function used to move through poly2 ensuring each term is applyed to
; poly1, it then adds the results of apply-poly1-to-p2term following each application
(define (multiply-poly poly1 poly2)
    (if (> (length poly2) 1)
        ;check how many terms poly2 has left 
        ( if (> (length poly2) 2)
            ; poly2 hsa more ehen 2 terms reamining
            (append  (add (apply-poly1-to-p2term poly1 (car poly2)) (multiply-poly poly1 (cdr poly2)))) 
            ;poly2 has two terms left so apply current one and perform one more shift
            (append  (add (apply-poly1-to-p2term poly1 (car poly2)) (apply-poly1-to-p2term poly1 (cadr poly2))))
        )
        (append(apply-poly1-to-p2term poly1 (car poly2)))
    ))

; takes two polynomials, checks there types and then passes them to multiply-poly 
; to be multiplyed, following converting both tosparse if need.
; converts the product back to the peorper polynomial represnation
(define (multiply poly1 poly2)
    (cond
        ((or (is-zero? poly1)(is-zero? poly2))
            '(0)
        )
  
        ((and (is-sparse? poly1) (is-sparse? poly2))
            ; both polynomials are sparse so just multiply them and return
            ; there product
            (multiply-poly poly1 poly2)
        )
        ; a polynomial is dense so convert both to sparse, and then convert
        ; result to dense
        (else
            (to-dense (multiply-poly (to-sparse poly1) (to-sparse poly2)))
        )))

; helper fucntion used by poly-div to divide to terms
(define (div-terms term1 term2)

    (list (list
        (/ (car term1) (car term2)) ; divide term1 coef by term2 coef
        (- (cadr term1) (cadr term2)))) ; subtract term2 power from term1 power
)

; helper function used by quotient and get-remainder to divde poly1 by poly2
; following type checks and converting both to sparse if needed
(define (poly-div poly1 poly2)

    (cond
            
        ; check if degree of poly1 is less then poly2
        ; meaning that division is done
        ((< (degree (reverse poly1)) (degree (reverse poly2))) ; breaks in some cases 
            '()) 
        ;otherwise continue dvision
        (else
            ; apply disivon to p1
            ; divide terms  
            (append (div-terms (car poly1) (car poly2)) 
                    (poly-div (reverse (subtract (reverse poly1) 
                        (multiply (reverse poly2) 
                            (div-terms (car poly1) (car poly2))))) 
                                poly2))
        )))

; takes to polynomials, checks there types and calls poly-div inorder to divide poly1 by poly2
; following converting both to spasre. quotient then returns the result quotent following conversion
; to proper polynomial type 
(define (quotient poly1 poly2)
    (cond
        ; check if poly2 are 0
        ((or (is-zero?  poly2) (empty? poly2)) -inf.0)
        ;making sure both polys are sparse types
        ((or (is-dense?  poly1) (is-dense?  poly2))
            ; one poly was dense so convert both and then output
            ; dense
            (to-dense(quotient (to-sparse poly1) (to-sparse poly2)))
        )
        ;valid input, and both are sparse so proceed
        (else             
            (reverse(poly-div (reverse poly1) (reverse poly2)))            
        )))

; helper function use by remainder to get a polynomails remainder through performing the operation
; poly1 - poly2 * (poly-div (reverse poly1) (reverse poly2)) = remainder
(define(get-remainder poly1 poly2)
    
    (if (is-zero? (subtract (multiply (reverse (poly-div (reverse poly1) (reverse poly2)))  poly2) poly1))
    '(0)
    (subtract poly1 (multiply
        (reverse (poly-div (reverse poly1) (reverse poly2))) 
             poly2))))

; takes to polynomials, checks there types and calls poly-div inorder to divide poly1 by poly2
; following converting both to spasre. quotient then returns the result remainder following conversion
; to proper polynomial type 
(define (remainder poly1 poly2 )
    (cond
        ; check if either are 0
        ((or (is-zero?  poly2) (empty? poly2)) -inf.0)
        ;making sure both polys are sparse types
        ((or (is-dense?  poly1) (is-dense?  poly2))
            ; one poly was dense so just convert both and then output
            ; dense
            (to-dense(remainder (to-sparse poly1) (to-sparse poly2)))
        )
        ;valid input, and both are sparse so proceed
        (else             
            (get-remainder poly1 poly2)            
        )
    ))

; helper function used by derivative to apply a polynomials degree to it's coefficent
; through the use of the power rule
(define (power-rule poly1)
    ;making sure to not pass back negatives
    (cond
        ; if power is less then 0 becomes zero then just return empty list
        ((< (- (cadr poly1) 1) 0) '() )
        (else
            (list(list (* (car poly1) (cadr poly1))
                (- (cadr poly1) 1))))))

; finds the derivative of a polynomial by checking it's type, then converting it to sparse
; and calling the helper function power-rule. it then converts the result back to the correct
; type
(define (derivative poly1)
    (cond
        ;check if polynomial is empty and or at it's end
        ((or(empty? poly1))'())
        ; check if sparse
        ((is-sparse? poly1)
           (append (power-rule (car poly1)) (derivative (cdr poly1)))
        )
        ; not empty, or sparse meaning it is dense
        (else
            (to-dense (derivative (to-sparse poly1))))))

; helper function used by gcd to find the gcd of poly1 & poly2
(define (find-gcd poly1 poly2) 
    (cond
        ; pol2 is zero meaning gcds found so normialize it
        ((is-zero? poly2) (to-sparse (normalize-gcd (to-dense poly1) 
            (coeff poly1 (degree poly1)))))
        ;it's not so continue modding poly2  
        (else (find-gcd poly2 (remainder poly1 poly2)))
    ))

; helepr function used to normalize a gcd by divide it by the highest
; degrees coefficent ensure the leading coefficent is 1
(define (normalize-gcd Polynomial coefficent)
  (map (lambda (term) (/ term coefficent)) Polynomial))

; takes to polynomials, checks there types and then converts them to sparse, whcih it then 
; calls find-gcd passing the convert polynomials inorder to find the gcd
; it then converts the resulting gcd back the proepr type 
(define (gcd poly1 poly2) 
    (cond
        ; check if both empty/zero?
        ((and (is-zero? poly1) (is-zero? poly2)) '(0))
        ;is poly1 empty/zero?
        ((and (is-zero? poly1) ) poly2)
        ;is poly2 empty/zero?
        ((and (is-zero? poly2) ) poly1)

        ; check if both polynmoials are sparse
        ((and (is-sparse? poly1) (is-sparse? poly2))
            ;check which has the higher degree
            (cond
                ;poly 1 is lower degree so make it divider
                ((< (degree poly1) (degree poly2))
                    (find-gcd poly2 poly1)
                )
                (else
                    (find-gcd poly1 poly2)
                )))
        ; polynomials are both not zero or sparse, meaning ones dense so output result
        ; as dense
        (else
            (to-dense (find-gcd (to-sparse poly1) (to-sparse poly2)))
        )))

; ############################################################################
;     Test Cases
; ############################################################################

; Empty poly case
(define EMPTY '()) 

; sparse type test cases
(define S0 '((0 0)))
(define S1 '((1 0) (1 1))) ;  1 + 1x
(define S2 '((4 1) (6 2) (2 3))) ; 4x + 6X^2 + 2x^3
(define S3 '((2 1) (1 2))) ; 2x + 1x^2
(define S4 '((4 0) (3 1) (2 2) (4 3))) ; 4 + 3x + 2x^2) + 4x^3)

; dense type test cases
(define D0 '(0)) ; zero poly
(define D1 '(1 1)) ; 1 + 1x
(define D2 '(0 4 6 2)) ; 4x + 6X^2 + 2^3
(define D3 '(0 2 1)) ; 2x + 1x^2
(define D4 '(-4 -8 3 1)) ; -4 +-8X + 3X^2 + 1X^3
(define D5 '( -2 1)) ; -2 + X^2
(define D6 '(-7 23 6 -2 3)) ; -7 + 23x + 6x^2 -2x^3 + 3x^4
(define D7 '(5 -2 1)) ; 5 -2X + X^2

(newline)
(displayln "test cases for type check/converting")
(newline)
(displayln "test case for is-dense?")
(displayln (is-dense?  EMPTY)) ; = f
(displayln (is-dense?  S0)) ; = f
(displayln (is-dense?  D0)) ; = t
(displayln (is-dense?  D1)) ; = t
(displayln (is-dense?  D2)) ; = t
(displayln (is-dense?  D4)) ; = t

(newline)
(displayln "test case for to-dense")
(displayln (is-dense?(to-dense  EMPTY))) ; = t
(displayln (to-dense  S0)) ; = 0
(displayln (is-dense?(to-dense  S0))) ; = t
(displayln (to-dense  D1)) ; = 1 + 1x
(displayln (to-dense  S2)) ; =  (-4 -8 3 1)
(displayln (to-dense  S4)) ; = '(-4 -8 3 1)

(newline)
(displayln "test case for is-sparse?")
(displayln (is-sparse?  EMPTY)) ; = f
(displayln (is-sparse?  S0)) ; = f
(displayln (is-sparse?  S0)) ; = t
(displayln (is-sparse?  D1)) ; = f
(displayln (is-sparse?  D2)) ; = f
(displayln (is-sparse?  S4)) ; = t

(newline)
(displayln "test case for to-sparse")
(displayln (to-sparse  EMPTY)) ; = ()
(displayln (to-sparse  S0)) ; = ((0 0))
(displayln (to-sparse  D3)) ; = ((2 1) (1 2))
(displayln (to-sparse  D1)) ; = ((1 0) (1 1))
(displayln (to-sparse  D2)) ; = ((4 1) (6 2) (2 3))
(displayln (to-sparse  S4)) ; = ((4 0) (3 1) (2 2) (4 3))

(newline)
(displayln "test case for is-zero")
(displayln (is-zero?  EMPTY)) ; = t
(displayln (is-zero?  S0)) ; = t
(displayln (is-zero?  S1)) ; = f
(displayln (is-zero?  S3)) ; = f
(displayln (is-zero?  S4)) ; = f
(displayln (is-zero?  D0))  ; = t
(displayln (is-zero?  D1))   ; = f
(displayln (is-zero?  D3))   ; = f
(displayln (is-zero?  S4))   ; = f

(newline)
(displayln "test caseS for type math functions")
(newline)
(displayln "test case for coeff")
(displayln (coeff  EMPTY 1)) ; = 0
(displayln (coeff  S0 1)) ; = 0
(displayln (coeff  D3 1)) ; =  2
(displayln (coeff  S2 3)) ; = 0
(displayln (coeff  D2 1)) ; = 4
(displayln (coeff  S4 3)) ; =  4

(newline)
(displayln "test case for degree")
(displayln (degree S0)) ; = -INF.0
(displayln (degree S4)) ; = 3
(displayln (degree D4)) ; = 3
(displayln (degree D6)) ; = 4
(displayln (degree D7)) ; = 2
(displayln (degree S2)) ; = 3
(displayln (degree S3)) ; = 2

(newline)
(displayln "test case for eval")
(displayln (eval S0 1)) ; = 0
(displayln (eval S1 2)) ; = 3
(displayln (eval S4 2)) ; = 34
(displayln (eval D0 5)) ; = 0
(displayln (eval D4 2)) ; = 34
(displayln (eval D7 5)) ; = 20

(newline)
(displayln "test case for add")
(displayln (add EMPTY EMPTY)) ; = (0 0)
(displayln (add S0 EMPTY)) ; = (0 0)
(displayln (add S1 S1)) ; = ((2 0) (2 1))
(displayln (add S1 D1)) ; = (2 2)
(displayln (add D2 D3)) ; = (0 6 7 2)
(displayln (add S2 S3)) ; = ((6 1) (7 2) (2 3))

(newline)
(displayln "test case for subtract")
(displayln (subtract EMPTY EMPTY)) ; = (0)
(displayln (subtract S0 EMPTY)) ; = (0 0)
(displayln (subtract S1 S1)) ; = ()
(displayln (subtract S1 D1)) ; = (0)
(displayln (subtract D2 D3)) ; = (0 6 7 2)
(displayln (subtract S2 S3)) ; = ((6 1) (7 2) (2 3))

(newline)
(displayln "test case for multiply")
(displayln (multiply  EMPTY EMPTY)) ; = (0)
(displayln (multiply  S0 S1)) ; = (0) / ((1 0) (1 1))
(displayln (multiply  S1 S2)) ; = ((4 1) (10 2) (8 3) (2 4))
(displayln (multiply  S3 S2)) ; = ((8 2) (16 3) (10 4) (2 5))
(displayln (multiply  S4 S2)) ; = ((16 1) (36 2) (34 3) (34 4) (28 5) (8 6))
(displayln (multiply  D0 D1))  ; = (0)
(displayln (multiply  D1 D2))   ; = (0 4 10 8 2)
(displayln (multiply  D3 D2))   ; = (0 8 16 10 2)
(displayln (multiply  S4 D2))   ; = (0 16 36 34 34 28 8)

(newline)
(displayln "test case for quotient")
(displayln (quotient S0 S1)) ; = ()
(displayln (quotient S1 S0)) ; = -inf.0
(displayln (quotient S2 S1)) ; = ((4 1) (2 2))
(displayln (quotient S4 S2)) ; = (2 0)
(displayln (quotient D1 D0)) ; = -inf.0
(displayln (quotient D3 D1)) ; = (1 1)
(displayln (quotient D4 D5 )) ; = (2 5 1)
(displayln (quotient D6 D7)) ; = (-1 4 3)
(displayln (quotient S2 D1)) ; = (0 4 2)

(newline)
(displayln "test case for remainder")
(displayln (remainder S0 S1)) ; = (0)
(displayln (remainder S1 S0)) ; = -inf.0
(displayln (remainder S4 S2)) ; = ((4 0) (-5 1) (-10 2))
(displayln (remainder D4 D5)) ; = (0)
(displayln (remainder D6 D7)) ; = (-2 1)
(displayln (remainder D3 D1)) ; = (-1)
(displayln (remainder S1 D1)) ; = (0)
(displayln (remainder S2 D1)) ; = (0)

(newline)
(display "test case for derivative")
(displayln (derivative EMPTY)) ; = ()
(displayln (derivative S0)) ; = ()
(displayln (derivative S1)) ; =((1 0))
(displayln (derivative D1)) ; = (1)
(displayln (derivative D7)) ; = (-2 2)
(displayln (derivative D4)) ; = (-8 6 3)
(displayln (derivative S4)) ; = ((3 0) (4 1) (12 2))

(newline)
(displayln "test case for gcd")
(displayln (gcd S0 S0)) ; = 0
(displayln (gcd S1 S2)) ; = ((1 0) (1 1))
(displayln (gcd S2 S3)) ; = ((2 1) (1 2))
(displayln (gcd D1 D2)) ; = (1 1)
(displayln (gcd D2 D3)) ; = (0 2 1)
(displayln (gcd D6 D7)) ; = 1
(displayln (gcd S2 D3)) ; = ((2 1) (1 2))