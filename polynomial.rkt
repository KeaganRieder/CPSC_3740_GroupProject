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
        )))

; convert a polynomal into being sparse
(define(to-sparse polynomial index)
    ; checking if polynomial is already sparse
    ( if (is-sparse? polynomial) 
        polynomial
        ; it isn't so convert
        (make-sparse polynomial index)
    ))

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
      (make-dense x y)))

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
            (append (list 0) (make-dense x (+ 1 y))))) )

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
        )))

#|
############################################################################
    defining function to check the degree and coefficient
############################################################################
|#
; returns the coefficent of x^y converts all lists to dense for ease of implementation negative y will break this
(define (coeff x y)
    (cond
        ;check if end of polynomial has been reached 
        ((empty? x) '(0))
        ;checking if list is sparse
        ((is-sparse? x)
            ;check if current element is the power being searched for
            (if ( = (last (car x))  y)
                ; it is so return it
                (car (car x))
                ;it's not 
                (coeff (cdr x) y)
            )
        )
        ;otherwise it's dense so convert
        (else
            (coeff (to-sparse x 0) y)
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
            (degree (to-sparse polynomial 0))
        )))

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

    (cond
        ; check if both list is empty
        ((and (empty? x) (empty? y))
            '()
        )
        ;checking either list is empty
        ((or (empty? x) (empty? y))
            (if (empty? x)
                y ; x is empty so return y as result
                x ; y is empty so return x as result
            )
        )
        ;neither are so check type

        ((and (is-sparse? x) (is-sparse? y))
            ; both are sparse polynomials
            (to-sparse(add-Poly (to-dense x 0) (to-dense y 0)) 0)
        )

        (else
            ; one of them isn't convert which ever one is 
            ; sparse to be dense and call helper function to calcualte addtion
            (add-Poly (to-dense x 0) (to-dense y 0)))
        )
)

; helper function to handle adding to polynomials together
(define (add-Poly x y) 
    (cond
        ;check if both lists are empty
        ((and (empty? x) (empty? y) ) '())

        ;x and y are both not empty, see if x is
        ((empty? x)
            (append (list (car y)) ( add-Poly x (cdr y))))

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
    (map (lambda (x) (* -1 x)) polynoimal))

; subtracts polynomial y from polynomial X
; first checks the types of poly nomials
(define (subtract x y)
    ; check if both polynomials are sparse
    (if (and (is-sparse? x) (is-sparse? y))
        ; both are sparse polynomials
        (to-sparse (add-Poly (to-dense x 0) (invert-coef (to-dense y 0))) 0)
    
        ; one of them isn't convert which ever one is 
        ; sparse to be dense and call helper function to calcualte addtion
        (add-Poly (to-dense x 0) (invert-coef (to-dense y 0)))))

; helper function to multiply polynomial term x by term y
(define (multiply-terms x y)
    (list (list
        (* (car x) (car y))
        ;cadr means get character of the next element form current point
        (+ (cadr x) (cadr y)))))

; helper function to apply polynomials y term to polynomial x
(define (apply-y-to-x x y)
    ; check if the end of the polynomial x ahs been reached
    (cond
        ; check if y is empty
        ((empty? x) '())
        ;still not at end of polynomial so still able to apply y to x
        (else
           (append  (multiply-terms (car x) y) (apply-y-to-x (cdr x) y)))))

; helper function to move through the terms in polynomial y 
(define (multiply-poly x y)
    (if (> (length y) 1)
        ( if (> (length y) 2) 
            (append  (add (apply-y-to-x x (car y)) (multiply-poly x (cdr y))))
            (append  (add (apply-y-to-x x (car y)) (apply-y-to-x x (cadr y))))
        )
        (append(apply-y-to-x x (car y)))
    ))

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
    ))

;function used to find the quotient of p(x) and q(x)
(define (quotient polyOne polyTwo x)
 ; is this still a polynomial
  (if (list? polyOne)
      ;make it an int
      (quotient (eval polyOne x) (eval polyTwo x) 0)
      ;is poly one negative?
      (if(negative? polyOne)
         ;are both poly's negative?
         (if(negative? polyTwo)
            ; now that we know how to deal with it, can we get any closer to 0?
            (if(positive? (- polyOne polyTwo))
               x
               (quotient (- polyOne polyTwo) polyTwo (+ x 1)))
            (if(positive? (+ polyOne polyTwo))
               x
               (quotient (+ polyOne polyTwo) polyTwo (- x 1))))
         ;poly one is positive, is polytwo negative?
         (if(negative? polyTwo)
            ;now that we know how to deal with it, can we get closer to 0?
            (if(negative? (+ polyOne polyTwo))
               x
               (quotient (+ polyOne polyTwo) polyTwo (- x 1)))
            (if(negative? (- polyOne polyTwo))
               x
               (quotient (- polyOne polyTwo) polyTwo (+ x 1)))))))

;function used to find the remainder of p(x) and q(x)
;works the same as quotient, just returns remainder instead
(define (remainder polyOne polyTwo x)
 ; is this still a polynomial
  (if (list? polyOne)
      ;make it an int
      (remainder (eval polyOne x) (eval polyTwo x) 0)
      ;is poly one negative?
      (if(negative? polyOne)
         ;are both poly's negative?
         (if(negative? polyTwo)
            ; now that we know how to deal with it, can we get any closer to 0?
            (if(positive? (- polyOne polyTwo))
               polyOne
               (remainder (- polyOne polyTwo) polyTwo x))
            (if(positive? (+ polyOne polyTwo))
               polyOne
               (remainder (+ polyOne polyTwo) polyTwo  x)))
         ;poly one is positive, is polytwo negative?
         (if(negative? polyTwo)
            ;now that we know how to deal with it, can we get closer to 0?
            (if(negative? (+ polyOne polyTwo))
               polyOne
               (remainder (+ polyOne polyTwo) polyTwo x))
            (if(negative? (- polyOne polyTwo))
               polyOne
               (remainder (- polyOne polyTwo) polyTwo  x))))))

; helper function of derivative that applys the power rule of a polynomial 
; passed into it

(define (power-rule x)
    ;making sure to not pass back negatives
    (cond 
        ; if power is less then 0 becomes zero then just return empty list
        ((< (- (cadr x) 1) 0)
            '()
        )
        (else 
            (list(list (* (car x) (cadr x))
                (- (cadr x) 1)))) 
    )
)

; finds the dervative of a polynomial  x
; works for both sparse and dense representations of polynomials
(define (derivative x)
    (cond
        ;check if polynomial is empty and or at it's end
        ((empty? x) '())

        ; check if sparse
        ((is-sparse? x) 

           (append (power-rule (car x)) (derivative (cdr x)))
        )

        ; not empty, or sparse meaning it is dense
        (else
            (to-dense (derivative (to-sparse x 0)) 0)
        )

    
    )
)

