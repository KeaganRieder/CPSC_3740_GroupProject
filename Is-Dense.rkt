#lang racket
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

; returns the coefficent of x^y converts all lists to dense for ease of implementation negative y will break this
(define (coeff x y)
  ;round about way of asking is y less then or equal to length x
  (if (negative? (- y (length x)))
              (list-ref (make-dense x) y)
              ;returns false if y will not work
              false))

(coeff (list (list 1 0) (list 2 1) (list 3 2) (list 4 3)) 4)
