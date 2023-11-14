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

