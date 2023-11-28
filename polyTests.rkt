      
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
(coeff (coeff '((24 0) (10 1) (6 2)) (degree '((24 0) (10 1) (6 2)))) (degree '((24 0) (10 1) (6 2)))) ; need a check 

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

;should return none empty list
(display(add '((1 1) (2 2) (3 3) (6 5)) '()))
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

#|
   (2 + 1x + 2x^2 + 3x^3)(4 + 2x + 3x^2 + 4X^3)=
   12x^8 + 12x^7 + 12x^6 + 23x^5 + 22x^4 +20x^3 + 12x^2 + 6x + 8
|#
(display(to-sparse(multiply  '(2 1 2 3) '(4  2 3 4))0))
(newline)
#|
  (5 + 1875)/ (5 + 50)
  1885/55
  34
|#

(display(quotient '((2 1) (3 4)) '((1 1) (2 2)) 5))
(newline)
#|
  (5 + 1875)/ (-5 - 50)
  1885/-55
  -34
|#

(display(quotient '((2 1) (3 4)) '((-1 1) (-2 2)) 5))
(newline)

#|
  (-5 - 1875)/ (5 + 50)
  -1885/55
  -34
|#

(display(quotient '((-2 1) (-3 4)) '((1 1) (2 2)) 5))
(newline)

#|
  (-5 - 1875)/ (-5 - 50)
  -1885/-55
  34
|#

(display(quotient '((-2 1) (-3 4)) '((-1 1) (-2 2)) 5))
(newline)


#|
  (5 + 1875)/ (-5 - 50)
  1885/55
  15
|#

(display(remainder '((2 1) (3 4)) '((2 1) (2 2)) 5))
(newline)
#|
  (5 + 1875)/ (-5 - 50)
  1885/-55
  15
|#

(display(remainder '((2 1) (3 4)) '((-1 1) (-2 2)) 5))
(newline)

#|
  (-5 - 1875)/ (5 + 50)
  -1885/55
  -15
|#

(display(remainder '((-2 1) (-3 4)) '((1 1) (2 2)) 5))
(newline)

#|
  (-5 - 1875)/ (-5 - 50)
  -1885/-55
  -15
|#

(display(remainder '((-2 1) (-3 4)) '((-1 1) (-2 2)) 5))
(newline)

;should outpout ((-2 0) (6 1))
(display (derivative '((5 0) (-2 1) (3 2)) ))

(newline)
;should outpout ((-2 0) (6 1) (20 4))
(display (derivative '((5 0) (-2 1) (3 2) (4 5)) ))

;should outpout (-2 6  0  0 20)
(display (derivative '(5 -2 3 0 0 4 )))