#|
    defines how to chekc if a polynomial is zero.
    designed in a way it doesn't matter the type of polynomial 
    inputed sense it will just get th esub list if it's sparse 
    or the ucrrnet element if ot's dense
|#

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

(display(is-zero? '(0)))
(newline)
(display(is-zero? '((0 0))))
(newline)