; Utility functions useful throughout.

; Square of a number.
(define (square x)
  (* x x))

; nth item in a list, one-indexed.
; so (nth lst 2) gives 2nd element in the list.
(define (nth lst i)
  (if (= i 1)
    (car lst)
    (nth (cdr lst) (- i 1))))

; Integer exponentiation. 
(define (ipow a x)
  (if (=  x 0) 
    1
    (* a (ipow a (- x 1)))))

; Extract the maximum a-th power that divides n
(define (factor-power n a)
  (if (not (= 0 (remainder n a)))
    0
    (+ 1 (factor-power (/ n a) a))))
