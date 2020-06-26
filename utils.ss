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

; Remove an item from list if it exists and return the remaining elements 
; in a new list.
(define (list-remove x seq)
  (filter (lambda (e) (not (equal? x e))) seq))

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

; List of all integers within the given range.
(define (enumerate-interval a b)
  (define (iter x)
    (if (> x b)
      '()
      (cons x (enumerate-interval (+ x 1) b))))

  (iter a))

; Filter filters a list according to a given predicate.
(define (filter predicate? elems)
  (if (null? elems)
    '()
    (let ((rest (filter predicate? (cdr elems))))
      (if (predicate? (car elems))
        (cons (car elems) rest)
        rest))))

; Let's now define reduce aka accumulate.
(define (accumulate op initial elems)
  (if (null? elems)
    initial
    (op (car elems)
        (accumulate op initial (cdr elems)))))

; flatmap takes a list, a function that operates on elements of the list
; and which produces lists - flatmap then appends all the lists together into a 
; single list. Flatmap is a frequently useful abstraction.
(define (flatmap proc sequence)
  (accumulate append '() (map proc sequence)))

; primality check.
(define (prime? n)
  (define (iter x)
    (or (> (* x x) n)
        (and (not (= 0 (remainder n x)))
             (iter (+ x 1)))))
  
  (and (> n 1) (iter 2)))

; zip two sequences together
(define (zip seq1 seq2)

  (define (zip-iter s1 s2 i)
    (cond
      ((or (null? s1) (null? s2)) '())
      (else (cons (cons (car s1) (car s2)) (zip-iter (cdr s1) (cdr s2) (+ i 1))))))

  (zip-iter seq1 seq2 0))

; Enumerator of a list is a list of its (element, index) pairs.
; e.g., (enumerate '(a b c)) = '((a 0) (b 1) (c 2))
(define (enumerate seq)
  (zip seq (enumerate-interval 0 (- (length seq) 1))))

; Filters a list using a predicate and indices of the elements.
; predicate must be a function that takes a pair of (element, index) and
; returns a boolean value. The returned list includes  the indices as well.
; e.g. ifilter (lambda (pair) (number? (car pair))) '(a b c d e 1 2 3) 
;       returns  ((1 . 5) (2 . 6) (3 . 7))
(define (ifilter pred seq)
    (filter pred (enumerate seq)))

; Check whether all elements in a list satisfy a given predicate.
(define (all? pred seq)
  (cond
    ((null? seq) #t)
    (else (and (pred (car seq))
               (all? pred (cdr seq))))))

; TODO - Note that all? and ifilter have predicates that accept a single pair
; as their arguments. A more readable solution is to have them accept two arguments - 
; the list index and the list element.
