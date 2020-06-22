(load "utils.ss")

; Chapter 2, Building abstractions with data.

; Data abstraction is the idea of separating how a compound data object is used
; from how such an object is represented. It enables us in thinking at a higher 
; conceptual level, just as we were able to do with procedural abstraction.

; Scheme has an in-built gcd function. 

; Ex 2.1, make-rat, numer, denom and print-rat procedures. Make-rat will work with 
; positive and negative denominators.
(define (make-rat n d)
  (if (< d 0)
    (make-rat (- n) (- d))
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g)))))

(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat))
  (newline))

; Ex 2.2, representing line segments on a 2-d plane.
; First, we represent points
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))

; Print-point displays a point.
(define (print-point pt)
  (display "(")
  (display (x-point pt))
  (display ",")
  (display (y-point pt))
  (display ")")
  (newline))

; Now, we represent each line segment in terms of its start/end points.
(define (make-segment point1 point2)
  (cons point1 point2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

; midpoint of a line segment.
(define (mid-point segment)
  (let ((p1 (start-segment segment))
        (p2 (end-segment segment)))
    (let ((x1 (x-point p1)) (y1 (y-point p1))
          (x2 (x-point p2)) (y2 (y-point p2)))
      (make-point (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0)))))

; Ex 2.3, a representation for rectangles on 2-d plane. We can use
; its four corners in clockwise order for the representation. 
; Note, we're supposed to use cons instead of list as the glue here.

; We first define point-dist function to calculate euclidean distance b/w
; two points.
(define (point-dist p1 p2)
  (let ((x1 (x-point p1)) (y1 (y-point p1))
        (x2 (x-point p2)) (y2 (y-point p2)))
    (let ((dx (- x1 x2)) (dy (- y1 y2)))
      (sqrt (+ (* dx dx) (* dy dy))))))

; We use a list of the 4 corners in clockwise order to represent the rectangle.
; Interface we provide to construct & manipulate rectangles is
; make-rect -> constructor
; get-corner i -> returns ith corner of the rectangle. 1 <= i <= 4
; We also provide rect-height & rect-width routines to make it easy for users
; to compute area/perimeter of the rectangles.

(define (make-rect p1 p2 p3 p4)
  (list p1 p2 p3 p4))

(define (get-corner rect i)
  (nth rect i))

(define (rect-height rect)
  (point-dist (get-corner rect 1) (get-corner rect 4)))

(define (rect-width rect)
  (point-dist (get-corner rect 1) (get-corner rect 2)))

(define (rect-area rect)
  (let ((h (rect-height rect)) (w (rect-width rect)))
    (* h w)))

(define (rect-perimeter rect)
  (let ((h (rect-height rect)) (w (rect-width rect)))
    (* 2 (+ h w))))

; If we wish to use another representation, we only need modify definitions of
; make-rect and get-corner. Our abstractions rect-height and rect-width will
; still work. Other potential representations could be to use only 3 corners
; instead of all 4 or to use center of the rectangle and one of the sides (side
; can be represented as a line segment)

; Ex 2.4 Procedural representation of pairs. cons-1, car-1 and cdr-1 are valid
; implementation of the pair data type. 
(define (cons-1 x y)
  (lambda (m) (m x y)))
(define (car-1 pair)
  (pair (lambda (p q) p)))
(define (cdr-1 pair)
  (pair (lambda (p q) q)))

; Ex 2.5 Represent pairs (a, b) where a and b are non-negative integers as
; integers.
; ipow a b = a^b where a and b are integers and b is non-negative.
; ipow, factor-power are defined in the utils module.
(define (make-int-pair a b)
  (* (ipow 2 a) (ipow 3 b)))

(define (int-car ipair)
  (factor-power ipair 2))

(define (int-cdr ipair)
  (factor-power ipair 3))
; We can easily verify in REPL that make-int-pair, int-car and int-cdr are a
; valid representation scheme for pairs of non-negative integers (not
; considering integer overflow issues)

; Ex 2.6 
; We assume that we're dealing the space of (number -> number) functions here.
; Let's denote these as r-functions.
; id is the identity function. i.e. id(x) = x for all numbers x.
; In the Church numeral system, each non-negative integer represents a mapping
; from one r-function to another r-function, i.e. each integer is itself a 
; function of r-function whose result is also an r-function.

; In this system, 0 maps every r-func to id. i.e. zero(f) = id for all
; r-functions f.
; 1 maps every r-func to itself. i.e., one(f) = f for all r-funcs f.
; 2 maps every r-func f to f o f (f of f). i.e. 2(f)(x) = f(f(x)) for all x.

; Intuitively, church numeral n represents n-time repeated application of a
; function.

; Addition of two Church numerals n and m is simply composition of n and m.
; (define add-church n m)
;   (lambda (f) (n (m f))))

; Ex 2.7 Interval arithmetic.
(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (min (car interval) (cdr interval)))

(define (upper-bound interval)
  (max (car interval) (cdr interval)))

; Add-interval.
(define (add-interval r1 r2)
  (let ((a1 (lower-bound r1)) (b1 (upper-bound r1))
        (a2 (lower-bound r2)) (b2 (upper-bound r2)))
    (make-interval (+ a1 a2) (+ b1 b2))))

; Mul-interval
(define (mul-interval r1 r2)
  (let ((a1 (lower-bound r1)) (b1 (upper-bound r1))
        (a2 (lower-bound r2)) (b2 (upper-bound r2)))
    (let ((p1 (* a1 a2))
          (p2 (* a1 b2))
          (p3 (* b1 a2))
          (p4 (* b1 b2)))
      (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))))

; reciprocal-interval, assuming the given interval doesn't include zero.
(define (reciprocal-interval r)
  (let ((a (lower-bound r)) (b (upper-bound r)))
    (make-interval (/ 1.0 b) (/ 1.0 a))))

; Div-interval, assuming that the second interval doesn't include zero.
(define (div-interval r1 r2)
  (mul-interval r1 (reciprocal-interval r2)))

; Scale-interval, where the interval is multiplied by a scalar.
(define (scale-interval x r)
  (let ((a (* x (lower-bound r))) (b (* x (upper-bound r))))
    (make-interval (min a b) (max a b))))

; Ex 2.8, Sub-interval interval x - y = x + (-1) * y
(define (sub-interval r1 r2)
  (add-interval r1 (scale-interval -1 r2)))

; Ex 2.9, Adding or subtracting two intervals results in a width that's 
; sum of the widths of the argument intervals. Multiplying or dividing
; intervals, resulting width depends not just upon the widths, but on the 
; actual lower and upper bounds of the argument intervals.

; Ex 2.10, Here, we redefine reciprocal-interval so it throws an error
; when the argument spans zero.
(define (reciprocal-interval r)
  (let ((a (lower-bound r)) (b (upper-bound r)))
    (if (and (<= a 0) (>= b 0))
      (error "The given interval spans zero, reciprocal is undefined.")
      (make-interval (/ 1.0 b) (/ 1.0 a)))))

; Ex 2.12 is essentially asking us to define a new interface  for interval
; arithmetic - one that uses center and tolerance percentages. We can reuse
; the earlier endpoint based interface.
(define (make-center-percent center tolerance)
  (let ((width (* 0.01 (abs center) tolerance)))
    (make-interval (- center width) (+ center width))))

(define (center interval)
  (let ((a (lower-bound interval)) (b (upper-bound interval)))
    (/ (+ a b) 2.0)))

(define (width interval)
  (let ((a (lower-bound interval)) (b (upper-bound interval)))
    (/ (- b a) 2.0)))

(define (print-center-percent r)
  (display (center r))
  (display " +- ")
  (display (* 100.0 (/ (width r) (center r))))
  (newline))

; Ex 2.13, 2.14, 2.15 and 2.16 deal with precision arithmetic and how different
; ways of computing mathematical expressions can have different errors.
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2) (add-interval r1 r2)))

(define (par2 r1 r2)
  (reciprocal-interval 
    (add-interval (reciprocal-interval r1) (reciprocal-interval r2))))

; Ex 2.14 par1 and par2 don't always result in the same results, reason being that
; in computers, floating point arithmetic is not exact and different ways
; of computing the same algebraic expression can result in different values.

; Ex 2.15 and 2.16, testing with few different intervals, it looks like par2 always 
; results in lower errors. Here is a simple procedure to help in testing.
; We take two intervals r1 and r2, we make r2 an integer multiple of r1.
; that is center(r2) = k * center(r1) where k is a positive integer.
; We use a small tolerance of 0.001 %, that is our width is 1 / 100000 of
; the center.

(define (check-parallel-methods k)
  (let ((tolerance 0.001))
    (define r1 (make-center-percent 1.0 tolerance))
    (define r2 (make-center-percent k tolerance))
    (let ((p1 (par1 r1 r2))
          (p2 (par2 r1 r2)))
      (print-center-percent p1)
      (print-center-percent p2))))

; Ex 2.15 and 2.16, It turns out that par2 always results in an error that is three times 
; smaller than par1. So, Eva's assertion seems true atleast in case of 
; parallel resistance computation. In general, I think every occurrence of a 
; variable contributes to the total error - here, par1 has 4 variables vs par2 has
; only 2.

; In general, to reduce our total error, we'll need to rewrite a given
; algebraic expression into an equivalent form that uses the least no. of
; variable occurrences - I think this is a very hard theoretical problem.


; Section 2.2
; Lists, recursive data structures, closure property of data glue.

; Ex 2.17
; Returns a list with single element - the last element of the given list.
; elems - a non-empty list.
(define (last-pair elems)
  (let ((t (cdr elems)))
    (if (null? t)
      (list (car elems))
      (last-pair t))))

; Ex 2.18, Reverse a list.
(define (list-reverse elems)
  (define (reverse-internal elems acc)
    (if (null? elems)
      acc
      (reverse-internal (cdr elems) (cons (car elems) acc))))
  (reverse-internal elems '()))

; Ex 2.19
; first-denomination is car
; except-first-denomination is cdr
; no-more? is null?
; Order of the values in the list doesn't matter.

; Ex 2.20 Here, the rest-arguments construct is introduced. 
(define (same-parity first . rest)

  (define (filter-internal test? elems)
    (if (null? elems) 
      '()
      (let ((f (car elems)) (r (filter-internal test? (cdr elems))))
        (if (not (test? f)) 
          r
          (cons f r)))))

  (filter-internal 
    (lambda (x) (= (remainder first 2) (remainder x 2)))
    (cons first rest) ))

; Ex 2.21
(define (square-list elems)
  (map (lambda (x) (* x x)) elems))

; Ex 2.22
; First way starts with an empty list and appends the squares to the front
; of the list - thus the end result is reverse of what we want. 

; Changing the order of the cons doesn't work because cons needs second 
; argument as a list to produce a list - otherwise it's just a pair.

; Ex 2.23, a recursive implementation of for-each. 
; Note - foreach is already built into scheme.
(define (for-each-2 proc elems)
  (if (null? elems)
    #t
    (begin
      (proc (car elems)) 
      (for-each-2 proc (cdr elems)) )))

; Ex 2.27 deep reversing a list is to reverse it and reverse all
; its element lists as well, recursively.
; We can use list-reverse defined earlier and map to accomplish this.
(define (deep-reverse elems)
  (define (rev elem)
    (if (list? elem) 
      (deep-reverse elem)
      elem))

  (list-reverse (map rev elems)))

; Ex 2.28 Accumulate all the leaf elements in a tree into a single list.
; We define a tree as a list where some of its elements can themselves be trees.

; Note, we use apply function here which is not introduced yet in the book. 
; `apply` takes a procedure, a list which specifies the arguments of the 
; procedure and returns the result obtained by calling the procedure with those
; arguments. 
(define (fringe tree)
  (if (list? tree)
    (apply append (map fringe tree))
    (list tree)))

; Ex 2.29 binary mobile.

; Branch of a binary mobile - length of the branch and a structure which is
; either a simple weight or a binary mobile itself.
(define (make-branch len structure)
  (list len structure))

; A binary mobile has left and right branches.
(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

; Total weight of a binary mobile is the sum of all weights in it.
; We can construct a list of all weights in the mobile and sum them.
(define (total-weight mobile)
  (apply + (fringe mobile)))

; Whether the mobile is balanced. 
(define (balanced? mobile)
  (if (null? mobile)
    #t
    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
      (and
        (balanced? left)
        (balanced? right)
        (= (* (branch-length left) (total-weight left))
              (branch-length right) (total-weight right))))))

; Ex 2.30 & 2.31 square-tree analogous to square-list.
; Let's first define a map-tree procedure that works similar to map-list.
; It preserves the tree structure while transforming the leaves.
(define (tree-map func tree)
  (if (list? tree)
    (map (lambda (e) (tree-map func e)) tree)
    (func tree)))

; Now, square-tree is simply map-tree square.
(define (square-tree tree)
  (tree-map square tree))

; Ex 2.32 Powerset aka the set of all subsets of a given set.
(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
      (append
        rest
        (map (lambda (set) (cons (car s) set)) rest)))))

; Now we learn to use standard sequence operations (map, filter, reduce etc..)
; for composing higher level transformations on list/sequence data.
; Because these are standard operations, their meaning is well understood and
; and clarity of the program is improved.
; This improves program modularity as mapping, filtering, accumulating are
; standard sequence operations and are independent of the specific transform-
; ations being performed.

; Filter is not a scheme builtin, so we define our own.
(define (filter predicate? elems)
  (if (null? elems)
    '()
    (let ((rest (filter predicate? (cdr elems))))
      (if (predicate? (car elems))
        (cons (car elems) rest)
        rest))))

; Map is a builtin, no need to implement our own.

; Let's now define reduce aka accumulate.
(define (accumulate op initial elems)
  (if (null? elems)
    initial
    (op (car elems)
        (accumulate op initial (cdr elems)))))

; Ex 2.33
; (map p sequence) = (accumulate (lambda (e s) (cons (p e) s)) '() sequence)
; (append seq1 seq2) = (accumulate cons seq2 seq1)
; (length seq) = (accumulate (lambda (x y) (+ 1 y)) 0 seq)

; Ex 2.34
; coeffs are (a0 a1 ... an)
(define (horner-eval x coeffs)
  (accumulate (lambda (a p) (+ a (* x p))) 0 coeffs))

; Ex 2.35.
(define (count-leaves tree)
  (accumulate (lambda (x y) (+ 1 y)) 0 (fringe tree)))

; Ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons
      (accumulate op init (map car seqs))
      (accumulate-n op init (map cdr seqs)))))

; Ex 2.37, vector/matrix represented as lists(sequences).

; Dot product of two vectors.
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Matrix vector product - matrix is mxn, vector is nx1
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

; matrix product - matrix1 is mxn, matrix2 is nxp.

; Let's first define the transpose of a matrix. If a matrix is a list of 
; row vectors, its transpose is the list of its column vectors.
(define (transpose-matrix mat)
  (accumulate-n cons '() mat))

; matrix product A x B
(define (matrix-*-matrix m1 m2)
  (transpose-matrix
    (map (lambda (col) (matrix-*-vector m1 col)) (transpose-matrix m2))))

; Ex 2.38
; For fold-right and fold-left to yield equal values, operator must be 
; associative.

; Ex 2.39
; For fold-left, we keep consing the current element in sequence to the
; accumulated result list.
; (reverse sequence) = (fold-left (lambda (acc x) (cons x acc)) '() sequence) 

; For fold-right, we must append the current element in sequence to end of 
; the accumulated result. We use the append builtin to accomplish this.
; (reverse sequence) = (fold-right (lambda (x acc) (append acc (list x))) '() sequence)

; enumerate-intervals and flatmap are defined in utils module.

; Ex 2.40
; prime? procedure is defined in utils.
(define (unique-pairs n)
  (flatmap 
    (lambda (i) (map (lambda (j) (list j i)) (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (filter
    (lambda (pair) (prime? (+ (car pair) (cadr pair))))
    (unique-pairs n)))

; Ex 2.41

; Let's first define unique-triples analogous to unique-pairs above.
(define (unique-triples n)
  (flatmap
        (lambda (pair) 
          (map (lambda (k) (cons k pair))
                   (enumerate-interval 1 (- (car pair) 1))))
        (unique-pairs n)))

; Find all triples 1 <= i < j < k <= n s.t i + j + k = s
(define (find-triples-with-sum n s)
  (filter
    (lambda (triple) (= s (apply + triple)))
    (unique-triples n)))

; Ex 2.42 8 queens problem.


; n stands for board-size - nxn board.
(define (queens n)
  (define empty-board '())

  ; Let's insert the latest queen in the beginning.
  (define (adjoin-position new-row k rest-of-queens)
    (cons (cons new-row k) rest-of-queens))

  ; check if two queens are safe i.e. they don't attack each other.
  (define (safe-pair? queen1 queen2)
    (let ((r1 (car queen1)) (c1 (cdr queen1))
          (r2 (car queen2)) (c2 (cdr queen2)))
      (not 
        (or (= r1 r2)
            (= c1 c2)
            (= (+ r1 c1) (+ r2 c2))
            (= (- r1 c1) (- r2 c2))))))

  ; Check if the latest(first in position) queen is safe w.r.t all other
  ; queens in the given arrangement. 
  (define (safe? k positions)
    (let ((qk (car positions)) (rest (cdr positions)))
      (accumulate (lambda (pos acc) (and acc (safe-pair? pos qk))) #t rest)))

  (define (queen-cols k)
    (if (= 0 k)
      (list empty-board)
      (filter (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
              (enumerate-interval 1 n)))
          (queen-cols (- k 1))))))

  (queen-cols n))

; Ex 2.43
; At k-th step, queen-cols(k - 1) will be evaluated n times instead of just
; once. If F(k) denotes the running time upto k steps and R(k) denotes the 
; no. of valid arrangements upto step k,
; F(k) = F(k - 1) * n + (filtering & flatmapping at k-th step)
; F(k) = F(k - 1) * n + O(R(k - 1) * n)

; But in the original algorithm, queen-cols(k - 1) will be evaluated only once.
; F(k) = F(k - 1) + O(R(k - 1) * n)
