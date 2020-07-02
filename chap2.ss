(load "utils.ss")

; Chapter 2, Building abstractions with data.

; Data abstraction is the idea of separating how a compound data object is used
; from how such an object is represented. It enables us in thinking at a higher 
; conceptual level, just as we were able to do with procedural abstraction. It
; also enables us to mix and match multiple representations of data.

; Main technique that enables data abstraction is the usage of generic interfaces
; for using/manipulating data - these interfaces work across different represe-
; ntations and even different types of data altogether, the user level code
; need not worry about the underlying structure or implementation and focus on
; higher level concerns.
;
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

; Addition of two Church numerals n and m.
; (define add-church n m)
;   (lambda (f)
;	(let ((g (m f)) (h (n f)))
;	   (lambda (x) (h (g x))))))

; Multiplication of church numerals is simply their composition.

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

; Ex 2.13
; Under small tolerance assumptions, tolerance of product of
; two intervals is sum of their tolerances.

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
  (define (branch-weight branch)
    (let ((st (branch-structure branch)))
      (if (number? st)
        st
        (total-weight st))))
  
  (+ (branch-weight (left-branch mobile)) 
     (branch-weight (right-branch mobile))))

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
; Map is a builtin, no need to implement our own.

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
  (length (fringe tree)))

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

; There's no way we can actually test painter operations without implementing
; a few basic painters.
;
; Ex 2.44
; up-split is similar to right split

; Dummy definitions for below and beside.
(define (below painter1 painter2)
  painter1)
(define (beside painter1 painter2)
  painter2)

(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

; Ex 2.45
(define (split op1 op2)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split op1 op2) (- n 1))))
        (op1 painter (op2 smaller smaller))))))

; Ex 2.46 2-d vectors.
(define (make-vect x y)
  (cons x y))

(define (x-coord vec)
  (car vec))

(define (y-coord vec)
  (cdr vec))

(define (add-vect vec1 vec2)
  (let ((x1 (x-coord vec1)) (y1 (y-coord vec1))
        (x2 (x-coord vec2)) (y2 (y-coord vec2)))
    (make-vect (+ x1 x2) (+ y1 y2))))

(define (scale-vect scale vec)
  (let ((x (x-coord vec)) (y (y-coord vec)))
    (make-vect (* scale x) (* scale y))))

(define (sub-vect vec1 vec2)
  (add-vect vec1 (scale-vect -1 vec2)))

; Ex 2.47 we only use the list constructor.
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

; Image ([0,0] x [1,1] unit square coordinates) to frame coordinate mapper
; function.
(define (frame-coord-map frame)
  (lambda (ivec)
    (add-vect (origin-frame frame)
                (add-vect (scale-vect (x-coord ivec) (edge1-frame frame))
                          (scale-vect (y-coord ivec) (edge2-frame frame))))))

; Ex 2.48 2-d line segment. 
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

; Ex 2.49, We assume the existence of segments->painter.
; A, B, C, D are four corners of the frame.
; A - (origin-frame frame)
; B - (add-vector A (edge1-frame frame))
; C - (add-vector B (edge2-frame frame))
; D - (add-vector A (edge2-frame frame))
; a) (segments->painter (list (make-segment A B) (make-segment B C) 
;      (make-segment C D) (make-segment D A))

; b) (segments-painter (list (make-segment A C) (make-segment B D)))
; c) define a procedure to find midpoint of a segment and use it.
; d) the `wave` painter paints a human standing and waving with her right 
; hand. It can be implemented as a segment-painter with a series of line segments 

; Exercises 2.50 - 2.52 not included.

; Section 2.3
; We learn to work with symbolic data.

; Ex 2.53
; (a b c)
; ((george))
; ((y1 y2))
; #f
; (y1 y2)
; #f
; (red shoes blue socks)

; Ex 2.54
; equal? is already a scheme builtin.
(define (my-equal? a b)
  (cond ((and (symbol? a) (symbol? b)) (eq? a b))
        ((and (null? a) (null? b)) #t)
        ((and (list? a) (list? b))
            (and (my-equal? (car a) (car b))
                 (my-equal? (cdr a) (cdr b))))
        (else #f)))

; Ex 2.55
; quote is a scheme keyword. quote special form quotes its argument form.
; thus the symbol 'a and (quote a) are eq? to each other.
; thus if we have a form like ''abcdef, it's equal? to '(quote abcdef)
; which is a list containing 2 symbols 'quote and 'abcdef
; this is why we can call car on expressions like ''abcdef and we'll get
; the symbol 'quote, which the interpreter displays without the quote.

; Section 2.3.2 Symbolic differentiation.

; Check for numbers
(define (=number? expr num)
  (and (number? expr) (= expr num)))

; variable
(define (variable? e)
  (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

; sum
(define (sum? e)
  (and (list? e) (eq? (car e) '+)))

; sum expressions can handle arbitrary number of addend terms.
(define (make-sum . es)
  (cons '+ es))

; First summand.
(define (addend sum-expr)
  (cond
    ((null? sum-expr) 0) ; '()
    ((= 1 (length sum-expr)) 0) ; '(+)
    (else (cadr sum-expr))))

; rest of the summands
(define (augend sum-expr)
  (cond
    ((null? sum-expr) 0)
    ((null? (cdr sum-expr)) 0)
    ((null? (cddr sum-expr)) 0)
    (else (apply make-sum (cddr sum-expr)))))

; product
(define (product? e)
  (and (list? e) (eq? (car e) '*)))

(define (multiplier e)
  (cadr e))
(define (multiplicand e)
  (caddr e))

(define (make-product e1 e2)
  (cond
    ((=number? e1 0) 0)
    ((=number? e2 0) 0)
    ((=number? e1 1) e2)
    ((=number? e2 1) e1)
    (else (list '* e1 e2))))

; exponentiation with a numeric exponent.
(define (make-exponent base power)
  (cond
    ((=number? power 0) 1)
    ((=number? power 1) base)
    (else (list '** base power))))

(define (exponentiation? expr)
  (and (list? expr) (eq? '** (car expr))))

(define (get-base expr)
  (cadr expr))
(define (get-power expr)
  (caddr expr))

; Derivative of an expression w.r.t a variable.
;
; Includes Ex 2.56, 2.57 but only accepts arbitrary no. of terms for
; sum expressions. Main idea is that deriv procedure depends only on
; addend/augend abstractions, but not on internal representations which
; improves code modularity and allows programmers to experiment with 
; various representation schemes.
(define (deriv expr var)
  (cond
    ((number? expr) 0)
    ((same-variable? expr var) 1)
    ((variable? expr) 0)
    ((sum? expr)
        (let ((e1 (addend expr))
              (e2 (augend expr)))
          (make-sum (deriv e1 var) (deriv e2 var))))
    ((product? expr)
        (let ((e1 (multiplier expr))
              (e2 (multiplicand expr)))
          (make-sum
            (make-product e2 (deriv e1 var))
            (make-product e1 (deriv e2 var)))))
    ((exponentiation? expr)
      (let ((base (get-base expr))
            (n (get-power expr)))
        (let ((f1 (deriv base var)))
          (make-product
            (make-product n (make-exponent base (- n 1)))
            f1))))
    ((infix-expr? expr)
      (deriv (parse-infix-expr expr) var))
    (else 0)))

; Ex 2.58
; for part a, it's simply a matter of modifying selectors and constructors of
; sum and product objects. Because `deriv` only uses these abstractions and
; doesn't directly access internal representations, it needs no changes.
; Modifying selectors and constructors to handle this part is easy enough 
; because there are always exactly 2 terms in sums and products and position
; of operators +/* is fixed in the middle. This is the whole point of this 
; exercise - by choosing suitable abstractions, we can separate the code
; that uses the objects from their representations - this way, our user level
; code(in this case, deriv procedure) doesn't have to change.

; part b is a much harder problem because we're dealing with arbitrarily 
; complex algebraic sum/product expressions.
; a given expression needs to be parsed to separate out sub-expressions 
; into addend/augends or multiplier/multiplicands.

; expr could be a number, a symbol or an infix algebraic expression with +/* 
; operators.
; e.g. 1, a, (a + b + c), (a + b * c + e * (1 + x))

; Out goal is to have `deriv` procedure work with infix expressions. We will
; need to modify it so that it tests for infix expressions and calls
; appropriate selectors/routines.

; We note that operator * has lower precedence than + and both operators are
; left associative.
; An expression like a + b * c can be parsed as (make-sum a (make-product b c))

; For simplicity, we only deal with variables that are single lowercase letters. 
(define (good-variable? expr)
  (and
    (symbol? expr)
    (let ((estr (symbol->string expr)))
      (and
        (= 1 (string-length estr))
        (char-lower-case? (string-ref estr 0))))))

; Infix expressions are lists whose elements can be numbers, symbols, operators
; and other infix expressions enclosed in parentheses. 
;
; Checks whether the expression is an infix-expression.
(define (infix-expr? expr)
  (cond
    ((number? expr) #t)
    ((good-variable? expr) #t)
    ((list? expr)
     (let ((operators (ifilter (lambda (pair) (odd? (cdr pair))) expr))
           (terms (ifilter (lambda (pair) (even? (cdr pair))) expr)))
       (and
         (all? (lambda (pair) (or (eq? '+ (car pair)) (eq? '* (car pair)))) operators)
         (all? (lambda (pair) (infix-expr? (car pair))) terms))))
    (else #f)))

; We have a way to detect whether a given expression is infix. We need to parse
; such expressions into equivalent sum/product expressions. In `deriv`, we will
; then first check for infix expressions and convert them into sum/product 
; expressions.
(define (parse-infix-expr expr)

  (define unity 1)

  (define (process-expr-list expr terms next_term)
    (if (null? expr)
      (apply make-sum (cons next_term terms))
      (let ((token (car expr))
            (rest (cdr expr)))
        (cond
          ((eq? token '+) (process-expr-list rest (cons next_term terms) unity)) ; A new term begins
          ((eq? token '*) (process-expr-list rest terms next_term))
          (else (process-expr-list
                        rest
                        terms 
                        (make-product next_term (parse-infix-expr token))))))))

  (cond
    ((not (infix-expr? expr)) (error "Invalid infix expression argument"))
    ((number? expr) expr)
    ((good-variable? expr) expr)
    ((list? expr)
      (process-expr-list expr '() unity))
    (else 0)))

; Section 2.3.3 Set data structure.
; Set data abstraction consists of the operations - element-of-set?, 
; adjoin-set, intersection-set, union-set. We explore possible alternatives 
; for set representation and their performance implications.

; Our first choice for set representation is to use lists of distinct elements.
(define (element-of-set? elem set)
  (cond
    ((null? set) #f)
    ((equal? (car set) elem) #t)
    (else (element-of-set? elem (cdr set)))))

(define (adjoin-set elem set)
  (cond
    ((element-of-set? elem set) set)
    (else (cons elem set))))

(define (intersection-set set1 set2)
  (cond
    ((or (null? set1) (null? set2)) '())
    (else (let ((f1 (car set1)) (r1 (intersection-set (cdr set1) set2)))
            (if (element-of-set? f1 set2)
              (cons f1 r1)
              r1)))))

; Ex 2.59
(define (union-set set1 set2)
  (accumulate adjoin-set set2 set1))

; Ex 2.60
; Allowing for duplicates lets us simplify some of the set operations.
; element-of-set? and intersection-set remain same, adjoin-set is simply cons
; and union-set is simply list append

; Ex 2.61
; adjoin-set operation under ordered list representation of sets.
(define (adjoin-set-2 elem set)
  (cond
    ((null? set) (list elem))
    ((= (car set) elem) set)
    ((< (car set) elem) (cons (car set) (adjoin-set-2 elem (cdr set))))
    (else (cons elem set))))

; Ex 2.62
; union-set operation using ordered list representation.
(define (union-set-2 set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else (let ((x1 (car set1)) (x2 (car set2)))
      (cond
        ((= x1 x2) (cons x1 (union-set-2 (cdr set1) (cdr set2))))
        ((< x1 x2) (cons x1 (union-set-2 (cdr set1) set2)))
        (else (cons x2 (union-set-2 set1 (cdr set2)))))))))

; Binary search tree representation of sets.

; Ex 2.63
; Both tree->list-1 and tree->list-2 produce the same result for any tree -
; the list of all the tree entries in sorted order. 
; tree->list-2 is O(n) (n is no. of nodes in tree) for any tree shape because
; it starts with an empty list and keeps consing onto it one element at a time.
;
; tree->list-1 is O(n^2) because it appends two subtree lists into a single 
; list. Imagine a right leaning chain of n nodes (each node has empty left tree).
; tree->list-1 is theta(n^2) in such cases.

; Ex 2.64
; partial-tree splits a list into two sublists(sub-trees) of roughly equal size.
; Because it does this recursively at every level, resulting binary tree will
; be a balanced binary tree and it's height will be O(logN)  where N is the 
; tree(list) size.
;
; Overall, list->tree is O(N) both in time and memory, where N is input list
; size. If F(n) denotes the runtime for list->tree on list of size n, then
; F(N) = F(N / 2) + F(N / 2) + O(1) (O(1) for make-tree operation). Overall,
; there are N make-tree operations and N recursive function calls, thus the 
; total complexity is O(N), both memory wise and cpu wise.

; Ex 2.65
; Given two sets in BST representation, we can call tree->list-2 on each of 
; them to produce 2 sorted lists in O(n) time and call the intersection-set,
; union-set procedures on these sorted lists. Both these operations are O(n)
; with sorted list representation, overall complexity is O(n).

; Ex 2.66
; lookup for a BST representation of database is same as the element-of-set?
; method of set in BST representation. I'm not going to implement it.

; Section 2.3.4 Huffman encoding. 

; Huffman tree representation - leaf nodes, non-leaf nodes etc...
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? node)
  (eq? 'leaf (car node)))

(define (symbol-leaf node)
  (cadr node))

(define (weight-leaf node)
  (caddr node))

(define (make-code-tree left right)
  (list
    left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))))

(define (left-branch tree)
  (car tree))

(define (right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

; Note - we use slightly different names for some of the procedures below.
;
; Decoding algorithm - given a bit sequence and a huffman tree, it decodes the
; sequence into the original message. 
(define (decode-huffman bits tree)

  (define (decode-internal bits node)
    (cond
      ((leaf? node) (cons (symbol-leaf node) (decode-internal bits tree)))
      ((null? bits) '())
      ((= 0 (car bits)) (decode-internal (cdr bits) (left-branch node)))
      (else (decode-internal (cdr bits) (right-branch node)))))
  
  (decode-internal bits tree))

; For generating huffman encoding trees, we need to work with sets of 
; nodes and keep adding two smallest elements at every step. An ordered list
; representation of sets make this task easier.

; Inserts a node into an ordered set of nodes at the right index.
(define (insert-huffman-set x set)
  (cond
    ((null? set) (list x))
    (else (let ((fst (car set)) (rst (cdr set)))
      (if (< (weight x) (weight fst))
        (cons x set)
        (cons fst (insert-huffman-set x rst)))))))

; Ex 2.69
; Huffman encoding tree generation algorithm - takes a list of (symbol, weight)
; pairs and constructs a code tree structure.
(define (generate-huffman-tree weighted-symbols)

  ; Keep merging the two smallest elements at each step until only one node
  ; remains.
  (define (generate-huffman-internal nodes)
    (cond
      ((= 1 (length nodes)) (car nodes))
      (else (generate-huffman-internal (insert-huffman-set
              (make-code-tree (car nodes) (cadr nodes))
              (cddr nodes))))))
  
  (define (order-nodes nodes)
    (cond
      ((null? nodes) '())
      (else (insert-huffman-set (car nodes) (order-nodes (cdr nodes))))))

  (generate-huffman-internal
    (order-nodes
      (map (lambda (pair) (make-leaf (car pair) (cdr pair))) weighted-symbols))))

; Ex 2.67
; Decoding the given message gives us the message "ADABBCA"

; Ex 2.68
(define (encode-huffman message tree)
  (if (null? message)
    '()
    (append (encode-huffman-symbol (car message) tree) 
            (encode-huffman (cdr message) tree))))

(define (encode-huffman-symbol symbol tree)
  (if (leaf? tree)
    (if (eq? (symbol-leaf tree) symbol)
      '()
      (error "Symbol not present in tree"))
    (let ((left (left-branch tree)) (right (right-branch tree)))
      (if (memq symbol (symbols left))
        (cons 0 (encode-huffman-symbol symbol left))
        (cons 1 (encode-huffman-symbol symbol right))))))

; Encoding the result from 2.67 returns the correct result using the above
; encode-huffman procedure.

; Ex 2.69 is implemented in generate-huffman-tree procedure above. Note, we're 
; using different names here from those in the book. 

; Ex 2.70
; Encoding the given song resulted in the sequence
; (1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 0 1 1 0 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0)
; which has 84 bits in it.
;
; With fixed bit encoding, we must use 3 bits per symbol. The original message 
; has 36 symbols in it, so it would need 36x3 = 108 bits. Using the huffman 
; encoding is clearly better which is expected.

; Ex 2.71
; With n symbols having weights (2^0, 2^1 ... 2^(n-1)), the resulting huffman 
; tree will have every internal node having a leaf node as one of its children.
; Height of such a tree will be n. In such an encoding, most common symbol
; will need 1 bit and least common symbol will need (n - 1) bits.

; Ex 2.72
; In the special case of 2.71, a symbol of frequency 2^i will need 
; i bits to encode and O(n^2) symbol set searches are required in order to 
; reach the symbol. For the most frequent symbol - will need theta(n) set searches
; and 1 bit to encode. For the least frequent symbol - will need theta(n^2)
; set searches and n - 1 bits to encode.

; Section 2.4
; Multiple data representations aka polymorphism in common parlance. We learn
; how to incorporate multiple designs(representations of a data object) into
; the same program by using `generic procedures` (aka interfaces), tagged types 
; and data-directed programming. 

; attach-tag, type-tag and contents can be used to tag data with types and
; in defining generic procedures that can support multiple representations
; of data.
(define (attach-tag tag contents)
  (cons tag contents))

(define (type-tag data)
  (if (pair? data)
    (car data)
    (error "Not valid type-tagged data")))

(define (contents data)
  (if (pair? data)
    (cdr data)
    (error "Not valid type-tagged data")))

; Ex 2.73
; a) Essentially, we're defining the `deriv` operation for various types of
; expressions. For numbers and variables, because we used them in their raw 
; form (i.e. without attaching any tag), there's no way to call operator on
; them and thus we can't put them into the dispatch table. For sum/product
; expressions, we type-tagged them e.g., (make-sum 'x 1) has the tag '+

; b) and c) are about installing differentiation procedures for sum/product/exponent
; expression types into the dispatch table.

; d) When installing differentiation procedures for various types, we'll need
; to change the operator and type order as well e.g., (put '+ 'deriv proc)
; But for this whole thing to make sense, we have to think of a derivative 
; datatype and +, *, ** as valid operations defined on that type - but the 
; problem is derivative datatype doesn't make sense - why would we create 
; derivative objects at all? Normally, we have expressions and we want to 
; find derivatives of them.
; 
; It makes more sense to define an expression datatype and procedures +, *
; etc... on them. So, we should be doing (put '+ 'expression proc)

; Ex 2.74
; We'll implement a simple dispatch table using lists so we can actually 
; build a working company-employee database system.

; Our dispatch table is implemented as two procedures put and get.
; Whenever we register a new operator for a datatype, we must change the value
; of this dispatch-table.
(let ((table '()))

  (define (put-internal operator type procedure)
    (let ((key (cons operator type)))
      (set! table
        (cons (cons key procedure)
              (filter (lambda (e) (not (equal? key (car e)))) table)))))

  (define (get-internal operator type)
    (let ((match (filter (lambda (e) (equal? (cons operator type) (car e)))
                         table)))
      (if (null? match)
        (error "Given operator/type combination not registered.")
        (cdr (car match) ))))

  ; Define the put/get variables at global level.
  (set! put put-internal)
  (set! get get-internal))

; let's now define the `apply-generic` procedure as in the book.` 
; `op` - operator (abstract procedure) name
; `args` - parameters for the operator
; Applies the matching procedure from dispatch table to the given arguments and
; returns the result or throws an error when there is no match.
(define (apply-generic op . args)
  (let ((types (map type-tag args)))
    (let ((proc (get op types)))
      (apply proc (map contents args)))))

; Now, to solve the actual problem, we need 2 abstract data types - employee 
; and personnel-file.
;
; The `employee` datatype must support `get-salary` and `get-name` abstract 
; procedures.
;
; The `personnel-file` datatype must support `get-record`, `add-record` and
; `delete-record` abstract procedures.
;
; As an example implementation, we'll use name/salary pairs for employees and
; list of employee records for personnel-file representations. It should be 
; noted that representations for these objects are independent - these objects 
; only interact with each other through their interfaces, if at all.

; employee datatype using a pair representation.
(begin
  (define EMP-TAG 'employee-pair)

  (define (make-employee name salary)
    (attach-tag EMP-TAG (cons name salary)))
  
  (define (get-name employee)
    (car employee))
  (define (get-salary employee)
    (cdr employee))

  (put 'make-employee (list EMP-TAG) make-employee)
  (put 'get-name (list EMP-TAG) get-name)
  (put 'get-salary (list EMP-TAG) get-salary))

; `get-name` abstract procedure for employee datatype.
(define (get-name employee)
  (apply-generic 'get-name employee))

; `get-salary` abstract procedure for employee datatype.
(define (get-salary employee)
  (apply-generic 'get-salary employee))

; `make-employee-pair` constructor creates an employee object with name 
; and salary. For each employee representation, we can create similar
; constructors.
(define (make-employee-pair name salary)
  ((get 'make-employee '(employee-pair)) name salary))

; Personnel-file abstract interface
;
; `get-record` accepts an employee name and returns either nil or an employee
; record with the given name.
;
; `add-record` accepts an employee object and adds it into its file.
;
; `delete-record` accepts an employee name and deletes the record with the name
; if it exists.

; Instead of having add-record and delete-record modify the personnel-file 
; object, they return a new modified(with a record added/removed resp.)
; personnel-file

; List representation for personnel-file
; For the moment, we ignore checking for duplicate employee names.
(begin
  (define PERSONNEL-FILE-LIST 'personnel-file-list)
  
  (define (make-personnel-file)
    (attach-tag PERSONNEL-FILE-LIST '()))

  (define (get-record record-list)
    (lambda (emp-name)
    (let ((matches (filter (lambda (emp) (equal? emp-name (get-name emp))) 
                           record-list)))
      (if (null? matches)
        '()
        (car matches)))))

  (define (add-record record-list employee)
      (attach-tag PERSONNEL-FILE-LIST (cons employee record-list)))

  ; Note, here we use get-name abstract procedure of employee objects.
  (define (delete-record record-list)
    (lambda (emp-name)
    (attach-tag PERSONNEL-FILE-LIST
        (filter 
          (lambda (emp) (not (equal? emp-name (get-name emp)))) 
          record-list))))

  (put 'get-record (list PERSONNEL-FILE-LIST) get-record)
  (put 'make-personnel-file (list PERSONNEL-FILE-LIST) make-personnel-file)
  (put 'add-record (list PERSONNEL-FILE-LIST) add-record)
  (put 'delete-record (list PERSONNEL-FILE-LIST) delete-record))

; `make-personnel-file-list` constructor for personnel files.
(define (make-personnel-file-list)
  ((get 'make-personnel-file '(personnel-file-list))))

; get-record, add-record and delete-record abstract procedures in the 
; personnel-file interface. We can't directly use apply-generic because
; sometimes our procedures accept raw strings as arguments. Note also that
; internal procedure implementations accept stripped-off contents of objects.
(define (get-record personnel-file emp-name)
  (let ((proc (get 'get-record (list (type-tag personnel-file)))))
    ((proc (contents personnel-file)) emp-name)))

(define (add-record personnel-file employee)
  (let ((proc (get 'add-record (list (type-tag personnel-file)))))
    (proc (contents personnel-file) employee)))

(define (delete-record personnel-file emp-name)
  (let ((proc (get 'delete-record (list (type-tag personnel-file)))))
    ((proc (contents personnel-file)) emp-name)))

; Now we have 2 abstract data types `employee` and `personnel-file` and we 
; have one representation for each.

; `find-employee-record` is a higher level procedure that takes a list of 
; `personnel-file` objects and find all the records among them with matching
; employee name.
(define (find-employee-record personnel-files emp-name)
  (filter
    ; Because our `get-record` abstract procedure may return an empty list
    ; if no matching record is found, we must filter them out.
    (lambda (entry) (not (null? entry))) 
    (map
      (lambda (file) (get-record file emp-name))
      personnel-files)))

; When a new company is acquired, glue code that translates from their existing
; interfaces to Insatiable's interfaces needs to be written.

; Ex 2.75
(define (make-from-mag-angle mag ang)
  (define (dispatch op)
    (cond
      ((eq? op 'magnitude) mag)
      ((eq? op 'angle) ang)
      ((eq? op 'real-part)
        (* (cos ang) mag))
      ((eq? op 'imag-part)
       (* (sin ang) mag))
      (else (error "Invalid operation" op))))

  dispatch)

; Ex 2.76
; With explicit dispatch, whenever a new type is introduced into the system,
; all the relevant existing generic procedures must be modified to accomodate 
; this type, which can require a lot of changes in many different places.
; When a new generic operation is defined, it must be compatible with all the
; relevant existing types. Because this is typically implemented as a bunch of
; if/else condition chains, it gets messier and more prone to bugs. 
; 
; With data directed programming, when introducing a new generic procedure, 
; we must define its implementations for all relevant and existing types and 
; register them with the dispatch table. Likewise when a new type is defined,
; we must identify its abstract interface, implement that interface for all its
; representations and register them in the table.

; With message passing, when a new generic procedure is introduced, it must be 
; implemented for all the relevant existing types. Whenever a new type is
; defined, it's abstract interface must be specified and implemented for its
; representations.

; If we're often introducing new types, then we have a lot of types and a few
; generic procedures. It's more or less equal work with either message passing
; or data directed programming (see the 2 paragraphs above). Message passing
; technique is preferred because it cleanly separates different types and 
; improves modularity of the program.

; If we're often introducing new generic operations, then we have a small no.
; of types and quite a no. of operations. With message passing system, we'll
; have to change existing code for types every time a new operation is added.
; It's not going to be additive. With data directed programming, by simply adding 
; new implementations and regsiter them in the table, we don't have to change
; any existing code. So, DDP is to be preferred here.

; Section 2.5
; We learn to build procedures that are generic over not only different
; representations, but over different kinds of objects altogether.

; Ex 2.77
; We assume that complex number interface has already been defined, its two
; representations implemented and installed.
;
; We have the object '(complex (rectangular 3 4)). Internally, complex 
; arithmetic package deals with objects of rectangular and polar 
; representations. When we include the new statements suggested by Alyssa,
; we're defining a new generic `magnitude` procedure for numbers (though it
; only works with complex numbers for now).
;
; Overall, apply-generic is called twice.
;   1) - (apply-generic 'magnitude '(complex (rectangular 3 4)))
;        which translates to (magnitude '(rectangular 3 4)) where magnitude is
;        the generic procedure for complex numbers, which again calls 
;   2) - (apply-generic 'magnitude '(rectangular 3 4)) which essentially 
;       computes the hypotenuse.

; Ex 2.78
; We can get away without using type tags for scheme numbers by using 
; number? primitive.
; type-tag will then look like
; (define (type-tag data)
;    (if (number? data) 
;          'scheme-number
;          (if (pair? data) (car data) (error "Invalid data"))))
;
; contents will also be similarly modified - first calls number?, if matches
; then simply returns the data, otherwise strips off the tag.
;
; attach-tag will simply return any primitive number as it is, without
; attaching any tag. 

; Ex 2.79
; We assume equ? will be called with two arguments of same type - either both
; are rationals or both are complex or both are primitive numbers. To do this
; we must implement this operation for all the three types. 
; Note here that scheme's `=` builtin checks if two numbers are equal in value.
;
; scheme-numbers -> (equ? num1 num2) = (= num1 num2)
; rationals -> (equ? rat1 rat2) = (= (* (numer rat1) (denom rat2)) (* (numer
; rat2) (denom rat1)))
; complex -> (equ? comp1 comp2) = (and (= (real comp1) (= real comp2)) 
;            (= (imag comp1) (imag comp2)))

; We install the implementations like this (within our primitive arith. package)
; (put 'equ? '(scheme-number scheme-number) equ?)

; Then we can simply write generic procedure `equ?` as 
(define (equ? num1 num2)
  (apply-generic 'equ? num1 num2))

; Ex 2.80
; Similar exercise to that of 2.79, define implementations for the three
; types, install them in dispatch table, define the `zero?` generic procedure
; using apply-generic.

; Ex 2.81
; a) Because the `expt` generic procedure is implemented only for integers, 
; (expr complex1 complex2) will raise an error, because even after trying 2 
; ways of coercions, no matching implementation will be found.
;
; b) We can modify `get-coercion` such that (get-coercion t t) returns t for
; all types t. Then apply-generic can work as it is.
;
; c) We can check in `apply-generic` if the two arguments are of same type and
; avoid coercion. But modifying `get-coercion` is the cleaner approach.

; Ex 2.82
; Even with this modification, `apply-generic` only looks for procedures in the
; dispatch table whose argument types are all same. Suppose we have a generic
; procedure fp that takes three numbers and it only has an implementation for
; the type combination '(scheme-number complex complex). This combination wont
; be tried.

; Ex 2.83
; Hierarchy is integer -> rational -> real -> complex
(define (raise-integer number)
  (make-rat number 1))
(define (raise-rational number) ; denominator is not zero for rationals
  (make-real (/ (numer number) (denom number))))
(define (raise-real number)
  (make-complex-from-real-imag (number) 0))
; For complex numbers, raise does nothing, returns the input itself.
(define (raise-complex number)
  number)

(put 'raise 'integer raise-integer)
(put 'raise 'rational raise-rational)
(put 'raise 'real raise-real)
(put 'raise 'complex raise-complex)

; Now, the generic `raise` is defined as
(define (raise number)
  (apply-generic 'raise number))

; Ex 2.84
; For each type hierarchy, if we can find out it's level in the hierarchy,
; then we can figure out which type is higher up by comparing their levels. 
; e.g., in the integer -> rational -> real -> complex hierarchy, we can think 
; of integer having level 3, rational at 2, real at 1 and complex at 0.
; Higher level number means lower position in the hierarchy.
; 
; To compute the levels of types in a hierarchy, all we need to know is 
; the highest type in that hierarchy. Let's assume 'complex is the highest
; level for this exercise.
(define highest-level 'complex)

(define (find-type-level data)
  (if (equal? (type-tag data) highest-level)
    0
    (+ 1 (find-type-level (raise data)))))

; With `highest-level` and `find-type-level` defined, it's easy to add new
; types into the hierarchy. If we're adding a supertype of current highest
; type, then we simply update the value of `highest-level` and implement
; `raise` for this new type and the old highest type. Otherwise, we must
; implement `raise` for this new type and also possibly need to modify `raise`
; for a type just one level lower.

; Ex 2.85
; `project` is like the inverse of `raise`. One way to implement `project` is
; - for complex numbers, if the imaginary part is zero (can be checked with
; zero?), then project returns (make-real (real-part num)), otherwise it
; doesn't change the argument - returns the number as it is. For real numbers, 
; we must pick a close enough (within an accepted error range) rational number.
; We can convert rationals to integers if their denominators divide numerators.
; For integers, project does nothing.

(define (project-integer num)
  num)
(define (project-complex num)
  ; real-part and imag-part are generic procedures we defined for complex
  ; numbers.
  (let ((r (real-part num)) (c (imag-part num)))
    (if (zero? c)
      (make-real r)
      num)))
(define (project-rational num)
  (let ((n (numer num)) (d (denom num)))
    (if (= 0 (remainder n d))
      (make-integer (/ n d))
      num)))
(define (project-real num)
  ; error = 10^-9
  ; Pick n and d s.t. num - error <= n / d <= num + error
  ; pick large enough d s.t. d * error >= 1, then n = ceiling(d * (num - error))
  ; We must modify `equ?` for reals. Two reals are considered equal if they're 
  ; within the error range.
  (let ((d 1000000000))
    (make-rational (ceiling (* d (num - error))) d)))

; We can install project-* into our dispatch table as below
; (put 'project integer project-integer)
; (put 'project real project-real)
; ....
(define (project num)
  (apply-generic 'project num))

; Now we can define the `drop` procedure as follows. We first project the given
; number and compare the projected type to original. If they're equal, we return
; the original argument as it is - no more dropping is possible. Otherwise, 
; we return (drop projected)
(define (drop num)
  (let ((t (type-tag num)))
    (let ((projected (project num)))
      (if (eq? t (type-tag projected))
        num
        (drop projected)))))

; In the numeric type tower hierarchy above, we managed to drop all the way down to
; the lowest level. In some hierarchies dropping may not be possible from an
; intermediate level - e.g., in some programs it makes sense to disallow 
; dropping reals to rationals (because it can't be done exactly in general case). 
; Drop procedure defined above will work as expected even in such scenarios - 
; it stops at that level from where further dropping is impossible.

; Ex 2.86
; We want to be able to handle complex numbers where magnitudes, angles, 
; real and imaginary parts are of any generic number type. We assume that
; complex number type is the supertype of all these argument types. Because in 
; mathematics, sine and cosine can operate on complex numbers. If x is a 
; complex number, sin(x) and cos(x) are also complex. Using Euler's formula
; e^(ix) = cos(x) + i * sin(x), we can compute the cos and sin of any complex
; number in general. 
;
; The trick is then to implement `sin` and `cos` for complex numbers, which can
; be done using 3 formulae.
; cos(a + ib) = cos(a) * cos(ib) - sin(a) * sin(ib)
; sin(a + ib) = cos((pi / 2 - a) + i * (-b))
; e^(ix) = cos(x) + i * sin(x)
; but e^(ix) = 1 + (ix) / 1! + (ix)^2 / 2! ...
; e^ix = (1 - x^2 / 2! + x^4 / 4! ...) + i (x / 1! - x^3 / 3! + ...)
; thus cos(x) = (1 - x^2 / 2! + x^4 / 4! ...)
; and sin(x) = (x / 1! - x^3 / 3! ...)
; thus cos(ix) = (1 - (ix)^2 / 2! ...)
; and a similar formula for sin(ix) 

; Once `sin` and `cos` have been implemented for complex numbers, we can
; install them in the dispatch table.
; (put 'sin '(real) sin)
; (put 'cos '(real) cos)
; (put 'sin '(complex) sin-complex)
; (put 'cos '(complex) cos-complex)

; And now `sin` and `cosine` implementations are installed, redefine sin and 
; cos using `apply-generic`
;
; When dealing with generic real and imaginary parts, it's not clear enough
; what will the analogues for magnitude and angle computations look like.
;
; Section 2.5.3 Symbolic Algebra
;
; We'll limit ourselves to polynomial arithmetic. In this discussion, by a 
; polynomial, we mean a syntactic expression with an indeterminate. Such an
; expression will have terms where each term is a coefficient multiplied by
; the power of the indeterminate. (e.g. 3 * x^4). We will define two operations
; on polynomials - addition and multiplication. Two polynomials must have the
; same indeterminate for us to be able to add or multiply them.

; Data objects are polys, term-lists and terms.
;
; poly object interface - `add-poly`, `mul-poly`, `variable`, `term-list` and
;                       - `make-poly`
; `add-poly` and `mul-poly` are also the polynomial implementtions for `add` 
; and `mul` generic operations.

; term-list object interface - `make-term-list`, `empty-term-list?`,
;                            - `first-term`, `rest-of-terms`, `add-terms`,
;                            - `mul-terms`

(define (install-polynomial-package)

  ; polynomial type tag.
  (define (tag data)
    (attach-tag 'polynomial data))

  ; Constructor and selectors.
  (define (make-poly variable term-list)
    (tag (cons variable term-list)))
  (define (variable poly)
    (car poly))
  (define (term-list poly)
    (cdr poly))

  ; `add` and `mul` implementations
  (define (add-poly poly1 poly2)
    (if (same-variable? (variable poly1) (variable poly2))
      (make-poly (variable poly1) 
                 (add-terms (term-list poly1) (term-list poly2)))
      (error "Polynomials must have the same variable - add-poly")))

  (define (mul-poly poly1 poly2)
    (if (same-variable? (variable poly1) (variable poly2))
      (make-poly (variable poly1)
                 (mul-terms (term-list poly1) (mul-terms poly2)))
      (error "Polynomials must have the same variable - mul-poly")))

  ; Register add-poly and mul-poly as `add` and `mul` implementations
  ; for polynomials respectively.
  (put 'add '(polynomial polynomial) add-poly)
  (put 'mul '(polynomial polynomial) mul-poly)

  ; Register constructors and selectors for polynomials.
  (put 'make-poly '(polynomial)
       (lambda (var terms) (make-poly var terms)))
  (put 'variable '(polynomial) variable)
  (put 'term-list '(polynomial) term-list)

  'done)

; term-list interface - `the-empty-termlist`, `is-empty-termlist?`,
;                     - `adjoin-term`, `add-terms`, `mul-terms`, `first-term`
;                     - `rest-terms`
; 
; We'll do both sparse-list and dense-list implementations for term-list
; objects.
;
(define (install-term-list-package)

  ; term-list type tag 
  (define (tag data)
    (cons 'term-list data))

  ; add-
  (define (add-terms terms1 terms2)
    (cond
      ((is-empty-termlist? terms1) terms2)
      ((is-empty-termlist? terms2) terms1)
      (else
        (let ((t1 (first-term terms1)) (t2 (first-term terms2))
              (r1 (rest-terms terms1)) (r2 (rest-terms terms2)))
          (cond
            ((> (order t1) (order t2)) (adjoin-term t1 (add-terms r1 terms2)))
            ((< (order t1) (order t2)) (adjoin-term t2 (add-terms terms1 r2)))
            (else
              (adjoin-term (make-term (order t1) (add (coeff t1) (coeff t2)))
                           (add-terms r1 r2))))))))

  'done)
