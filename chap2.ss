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
