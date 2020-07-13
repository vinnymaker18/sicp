(load "utils.ss")

; Ex 1.3
(define (sum-squares-two-largest x y z)

  ; Smallest of the 3.
  (define smallest (min x y z))
  
  ; We take the sum of squares of all 3 nos and subtract the square of the
  ; smallest
  (- (+ (square x) (square y) (square z)) (square smallest)))

; Ex 1.4
; It computes a + b if b > 0 and a - b if b <= 0. In effect, it computes
; a + abs(b)

; Ex 1.5
; Applicative order - arguments are first evaluated.
; Normal order - Lazy evaluation
; In applicative order, x and y are both evaluated first - in doing so, the 
; evaluator goes into an infinite recursion because of `p`. 
; In normal order, only x is evaluated (inside procedure) and because it's
; zero, second parameter is not evaluated and thus program finishes. 

; Ex 1.6
; Because `new-if` is a procedure and applicative order is the default, 
; both then-clause and else-clause are first evaluated. Desired behavior is 
; that only one of them is evaluated depending upon whether the predicate 
; is true or false.
;
; `sqrt-iter` recursively calls itself ad infinitum and program never finishes.

; A fixed point of a function f is a value x s.t f(x) = x. The following
; iterative technique can sometime converge to a fixed point of the function.
(define (fixed-point f initial-guess)

  (define (fixed-point-iter guess)
    (let ((next-guess (/ (+ guess (f guess)) 2.0)))
        (if (< (abs (- next-guess guess)) 0.00001)
            guess
            (fixed-point-iter next-guess))))

  (fixed-point-iter initial-guess))

; Ex 1.35, fixed point of f(x) = 1 + 1 / x is phi aka the golden ratio.
(print (fixed-point (lambda (x) (+ 1.0 (/ 1.0 x))) 1.0))

; Ex 1.36, x^x = 1000 => x * ln(x) = ln(1000) => x = ln(1000) / ln(x)
; Thus x is the fixed-point of f(x) = ln(1000) / ln(x)
; Here, we can't start with 1.0 as our initial guess as log(1) = 0.
; We're also not printing the successive values of guesses.
(print (fixed-point (lambda (x) (/ (log 1000) (log x))) 5.0))

; Newton's method for finding zeros of a function.
; To find x s.t. g(x) = 0, it's sometimes easier to find fixed point of the 
; function f(x) = x - g(x) / g'(x), where g' denotes the derivative of g.
; x = f(x) => x = x - g(x) / g'(x) => g(x) = 0

; Let's define derivative function of a given function f.
(define (derivative f)
  (let ((delta 0.00001))
    (lambda (x)
      (/ (- (f (+ x delta)) (f x)) delta))))

; Newton-transform computes x - g(x) / g'(x) for a given function g.
(define (newtons-transform g)
    (let ((deriv-g (derivative g)))
        (lambda (x) (- x (/ (g x) (deriv-g x))))))

; Newton's method for finding a zero of g is simply finding fixed point for 
; (newtons-transform g).
(define (newton-method g)
  (fixed-point (newtons-transform g) 1.0))

; Ex 1.37, implement the continued fraction function, approximated upto k-terms, 
; given procedures that provide i-th numerator and denominator.
;
; k -> no. of terms to use in approximating the continued fraction. 
; n-th -> Generator function for numerator term.
; d-th -> Generator function for denominator term.
; This is an iterative process, works backwards from kth-term down to 1st term.
(define (cont-frac k n-th d-th)
  (define (iter i cur)
    (if (= i 0)
      cur
      (let ((n (n-th i)) (d (d-th i)))
        (iter (- i 1) (/ n (+ d cur))))))
  
  (iter k 0.0))

; As seen below, a 20-term continued fraction with all numerators and 
; denominators as 1 is accurate enough approximation of 1/phi (upto 4 decimal
; digits)
(print (cont-frac 20 (lambda (i) 1.0) (lambda (i) 1.0)))

; Ex 1.38, a 100-term approximation of e - 2.
(let ((n-th (lambda (i) 1.0))
      (d-th (lambda (i) 
              (if (= 2 (remainder i 3)) 
                (+ 2.0 (/ (* 2 (- i 2)) 3.0))
                1.0))))
  (print (cont-frac 100 n-th d-th)))

; Ex 1.39, Lambert's approximation of tan(x), x is in radians.
; (-x) * tanx = (x^2 / (1 - (x^2 / (3 - (x^2 / (5  - ...))))))
; RHS is cont-frac with n-th i = x^2 and d-th i = 2 * i - 1
; 
; Doesn't work for x = 0.
(define (tan-cf k x)
  (/ (cont-frac k (lambda (i) (- (* x x))) (lambda (i) (- (* 2 i) 1))) (- x)))

; Displays approximately 1.0
(print (tan-cf 100 (atan 1.0)))

; Displays approximately 2.0
(print (tan-cf 100 (atan 2.0)))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

; Square root and cube root functions.
(define (sqrt-1 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (* y y)))) 1.0))

; Ex 1.40, use newton's method to find zeros of the cubic equation
; x^3 + a * x^2 + b * x + c = 0
(define (cubic a b c)
  (lambda (x) (+ (* x x x)  (* a x x) (* b x) c)))

(print (newton-method (cubic 1 -3 2)))

; Ex 1.41, doubler(f) (x)  = f(f(x))
; (((double (double double)) inc) 5) returns 21
(define (doubler f)
  (lambda (x) (f (f x))))

; Prints 21
(print (((doubler (doubler doubler)) (lambda (x) (+ x 1))) 5))

; Ex 1.42, f o g (x) = f (g (x) )
(define (my-compose f g)
  (lambda (x) (f (g x))))

; should print (6 + 1)^2 = 49
(print ((my-compose (lambda (x) (* x x)) (lambda (x) (+ x 1))) 6))

; Ex 1.43, f^n (x) = f(f(...(x)....) i.e., f applied n times to x.
(define (repeated n f)
  (lambda (x)
    (if (= n 0)
      x
      ((my-compose f (repeated (- n 1) f)) x))))

; Should print (5^2)^2 = 625
(print ((repeated 2 (lambda (x) (* x x))) 5))

; Ex 1.44, smoothed-f
(define (smoothed f)
  (lambda (x)
    (let ((x1 (- x 0.00001)) (x2 x) (x3 (+ x 0.00001)))
      (/ (+ (f x1) (f x2) (f x3)) 3.0) )))

; n-fold-smoothed
(define (n-fold-smoothed n f)
  (repeated n (smoothed f)))

; Ex 1.45, finding n-th roots of a positive number using repeated average damping
(define (power a n)
  (if (= n 0)
    1.0
    (* a (power a (- n 1)))))

; Average-damping can be repeatedly applied so that n-th root finding 
; converges. Here, we apply it 3 times.
(define (n-th-root n)
  (lambda (x)
    (fixed-point (repeated 3 (average-damp (lambda (t) (/ x (power t (- n 1)))))) 1.0)))

; Ex 1.46 Iterated improvement.
(define (iterative-improve good-enough? improve-guess)
  (define (f guess)
    (if (good-enough? guess)
      guess
      (f (improve-guess guess))))
  f)

(define (sqrt-2 x)
    (let ((good? (lambda (g) (< (abs (- x (* g g))) 0.00001)))
          (improve (lambda (g) (/ (+ g (/ x g)) 2.0))))
      ((iterative-improve good? improve) x)))

(define (fixed-point-2 f first-guess)
  (let ((good? (lambda (g) (< (abs (- g (f g))) 0.00001))))
    ((iterative-improve good? f) first-guess)))
