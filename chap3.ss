(load "utils.ss")

; Chapter 3

; Ex 3.1
(define (make-accumulator initial-sum)
  (lambda (change)
    (begin
      (set! initial-sum (+ change initial-sum))
      initial-sum)))

; Ex 3.2
(define (make-monitored func)
  (let ((num-calls 0))
    (define (mf arg)
      (cond
        ((equal? arg 'how-many-calls?) num-calls)
        ((equal? arg 'reset-count) (set! num-calls 0))
        (else
          (begin
            (set! num-calls (+ 1 num-calls))
            (func arg)))))
    mf))

; Ex 3.3, 3.4 and 3.7
(define (call-the-cops) (print "Calling the cops"))

(define (make-account balance password)
  ; We create a new environment with the local variable `incorrect-attempts` 
  ; Whenever an incorrect password is supplied, we increment it and once it
  ; reaches 7, we call the procedure `call-the-cops`. Otherwise, we reset it
  ; to 0.
  (let ((incorrect-attempts 0) (password-list (list password)))
    (define (account-access access-password action)
      (lambda (value)
      (if (memq access-password password-list)
        (begin
          (set! incorrect-attempts 0)
          (cond
            ((equal? action 'deposit)
             (begin
               (set! balance (+ balance value))
               balance))
            ((equal? action 'withdraw)
             (if (>= balance value)
               (begin
                 (set! balance (- balance value))
                 balance)
               "Insufficient funds"))
            ((equal? action 'new-password)
             (begin 
               (set! password-list (cons value password-list))
               account-access))
            (else 
              "Invalid account action")))
        (begin
          (set! incorrect-attempts (+ 1 incorrect-attempts))
          (if (>= incorrect-attempts 7)
            (begin
              (call-the-cops)))
          "Incorrect password"))))
    account-access))

; Monte carlo experiment returns the fraction of conducted trials that resulted
; in success.
(define (monte-carlo trials experiment)
  (define (iter pass remaining)
    (if (zero? remaining)
      (/ pass trials)
      (let ((res (experiment)))
        (iter (+ pass (if res 1 0)) (- remaining 1)))))
    
    (iter 0 trials))

; Now, the mathematical constant pi can be estimated using the approximate
; equality 6/pi^2 =~ (monte-carlo 10000 relatively-prime-random-pair)

; For chicken scheme, random generators are in the chicken random module.
(import (chicken random))

; For our purposes we only consider random integers in the range 1 and 2^32.
(define (random)
  (+ 1 (pseudo-random-integer (expt 2 32))))

; Experiment used in estimating pi - check if two random integers are relatively 
; prime. 
;
; (pseudo-random-integer n) returns a random integer b/w 0 and n - 1
(define (relatively-prime-random-pair)
  (let ((a (random))
        (b (random)))
    (= 1 (gcd a b))))

; Running the experiment 10000 times estimated the value of pi at
; 3.12195270527231 which is a pretty good estimate.
(let ((result (monte-carlo 10000 relatively-prime-random-pair)))
  (print (sqrt (/ 6.0 result))))

; Ex 3.5 
; Estimates a definite integral using monte-carlo simulation. 
; P - predicate function used to test whether a point is inside the region.
; x1, y1 - bottom left
; x2, y2 - top right
(define (estimate-integral P x1 y1 x2 y2)

  (define (experiment)
    (let ((x (+ x1 (* (- x2 x1) (pseudo-random-real))))
          (y (+ y1 (* (- y2 y1) (pseudo-random-real)))))
      (P x y)))

  (let ((width (- x2 x1)) (height (- y2 y1)))
    (* width height (monte-carlo 10000 experiment))))

; Pi can now be estimated by considering the unit circle centered at origin 
; and its bounding square.
; Running this got me an estimate of 3.1464
(print
  (estimate-integral
    (lambda (x y) (<= (+ (* x x) (* y y)) 1.0))
    -1 -1 1 1))

; Ex 3.6
; This and the previous exercises assumed the existence of a procedure called
; `rand-init` upon which the procedure `random` was based on. In chicken
; scheme, to achieve a similar effect of reset, we must use the procedure
; `set-pseudo-random-seed!`
;
; (rand 'generate) -> returns a random number
; (rand 'reset) -> resets the rng state, so the pattern of resetting and 
; generating random numbers always results in the same sequence.
(define (rand action)
  (cond
    ((equal? action 'generate) (random))
    ((equal? action 'reset) (set-pseudo-random-seed! "iuewkfhjkfhgerjh"))
    (else
      (error "Invalid action - rand"))))

; Ex 3.7 
; Note, once an account is converted to a joint account using this procedure,
; either the old or new password can be used to access it. Even more, we can
; call make-joint any no. of times and that many new passwords will be added
; to the 'same' account.
(define (make-joint protected-acc old-password new-password)
  ((protected-acc old-password 'new-password) new-password))

; Ex 3.8
(let ((state 0))
  (define (f-internal x)
    (begin
      (define oldstate state)
      (set! state (+ state x))
      oldstate))
  
  (set! f f-internal))

; Section 3.2
; Environment model of evaluation

; Ex 3.9 
; Recursive version - The symbol `factorial` is bound to a function value
; in the global environment. When (factorial x) is called, a new frame is
; created with a binding for `n` to x and a pointer to global environment. In
; evaluating this, a call to (factorial (x - 1)) will be made, in which another 
; frame is created with binding for `n` to (x - 1) and it's pointer is also to 
; the global environment. In all, there will be 6 calls to factorial function,
; each having their own frames with bindings for `n`. Each of these frames
; point to the global environment.
;
; Iterative version - The symbols `factorial` and `fact-iter` are bound to two
; function values in the global environment. In all the `fact-iter` function 
; call frames, `max-count` symbol is bound to the same value.

; Ex 3.10
; Essentially, a new extra frame is created in calls to modified version of 
; `make-withdraw`.

; Ex 3.12
; (b), (b, c, d)

; Ex 3.13
; Because tail recursive calls are converted to loops, `last-pair` on z will go 
; into an infinite loop, never terminating. Some lisps don't have tail recursion, 
; in which case, it will eventually cause a stack limit exceeded error.

; Ex 3.14
; A pair is a box holding 2 pointers - one points to its car, other to its cdr.
; A list is a chain of pairs, with cdr of each pair pointing to the next pair
; in the chain.
;
; `mystery` reverses the list structure by modifying the cdr pointers of pairs
; in the list. cdr of each pair(except the first pair) in the list now points 
; to the pair the came before it in the original list and the first pair points 
; to nil. Finally, `mystery` returns the last pair of original list which
; is the beginning of the reversed list.

; v is originally '(a b c d)
; w is '(d c b a)
; v still points the pair with 'a in it, whose cdr is now nil.
; v is thus now '(a)

; Ex 3.15
; x is the list '(a b)
; z1 has both car and cdr pointing the same object - which is x.
; z2 has car and cdr pointing to two different objects, both different from x.
; content wise, z1 and z2 are equal.
;
; set-to-wow! takes a pair p whose car must also be a pair and sets the car of
; (car p) to 'wow.
; when set-to-wow! is called on z1 - the car of z1's car is changed to 'wow and
; because z1's car and cdr are same, z1's cdr's car is also changed to 'wow,
; so now z1 becomes '((wow b) wow b)
; because z2 has different car and cdr, modifying car doesn't affect its cdr
; set-to-wow! on z2 modifies it to '((wow b) a b)

; Ex 3.16
; I think the question is asking for us to make up list structures out of 3
; physical pair objects in memory s.t count-pairs on those structures will 
; return different values.
;
; a) The list (1 2 3) has 3 pairs in it, count-pairs returns 3.
; b) If y is list (2) then count-pairs returns 4 for (cons 1 (cons y y)), even
;    though there are only 3 pairs in memory.
; c) If x is the list (1) and y is (cons x x), then y has 2 pair objects in it.
;    And if z is (cons x x), then z has 3 different pair objects in it. Calling
;    count-pairs on z returns 7.
; d) We take a list s = (1 2 3) and set-cdr! on its last pair to itself
;    like (set-cdr! (cddr s) s). Then s will still have only 3 physical pair
;    objects in it and yet (count-pairs s) never terminates.
;
; The issue arises here because while we're only interested in counting the 
; no. of pairs actually present in memory, traversing the list structure 
; may encounter some of these pairs more than once and hence they are counted
; more than once. And in examples like d, where there are cycles in the
; structure, count-pairs never terminates.

; Checks whether the object y is present in the list
(define (present? seen-list y)
  (if (null? seen-list)
    #f
    (or
      (eq? y (car seen-list))
      (present? (cdr seen-list) y))))

; Ex 3.17
; To avoid overcounting the same pair, we can use the fact that eq? returns 
; true only when its two arguments are the same in memory. We can maintain a 
; list of seen pairs and discard the current pair if it's already seen.
; After the modification, count-pairs now returns 3 for all the 4 cases above.
(define (count-pairs x)

  (let ([seen '()])
    (define (count x)
      (if (or (not (pair? x)) (present? seen x))
        0
        (begin
          (set! seen (cons x seen))
          (count (car x))
          (count (cdr x)))))

    (count x)
    (length seen)))

; Ex 3.18
; The classic cycle finding problem in list structures. 
(define (cycle-exists? x)
  (define (visit x seen)
    (cond
      ((null? x) #f)
      ((present? seen x) #t)
      (else (visit (cdr x) (cons x seen)))))

  (visit x '()))

; Ex 3.19
; The implementation in 3.18 requires O(n) space, where n is the length of x.
; This is so because we kept consing all the seen pointers in a list. To solve
; this problem in O(1) space, we can use the classic tortoise-hare or Brent's 
; cycle finding algorithms. Alternatively, instead of a list of seen pointers,
; we maintain the current index i and modify present? to iterate through 
; first i pointers in the list. This makes the algorithm slower to O(n^2) 
; however.
(define (cycle-exists2? x)
  (define (present? x e i n)
    (cond
      ((>= i n) #f)
      ((eq? e x) #t)
      (else
        (present? (cdr x) e (+ i n) n))))

  (define (visit x i)
    (cond
      ((null? x) #f)
      (else
        (or 
          (present? x x 0 i)
          (visit (cdr x) (+ i 1))))))
  
  (visit x 0))

; Ex 3.20
; z has both car and cdr pointing to the same object which is x = (1 2)
; This means a set-car! on (cdr z) is equivalent to set-car! on x which is
; equivalent to set-car! on (car z), because x, (cdr z), (car z) are all
; same. Thus (car x) gives the result 17.

; Section 3.3.2
; Queue data structure.

; Queue interface
; `make-queue` - Consructs and returns an empty queue.
; `empty-queue?` - Tests if the queue is empty.
; `front-queue` - Returns the element at the front end of the queue. Raises an
;                 error if the queue is empty.
; `insert-queue!` - Inserts an element at the back end of the queue.
; `delete-queue!` - Deletes and returns the element at the front end. Raises an
;                   error if the queue is empty.

(define (make-queue)
  (cons '() '()))

(define (empty-queue? queue)
  (null? (car queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "Queue is empty - front-queue")
    (car (car queue))))

(define (insert-queue! queue element)
  (let ([e (list element)]
        [first (car queue)]
        [last (cdr queue)])


    (if (null? first)
      (begin
        (set-car! queue e)
        (set-cdr! queue e))
      
      (begin
        (set-cdr! queue e)
        (set-cdr! last e)))

    queue))

; Note, delete-queue! here returns the just deleted value.
(define (delete-queue! queue)
  (if (empty-queue? queue)
    (error "Queue is empty - delete-queue!")
    (let ([first (car queue)])
      (set-car! queue (cdr first))
      (if (null? (cdr first))
        (set-cdr! queue '()))
      (car first))))

; Ex 3.21
; In the book implementation, `delete-queue!` doesn't change cdr pointer of 
; the queue when there's only one element left - this gives the impression
; to Ben that the element 'b is still there even after 2 delete operations.
; In my implementation this case is considered and thus the 'b doesn't show
; after 2 deletions.

; To print a queue, we simply print all its elements one by one.
(define (print-queue queue)
  (define (print-inner x)
    (if (null? x)
      #f
      (begin
        (print (car x))
        (print-inner (cdr x)))))

  (print (car queue)))

; Ex 3.22
; Shouldn't be hard implementing queue in message passing style.

; Ex 3.23
; deque data structure - make-deque, empty-deque?, insert-front!, insert-back!,
;                      - delete-front!, delete-back!, front-deque, back-deque
;
; One approach is to use a doubly linked list of deque entries and maintain 
; 2 front and rear pointers to the list.

; deque-entry object
(define (make-deque-entry x)
  (list x '() '()))

(define (get-datum deque-entry)
  (car deque-entry))

(define (get-prev-ptr deque-entry)
  (cadr deque-entry))

(define (get-next-ptr deque-entry)
  (caddr deque-entry))

(define (set-prev-ptr! deque-entry prev)
  (set-car! (cdr deque-entry) prev))

(define (set-next-ptr! deque-entry next)
  (set-car! (cddr deque-entry) next))

; deque object
(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque)
  (null? (car deque)))

; Returns the entry at the front of the deque
(define (front-deque-entry deque)
  (if (empty-deque? deque)
    (error "Deque is empty - front-deque-entry")
    (car deque)))

; Returns the entry at the back of the deque
(define (back-deque-entry deque)
  (if (empty-deque? deque)
    (error "Deque is empty - back-deque-entry")
    (cdr deque)))

(define (back-deque deque)
  (get-datum (back-deque-entry deque)))
(define (front-deque deque)
  (get-datum (front-deque-entry deque)))

(define (insert-front! deque x)
  (let ([new-entry (make-deque-entry x)])
    (if (empty-deque? deque)
      (begin
        (set-car! deque new-entry)
        (set-cdr! deque new-entry))
      (let ([front (front-deque-entry deque)])
          (set-prev-ptr! front new-entry)
          (set-next-ptr! new-entry front)
          (set-car! deque new-entry)))))

(define (insert-back! deque x)
  (let ([new-entry (make-deque-entry x)])
    (if (empty-deque? deque)
      (begin
        (set-car! deque new-entry)
        (set-cdr! deque new-entry))
      (let ([back (back-deque-entry deque)])
          (set-prev-ptr! new-entry back)
          (set-next-ptr! back new-entry)
          (set-cdr! deque new-entry)))))

(define (delete-front! deque)
  (if (empty-deque? deque)
    (error "Empty deque - delete-front!")
    (let ([front (front-deque-entry deque)]
          [next (get-next-ptr (front-deque-entry deque))])
      (if (null? next)
        (begin
          ; Only one element in deque.
          (set-car! deque '())
          (set-cdr! deque '())
          (get-datum front))
        (begin
          (set-car! deque next)
          (set-prev-ptr! next '())
          (get-datum front))))))

(define (delete-back! deque)
  (if (empty-deque? deque)
    (error "Empty deque - delete-back!")
    (let ([back (back-deque-entry deque)]
          [prev (get-prev-ptr (back-deque-entry deque))])
      (if (null? prev)
        (begin
          (set-car! deque '())
          (set-car! deque '())
          (get-datum back))
        (begin
          (set-next-ptr! prev '())
          (set-cdr! deque prev)
          (get-datum back))))))

; We also need a print-deque procedure for printing deques.
(define (print-deque deque)
  (define (print-inner deque-entry)
    (if (not (null? deque-entry))
      (begin
        (print (get-datum deque-entry))
        (print-inner (get-next-ptr deque-entry)))))

  (print-inner (front-deque-entry deque)))

; Section 3.3.3
; Hash tables

; We'll implement an unordered list based hash-table that accepts a custom
; key equality checker in message passing style. I don't quite understand the
; need for a dummy header record as used in the book. Our hash table doesn't
; support a delete-key operation, neither does the book implementation.
(define (make-table same-key?)
  ; Our hash table is simply an unordered list of records, where each record
  ; is a (key, value) pair.
  (define records '())

  ; Returns the matching record (the key, value pair)
  (define (get key records)
    (cond
      ((null? records) #f)
      ((same-key? key (caar records)) (car records))
      (else (get key (cdr records)))))

  (define (set key value)
    (let ([match (get key records)])
      (if match
        (set-cdr! match value)
        (set! records (cons (cons key value) records)))))
  
  (define (dispatch message)
    (cond
      ((eq? message 'get) 
        (lambda (key)
          (let ([match (get key records)])
            (if match
              (cdr match)
              #f))))
      ((eq? message 'set)
        (lambda (key value)
          (set key value)))
      (else (error "Invalid operation for hash-table"))))

  dispatch)

; Helper routines to get/set on hash table.
(define (lookup-table table key)
  ((table 'get) key))

(define (insert-table table key value)
  ((table 'set) key value))

; Ex 3.24 - Included in the above implementation.

; Ex 3.25 - The above implementation works, our keys will be lists and
; same-key? is equal?

; Ex 3.26 - A BST based hash table. Implementation is pretty much the same as 
; a BST based set from chapter 2.

; Ex 3.27 - (memoize fib) doesn't quite work cause fib internally calls itself 
; rather than calling the memoized version and so performance doesn't improve.
; The mem-fibo version in the book works because it calls itself, so the 
; intermediate results are also cached and reused. 

; Section 3.3.4
; Digital circuit simulator as an example of simulating real world event driven
; systems computationally using mutable data.

; Let's first implement `logical-not`, `logical-and` and `logical-or` primitives.

; A helper routine to denote an erroneous input.
(define (check-input input)
  (if (not (or (= 0 input) (= 1 input)))
    (error "Invalid input signal")))

(define (logical-not input)
  (check-input input)
  (- 1 input))

(define (logical-and input1 input2)
  (check-input input1)
  (check-input input2)
  (cond
    ((and (= 1 input1) (= 1 input2)) 1)
    (else 0)))

(define (logical-or input1 input2)
  (check-input input1)
  (check-input input2)
  (cond
    ((or (= 1 input1) (= 1 input2)) 1)
    (else 0)))

; Ex 3.28 - or gate function box.
; We assume the existence of the procedures `get-signal`, `set-signal!`,
; `add-action!` and `after-delay` for now. Likewise, `or-gate-delay`, 
; `inverter-delay` and `and-gate-delay` are assumed to exist.

(define (inverter in out)

  (define (inverter-action)
    (let ([input (get-signal in)])
      (after-delay 
        inverter-delay
        (lambda () (set-signal! out (logical-not input))))))

  (add-action! in inverter-action)
  'done)

(define (and-gate a1 a2 out)

  (define (and-action)
    (let ([i1 (get-signal a1)]
          [i2 (get-signal a2)])
      (after-delay
        and-gate-delay 
        (lambda () (set-signal! out (logical-and i1 i2))))))
  
  (add-action! a1 and-action)
  (add-action! a2 and-action)
  'done)

(define (or-gate a1 a2 out)

  (define (or-action)
    (let ([i1 (get-signal a1)]
          [i2 (get-signal a2)])
      (after-delay
        or-gate-delay
        (lambda () (set-signal! out (logical-or i1 i2))))))
  
  (add-action! a1 or-action)
  (add-action! a2 or-action)
  'done)

; Ex 3.29 - or gate can be defined in terms of inverters and and gates.
; x|y = ~((~x) & (~y))
; We assume the existence of `make-wire`
(define (or-gate2 a1 a2 out)

  ; i1 and i2 are for inverted signals of a1 and a2.
  (define i1 (make-wire))
  (define i2 (make-wire))
  (inverter a1 i1)
  (inverter a2 i2)

  ; i3 = i1 & i2
  (define i3 (make-wire))
  (and-gate i1 i2 i3)

  ; out = ~i3
  (inverter i3 out)

  'done)

; Ex 3.30
; Ripple-carry adder

; Let's first define half-adder and full-adder.
(define (half-adder a b s c)
  (and-gate a b c)

  (define d (make-wire))
  (or-gate a b d)

  (define e (make-wire))
  (inverter c e)

  (and-gate d e s)
  'done)

(define (full-adder a b c-in sum c-out)
  (define c1 (make-wire))

  (define s2 (make-wire))
  (define c2 (make-wire))

  (half-adder b c-in s2 c2)
  (half-adder a s2 sum c1)
  (or-gate c1 c2 c-out)
  'done)

; half-adder has a delay of max(or-gate-delay, and-gate-delay + inverter-delay) 
; + and-gate-delay. Let's simplify it to or-gate-delay + and-gate-delay +
; inverter-delay

; full-adder has a delay of 2 * half-adder-delay + or-gate-delay. 
; An n-bit ripple-carry-adder has n times the full-adder delay.
; ripple-carry-delay = N * (2 * (OR + AND + INV) + OR)

; A1, A2 ... An are the n bits of an integer A, with A1 being the most significant.
; B1, B2 ... Bn are likewise the n bits of an integer B.
; ripple-carry-adder accepts two in parameters - 2 lists A, B. Both having n
; wires each.
; It also accepts 2 out parameters - A list of wires S and a wire C.
; Within the 3 lists, wires are ordered from least to most significant.
(define (ripple-carry-adder A B S C)
  ; C-in is always zero.
  (define C-in (make-wire))

  (define (iter a b c s)
    (if (= 1 (length a))
      (full-adder (car a) (car b) c (car s) C)
      (let ([c2 (make-wire)])
        (full-adder (car a) (car b) c (car s) c2)
        (iter (cdr a) (cdr b) c2 (cdr s)))))

  ; We assume ripply-carry-adder is always called with n >= 1.
  (iter A B C-in S)
  'done)

; Wire implementation in message passing style. 
(define (make-wire)
  (let ([signal-value 0] [actions '()])

    ; Returns the current signal value
    (define (get) signal-value)

    ; Calls the actions one by one.
    (define (call-each actions)
      (if (not (null? actions))
        (begin
          ((car actions))
          (call-each (cdr actions)))))

    ; When the signal value changes, all the actions procedures are called.
    (define (set new-value)
      (if (not (= new-value signal-value))
        (begin
          (set! signal-value new-value)
          (call-each actions)
          'done)))

    ; Adds a new action procedure and calls it once.
    (define (add-action action)
      (set! actions (cons action actions))
      (action))

    (define (dispatch message)
      (cond
        ((eq? message 'get-signal) signal-value)
        ((eq? message 'set-signal!) set)
        ((eq? message 'add-action!) add-action)
        (else (error "Invalid operation - make-wire dispatch"))))

    dispatch))

; Helper routines for wire objects.
(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action)
  ((wire 'add-action!) action))

; Ex 3.31
; We can classify the states of a circuit into two kinds - rest states and
; transition states. At rest, values of signals will remain the same until
; there is a change detected in one or more wires (this change may be forced 
; by an external agent) - at which point the circuit is considered to be in
; transition from one rest state to another. Because of gate delays, these
; transitions may take a while to finish. During a transition, wires in a
; circuit may be inconsistent w.r.t each other - but because transitions are
; only temporary and the circuit will eventually move to a rest state, inconsi-
; stencies can be allowed during transitions.
;
; A good circuit will always have its wire signals consistent w.r.t each other at 
; rest e.g., if there is an inverter b/w wires A & B, A is always equal to 
; not B at rest. In our simulation, initially the circuit is at rest. If we 
; change the signal in a wire, that triggers a series of events to be processed
; by our simulator. Once all these events are processed and values of wires are
; set, our circuit reaches rest state again. Because initially we're at rest,
; we should have all the wire values consistent w.r.t each other and thus we 
; need to run the action once initially whenever two wires are first connected.

; Simulation using the agenda data structure.
(define (make-agenda)

  ; Current time in the simulation.
  (define curtime 0)

  ; A time-sorted list of events to process.
  (define agenda-entries '())

  (define (empty?)
    (null? agenda-entries))

  (define (current-time)
    curtime)

  ; Removes the next action from the agenda and updates the current time.
  (define (remove-first-agenda-item!)
    (if (empty?)
      (error "No actions left in the agenda - remove-first-agenda-item!")
      (let ([first (car agenda-entries)] [rest (cdr agenda-entries)])
        (set! agenda-entries rest)
        (set! curtime (car first))
        (cdr first))))

  ; Inserts a new action item into the agenda at the right position within
  ; the entries list.
  (define (add-to-agenda! time action)

    (define (insert time action entries prev)
      (cond
        ((or (null? entries) (< time (caar entries)))
         (if (null? prev)
           (set! agenda-entries (cons (cons time action) agenda-entries))
           (set-cdr! prev (cons (cons time action) entries))))
        (else (insert time action (cdr entries) entries))))

    (insert time action agenda-entries '()))

  (define (dispatch message)
    (cond
      ((equal? message 'empty-agenda?) (empty?))
      ((equal? message 'current-time) curtime)
      ((equal? message 'remove-first-agenda-item!) remove-first-agenda-item!)
      ((equal? message 'add-to-agenda!) add-to-agenda!)
      (else (error "Invalid operation for agenda object - dispatch"))))

  dispatch)

(define (empty-agenda? agenda)
  (agenda 'empty-agenda?))

(define (current-time agenda)
  (agenda 'current-time))

(define (remove-first-agenda-item! agenda)
  ((agenda 'remove-first-agenda-item!)))

(define (add-to-agenda! agenda time action)
  ((agenda 'add-to-agenda!) time action))

; Initialize the agenda.
(define the-agenda (make-agenda))

; Define a probe action on wire
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " new value = ")
                 (display (get-signal wire))
                 (newline))))

; Define sample delays
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; Define after-delay
(define (after-delay time action)
  (add-to-agenda! the-agenda (+ time (current-time the-agenda)) action))

; The propagate procedure.
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ([first (remove-first-agenda-item! the-agenda)])
      (first)
      (propagate))))

; Ex 3.32
; Events must be simulated in the order they occur in the real world. If we
; only used a simple list as a stack to maintain the agenda entries, events 
; can be processed out of order.

; Section 3.3.5
; Constraint propagation system
;
; A constraint propagation system has two kinds of objects - connectors and
; constraints. A constraint specifies a mathematical relation b/w one or more 
; connectors. More generally, a constraint is some object interested in learning
; about changes in values of one or more connectors.
;
; A connector holds a numeric value in accordance with the set of constraints
; it participates in.

; Connector object interface - `make-connector`, `has-value?`, 'get-value',
;                            - `set-value!`, `forget-value!`, `connect`

; Connector implementation
(define (make-connector)

  ; The value this connector holds.
  (define value '())

  ; The set of constraints this connector participates in.
  (define constraints '())

  ; The informant(actor) that set a value for this connector. This is either
  ; an external agent or an attached constraint.
  (define informant '())

  ; Helper routine to inform connected constraints about change in this
  ; connectors state.
  (define (for-each-except actor proc constraints)
    (if (null? constraints)
      'done
      (begin
        (if (not (eq? (car constraints) actor))
          (proc (car constraints)))
        (for-each-except actor proc (cdr constraints)))))

  ; Check if this connector holds a value. We assume '() stands for no value.
  (define (has-value?)
    (not (null? value)))

  ; Return the value stored. 
  (define (get-value)
    value)

  ; Sets a new value and informs the connected constraints about it.
  (define (set-value! new-value setter)
    (cond
      ((and (has-value?) (not (= new-value value)))
           (error "Contradicts existing value"))
      ((has-value?) 'ignored)
      (else
        (begin
          (set! value new-value)
          (set! informant setter)
          (for-each-except setter inform-about-value constraints)))))

  ; Forgets the current value and informs the connected constraints.
  (define (forget-value retractor)
    (if (eq? retractor informant)
      (begin
        (set! value '())
        (set! informant '())
        (for-each-except retractor inform-about-no-value constraints))
      'ignored))

  ; Connect to a new constraint.
  (define (connect constraint)
    (if (not (memq constraint constraints))
          (begin
            (set! constraints (cons constraint constraints))
            (if (has-value?)
              (inform-about-value constraint)))
          'ignored))

  ; Message dispatcher for this object.
  (define (me message)
    (cond
      ((eq? message 'has-value?) has-value?)
      ((eq? message 'get-value) get-value)
      ((eq? message 'set-value!) set-value!)
      ((eq? message 'forget-value) forget-value)
      ((eq? message 'connect) connect)
      (else (error "Invalid message for connector objects"))))

  me)

; Helper routines for connector objects.
(define (has-value? connector)
  ((connector 'has-value?)))

(define (get-value connector)
  ((connector 'get-value)))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value connector retractor)
  ((connector 'forget-value) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

; Constraint object interface - constructors, `inform-about-value` and
;                             - `inform-about-no-value`
;
; Now we define three primitive constraints - adder, multiplier and constant.

; Constant constraint says that the connector has constant value at all time.
(define (constant value connector)

  ; Any message to constant constraints will simply be ignored.
  (define (me message)
    (lambda () 'ignored))

  (connect connector me)

  (set-value! connector value me)

  me)

; Adder constraint says that connector `sum` has the value equal to the
; addition of connectors `a1` and `a2`
(define (adder a1 a2 sum)
  
  (define (inform-about-value)
    (cond
      ((and (has-value? a1) (has-value? a2))
          (set-value! sum (+ (get-value a1) (get-value a2)) me))
      ((and (has-value? sum) (has-value? a1))
          (set-value! a2 (- (get-value sum) (get-value a1)) me))
      ((and (has-value? sum) (has-value? a2))
          (set-value! a1 (- (get-value sum) (get-value a2)) me))
      (else 'ignored)))

  (define (inform-about-no-value)
    (begin
      (forget-value a1 me)
      (forget-value a2 me)
      (forget-value sum me)
      (inform-about-value)))


  (define (me message)
    (cond
      ((eq? message 'I-have-value) inform-about-value)
      ((eq? message 'I-have-no-value) inform-about-no-value)
      (else (error "Invalid message for adder constraint"))))

  ; Connect this constraint to the connectors a1, a2 and sum.
  (connect a1 me)
  (connect a2 me)
  (connect sum me)

  me)

; Multiplier constraint says that connector `product` has the value equal to
; product of connectors `a1` and `a2`
(define (multiplier a1 a2 product)

  (define (inform-about-value)
    (cond
      ((and (has-value? a1) (= 0 (get-value a1)))
        (set-value! product 0 me))
      ((and (has-value? a2) (= 0 (get-value a2)))
        (set-value! product 0 me))
      ((and (has-value? a1) (has-value? a2))
        (set-value! product (* (get-value a1) (get-value a2)) me))
      ((and (has-value? a1) (has-value? product))
        (set-value! a2 (/ (get-value product) (get-value a1)) me))
      ((and (has-value? a2) (has-value? product))
        (set-value! a1 (/ (get-value product) (get-value a2)) me))))

  (define (inform-about-no-value)
    (forget-value a1 me)
    (forget-value a2 me)
    (forget-value product me)
    (inform-about-value))

  (define (me message)
    (cond
      ((eq? message 'I-have-value) inform-about-value)
      ((eq? message 'I-have-no-value) inform-about-no-value)
      (else (error "Invalid message for multiplier constraint"))))

  (connect a1 me)
  (connect a2 me)
  (connect product me)
  me)

; A probe object displays the value held by a connector. A probe can also be
; considered a special type of constraint.
(define (probe name connector)

  (define (print-probe value)
    (display name)
    (display " ")
    (display value)
    (newline))
  
  (define (me message)
    (cond
      ((eq? message 'I-have-value) 
       (lambda () (print-probe (get-value connector))))
      ((eq? message 'I-have-no-value) 
       (lambda () (print-probe "?")))
      (else (error "Invalid message for probe objects"))))

  ; Connect this probe to the connector so that value changes will be recorded.
  (connect connector me)

  me)

; Helper routines for constraint objects.
(define (inform-about-value constraint)
  ((constraint 'I-have-value)))

(define (inform-about-no-value constraint)
  ((constraint 'I-have-no-value)))

; Fahrenheit - Celsius conversion example from the book works with the 
; implementation above.
(define (test-temperature-conversion)
    ; C * 9 = 5 * ( F - 32 )

    ; Constant 9
    (define nine (make-connector))
    (constant 9 nine)

    ; Celsius value C.
    (define C (make-connector))
    (probe "Celsius" C)

    ; C * 9
    (define C-9 (make-connector))
    (multiplier C nine C-9)

    ; Fahrenheit value
    (define F (make-connector))
    (probe "Fahrenheit" F)

    ; Constant 32
    (define thirty_two (make-connector))
    (constant 32 thirty_two)

    ; a = F - 32 => F = a + 32
    (define a (make-connector))
    (adder a thirty_two F)

    ; Constant 5
    (define five (make-connector))
    (constant 5 five)

    ; Constraint specifying that C-9 = 5 * a or 9 * C = 5 * (F - 32)
    (multiplier a five C-9)

    ; Let's set a value of 100 to C, it should set a value of 212 in F.
    ; Because we've installed probes on C & F, this statement will show 
    ; two lines one each for C and F.
    (set-value! C 100 'user)

    ; Forget this value. Now again two lines are printed with ? for C and F.
    (forget-value C 'user)

    ; Setting a value of 300 in F, should show 148.9 in C
    (set-value! F 300 'user)

    'done)

; Ex 3.33 Averager constraint.
(define (averager a b c)

  ; sum = a + b
  (define sum (make-connector))
  (adder a b sum)

  ; Constant 2
  (define two (make-connector))
  (constant 2.0 two)

  ; 2 * c = sum( = a + b) => c = (a + b) / 2
  (multiplier two c sum)

  'done)

; Ex 3.34
; In such a squarer constraint, we must be able to set the value of b and 
; have the system find the value of a, but this is not possible with this 
; setup.

; Ex 3.35 - Slightly different from the template given in the book, though the
; logic is similar.
(define (squarer a b)

  (define (process-new-value)
    (cond
      ((and (has-value? b) (< (get-value b) 0))
        (error "b cannot be less than 0"))
      ((has-value? b)
          (set-value! a (sqrt (get-value b)) me))
      ((has-value? a)
          (set-value! b (* (get-value a) (get-value a)) me))
      (else 'ignored)))

  (define (process-forget-value)
    (forget-value a me)
    (forget-value b me)
    (process-new-value))

  (define (me message)
    (cond
      ((eq? message 'I-have-value) process-new-value)
      ((eq? message 'I-have-no-value) process-forget-value)
      (else (error "Invalid message for squarer constraint"))))

  (connect a me)
  (connect b me)
  
  me)

; Ex 3.36
; We can think of the assignment happening in the context of the connecter a.
; I'm not going through this exercise.

; Ex 3.37
(define (cv value)
  (let ([ret (make-connector)])
    (constant value ret)
    ret))

(define (c/ a b)
  (let ([ret (make-connector)])
    (multiplier ret b a)
    ret))

(define (c* a b)
  (let ([ret (make-connector)])
    (multiplier b a ret)
    ret))

(define (c+ a b)
  (let ([ret (make-connector)])
    (adder a b ret)
    ret))

(define (c- a b)
  (let ([ret (make-connector)])
    (adder ret b a)
    ret))

; Ex 3.38
; a) Possible values are 35, 40, 45, 50
; b) If processes can be interleaved, other values like 55, 80, 110 are also
; possible.

; Ex 3.39
; 11, 100, 101, 121 are possible.

; Ex 3.40
; a) 10^6, 10^4, 10^2, 10^5, 10^3
; b) 10^6

; Ex 3.41
; Even without this modification, it's still safe to access balance e.g., even 
; if we read balance in the middle of a deposit txn, we'll either get the balance 
; before the deposit took place or after the deposit. In general, we won't get 
; any impossible value(A value is possible if it could've happened after a
; series of serialized withdrawal/deposit transactions).

; Ex 3.42
; This is a safe change to make and concurrency wise everything still works as
; before. Each account still has its own serializer object. This is a possible
; optimization that may save some object creation overhead (depending on the 
; serializer implementation) every time a deposit or withdraw message is called.

; Ex 3.43
; If the processes are run sequentially, then each exchange can be considered
; to be run in isolation from others. After each such isolated exchange, 
; balances are swapped between a pair of accounts and the third account remains 
; unchanged. Thus after each such exchange, the set of balances remains the 
; same {10, 20, 30}.
;
; In the first version of account-exchange program, suppose we have two
; concurrent processes - one trying to exchange accounts A, B and the other 
; accounts B, C.
;
; Each process can be rewritten as follows.
;
; exchange(account A, account B)
;   b1 = read (balance A)
;   b2 = read (balance B)
;   diff = b1 - b2
;   add (balance A) (-diff)
;   add (balance B) (+diff)

; (balance X) can be thought of as a shared memory location that stores the
; balance of account X. 
; add in the above exchange function can be considered uninterruptible 
; operation because it's protected by serializers. And as we saw earlier, 
; read operation is also serialized in effect(because it can't read an illegal
; value).
;
; Consider the sequence of operations below
;
; 1. Processes P1 and P2 both read account balances.
;       Now for P1 -> b1 = 10, b2 = 20, diff = -10
;           for P2 -> b1 = 20, b3 = 30, diff = -10
;       Balances -> A = 10, B = 20, C = 30
; 2. P1 subs 10 from account B. 
;       Balances -> A = 10, B = 10, C = 30
; 3. P2 subs 10 from account C. 
;       Balances -> A = 10, B = 10, C = 20
; 4. P2 adds 10 to account B.
;       Balances -> A = 10, B = 20, C = 20 
; 5. P1 adds 10 to account A.
;       Balances -> A = 20, B = 20, C = 20
;
; Now the balances are {20, 20, 20} which is clearly illegal.
;
; On the other hand, the total amount across all the three accounts is
; preserved once all the exchanges are finished. This is because whenever a process
; reads balances and takes the difference d, it will add d to one account and
; subtract d from another account. So the total balance is preserved (once all the
; exchanges are complete).
;
; And if we don't even use individual account serializers, then all the
; operations can be interleaved arbitrarily and total chaos ensues.

; Ex 3.44
; Ben's method works and Louis is wrong. 
; 
; Exchange problem is a more difficult problem because not only the total balance 
; across all a/cs should be preserved, but the set of all balances should also 
; be the same. This is because for an exchange involving a deposit to A with ;
; a withdraw from B to work correct, no changes should be made to these accounts 
; between the time their balances are read and deposit/withdraw are made. 

; Ex 3.45
; This can cause deadlocks at runtime. `serialized-exchange` works by acquiring
; both the accounts' serializers and then calling the deposit/withdraw routines
; If the deposit/withdraw routines themselves are serialized, then the process
; is stuck waiting for a mutex to be acquired & released while the mutx is 
; acquired by this very process.

; Ex 3.46
; Suppose processes P1 and P2 are trying to acquire a mutex M. If the
; test-and-set! procedure is not atomic, they can both read the cell value as 
; false - when this happens, they both set it to true and 'acquire' the mutex
; and proceed to access critical shared resources.

; A dummy mutex implementation.
(define (make-mutex)
  (define (me message)
    (cond
      ((eq? message 'acquire) #t)
      ((eq? message 'release) #t)
      (else (error "Illegal message for mutex object."))))
  me)

; Ex 3.47
;
; A) Semaphore implementation using a mutex and a counter.
; B) Similar to the mutex implementation, we use the lower level 
; test-and-set! primitive instead of a mutex.
(define (make-semaphore n)

  ; Mutex to protect access to the counter variable below. 
  (define mutex (make-mutex))

  ; Counter to represent the no. of processes currently accessing the resource.
  ; `read-counter` and `change-counter` are the user's interface to this
  ; counter.
  (define counter 0)

  ; This is called to check the current counter value.
  (define (read-counter)
    (mutex 'acquire)
    (define val counter)
    (mutex 'release)
    val)

  ; x = -1 or 1.
  (define (change-counter x)
    (mutex 'acquire)
    (set! counter (+ counter x))
    (mutex 'release)
    #t)

  ; Check the current counter value - if counter < n, then increment it and 
  ; return. Otherwise block(or busy-wait).
  (define (sem-acquire)
    (let ([val (read-counter)])
      (if (< val n)
        (change-counter 1)
        (sem-acquire))))

  ; Decrement the current counter value.
  (define (sem-release)
    (change-counter -1))

  (define (me message)
    (cond
      ((eq? 'acquire) (sem-acquire))
      ((eq? 'release) (sem-release))
      (else (error "Illegal message for semaphore object."))))
  me)

; Ex 3.48
; Deadlocks can occur because of cyclic dependencies in processes trying to 
; access shared resources. Consider the situation below with 3 processes and
; 3 mutexes. (let's assume the corresponding acct_nos are in the order 
; A < B < C)
;
; P1 -> (acquired A, waiting for B)
; P2 -> (acquired B, waiting for C)
; P3 -> (acquired C, waiting for A)
;
; With numbered accounts, the above situation is not possible because P3 would 
; not have acquired C without first acquiring A. And this is true in general
; for any no. of processes trying to exchange accounts using
; serialized-exchange.
;
; To design numbered accounts, we can use a global (mutex protected) integer 
; counter and whenever we create an account, increment it and attach it to the
; account object. Account object interface will need to provide a method for
; users to access the account number.

; Ex 3.49
; 

; Section 3.5
; Stream processing

; Streams can be thought of as lazily evaluated sequences.

; Ex 3.50
; stream-null?, cons-stream, stream-car, stream-cdr

; Ex 3.51
; (stream-map proc x) =~ (cons-stream (proc (stream-car x)) 
;                               (stream-map proc (stream-cdr x)))
; (stream-ref x n) =~ (stream-car x) or (stream-ref (stream-cdr x) (n - 1))
;
; 0, 1, 2, 3, 4, 5 are printed one per line and 5 is returned.
; 6, 7 are printed one per line and 7 is returned. 

; Ex 3.52
;
; Memoized version
; After stream-ref, sum is 136. stream-ref prints 136 to console.
; Underlying `seq` is partially evaluated at this point. If display-stream is
; called now, the items evaluated so far will be printed, but they wont be
; added to sum again. After display-stream, sum is 210. It prints values 
; 10, 15, 45, 55, 105, 120, 190, 210 one per line.
;
; Unmemoized version
; Results would be different from the memoized version. Note the streams here
; are odd streams - which have the first element already evaluated. More common
; in real world are even streams which only evaluate even the first element on
; demand. Results for this unmemoized version would  be different for 
; odd and even streams

; Ex 3.53
; It generates powers of 2 - (1, 2, 4, 8, 16 ...)

; Chicken Scheme has streams implemented as per SRFI-41. We must install 
; that module on the system and then import it. We must accordingly use the 
; primitives and special forms provided in that library, most notably 
; stream-cons instead of cons-stream.

(import streams)

(define (add-streams s1 s2)
  (stream-map + s1 s2))

; Ex 3.54
; With this definition, (stream-ref factorials n) = (n + 1)!
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (stream-cons 1 ones))
(define integers (stream-cons 1 (add-streams ones integers)))

(define factorials (stream-cons 1 
    (mul-streams (stream-cdr integers) factorials)))


; Ex 3.55
; Note how `partial-sums` is recursively defined in terms of itself. In
; computing the nth term, it utilizes its own (n-1)th term.
(define (partial-sums s)

  (define partials 
    (stream-cons
      (stream-car s)
      (add-streams (stream-cdr s) partials)))
    
    partials)

; Ex 3.56 
; Merge two sorted streams while eliminating duplicates.
(define (merge s1 s2)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
      (let ([s1car (stream-car s1)] [s2car (stream-car s2)])
        (cond
          ((< s1car s2car) (stream-cons s1car (merge (stream-cdr s1) s2)))
          ((< s2car s1car) (stream-cons s2car (merge s1 (stream-cdr s2))))
          (else (stream-cons 
                  s1car 
                  (merge (stream-cdr s1) (stream-cdr s2)))))))))

; Scale a stream of numbers by a constant factor.
(define (scale-stream s x)
  (cond
    ((stream-null? s) stream-null)
    (else
      (stream-cons
        (* x (stream-car s))
        (scale-stream (stream-cdr s) x)))))

; Hamming numbers are of the form 2^a * 3^b * 5^c, where a, b, c are
; non-negative integers.
(define S (stream-cons
            1
            (merge (scale-stream S 2) 
                   (merge (scale-stream S 3) (scale-stream S 5)))))

; Ex 3.57
; fibs is defined as (stream-cons 0 
;                       (stream-cons 1 
;                           (add-streams (stream-cdr fibs) fibs)))
; n - 1 additions will need to be performed to compute nth fibonacci number.
; Without memoization, O(F(n) - 1) additions are performed to compute nth 
; fibonacci number.

; Ex 3.58
; Digits of fractional part of (`num` / `den`) in base `radix`. With 
; `num` < `den`, integral part is zero and is ignored.
; (expand 1 7 10) gives the result 0.14285....
; (expand 3 8 10) gives the result 0.37500....

; Ex 3.59
; A) Integral of a power series A
(define (integral-series A)
  (define (iter A n)
    (cond
      ((stream-null? A) stream-null)
      (else (stream-cons 
              (exact->inexact (/ (stream-car A) n))
              (iter (stream-cdr A) (+ 1 n))))))

  (iter A 1))

; integral (e^x)  = e^x and e^0 = 1
(define exp-series (stream-cons 1 (integral-series exp-series)))

; B) Sin and Cosine serie.
; sin x = integral (cos x) and sin(0) = 0
; cos x = integral (- sin x) and cos(0) = 1

(define sin-series 
  (stream-cons 0 (integral-series cos-series)))

(define cos-series
  (stream-cons 1 (integral-series (scale-stream sin-series -1))))

; Ex 3.60
; Adding & multiplying two power series A and B - these are infinite sequences.
(define (add-series A B)
  (stream-map + A B))

(define (mul-series A B)
  (stream-cons 
    (* (stream-car A) (stream-car B)) 
    (add-series 
      (scale-stream (stream-cdr B) (stream-car A)) 
      (mul-series (stream-cdr A) B))))

; With add-series, mul-series, sin and cos defined as above, we can run the 
; following test to verify the identity sin^2(x) + cos^2(x) = 1
(define (test-verify-pythagorean-identity)

  ; sin^2(x) & cos^2(x)
  (define sin-square (mul-series sin-series sin-series))
  (define cos-square (mul-series cos-series cos-series))

  ; sin^2(x) + cos^2(x)
  (define one (add-series sin-square cos-square))

  ; Take the first 100 terms of the sum series and check that the first term
  ; is one and everything else is zero.
  (print (stream->list (stream-take 100 one)))
  
  'done)

; Ex 3.61
; Inverse of a power series with constant term = 1.
(define (invert-unit-series S)

  (define inv-series 
    (stream-cons 1 (mul-series (scale-stream (stream-cdr S) -1) inv-series)))
  
  inv-series)

; Ex 3.62
; div-series 
;
; Let's first define a more general invert-series whose constant term need not
; be 1, but has to be non-zero though.
; 1 / A = 1 / (a0 * A') where A' is a series with constant term = 1
; 1 / A = (scale-stream (invert-unit-series A'))
(define (invert-series A)
  (let ([a0 (stream-car A)] [ar (stream-cdr A)])
    (if (zero? a0)
      (error "Constant term cannot be zero for inverse to exist")
      (let ([scaled (scale-stream A (/ 1.0 a0))])
        (scale-stream (invert-unit-series scaled) (/ 1.0 a0))))))

(define (div-series A B)
  (mul-series A (invert-series B)))

; tan(X) = sin(X) / cos(X) = div-series sin cos
(define tan-series (div-series sin-series cos-series))

; Ex 3.63
; Every time `cons-stream` is called, it creates a fresh unevaluated stream. In
; the version without the local variable `guesses`, (sqrt-stream x) recursively
; calls (sqrt-stream x) which creates a new unevaluated stream, instead of 
; utilizing the already existing partially evaluated stream and thus leads to
; redundant computations.
;
; Without memoization, even the local variable version will involve redundant
; computations. 
;
; In recursively defined sequences like sqrt-stream, factorials, prime filters
; etc... it's always good to use a single stream object for the sequence with
; a memoized implementation. As our computations proceed, more and more
; elements will be evaluated and added onto this stream and these values won't
; have to be computed again.

; Ex 3.64
; Let's first define the `sqrt-stream` sequence. We assume the argument will be
; non-negative.
(define (sqrt-stream x)

  (define (improve a)
    (/ (+ a (/ x a)) 2.0))

  (define seq
    (stream-cons
      1.0
      (stream-map improve seq)))
  
  seq)

(define (stream-limit s tolerance)
  (let ([s0 (stream-ref s 0)] [s1 (stream-ref s 1)])
    (if (< (abs (- s1 s0)) tolerance)
      s1
      (stream-limit (stream-cdr s) tolerance))))

; Ex 3.65
; Not doing this.

; Infinite streams of pairs.

; Ex 3.66
; Interleaves two streams s.t the resulting stream can generate any element
; from either stream(eventually, in finite time, in a countable way)
(define (interleave-streams A B)
  (if (stream-null? A)
    B
    (stream-cons (stream-car A)
                 (interleave-streams B (stream-cdr A)))))

; Generates every possible pair (x, y) s.t x <- s, y <- t and every such pair
; is generated eventually(in finite time, in a countable way).
(define (pairs s t)
  (if (or (stream-null? s) (stream-null? t))
    stream-null
    (let ([s0 (stream-car s)] [sr (stream-cdr s)]
          [t0 (stream-car t)] [tr (stream-cdr t)])
      (stream-cons (list s0 t0)
                   (interleave-streams
                        (stream-map (lambda (tt) (list s0 tt)) tr)
                        (pairs sr tr))))))

; Ex 3.67
; We need to mix in the rest of the first column as well and it'll recursively 
; include all other columns' missing parts.
(define (pairs2 s t)
  (if (or (stream-null? s) (stream-null? t))
    stream-null
    (let ([s0 (stream-car s)] [sr (stream-cdr s)]
          [t0 (stream-car t)] [tr (stream-cdr t)])
      (stream-cons (cons s0 t0)
                   (interleave-streams
                        (stream-map (lambda (s) (cons s t0)) sr)
                        (interleave-streams 
                          (stream-map (lambda (t) (cons s0 t)) tr) 
                          (pairs2 sr tr)))))))

; Ex 3.68
; `stream-cons` is a special form that treats its 2nd argument as an
; expression. The earlier definition of `pairs` uses this `stream-cons` special
; form.
;
; Louis's `pairs` however uses `interleave-streams` which is a procedure that
; expects 2 streams as its arguments. So instead of treating `(pairs sr tr)` 
; as an expression, interleave-streams tries to evaluate it and thus the
; definition goes into a infinite recursive loop never terminating.

; Ex 3.79
; triples -> defined using pairs.
; This is inefficientG
(define (triples S T U)
  (if
    (or (stream-null? S)
         (stream-null? T)
         (stream-null? U)) 
        stream-null
    (let ([s0 (stream-car S)] [sr (stream-cdr S)]
          [t0 (stream-car T)] [tr (stream-cdr T)]
          [u0 (stream-car U)] [ur (stream-cdr U)])
      (let ([s0t0s (stream-map (lambda (uu) (list s0 t0 uu)) ur)]
            [s0t0u0 (list s0 t0 u0)]
            [s0trurs (stream-map (lambda (pr) (cons s0 pr)) (pairs tr ur))])
        (stream-cons
            s0t0u0
            (interleave-streams
              s0trurs
              (interleave-streams
                s0t0s
                (triples sr tr ur))))))))

; Using the above triples generator, we can select only pythagorean triples
; as follows. Problem with generating pythagorean triples this way is that 
; to reach the triple (x, y, z) it takes exponential steps in x.
(define (check-pyth? x y z)
  (= (square z) (+ (square x) (square y))))

(define pythagorean-triples
  (stream-filter
    (lambda (tpl) (apply check-pyth? tpl))
    (triples integers integers integers)))

; Ex 3.70, weighted merging of sequences. 
(define (merge-weighted weight A B)
  (cond
    ((stream-null? A) B)
    ((stream-null? B) A)
    (else
      (let ([a0 (stream-car A)] [ar (stream-cdr A)]
            [b0 (stream-car B)] [br (stream-cdr B)])
        (cond
          ((< (weight a0) (weight b0)) 
                (stream-cons a0 (merge-weighted weight ar B)))
          ((> (weight a0) (weight b0))
                (stream-cons b0 (merge-weighted weight A br)))
          (else
                (stream-cons a0 (merge-weighted weight ar B))))))))

; Instead of interleaving, it merges the parts by a weight function. Still 
; respects the ordering of pairs - for each pair (ai, bj) in the result, i <= j
(define (weighted-pairs weight A B)
  (cond
    ((or (stream-null? A) (stream-null? B)) stream-null)
    (else
      (let ([a0 (stream-car A)] [ar (stream-cdr A)]
            [b0 (stream-car B)] [br (stream-cdr B)])
        (stream-cons
          (list a0 b0)
          (merge-weighted
            weight
            (stream-map (lambda (b) (list a0 b)) br)
            (weighted-pairs weight ar br)))))))

; Ordered integer pairs with weight(i, j) = i + j and i <= j
(define sum-weighted-ipairs (weighted-pairs (lambda (pr) (+ (car pr) (cadr pr))) 
                                            integers
                                            integers))
; Similarly for any pair weight function.

; Ex 3.71
; weight(i, j) = i^3 + j^3

(define (sum-of-cubes pr)
  (define (cube x)
    (* x x x))
  
  (+ (cube (car pr)) (cube (cadr pr))))

(define sum-cubed-pairs (weighted-pairs sum-of-cubes
                                          integers
                                          integers))

; We can zip sum-cubed-pairs with its stream-cdr and select only those 
; with matching weights.
(define adj-pairs (stream-map cons
                              sum-cubed-pairs
                              (stream-cdr sum-cubed-pairs)))

; This definition finds the first 6 ramanujan numbers as 1729, 4104, 13832,
; 20683, 32832 and 39312
(define ramanujan-numbers
  (stream-filter (lambda (ppr) (= (sum-of-cubes (car ppr)) 
                                  (sum-of-cubes (cdr ppr))))
                 adj-pairs))

; Ex 3.72
; Similar to 3.71, we take zip(s, cdr(s), cdr(cdr(s)) and filter for relevant
; entries.

; Ex 3.73
; Let's first define the integrator circuit.
; Returns the stream Si = C + SIGMA(Xj * dt, 1 <= j <= i)
(define (integral X C dt)

  (define X-scaled (scale-stream X dt))

  (define S
    (stream-cons C
                 (add-streams S X-scaled)))
  S)

; Vi = V0 + (1 / C) * dt * SIGMA(i) + i * R
(define (RC R C dt)

  (lambda (i v0)
    (add-streams
      (scale-stream i R)
      (integral (scale-stream i (/ 1.0 C)) v0 dt))))

; Ex 3.74
; Since Alyssa's `sign-change-detector` has the parameters new-val followed by
; old-val, the missing expression is (stream-cons 0 sense-data)

; Ex 3.75
; At first, I couldn't figure out why Alyssa's solution is wrong. It turns out
; that Alyssa's code is computing the new average from old average and new
; value in stream. Instead, it should be computing new average from old stream
; value and next stream value. Alyssa's procedure should be modified
; accordingly and should be passed 3 arguments - stream, old_avg, old_value.
; It should then compute the new_avg from old_value and next_value, compare
; this new_avg with old_avg argument to detect sign changes.

; Ex 3.76
(define (average x y) (/ (+ x y) 2.0))

; Note the change of parameter order from the order given in the book.
(define (sign-change-detector old new)
  (cond
    ((and (< old 0) (> new 0)) 1)
    ((and (> old 0) (< new 0)) -1)
    (else 0)))

; Smooth is the stream of consecutive averages of the input stream.
(define (smooth sensor-data)
  (stream-map average sensor-data (stream-cdr sensor-data)))

; This approach is more modular because it allows us to swap out sign-change-
; detector with another kind of detector. 
(define (make-zero-crossings sensor-data)
  (stream-map sign-change-detector (smooth sensor-data)))

; Chicken Scheme has `delay` special form which generates promises and `force`
; procedure which can be used to evaluate a promise. delay/force are thus a
; lazy evaluation mechanism in Scheme. They cache the evaluated objects, so 
; calling force twice on a promise will only do the computation once.

; In lazy evaluation, parameters are evaluated only when they are actually
; needed.

; A delayed version of integral.
(define (integral-delayed X-delayed C dt)
  (define int
    (stream-cons C
                 (let ([X (force X-delayed)])
                   (add-streams (scale-stream X dt) int))))
  
  int)

; Solution for the differential equation dy/dt = f(y)
; y = integral(stream-map f y, y0, dt) for some given initial value y0
(define (solve-deriv f y0 dt)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; If f(y) = y, then solve-deriv(f, 1, 0.001) approximates the function e^(y * 0.001). 
; Then e can be calculated as (stream-ref (solve-deriv f 1 0.001) 1000)

; Ex 3.77
; Another version of integral-delayed. Assumes X-delayed is a delayed inifinite
; stream. Produces the same result for e as before.
(define (integral-delayed X-delayed C dt)
  (stream-cons C
               (let ([X (force X-delayed)])
                 (integral-delayed (delay (stream-cdr X)) 
                                   (+ C (* dt (stream-car X) )) dt))))

; Ex 3.78
; Solving the 2nd-degree differential equation 
; d^2(y)/d^2(t) = a * (dy / dt) + b * y
; We have three streams y, dy, ddy
; 
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral-delayed (delay dy) y0 dt))

  (define dy (integral-delayed (delay ddy) dy0 dt))

  ; ddy = a * dy + b * y
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))

  y)

; Ex 3.79, generalizing the procedure above to solve the differential equation
; d^2(y)/d^2(t) = f(dy / dt, y)
(define (solve-2nd-general f y0 dy0 dt)
  (define y (integral-delayed (delay dy) y0 dt))

  (define dy (integral-delayed (delay ddy) dy0 dt))

  ; ddy = f(dy, y) = stream-map f dy y
  (define ddy (stream-map f dy y))

  y)

; Ex 3.80, RLC circuit
(define (RLC R L C dt)

  (define (f vc0 i0)
    (define vc (integral-delayed (delay dvc) vc0 dt))
    (define i (integral-delayed (delay di) i0 dt))
    (define dvc (scale-stream i (/ -1.0 C)))

    (define (combo v i)
      (* (- v (* i R)) (/ 1.0 L)))

    (define di (stream-map combo vc i))
    
    (cons vc i))

  f)

; As we can see now, explicit usage of `delay` and `force` gives us great 
; flexibility in our programs. 

; Ex 3.81, rand procedure implementation from Ex 3.6 on a stream of action
; requests (generate or reset state), without assignment. I think the point of 
; this problem is somewhat undermined by the reality that underneath, random 
; number manipulation involves dealing with a lot of state, assignments and 
; other side effects.

; `action-stream` consists of requests. Each request is either the symbol
; 'generate or the symbol 'reset and produces an output stream - for each 
; generate request a random integer is produced and for a reset request, a #t 
; is produced.
;
; We don't have `rand` like in the book. Instead, we accept an initial value
; and an updater procedure.
(define (rand-seq action-stream init rand-update)
  (if (stream-null? action-stream)
    stream-null
    (let ([action (stream-car action-stream)]
          [others (stream-cdr action-stream)])
      (cond
        ((pair? action) 
            (stream-cons #t
                (rand-seq others (rand-update (cdr action)) rand-update)))
        (else
          (let ([val (rand-update init)])
            (stream-cons val
                         (rand-seq others val rand-update))))))))

; Ex 3.81 - stream versions of monte-carlo and estimate-integral procedures.
; I'm not going to implement these.
