; Chapter 5
; Register machines

; We've implemented a lisp interpreter in terms of an underlying lisp system. We 
; still don't understand how an interpreter can be implemented on typical register 
; machine based computers. 

; A register machine consists of a set of registers and is able to perform
; certain primitive operations (add two registers, move a register's contents to
; another register). A 'data path' consists of a set of registers and a primitive 
; operation. A register machine comprises of several data paths and a controller.
; The controller sequences the machine through these operations.

; Ex 5.1
; We maintain 5 registers to hold product, counter, n, 1 and a temp register.
; We have 2 operations mul(product * counter -> product) and add(counter + 1 -> 
; counter), a test (> to check counter and n). 

; Ex 5.2
; This is the controller for factorial program. Note the quote. This is not 
; actually executable code. Input n is placed in its register. When finished,
; output is placed in register named 'product'.
;
; registers - n, product, counter, temp
; constants - 1
; labels - compare, done
; operations & tests - mul, add, >
'(controller
   (assign product (const 1))
   (assign counter (const 1))
   compare
   (test (op >) (reg counter) (reg n))
   (branch (label done))
   (assign temp (op mul) (reg product) (reg counter))
   (assign product (reg temp))
   (assign temp (op add) (reg counter) (const 1))
   (assign counter (reg temp))
   (goto (label compare))
   done)

; Ex 5.3
; Newton's method for computing square roots. At the beginning, input x is
; placed in register named 'x'. At completion, result(sqrt(x)) is placed in 
; register named 'guess'
; registers - x, guess, temp, temp2
; constants - 1.0, 0.001, 0.0
; labels - good-enough-test, compare, done
; operations - mul, add, div, <
'(controller
   (assign guess (const 1.0))

   ; Assign temp = abs(guess * guess - x)
   good-enough-test
   ; Note an operation can have same source for both its inputs.
   (assign temp (op mul) (reg guess) (reg guess))
   (assign temp2 (op sub) (reg temp) (reg x))
   (assign temp (reg temp2))
   (test (op <) (const 0.0) (reg temp))
   (branch (label compare))
   (assign temp2 (op sub) (const 0.0) (reg temp))
   (assign temp (reg temp2))

   ; Test abs(guess * guess - x) < 0.001
   compare
   (test (op <) (reg temp) (const 0.001))
   (branch (label done))

   ; Improve guess = (guess + x / guess) / 2.0
   (assign temp2 (op div) (reg x) (reg guess))
   (assign temp (op add) (reg guess) (reg temp2))
   (assign temp2 (op div) (reg temp) (const 2.0))
   (assign guess (reg temp2))

   (goto (label good-enough-test))
   done)

; Controller for recursive factorial program. Uses a stack with save/restore.
; fact(n) = 1 if n == 1 else n * fact(n - 1)
; Registers - n, continue, result
; Constants - 1
; Operations - =, mul, sub
; Labels - factorial-begin, factorial-resume, factorial-done
; The caller must save return address on stack prior to calling this routine.
; Control will be returned back to that address once this routine is finished
; and the result will be made available in the `result` register.
'(controller
   (assign result (const 1))

   factorial-begin
   (test (op =) (reg n) (const 1))
   (branch (label factorial-done))
   (save n)
   (assign n (op sub) (reg n) (const 1))
   (assign continue (label factorial-resume))
   (save continue)
   (goto (label factorial-begin))

   factorial-resume
   (restore n)
   (assign result (op mul) (reg n) (reg result))

   factorial-done
   ; Goes back to caller (external to this controller at final step).
   (restore continue)
   (goto (reg continue)))

; Fibonacci sequence computation
; fibo(n) = n if n < 2 else fibo(n - 1) + fibo(n - 2)
; Registers - n, result, continue, temp
; Constants - 0, 1, 2
; Labels - fibo-begin, fibo-resume-1, fibo-resume-2, fibo-done
; Operations - +, -, *, <, >, read, print
;
; Initially, caller saves return address on stack, places n in register 'n' and
; calls this routine. Upon completion, result will be available in the register
; 'result' and control returned to that return address.
'(controller
   fibonacci-begin
   (assign result (reg n))
   (test (op <) (reg n) (const 2))
   (branch (label fibonacci-done))
   (assign result (const 0))
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label fibonacci-resume-1))
   (save continue)
   (goto (label fibonacci-begin))

   fibonacci-resume-1
   (restore n)
   (save result)
   (save n)
   (assign n (op -) (reg n) (const 2))
   (assign continue (label fibonacci-resume-2))
   (save continue)
   (goto (label fibonacci-begin))

   fibonacci-resume-2
   (restore n)
   (restore temp)
   (assign result (op +) (reg temp) (reg result))

   fibonacci-done
   (restore continue)
   (goto (reg continue)))

; Ex 5.4
; a)
; Check expt-controller.ss file.
; b)
; Check expt-controller-iterative.ss file.

; Ex 5.5 & 5.7
; I've written a simple register machine simulator and seen that it works with
; factorial, fibonacci and exponent controllers. Note, we've used slightly 
; different conventions in our controller programs.

; Ex 5.6
; After the label 'afterfib-n-1', we have three instructions 
; restore continue, assign to register n and save continue. The effect of 
; restore and save is that we're assigning to continue the top most value on
; stack (without popping it). But right after, we're setting continue to label 
; 'afterfib-n-2'. So those 2 restore and save operations are unnecessary.

; Section 5.2
; We've implemented the register machine simulator as a single script (this is
; found in the controller-simulator.ss file) - that reads a controller program 
; from stdin, simulates it and produces an output. We've seen that this 
; simulator produces correct outputs with factorial, fibonacci and exponent 
; controller programs. In the book, simulator is defined as a procedure that 
; accepts a program as an input. Because all the exercises in section 5.2 use 
; this model, we'll once again implement the simulator, this time as suggested 
; in the book.

; We'll use the term 'machine model' to denote a simulator object. When the
; message `start` is called on the model, the program is simulated and its
; output displayed.
;
; `make-machine`, `start`, `set-register-contents!`, `get-register-contents`
; are the machine model interface.
(define (make-machine register-names operations controller-program)
  (let ([machine (make-new-machine)])
    (for-each (machine 'allocate-register) register-names)
    ((machine 'install-operations) operations)
    ((machine 'install-instruction-sequence) 
        (assemble controller-program machine))
    machine))

; We'll represent each register as a procedure with local state.
(define (make-register name)
  (let ([contents '*unassigned*])

    (define (dispatch message)
      (cond
        ((eq? message 'get) contents)
        ((eq? message 'set) (lambda (new-value) (set! contents new-value)))
        (else (error "Unknown request for register named " name))))

    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

; We similarly represent the program stack as a procedure with local state.
(define (make-stack)
  (let ([stack '()])

    (define (dispatch message)
      (cond
        ((eq? message 'push) (lambda (val) (set! stack (cons val stack))))
        ((eq? message 'pop)
            (if (null? stack)
              (error "Empty stack")
              (let ([top-item (car stack)])
                (set! stack (cdr stack))
                top-item)))
        ((eq? message 'initialize)
            (set! stack '()))
        (else "Invalid message for a stack object " message)))

    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

; We now have the 2 main components of a register machine - register set and 
; stack. Let's now implement the `make-new-machine` procedure which sets up an
; empty machine upon which any program can be loaded.
(define (make-new-machine)
  (let* ([pc (make-register 'pc)]
         [flag (make-register 'flag)]
         [stack (make-stack)]
         [the-instruction-sequence '()]
         [the-ops (list (cons 'initialize-stack
                              (lambda () (stack 'initialize))))]
         [register-table (list (cons 'pc pc) (cons 'flag flag))])
  
  ; Create a new register with the given name.
  (define (allocate-register name)
    (if (assoc register-table name)
      (error "Register with name " name " already exists")
      (set! register-table (cons (cons name (make-register name))
                                 register-table))))

  ; Look up the value of the register with the given name.
  (define (lookup-register name)
    (let ([match (assoc name register-table)])
      (if (not match)
        (error "No register with name " name "exists in the table.")
        (cdr match))))

  (define (execute)
    (let ([insts (get-contents pc)])
      (if (null? insts)
        'done
        (begin
          (instruction-execution-proc (car insts))
          (execute)))))

  (define (dispatch message)
    (cond 
      ((eq? message 'start) (set-contents! pc the-instruction-sequence)
                            (execute))
      ((eq? message 'install-instruction-sequence)
            (lambda (seq) (set! the-instruction-sequence seq)))
      ((eq? message 'allocate-register) allocate-register)
      ((eq? message 'get-register) lookup-register)
      ((eq? message 'install-operations)
            (lambda (ops) (set! the-ops (append the-ops ops))))
      ((eq? message 'stack) stack)
      ((eq? message 'operations) the-ops)
      (else (error "Ilegal message for register machine objects."))))

  dispatch))

; A few convenience operations on register machines.
(define (start machine)
  (machine 'start))

(define (get-register machine register-name)
  ((machine 'get-register) register-name))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

; The assembler translates controller program instructions into actually 
; executable (on our register machine simulator) instructions.
(define (assemble controller-program machine)
  (extract-labels
    controller-program
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

; `extract-labels` scans the controller program text, separates out labels and
; instructions and maps labels to their locations in the instruction sequence.
(define (extract-labels controller-program receive)
  (if (null? controller-program)
    (receive '() '())
    (extract-labels (cdr controller-program)
      (lambda (insts labels)
        (let ([next-inst (car controller-program)])
          (if (symbol? next-inst)
            (receive insts (cons (make-label-entry next-inst insts) labels))
            (receive (cons (make-instruction next-inst) insts)
                     labels)))))))

(define (update-insts! insts labels machine)
  (let ([pc (get-register machine 'pc)]
        [flag (get-register machine 'flag)]
        [stack (machine 'stack)]
        [ops (machine 'operations)])
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst)
            labels machine pc flag stack ops)))
      insts)))

(define (make-instruction inst)
  (cons inst '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

; Each label entry is a pair of label and a pointer to its location within 
; the instruction sequence list.
(define (make-label-entry label insts)
  (cons label insts))

(define (lookup-label labels label-name)
  (let ([match (assoc labels label-name)])
    (if (not match)
      (error "No such label found")
      (cdr match))))

; Ex 5.8
; a will have a value of 3 when control reaches label 'there'. This is because
; `extract-label` joins actual instructions in-order, joins labels in a
; separate list in-order with each label entry pointing to its location in the 
; instruction sequence. Because it joins those 2 lists in-order and because in 
; lookup-label `assoc` finds the first matching entry, first goto moves pc to 
; first 'here', which sets the value of a to 3.

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond
    ((eq? (car inst) 'assign) (make-assign inst machine labels ops pc))
    ((eq? (car inst) 'test) (make-test inst machine labels ops flag pc))
    ((eq? (car inst) 'branch) (make-branch inst machine labels flag pc))
    ((eq? (car inst) 'goto) (make-goto inst machine labels pc))
    ((eq? (car inst) 'save) (make-save inst machine stack pc))
    ((eq? (car inst) 'restore) (make-restore inst machine stack pc))
    ((eq? (car inst) 'perform) (make-perform inst machine labels ops pc))
    (else (error "Illegal instruction"))))

; assign instruction - (assign x op args) where op is either a primitive
; operator (with no args) or an operator expression like (op add) arg ... 
(define (make-assign inst machine labels ops pc)
  (let* ([target (get-register machine (assign-reg-name inst))]
        [value-exp (assign-value-exp inst)]
        [value-proc
          (if (operation-exp? value-exp)
            (make-operation-exp value-exp machine labels ops)
            (make-primitive-exp (car value-exp) machine labels))])
    (lambda ()
      (set-contents! target (value-proc))
      (advance-pc pc))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels ops flag pc)
  (let ([condition (test-condition inst)])
    (if (operation-exp? condition)
      (let ([condition-proc
              (make-operation-exp condition machine labels ops)])
        (lambda ()
          (set-contents! flag (condition-proc))
          (advance-pc pc)))
      (error "Illegal test instruction"))))

(define (test-condition test-inst)
  (cdr test-inst))

; branch instruction - (branch (label x))
(define (make-branch inst machine labels flag pc)
  (let ([dest (branch-dest inst)])
    (if (label-exp? dest)
      (let ([insts (lookup-label labels (label-exp-label dest))])
        (lambda ()
          (if (get-contents flag)
            (set-contents! pc insts)
            (advance-pc pc))))
      (error "Illegal branch destination"))))

; Extract the destination from a branch instruction.
(define (branch-dest branch-inst)
  (cadr branch-inst))

; Check if expression is of form (label some-label)
(define (label-exp? expr)
  (and (list? expr) (eq? (car expr) 'label)))

; Extract the destination label from a branch instruction's destination part.
(define (label-exp-label dest-expr)
  (cadr dest-expr))

; goto instruction - (goto (label x)) or (goto (reg y))
(define (make-goto inst machine labels pc)
  (let ([dest (goto-dest inst)])
    (cond
      ((label-exp? dest)
        (let ([insts (lookup-label labels (label-exp-label dest))])
          (lambda ()
            (set-contents! pc insts))))
      ((register-exp? dest)
        (let* ([reg (get-register machine (register-exp-reg dest))])
          (lambda ()
            (set-contents! pc (get-contents reg)))))
      (else (error "Illegal goto instruction")))))

(define (goto-dest inst)
  (cadr inst))

(define (register-exp? dest)
  (and (list? dest) (eq? (car dest) 'reg)))

(define (register-exp-reg dest)
  (cadr dest))

; save instruction - save x. Saves the contents of register x on the stack.
(define (make-save inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

; restore instruction - restore x. Pops a value from stack and saves it into
; register x.
(define (make-restore inst machine stack pc)
  (let ([reg (get-register machine (stack-inst-reg-name inst))])
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-inst)
  (cadr stack-inst))

; perform instruction - e.g., (perform (op print)), performs the specified primitive 
; operation.
(define (make-perform inst machine labels ops pc)
  (let ([action (perform-action inst)])
    (if (operation-exp? action)
      (let ([action-proc (make-operation-exp action machine labels ops)])
        (lambda ()
          (action-proc)
          (advance-pc pc)))
      (error "Illegal perform instruction"))))

; Extract operation parts from a perform instruction.
(define (perform-action action-inst)
  (cdr action-inst))

(define (tagged-list? expr tag)
  (and (list? expr) (eq? tag (car expr))))

(define (operation-exp? expr)
  (and (pair? expr)
       (tagged-list? (car expr) 'op)))

; Primitive expressions are either (reg x), (const 1), (label z). We must
; create a zero-argument procedure out of such expressions - which, when
; evaluated return the value (stored in register x or constant 1 or position of
; the label within the instruction sequence)
(define (make-primitive-exp expr machine labels)
  (cond
    ((constant-exp? expr)
        (let ([constant-value-expr (constant-exp-value expr)])
          (lambda () constant-value-expr)))
    ((register-exp? expr)
        (let ([reg (get-register machine (register-exp-reg expr))])
          (lambda () (get-contents reg))))
    ((label-exp? expr)
        (let* ([label-name (label-exp-label expr)]
               [insts (lookup-label labels label-name)])
          (lambda () insts)))
    (else (error "Illegal primitive expression"))))

(define (constant-exp? expr)
  (tagged-list? expr 'constant))

(define (constant-exp-value expr)
  (cadr expr))

; Operation expressions - (op +) arg1 arg2 ...
(define (make-operation-exp expr machine labels ops)
  (let ([operator (lookup-prim (operation-exp-op expr) ops)]
        [operands (map (lambda (e) 
                         (if (label-exp? e)
                           (error "Labels not allowed as operands in operator expressions")
                           (make-primitive-exp e machine labels)))
                       (operation-exp-operands expr))])
    (lambda ()
      (apply operator (map (lambda (p) (p)) operands)))))

; expr is of the form - (op +) 1 2
(define (operation-exp-op expr)
  (cadr (car expr)))

(define (operation-exp-operands expr)
  (cdr expr))

(define (lookup-prim operator ops)
  (let ([match (assoc operator ops)])
    (if match
      (cdr match)
      (error "Unsupported operator"))))

; Ex 5.9
; We've modified `make-operation-exp` to raise an error when it sees a label for
; one of the operands.

; Ex 5.10
; We've employed data abstraction and separated syntax handling from simulation
; logic with the help of syntax procedures (e.g., `operation-exp-op`). This
; enables us the change the syntax of our register machine instructions if we 
; want without modifying core simulator logic.

; Ex 5.12, 5.13, 5.15
; We've done much of this in our first implemntation of the simulator.

; Other exercises till 5.19 are interesting but I'm not doing them.

; Section 5.3
; Memory allocation and garbage collection.

; In our register machine simulator implementation, we've used lists and list
; operations to represent data objects like register table, stack etc... Basic
; operations of our simulator like pushing/popping stack, setting register
; values relied on list operations being primitive. Modern computers offer a 
; linear address based view of computer memory and we must learn to represent
; and perform our memory accesses using this linear address model.

; We first learn to represent lists and implement list operations in terms of 
; address arithmetic available on modern computers. We use typed pointers which
; store the data type along with the value of a data object. This enables us to 
; distinguish two data objects given just their pointers. We can also 'intern'
; the symbols to optimize operations on symbols.

; Scheme's vectors fit nicely into the linear address view of computer memory.
; Access nth element of a vector is a constant time operation.

; To represent pairs within our simulators, we use two vectors `the-cars` and 
; `the-cdrs`. A pair can now be represented as the typed pointer ('pair', i) 
; where i is an index into the cars and cdrs vectors. So this pair's car is 
; located at the-cars[i] and it's cdr is located at the-cdrs[i].

; Every data reference is done by the usage of (typed) pointers. e.g., If we
; have a pair p and its value(index) is i, the-cars[i] stores the pointer to its car
; element and the-cdrs[i] stores the pointer to its cdr element.

; Two objects are said to be `eq?` if their pointers are identical (type & data 
; stored (which is an integer for pairs and something else for other data
; objects) in the pointer.

; 
