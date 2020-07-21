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
; factorial, fibonacci and exponent controllers.

; Note, we're using slightly different conventions in our controller programs.
; In particular, 

; Ex 5.6
; After the label 'afterfib-n-1', we have three instructions 
; restore continue, assign to register n and save continue. The effect of 
; restore and save is that we're assigning to continue the top most value on
; stack (without popping it). But right after, we're setting continue to label 
; 'afterfib-n-2'. So those 2 restore and save operations are unnecessary.


; Section 5.2
; We've implemented the register machine simulator as a single script - that
; reads a controller program from stdin, simulates it and produces an output.
; We've seen that this simulator produces correct outputs with factorial,
; fibonacci and exponent controller programs. In the book, simulator is defined
; as a procedure that accepts a program as an input. Because all the exercises 
; in section 5.2 use this model, we'll once again implement the simulator, this 
; time as suggested in the book.
