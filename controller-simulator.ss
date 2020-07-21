; A simple stack based controller simulator. A controller is a simple register
; machine specification of an algorithm. So basically, this is a simple
; register machine simulator.
;
; Supports +, -, >, <, * primitive operations on integers.

; Call stack. Supports three operations - push, pop and printing its contents.
(let ([call-stack '()])

  (define (push val) 
    (set! call-stack (cons val call-stack)))

  (define (pop)
    (if (null? call-stack)
      (error "Call stack is empty")
      (let ([ret (car call-stack)])
        (set! call-stack (cdr call-stack))
        ret)))

  (define (print-contents)
    (print "stack is " call-stack))
  
  (set! stack-push push)
  (set! stack-pop pop)
  (set! stack-print print-contents))

; Register set is a list of (register-name, register-value) pairs. We can get
; a register's value(error if no such register) or set a value for the register.
(let ([registers '()])

  (define (get reg)
    (let ([match (assoc reg registers)])
      (if (not match)
        (error "No register with name" reg)
        (cdr match))))

  (define (set reg val)
    (let ([match (assoc reg registers)])
      (if (not match)
        (set! registers (cons (cons reg val) registers))
        (set-cdr! match val))))

  (define (show)
    (print "Regsiters are " registers))

  (set! register-get get)
  (set! register-set set)
  (set! register-show show))

; The simulator. Accepts a program(a register machine specification for an
; algorithm) as its input and simulates its execution using a stack and a 
; register set.
; Instructions supported are assign, save, restore, test, branch and goto. Ins-
; tructions can have labels.
; +, -, >, < are the primitive operators supported. Additionally 'read' is also
; supported. (op read) reads an integer from stdin and returns it. 'print' is
; another supported operation which prints a value to stdout.
(define (simulate program)

  ; Our simulator exits upon seeing an instruction label named 'quit
  (stack-push 'quit)

  ; Find the position of a label within the program.
  (define (find-label-pos label)

    (define (iter expressions i)
      (cond
        ((null? expressions) i)
        ((and (symbol? (car expressions)) (eq? label (car expressions))) i)
        (else (iter (cdr expressions) (+ i 1)))))

    (iter program 0))

  (define (tagged-list? instr label)
    (and (list? instr) (eq? (car instr) label)))

  (define (quit? instr)
    (eq? instr 'quit))

  (define (perform? instr)
    (eq? instr 'perform))

  (define (test? instr)
    (tagged-list? instr 'test))

  (define (assign? instr)
    (tagged-list? instr 'assign))

  (define (goto? instr)
    (tagged-list? instr 'goto))

  (define (branch? instr)
    (tagged-list? instr 'branch))

  (define (save? instr)
    (tagged-list? instr 'save))

  (define (restore? instr)
    (tagged-list? instr 'restore))

  (define (initial-set? instr)
    (tagged-list? instr 'initial-set))

  (define (goto-label instr)
    (let ([part (cadr instr)])
      (if (eq? (car part) 'reg)
        (register-get (cadr part))
        (cadr part))))

  ; source can be of form (const x), (reg register-name) or (op read) or 
  ; (label label-name)
  (define (read-from-input-source source)
    (let ([source-type (car source)]
          [source-name (cadr source)])
      (cond
        ((eq? source-type 'const) source-name)
        ((eq? source-type 'reg) (register-get source-name))
        ((eq? source-type 'label) source-name)
        ; We read a number from stdin.
        ((and
           (eq? source-name 'read)
           (eq? source-type 'op))
            (read))
        (else "Unknown source type" source-type))))

  (define (operator-func op)
    (cdr (assoc op (list    (cons '+ +)
                            (cons '- -)
                            (cons '> >)
                            (cons '< <)
                            (cons '= =)
                            (cons 'read read)
                            (cons 'print print)
                            (cons '* *)))))

  (let ([icount 0] [pc 0] [test-result #f])

    (define (goto label)
      (if (eq? label 'quit)
        (begin (register-show)
               'done)
        (begin
          (set! pc (find-label-pos label))
          (iter))))

    ; e.g., [(op +) (reg a) (reg b)]
    (define (apply-op op-and-args)
      (apply (operator-func (cadar op-and-args))
             (map read-from-input-source (cdr op-and-args))))

    ; Test instruction has the format - (test (op +) (reg a) (reg b))
    (define (test test-instr)
      (apply-op (cdr test-instr)))

    (define (iter)
      (register-show)
      (stack-print)
      (define instr (list-ref program pc))
      (print "instr is " instr)
      (set! pc (+ pc 1))
      (set! icount (+ icount 1))

      (cond
        ((symbol? instr) (iter))
        ((initial-set? instr) (register-set (cadr instr)
                                    (read-from-input-source (caddr instr)))
                              (iter))
        ((goto? instr) (goto (goto-label instr)))
        ((test? instr)
            (set! test-result (test instr)) (iter))
        ((branch? instr)
            (if test-result
              (goto (goto-label instr))
              (iter)))
        ((perform? instr)
            (apply-op (cdr instr))
            (iter))
        ((assign? instr)
            (let ([reg-name (cadr instr)])
              (register-set reg-name
                (if (eq? 'op (caaddr instr))
                  (apply-op (cddr instr))
                  (read-from-input-source (caddr instr)))))
            (iter))
        ; stack related instructions
        ((save? instr)
            (stack-push (register-get (cadr instr)))
            (iter))
        ((restore? instr)
            (register-set (cadr instr) (stack-pop))
            (iter))
        (else (error "Unsupported instruction"))))
    
    (iter)))

; Now we read a single controller program from stdin and simulate it.
(print (simulate (read)))
