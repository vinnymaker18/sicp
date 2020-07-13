; Chapter 4 - Metalinguistic abstraction

; In this chapter, we learn to view languages as levels of abstraction, learn
; to build more abstract and powerful languages on top of lower level languages,
; learn designing & implementing domain specific languages(DSLs), learn to view
; ourselves as language designers. 

; Building custom languages for complex problem domains is a powerful
; programming technique.

; Section 4.1
; The metacircular evaluator is a Scheme evaluator written in Scheme itself.

; Ex 4.1
; Notice that order of evaluating a procedure's operands depends on how `cons`
; is evaluated. This means in evaluating (cons x y), either x is evaled first 
; and then y - in which case it's a left-to-right evaluation order, or if y is 
; first evaled and then x, it's a right-to-left order. But overall, the result
; of cons itself is in left-to-right order, i.e 
; (cons (begin (print 1) 1) (cons (begin (print 2) 2) '())) always returns
; the list (1 2) whether it prints 1 followed by 2 or 2 by 1.
;
; We assume `first-operand` returns the first operand in the left-to-right
; order and `rest-operands` return the remaining operands. If we repeatedly 
; apply `first-operand`, then we'll see the operands in left-to-right order.
;
; The following implementation for list-of-values evaluates the expressions in
; left-to-right order no matter the evaluation order of cons. We assume that
; the let syntactic form evaluates its bindings in the given order.
;
; (define (list-of-values exps env)
;   (if (no-operands? exps)
;       '()
;       (let ([fst (eval (first-operand exps) env)]
;             [rst (list-of-values (rest-operand exps) env)])
;           (cons fst rst))))

; We simply change the order of fst and rst definitions in the let block to
; ensure right-to-left evaluation order.

; Section 4.1.2
; Representations for various expression types.

; A self-evaluating form.
(define (self-evaluating? expr)
  (or (number? exp)
      (string? exp)))

; Variable.
(define (variable? expr)
  (symbol? expr))

(define (tagged-list? expr sym)
  (and (pair? expr)
       (equal? (car expr) sym)))

; A quoted form.
(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (text-of-quotation expr)
  (cadr expr))

; An assignment form - (set! var expr)
(define (assignment? expr)
  (tagged-list? expr 'set!))

(define (assignment-variable expr)
  (cadr expr))

(define (assignment-value expr)
  (caddr expr))

; Lambda form - (lambda (params) expr_list)
(define (lambda? expr)
  (tagged-list? expr 'lambda))

(define (lambda-parameters expr)
  (cadr expr))

(define (lambda-body expr)
  (cddr expr))

; body is actually an expr_list.
(define (make-lambda params body)
  (cons ('lambda (cons params body))))

; Define form - (define var expr) or (define (var params) expr_list)
(define (define? expr)
   (tagged-list? expr 'define))

(define (define-variable expr)
  (if (variable? (cadr expr))
    (cadr expr)
    (caadr expr)))

; define form's value is either the value in the first type or a procedure 
; object in the second type.
(define (define-value expr)
  (if (variable? (cadr expr))
    (caddr expr)
    (make-lambda (cdadr expr) (cddr expr))))

; If form - (if <predicate> <consequent> <alternative>?) 
(define (if? expr)
  (tagged-list? expr 'if))

(define (if-predicate expr)
  (cadr expr))

(define (if-consequent expr)
  (caddr expr))

(define (if-alternative expr)
  (if (null? (cdddr expr))
    #f
    (cadddr expr)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin form - (begin <expr_list>)
(define (begin? expr)
  (tagged-list? expr 'begin))

; A sequence or a begin sequence is simply a list of expressions.
(define (begin-sequence expr)
  (cdr expr))

(define (last-exp? begin-sequence)
  (null? (cdr begin-sequence)))

(define (first-exp begin-sequence)
  (car begin-sequence))

(define (rest-exps begin-sequence)
  (cdr begin-sequence))

(define (sequence->exp sequence)
  (cond
    ((null? sequence) sequence)
    ((last-exp? sequence) (car sequence))
    (else (make-begin sequence))))

(define (make-begin sequence)
  (cons 'begin sequence))

; A procedure application is defined as none of the above forms.
(define (application? expr)
  (pair? expr))

(define (operator expr)
  (car expr))

(define (operands expr)
  (cdr expr))

; Helper routines on procedure operands.
(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

; cond form - (cond <clause_list>) where each clause is of the form
; (<clause-predicate> <expr_list>).
(define (cond? expr)
  (tagged-list? expr 'cond))

(define (cond-clauses expr)
  (cdr expr))

; Else predicate always evaluates to true - usually the last clause in a cond
; form.
(define (cond-else-clause? clause)
  (tagged-list? clause 'else))

(define (cond-predicate clause)
  (car clause))

; cond clause - (predicate_expr <action_list>) or (predicate => <recipient>)
(define (cond-actions clause)
  (if (equal? (cadr clause) '=>)
    ; This is a single procedure application expression.
    ; e.g. (cond (1 => print)) will give us '((print 1))
    ; This will evaluate the predicate twice, but we'll ignore this issue
    ; for now.
    ; TODO -> Fix this.
    (list (list (caddr clause) (car clause)))
    (cdr clause)))

; Transform a cond form into a nested if form.
(define (cond-if expr)
  (expand-clauses (cond-clauses expr)))

; Expands a sequence of cond clauses into a nested if expression. Treats 
; else clause specially.
(define (expand-clauses expr)
  (if (null? expr)
    #f ; Well, we return #f for any empty cond expression.
    (let ([first-clause (car expr)]
          [rest-clauses (cdr expr)])
      (if (cond-else-clause? first-clause)
        (if (null? rest-clauses)
            (sequence->exp (cond-actions first-clause))
            (error "else clause is not the last in a cond form"))
        (make-if (cond-predicate first-clause)
                 (sequence->exp (cond-actions first-clause))
                 (expand-clauses rest-clauses))))))

; So far, we've defined and represented the forms of following kinds
;
; Self evaluating forms - variables and symbols
; Quoted forms, assignment forms, lambda, define, if, begin, procedure 
; applications
; Cond forms are transformed into nested if forms.

; Ex 4.2
; Because we take anything that's not a variable, symbol, quoted form, assign-
; ment, lambda, define, if, begin or cond as a procedure application, we must 
; first test for these forms to determine the type of a given expressions. Only if
; none of these forms match, then we consider it a procedure application.
; For (define x 3), the interpreter will treat it as an application and tries
; to lookup a variable named define in the environment and then apply it to 2 
; arguments x and 3, which is clearly wrong thing to do - it should rather be 
; creating a new binding for x.
;
; 'call' can be defined as a new special form - (call <proc> <expr>*) and we
; can have routines to extract parts of this form.
;
; (define (call? expr)
;   (tagged-list? expr 'call))
;
; We can modify this to create a procedure object if it's not a variable.
; (define (call-procedure expr)
;   (cadr expr))
;
; (define (call-arguments expr)
;   (cddr expr))
;
; In eval, we should a test for call form before testing for other forms. It
; can be simply written as 
; (if (call? expr)
;   (apply (call-procedure expr) (call-argument expr))

; Ex 4.3
; For primitives(self-evaluating forms) and procedure applications `eval`
; remains the same. For special forms, we maintain a dispatch table which
; stores their execution strategies as procedures that operate on expressions.
; I'm not going to actually implement eval in data directed style.

; Ex 4.4
; `and` and `or` special forms.
;
; (and <expr_list>)
(define (and? expr)
  (tagged-list? expr 'and))

(define (and-clauses expr)
  (cdr expr))

(define (and-last-clause? expr)
  (null? (cdr expr)))

(define (eval-and clauses env)

  (define (iter exprs)
    (cond
      ((null? exprs) => #t)
      ((and-last-clause? exprs) => (eval2 (car exprs) env))
      (else => (let ([fst (eval2 (car exprs) env)])
                 (if (not fst)
                   #f
                   (iter (cdr exprs)))))))

  (iter clauses))

(define (or? expr)
  (tagged-list? expr 'or))

(define (or-clauses expr)
  (cdr expr))

(define (or-last-clause? expr)
  (null? (cdr expr)))

(define (eval-or clauses env)

  (define (iter exprs)
    (cond
      ((null? exprs) => #f)
      ((and-last-clause? exprs) => (eval2 (car exprs) env))
      (else => (let ([fst (eval2 (car exprs) env)])
                 (if fst
                   fst
                   (iter (cdr exprs)))))))

  (iter clauses))

; Alternatively, we can rewrite `and` and `or` clauses as nested if expressions.

; Ex 4.5
; Cond clauses interpretation above is now changed to accomodate this new
; style. Because we've used generic procedures `cond-predicate` and
; `cond-actions` to extract parts of cond clauses, we only need to change these
; two procedures. In fact, we only need to change cond-actions. But this causes 
; the predicate to be evaluated twice which is an issue we'll ignore for now.

; Ex 4.6
; Let form is essentially a lambda form. We make use of the `make-lambda`
; constructor for lambda expressions.
(define (let? expr)
  (tagged-list? expr 'let))

(define (let-body-exprs expr)
  (cddr expr))

(define (let-defn-vars expr)
  (map car (cadr expr)))

(define (let-defn-val-exprs expr)
  (map cadr (cadr expr)))

; Let combination is transformed into a lambda expression.
; (let (defn_list) expr_list) -> ((lambda (params) expr_list) let-defn-val-exprs)
(define (let-combination expr)
  (cons (make-lambda (let-defn-vars expr) (let-body-exprs expr))
        (let-defn-val-exprs expr)))

; Ex 4.7
; let* is like let except it makes available all previous let-var definitions
; to those that follow. It can be transformed into a nested let expression by
; using one let per definition. Each successive let extends the environment by
; one frame with a single variable binding.

(define (let*? expr)
  (tagged-list? expr 'let*))

; We can reuse let expression selectors for let* expressions as well.
