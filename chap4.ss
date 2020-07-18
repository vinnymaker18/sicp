(load "utils.ss")

; Redoing chapter 4 from scratch after a break.

; Chapter 4
; Metalinguistic abstraction
; Metalinguistic abstraction aka the art of establishing more and more abstract 
; languages in order to solve complex problems. A language applicable for a
; whole domain is preferred over an application designed to solve a specific
; problem. Once such a DSL is established, solutions to specific problems are
; expressed in terms of it. This way, software produced is more general and
; reusable.

; In this way, problem solving becomes the art of designing high level DSLs and
; expressing solutions in terms of these languages. Instead of concentrating on
; specific problems, we broaden our scope a little and design more general 
; systems applicable for an entire domain. This is a shift in our perspective
; as programmers. Examples include SQL for databases, unix shell scripting
; languages etc...

; Programming is special in that not only can we design abstract languages, we
; can also implement evaluators for them.

; Section 4.1
;
; In this chapter, we first learn to implement language evaluators by doing an 
; evaluator for Scheme language. Because we choose to implement this evaluator
; in Scheme itself - it's called a metacircular evaluator.

; Our scheme evaluator is essentially the formulation of the environment model
; of evaluation.

; We can think of the Scheme system as divided into the following components.
;
; Reader -> A program which reads primitives (symbols or constants) and s-expr-
; essions from an input source (e.g. a file or a cli). Each call to the
; reader's read() will return a single complete expression, which is either a 
; symbol or a constant or a nested list of symbols/expressions, e.g., one call
; to read() may return the symbol 'windowTitle or the list containing 4 symbols
; (modify a b 34). At this point, we don't know the meaning of these
; expressions - it's the job of the evaluator to interpret their meaning. Note
; the homoiconic quality of Scheme, the read() call's return values are either
; symbols or constants or lists(of symbols/lists...) which are basic scheme data
; elements themselves. The beauty lies in the simplicity of its syntax -
; everything from procedure calls to special forms to assignment statements can
; be represented as s-expressions. 

; Evaluator -> This is the core of the system. It reads forms from the reader
; and evaluates them, i.e. it computes the values represented by these forms.
; In computing these values, it may also affect the world. The process of the 
; evaluator can be seen as repeatedly reading forms from reader and evaluate them 
; in a loop.

; Thus the entire scheme system can be thought of as a read-eval loop - a loop
; that reads forms from the reader and evaluates them until the input source is
; exhausted.

; If we add a print step to the system above, which prints the evaluated
; objects to standard output, we get a read-eval-print loop aka a REPL which is 
; like an interactive interface to the scheme evaluator.

; By separating reader and the evaluator and by employing data abstraction (for
; identifying and selecting parts of various expression types which we refer to 
; as syntax procedures), we can keep evaluator isolated from the specific syntax 
; of the language we've chosen to write interpreter for. By appropriately 
; modifying our reader and the abstract procedures for identifying/selecting 
; expressions, we can keep our evaluator unchanged.

; For the purposes of this chapter, we're only interested in the evaluator com-
; ponent.

; Let's now define a few terms we'll be using often in this document.

; Expression - A symbol, constant or a nested list of symbols and constants.
; Our reader reads input from a source (file or stdin) and produces expressions 
; (also called forms). The meaning of an expression is unclear without an
; environment.

; `eval` - The essential procedure of the evaluator. It takes an expression and
; an environment and evaluates(computes the value of, possibly producing side
; effects) the expression.

; `eval` works by determining the type of the expression passed. For a self eval-
; uating expression, eval returns itself. For variables, it finds the binding
; in the environment.

; For quoted expressions, `eval` returns the expression after unquoting it.

; For assignment statements, `eval` first evaluates the value expression and 
; then modify the environment with a new binding.

; If expression - predicate is first evaluated, then one of the consequent and
; alternative is evaluated, depending on the predicate's value.

; cond is syntactically transformed into a nested if expression.

; A lambda expression is evaluated into a procedure object which packages the
; body of the lambda together with the current environment.

; A begin expression is evaluated by sequentially evaluating its subexpressions
; and returning the final subexpression's result.

; A procedure application is evaluated by evaluting the operator subexp and 
; the operand subexpressions and finally calling `apply` with operator and
; operands.

; We must note here that `eval` accepts syntactic forms as inputs.

; Note the names are changed to `eval-meta` and `apply-meta` to have the real `eval`
; and `apply` remain unchanged.
(define (eval-meta expr env)
  (cond ((self-evaluating? expr) expr)
        ((variable? expr) (lookup-variable-value expr env))
        ((quoted? expr) (text-of-quotation expr))
        ((call? expr) (eval-meta (call->application expr) env))
        ((and? expr) (eval-and expr env))
        ((or? expr) (eval-or expr env))
        ((let? expr) (eval-meta (let-combination expr) env))
        ((let*? expr) (eval-meta (let*-nested-lets expr) env))
        ((assignment? expr) (eval-assignment expr env))
        ((definition? expr) (eval-definition expr env))
        ((if? expr) (eval-if expr env))
        ((for? expr) (eval-meta (for-combination expr) env))
        ((lambda? expr) (make-procedure (lambda-parameters expr)
                                        (lambda-body expr)
                                        env))
        ((begin? expr) (eval-sequence (begin-actions expr) env))
        ; cond is transformed into a nested if expression
        ((cond? expr) (eval-meta (cond->if expr) env)) 
        ; Note the operator subexpression will evaluate to a procedure object
        ; which contains within it the base environment for its application.
        ((application? expr) 
                             (apply-meta (eval-meta (operator expr) env)
                                (map (lambda (operand) (eval-meta operand env))
                                     (operands expr))
                                env))
        (else (error "Unknown expression type."))))

; `apply` takes two arguments, a procedure and a list of arguments to be
; applied to the procedure. Procedures are classified into two types -
; primitive procedures and compound procedures. Primitives are applied
; directly. Compound procedures are applied by sequentially evaluating the 
; expressions that make up the procedure body. The environment for this
; evaluation is made up by extending the environment embedded in the procedure
; (when this procedure was first created using a lambda expression) with a new
; frame that binds the procedure's formal parameters to its arguments.
(define (apply-meta procedure arguments env)
  (cond
    ((primitive-procedure? procedure) 
        (apply-primitive-procedure
          procedure
          arguments))
    ((compound-procedure? procedure)
        (eval-sequence (procedure-body procedure)
                       (extend-environment (procedure-parameters procedure)
                                           arguments
                                           (procedure-environment procedure))))
    (else (error "Unknown procedure type - apply-meta"))))

; `list-of-arg-values` forces evaluation of the list of expressions passed to
; it.
(define (list-of-arg-values exprs env)
  (if (no-operands? exprs)
    '()
    (cons (actual-value (first-operand exprs) env)
          (list-of-arg-values (rest-operands exprs) env))))

; `list-of-delayed-args` produces a list of delayed objects from a list of 
; expressions.
(define (list-of-delayed-args exprs env)
  (if (no-operands? exprs)
    '()
    (cons (delay-it (first-operand exprs) env)
          (list-of-delayed-args (rest-operands exprs) env))))

; `list-of-values` takes the operand expressions and an environment and produces
; the list of values to be applied to the procedure. Notice we're here using
; `no-operands?`, `first-operand` and `rest-operands` abstract procedures
; instead of assuming that operands is a list. operands is actually obtained by
; calling `operands` on a procedure application.
(define (list-of-values operands env)
  (if (no-operands? operands)
    '()
    (let ([first (first-operand operands)]
          [rest (rest-operands operands)])
      (cons (eval-meta first env) (list-of-values rest env)))))

; `eval-if` evaluates the predicate part and then one of the consequent / 
; alternative parts.
(define (eval-if expr env)
  (if (true? (eval-meta (if-predicate expr) env))
    (eval-meta (if-consequent expr) env)
    (eval-meta (if-alternative expr) env)))

(define (empty-sequence? expr_list)
  (null? expr_list))

(define (last-expression? expr_list)
  (null? (cdr expr_list)))

(define (first-expression expr_list)
  (car expr_list))

(define (rest-expressions expr_list)
  (cdr expr_list))

; `eval-sequence` takes a list of expressions and an environment - evaluates
; them sequentially (one by one) and returns the result of the final
; expression. We also allow for empty expression list as well, which returns #f.
(define (eval-sequence expr_seq env)
  (cond
    ((empty-sequence? expr_seq) #f)
    ((last-expression? expr_seq) 
        (eval-meta (first-expression expr_seq) env))
    (else (eval-meta (first-expression expr_seq) env)
          (eval-sequence (rest-expressions expr_seq) env))))

; `eval-assignment` first evaluates the value expression and then modifies the
; environment.
(define (eval-assignment expr env)
  (let ([value (eval-meta (assignment-value expr) env)])
    (set-variable-value! (assignment-variable expr) value env)
    'ok))

; `eval-definition` is similar to assignment except a define only changes its 
; local environment.
(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (eval-meta (definition-value expr) env)
                    env)
  'ok)

; Ex 4.1
; We use `list-of-values` for evaluating operand expressions in a procedure
; application. `list-of-values` uses the `cons` procedure of the underlying
; lisp. If that `cons` evalues its arguments left to right, then the operands
; in our language's procedure applications are evaluated left to right. If 
; `cons` evaluates right to left, then the operands in our langauge are
; evaluated right to left. 

; We assume `first-operand` returns the first operand from left. By using let
; special form, we're here forcing left-to-right operand evaluation order.
(define (list-of-values2 operands env)
  (if (no-operands? operands)
    '()
    (let ([first (eval-meta (first-operand operands) env)]
          [rest (list-of-values2 (rest-operands operands) env)])
      (cons first rest))))

; We can similarly force a right-to-left operand evaluation order by switching
; the order first and rest evaluations.

; Section 4.1.2 
; Representations of various expression types

; Self evaluating expressions - numbers and strings.
(define (self-evaluating? expr)
  (or (number? expr) (string? expr)))

; Variables
(define (variable? expr)
  (symbol? expr))

; A helper routine.
(define (tagged-list? expr symbol)
  (and (pair? expr) (equal? symbol (car expr))))

; Quoted expressions - (quote <expr>)
(define (quoted? expr)
  (tagged-list? expr 'quote))

(define (text-of-quotation expr)
  (cadr expr))

; Assignments are of form - (set! variable value_expr)
(define (assignment? expr)
  (tagged-list? expr 'set!))

(define (assignment-variable expr)
  (cadr expr))

(define (assignment-value expr)
  (caddr expr))

; Definitions are of two kinds - (define variable value_Expr) or 
; (define (variable params) expr_seq)
(define (definition? expr)
  (tagged-list? expr 'define))

(define (definition-variable expr)
  (if (symbol? (cadr expr))
    (cadr expr)
    (caadr expr)))

(define (definition-value expr)
  (if (symbol? (cadr expr))
    (caddr expr)
    ; (make-lambda param_list expr_list) returns a lambda expression.
    (make-lambda (cdadr expr)
                 (cddr expr))))

; lambda expressions
(define (lambda? expr)
  (tagged-list? expr 'lambda))

(define (lambda-parameters expr)
  (cadr expr))

; lambda body is a list of expressions.
(define (lambda-body expr)
  (cddr expr))

; lambda body is an expr_list.
(define (make-lambda params body)
  (cons 'lambda (cons params body)))

; If expression - if?, if-predicate, if-consequent, if-alternative
; (if predicate consequent alternative?)
(define (if? expr)
  (tagged-list? expr 'if))

(define (if-predicate expr)
  (cadr expr))

(define (if-consequent expr)
  (caddr expr))

; alternative is optional. If not present, we return 'false. We accept more than
; one expression for alternative, though all but the first are ignored.
(define (if-alternative expr)
  (let ([alts (cdddr expr)])
    (if (null? alts)
      'false
      (car alts))))

; Constructor for if expressions. Useful when transforming cond expressions to
; nested ifs.
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; Begin is for sequentially evaluating its subexpressions. 
; (begin <expr_list>) 
; We allow for empty expression lists.
(define (begin? expr)
  (tagged-list? expr 'begin))

(define (begin-actions expr)
  (cdr expr))

(define (empty-expr-seq? expr_seq)
  (null? expr_seq))

(define (last-exp? expr_seq)
  (null? (cdr expr_seq)))

(define (first-exp expr_seq)
  (car expr_seq))

(define (rest-exps expr_seq)
  (cdr expr_seq))

; We also define a constructor for transforming an expression list into a
; single begin expression. This is a bit wasteful if expr_seq only has one
; expression, but we'll ignore this inefficiency for now!
(define (sequence->exp expr_seq)
  (cons 'begin expr_seq))

; Procedure application
(define (application? expr)
  (pair? expr))

(define (operator expr)
  (car expr))

(define (operands expr)
  (cdr expr))

(define (no-operands? ops)
  (null? ops))

(define (first-operand ops)
  (car ops))

(define (rest-operands ops)
  (cdr ops))

; A cond expression can be rewritten as a nested if expression. This is a 
; syntactic transformation and is implemented as the `cond->if` procedure.
(define (cond? expr)
  (tagged-list? expr 'cond))

(define (cond-clauses expr)
  (cdr expr))

(define (cond-predicate clause)
  (car clause))

; Each clause can actually have a list of action expressions. Here we transform
; it into a single expression using the `sequence->exp` routine.
(define (cond-actions clause)
  (sequence->exp (cdr clause)))

(define (cond-else-clause? clause)
  (equal? (cond-predicate clause) 'else))

(define (cond-=>-clause? clause)
  (equal? (cadr clause) '=>))

(define (cond-=>-procedure clause)
  (caddr clause))

; We can now define cond->if.
(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))

(define (expand-clauses clauses)
  (cond
    ((null? clauses) 'false)
    (else
      (let ([first (car clauses)]
            [rest (cdr clauses)])
        ; Either if (first-predicate) then first-procedure(first-predicate)
        ;                             else (expand-clauses rest)
        ; Or if (first-predicate) then first-action 
        ;                         else (expand-clauses rest)
        ; (let [fp first-predicate-val] ())
        ; Care must be taken that predicate expression is evaluated only once.
        (if (cond-else-clause? first)
            (if (null? rest)
              (cond-actions first)
              (error "Else must be the last clause in a cond expression."))
            (if (cond-=>-clause? first)
              ; We transform a => clause into a let form with predicate expression
              ; evaluated once and bound to a temporary variable.
                (list 'let ((list 'fp (cond-predicate first)))
                        (make-if 'fp (list (cond-=>-procedure first) 'fp)
                                (expand-clauses rest)))
                (make-if (cond-predicate first)
                        (cond-actions first)
                        (expand-clauses rest))))))))

; Ex 4.2
; a) We must first test for other kinds of expressions(e.g., special forms) before 
; testing for procedure applications. Only when a parenthesized expression fails 
; to match other types, then we consider it as a procedure application. If we 
; instead first call `application?` in our `eval-meta`, then every parenthesized
; expression will match and everthing is treated as a function call.

; b) We can implement `call` as a new special form and syntactically transform 
; it into a procedure application. Alternatively, `call` can also be
; implemented as a procedure (because we use applicative order in our language).
; Here, we treat `call` as a special form.

; call -> (call func . args)
(define (call? expr)
  (tagged-list? expr 'call))

(define (call-procedure expr)
  (cadr expr))

(define (call-arguments expr)
  (cddr expr))

; (call func . args) is transformed to (func args)
(define (call->application expr)
  (cdr expr))

; We've also modified `eval-meta` to test for call forms before testing for others.

; Ex 4.3
; We use a dispatch table which is indexed by the types of special forms. Each
; entry in the table consists of an evaluation procedure for special forms.
; We won't do this here. Basic gist of this exercise is that adding a new 
; special form becomes easy as we won't have to modify `eval-meta`.

; Ex 4.4
; We implement evaluators for `and` and `or` forms.
(define (and? expr)
  (tagged-list? expr 'and))

(define (and-clauses expr)
  (cdr expr))

(define (eval-and expr env)
  (define (iter clauses)
    (cond
      ((null? clauses) #t)
      ((null? (cdr clauses)) (eval-meta (car clauses) env))
      (else (let ([first (eval-meta (car clauses) env)])
        (if (true? first)
          (iter (cdr clauses))
          #f)))))
  
  (iter (and-clauses expr)))

(define (or? expr)
  (tagged-list? expr 'or))

(define (or-clauses expr)
  (cdr expr))

(define (eval-or expr env)
  (define (iter clauses)
    (cond
      ((null? clauses) #f)
      ((null? (cdr clauses)) (eval-meta (car clauses) env))
      (else (let ([first (eval-meta (car clauses) env)])
        (if (not (true? first))
          (iter (cdr clauses))
          first)))))
  
  (iter (or-clauses expr)))

; Alternatively, we can syntactically transform and/or expressions into cond or
; nested if expressions. We're not gonna do it here.

; Ex 4.5
; cond clauses can have an alternate syntax of (predicate => procedure). See
; `cond->if` and `expand-clauses` above.

; Ex 4.6
; Let form can be transformed into an equivalent lambda form.
(define (let? expr)
  (tagged-list? expr 'let))

(define (let-parameters expr)
  (map car (cadr expr)))

(define (let-binding-exprs expr)
  (map cadr (cadr expr)))

; let-body is actually a list of expressions.
(define (let-body expr)
  (cddr expr))

; Creates an empty let block.
(define (empty-let expr_list)
  (cons 'let (cons '() expr_list)))

; (let ((a ex1))* body), where body is actually an expr_list.
; ((lambda (param_list) let_body) binding_body_list)
; Named let expressions are transformed into a sequence of 2 expressions within
; an empty let block.
(define (let-combination expr)
  (if (variable? (cadr expr))
    (let* ([new-expr (let-combination (cons (car expr) (cddr expr)))]
           [named-proc (car new-expr)]
           [arguments (cdr new-expr)])
      (empty-let
        (list
          (list 'define (cadr expr) named-proc)
          (cons (cadr expr) arguments))))
    (cons (make-lambda (let-parameters expr) (let-body expr))
            (let-binding-exprs expr))))

; Let's define true? and false?. Here `value` is the result of evaluating a 
; language expression. 
;
; `value` here is either a primitive(symbol/constant) or the result of 
; evaluating an expression of the language. We use the primitive symbol 'false
; to denote falsity and because our `eval-meta` does sometimes return #f, we
; consider both the symbols '#f and 'false as falsity. Everything else is true
; in a boolean context.
(define (false? value)
  (or (equal? value #f) (equal? value 'false)))

; Anything that's not false.
(define (true? value)
  (not (false? value)))

; Ex 4.7
; let* form
(define (let*? expr)
  (tagged-list? expr 'let*))

(define (let*-nested-lets expr)

  (define (recur params binding_exprs)
    (if (null? params)
      (sequence->exp (let-body expr))
      (cons (make-lambda (list (car params))
                         (list (recur (cdr params) (cdr binding_exprs))))
            (list (car binding_exprs)))))

  (recur (let-parameters expr) (let-binding-exprs expr)))

; We can simply transform let* into a nested let expression and `eval-meta` will
; work. This is because `eval-meta` will recursively call itself with the nested
; let expression and it will then be transformed into a nested lambda form.
; This way, special forms defined on top of other special forms will eventually
; get transformed all the way down to core syntactic forms.

; Essentially the authors are asking here about the transformation step - 
; where newly defined special syntactic forms are rewritten into either
; existing special forms or core forms. Because one can define new special
; forms in terms of existing forms and yet new forms on top of those - in
; general to evaluate an expression, it needs to go through a series of trans-
; formation steps all the way down to core syntactic forms. This is either 
; achieved as in `eval-meta` above recursively calling itself or by introducing an 
; explicit `transform` step before calling `eval-meta`.

; Ex 4.8
; See `let-combination` implementation above. Named let expressions are transf-
; ormed into a begin sequence of two expressions.

; Ex 4.9
; Here we'll implement for construct.
; for will look like 
;           (for initializer_list test_expr body_expr_list)
; e.g.      (for ([a 0] [b 99]) (< a b) (set! a (+ a 1)) (set! b (- b 1)))
; 

(define (for? expr)
  (tagged-list? expr 'for))

(define (for-initializers expr)
  (cadr expr))

(define (for-test expr)
  (caddr expr))

; for body is actually an expression list.
(define (for-body expr)
  (cdddr expr))

(define (for-combination expr)
  (list 'let 'forcall (for-initializers expr)
        (list 'if (for-test expr)
                 (sequence->exp (append (for-body expr)
                                        (list (cons 'forcall
                                                    (map car (for-initializers expr))))))
                 )))

; If we keep adding new special forms like for and while, it's useful to have
; a transformation step before eval.

; Ex 4.10
; The advantage of isolating syntax details from `eval-meta` by using syntax
; procedures lets us change the syntax of language under evaluation without 
; changing the `eval-meta` procedure. All we need to change is the reader and 
; syntax procedures.

; We construct a compound procedure from a parameter list, procedure body and 
; an environment.
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define (compound-procedure? expr)
  (tagged-list? expr 'procedure))

(define (procedure-parameters expr)
  (cadr expr))

; A list of expressions actually.
(define (procedure-body expr)
  (caddr expr))

(define (procedure-environment expr)
  (cadddr expr))

; We think of an environment as an object containing a frame and a pointer to 
; its enclosing environment. All except the empty environment point to valid
; enclosing environments.
(define (first-frame env)
  (car env))

(define (enclosing-environment env)
  (cdr env))

; The empty environment, upon which all other environments are built.
(let ()
  (define frame '())
  (define enclosing '())
  (set! the-empty-environment (cons frame enclosing) ))

; Extend an environment with a new set of variable bindings.
(define (extend-environment variables values base_env)
  (if (= (length variables) (length values))
    (cons (make-frame variables values) base_env)
    (error "Variables and values must have same length - extend-environment")))

(define (make-frame vars vals)
  (map cons vars vals))

; Look up a variable's value in the given environment. Raise an error if 
; no such binding.
(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
    (error "Unbound variable: " var)
    (let ([match (assoc var (first-frame env))])
      (if match
        (if (eq? (cdr match) '*unassigned*)
          (error "Variable unassigned:" var)
          (cdr match))
        (lookup-variable-value var (enclosing-environment env))))))

; Defines a variable in the first frame of the environment. It rebinds the var
; to the value if this variable is already bound.
(define (define-variable! var val env)
  (let* ([frame (first-frame env)]
         [binding (assoc var frame)])
    (if binding
      (set-cdr! binding val)
      (set-car! env (cons (cons var val) frame)))))

; Rebinds an existing variable to a new value. This raises an error in case 
; the given variable is not bound. This is different behavior from the `set!`
; primitive of Scheme.
(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
    (error "Unbound variable: " var)
    (let ([binding (assoc var (first-frame env))])
      (if binding 
        (set-cdr! binding val)
        (set-variable-value! var val (enclosing-environment env))))))

; Ex 4.11
; Each frame as a list of (name, value) pairs. This is the representation we
; used.

; Ex 4.12
; Not clear what the authors' intentions are. The procedures
; `define-variable!`, `lookup-variable-value` and `set-variable-value!` share a
; common structure - they apply appropriate procedures on frames while
; traversing the environment. These can thus be generalized into a single 
; abstract procedure.

; Ex 4.13
; If we allow make-unbound! to erase binding from any frame in an environment, 
; our programs become harder to understand - they won't be lexically scoped 
; any more. Instead, they'll have a weird form of dynamic scope. We only allow
; unbinding in the first frame.
(define (make-unbound! var env)

  (define (except frame var)
    (filter (lambda (pr) (not (equal? var (car pr)))) frame))

  (if (not (eq? env the-empty-environment))
    (let ([frame (first-frame env)])
      (set-car! env (except frame var)))))

; Let's now setup a global environment. The global environment consists of 
; bindings to primitive procedures and to boolean constants true and false.
(define (setup-global-environment)

  (define global-env
    (extend-environment
      (primitive-procedure-names)
      (primitive-procedure-implementations)
      the-empty-environment))
  
  global-env)

(define (primitive-procedure-names)
    (list 'list 'true 'false '= 'cons 'car 'cdr 'null? '+ '- '* '/ 'exp))

(define (primitive-procedure-implementations)
    (list list #t #f = cons car cdr null? + - * / exp))

(define the-global-environment (setup-global-environment))

(define (primitive-procedure? expr)
    (memq expr (primitive-procedure-implementations)))

(define (primitive-implementation proc_name)
  (lookup-variable-value proc_name the-global-environment))

; To actually apply primitive procedures in our language, we rely on Scheme's 
; `apply`
(define (apply-primitive-procedure proc args)
  (apply proc args))

; We now implement a read-eval-print-loop for our langauge. In this, we rely on
; underlying Scheme's `read` procedure which reads text and returns s-expressions 
; We consider the symbol 'quit as a signal from user to exit the loop. 
(define (repl)
  (let ([expr (read)])
    (if (not (eq? expr 'quit))
      (let ([val (eval-meta expr the-global-environment)])
        (print val)
        (repl))
      'done)))

; Ex 4.14
; Difference is that the system map expects scheme procedures while the Eva's 
; map expects language expressions (which are interpreted as procedures in that
; language during evaluation)

; Ex 4.15
; The famous halting problem. Can we determine whether any given procedure p
; halts (returns in finite time, without error) on any given argument a?
;
; There is a beautiful proof by logical contradiction that such a determination 
; procedure doesn't exist.
;
; Suppose such a procedure `halts?` exists. (this procedure always halts). Now
; we devise a procedure named `try`.
;
; The `try` procedure does the following. It accepts a procedure p, it first 
; checks whether p halts on p (does so by calling `halts?`). If `halts?` says p will
; halt on p, then try calls `run-forever` (which runs forever and thus this
; `try` invocation doesn't halt). Otherwise, it simply returns 'halted (this
; `try` invocation halts)
;
; Now let's consider what happens when `try` is called on itself. Suppose
; `halts?` says `try` will halt on `try`, but then our `try` will call
; `run-forever` and thus it won't halt contradicting the prediction of 
; `halts?`. Now if `halts?` had said `try` won't halt, our `try` would simply
; have returned 'halted. It thus halts again contradicting `halts?`'s
; prediction. Thus either way, we've arrived at a contradiction. Our initial 
; assumption that a procedure like `halts?` exists is wrong.

; Ex 4.16
; At the outset, I don't quite get what's the benefit of having truly simulta-
; neous scope for internal definitions. As long as the value expressions of 
; defines dont depend on each other, shouldn't it be okay? 

; a) See `lookup-variable-value` above.

; Let's now define a `make-let` procedure which takes a list of variables, 
; a list of value expressions, a list of body expressions and constructs a let
; expression.
(define (make-let var-list val-expr-list body)
  (cons 'let (cons (map list var-list val-expr-list) body)))

; procedure_body is actually an expression list. We'll only scan out variable
; defines (not procedure defines) here.
(define (scan-out-defines procedure_body)

  (define (var-define? expr)
    (and (definition? expr)
         (variable? (cadr expr))))

  (define var-defines (filter var-define? procedure_body))
  (define other-exprs (filter 
                        (lambda (ex) (not (var-define? ex))) 
                        procedure_body))

  (define variables (map cadr var-defines))

  (define value-expressions (map caddr var-defines))

  (define assignments (map (lambda (var val-expr) (list 'set! var val-expr))
                           variables 
                           value-expressions))

  (define new-body (append assignments other-exprs))

  (define let-initials (map (lambda (var) ''*unassigned*) variables))

  ; In our `eval-meta`, we transform let expressions into equivalent lambda forms.
  ; Here, we're doing the opposite - Care must be taken to ensure we don't 
  ; fall into an infinite cycle of let to lambda to let ... transformations.
  (if (null? var-defines)
    procedure_body 
    (list (make-let variables let-initials new-body))))

; c) `scan-out-defines` is now called inside make-procedure when constructing new 
; procedures. `make-procedure` is the better place to call scan-out-defines as 
; it's a one time thing. In contrast, `procedure-body` is called everytime the
; procedure is executed.

; Ex 4.17
; Extra frame appears because we've introduced a new let block with all define
; variables initialized to '*unassigned*. The behavior of a program remains
; the same as before in correct programs - this is because these programs don't
; actually use other defined variables in evaluating value expressions. Once
; all defines are complete, these all become part of the top frame.

; Ex 4.19
; It looks like both MIT Scheme and Chicken Scheme raise an error in this case.
; They first bind 'a' & 'b' to unassigned and only then they try to compute+set
; their values. Because + will raise an error on non-numeric arguments, both 
; the scheme implementations therefore raise error. In general, to implement
; Eva's interpretation, we'd have to figure out dependencies among various
; defines - e.g., in this case b's definition depends on a's and reorder the 
; defines so that later defines depend on earlier ones. If there are circular
; depedencies, this is impossible to do so. To determine what other defines 
; a particular definition depends upon, we can parse its value expression and
; see what symbols are present in the body.

; Ex 4.20
; Rewrite letrec as a let. letrec allows for mutually recursive definitions, 
; by having its value expressions in the same scope as its variables.
; In let expressions, the scope of value expressions is outer to the scope of 
; let's variables. This rewriting should place the value expressions and the
; new vars in the same scope. This is infact quite similar to `scan-out-defines` 
; above.
(define (letrec? expr)
  (tagged-list? expr 'letrec))

; We can use let selectors `let-parameters`, `let-body` and `let-binding-exprs`
; and the `make-let` constructor here.
(define (letrec->let expr)
  (define vars (let-parameters expr))
  (define val-exprs (let-binding-exprs expr))
  (define initializers (map (lambda (var) ''*unassigned*) vars))

  (define setters (map (lambda (var val-expr) (list 'set! var val-expr)) 
                       vars val-exprs))

  (define new-body (append setters (let-body expr)))

  (make-let vars initializers new-body))

; Louis is wrong here cause simply placing internal defines inside a let wont
; allow for mutually recursive definitions. Letrec is required to accomplish
; this.

; Ex 4.21
; This definition for factorial/fibonacci works because we're passing the
; function(name) as one of the arguments which is required inside the function
; body. This is not as elegant as using letrec (a language level construct that
; allows for truly simultaneous scope)
(print ((lambda (n)
   ((lambda (fibo)
      (fibo fibo n))
    (lambda (ft k)
      (if (< k 2)
          k
          (+ (ft ft (- k 2)) (ft ft (- k 1)))))))
 20))

(define (f-even x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) #t (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) #f (ev? ev? od? (- n 1))))))

; Ex 4.22
; Let's ignore it for now.

; Ex 4.23
; For singleton lists, Alyssa's version calls `car` and `cdr` everytime a 
; sequence is being evaluated, which are avoided in text version.
; For 2 element lists, Alyssa's versions makes 2 `car` calls and 2 `cdr` calls.
; Text version simply accesses 2 references embedded in a `sequentially`
; procedure instead.

; Ex 4.24
; Ignoring it for now.

; Section 4.2
; In which we modify our Scheme evaluator to support lazy evaluation.

; Ex 4.25
; In regular applicative-order Scheme, this factorial call would go into
; infinite recursion.

; Ex 4.26
; unless form structure is (unless condition usual exceptional)
; Strictly speaking, unless can be implemented as a special form even in eager
; evaluation languages.
; If `unless` is implemented as a procedure under lazy evaluation, it can be 
; used in higher order functions (passed as an input, returned as an output). 
(define (unless? expr)
  (tagged-list? expr 'unless))

(define (unless-condition expr)
  (cadr expr))

(define (unless-usual expr)
  (caddr expr))

(define (unless-exceptional expr)
  (cadddr expr))

; (unless condition usual exceptional) -> (cond (condition exceptional) (else usual))
(define (unless->cond expr)
  (list 'cond
        (list (unless-condition expr) (unless-exceptional expr))
        (list 'else (unless-usual expr))))

; And then in our `eval-meta`, we must add a clause for unless forms as well.

; Let's now modify our evaluator to implement lazy evaluation.
(define (actual-value expr env)
  (force-it (eval-meta expr env)))

; thunks and evaluated-thunks
; Thunks are tagged lists of expression and environment.
(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-expr thunk)
  (cadr thunk))

(define (thunk-environment thunk)
  (caddr thunk))

(define (delay-it expr env)
  (list 'thunk expr env))

; Memoize the computed value.
(define (force-it obj)
  (cond 
    ((thunk? obj)
        (let ([value (actual-value (thunk-expr obj) 
                                   (thunk-environment obj))])
          (set-car! obj 'evaluated-thunk)
          (set-cdr! obj (cons value '()))
          value))
    ((evaluated-thunk? obj) (thunk-value obj))
    (else obj)))

; Evaluated thunks are simply computed values tagged with 'evaluated-thunk
; (list 'evaluated-thunk value)
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

; Ex 4.27
; After (define w (id (id 10)), count is 1 and w is (thunk (id 10)).
; To print w, repl forces its evaluation. count is 2 and w is (evaluated-thunk
; 10)

; Ex 4.28
; Why the need to force evaluating the operator expression while we can defer
; evaluating operand expressions?
; If we have an expression like (f arg1 arg2) where f is a procedure name and
; let's say this expression's value is needed now. If the evaluator even defers
; evaluating 'f' (looking up the value of f), then there is no way the
; evaluation can even get started. 
