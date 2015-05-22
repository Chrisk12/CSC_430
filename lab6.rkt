#lang plai-typed
(require plai-typed/s-exp-match)
(require (typed-in racket
                   [random : (number -> number)]
                   [random-seed : (number -> void)]))
; Defines Exprc
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [booleanC (b : boolean)]
  [appC (fun : ExprC) (arg : (listof ExprC))]
  [if (t : ExprC) (iff : ExprC) (ffi : ExprC)]
  [binop (sym : symbol) (l : ExprC) (r : ExprC)]
  [lamC (arg : (listof symbol)) (body : ExprC)])

; Defines Values
(define-type Value
  [numV (n : number)]
  [booleanV (b : boolean)]
  [closV (arg : (listof symbol)) (body : ExprC) (env : Env)])


; Defines the Binding Type which takes a symbol
; maps it to a number
(define-type Binding
  [bind (name : symbol) (val : Value)])

; An Alias for a list of bindings
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; Defines the Interpreter. Takes in an ExprC and a list of functions
; and then evalutes the ExperC recursively and returns a number.
(define (eval [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
    [numC (n) (numV n)]
    [booleanC (b) (booleanV b)]
    [idC (n) (lookup n env)]
    [appC (f a) (local ([define f-value (eval f env)])
                  (eval (closV-body f-value)
                        (let ([newEnv (get-list-binding (closV-arg f-value)
                                          (get-values-eval a env)
                                          (closV-env f-value))]) newEnv)))]
    [lamC (param body) (closV param body env)]
    [binop (s l r) ((get-binop s) (eval l env) (eval r env))]
    [if (t iff ffi) (cond
                      [(not (booleanV? (eval t env)))
                       (error 'if "Not a boolean expression")]
                      [(booleanV-b (eval t env)) (eval iff env)]
                      [else (eval ffi env)])]))

; Takes a list of exprc and returns a list of values. To be used in eval.
(define (get-values-eval [exprs : (listof ExprC)]
                         [env : Env]) : (listof Value)
  (cond [(empty? exprs) empty]
        [else (cons (eval (first exprs) env) (get-values-eval (rest exprs) env))]))
  

; Creates a list of binding from teh fdC-Arg and teh argument to the function
(define (get-list-binding [syms : (listof symbol)]
                          [a : (listof Value)]
                          [env : Env]) : (listof Binding)
  (cond [(empty? a) env]
        [else (cons (bind (first syms) (first a)) (get-list-binding (rest syms) (rest a) env))]))

;(test (get-list-binding (list 'a 'b 'c 'd) (numC 1) (numC 2) (numC 3)))
; Looks for a symbol in an env and returns the number that is
; represented by the symbol.
(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

; Checks to see if a symbol is a binary operator and returns true if it is
(define (check-if-binop [s : symbol]) : boolean
  (cond [(equal? s '+) true]
        [(equal? s '*) true]
        [(equal? s '-) true]
        [(equal? s '/) true]
        [(equal? s '<=) true]
        [(equal? s 'eq?) true]
        [else false]))

; Checks to see if a symbol is a reserved symbol and returns true if it is
(define (check-if-reserved-symbol [s : symbol]) : boolean
  (cond [(equal? s '+) true]
        [(equal? s '*) true]
        [(equal? s '-) true]
        [(equal? s '/) true]
        [(equal? s '<=) true]
        [(equal? s 'if) true]
        [(equal? s 'eq?) true]
        [(equal? s 'true) true]
        [(equal? s 'false) true]
        [(equal? s 'with) true]
        [(equal? s 'fn) true]
        [else false]))

; Checks to see if a symbol is a reserved symbol and returns true if it is
(define (check-if-reserved-symbol-excluding-true-false [s : symbol]) : boolean
  (cond [(equal? s '+) true]
        [(equal? s '*) true]
        [(equal? s '-) true]
        [(equal? s '/) true]
        [(equal? s '<=) true]
        [(equal? s 'if) true]
        [(equal? s 'eq?) true]
        [(equal? s 'with) true]
        [(equal? s 'fn) true]
        [else false]))


; Gets the function associated with the binary operator and returns that
; operator
(define (get-binop [s : symbol]) : (Value Value -> Value)
  (cond [(equal? s '+) num+]
        [(equal? s '*) num*]
        [(equal? s '-) num-]
        [(equal? s '/) num/]
        [(equal? s '<=) num<=]
        [(equal? s 'eq?) numeq?]
        [else (error 'get-binop "NOT A BINOP")]))

; Takes in two values and adds them together
(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "one argument was not a number")]))

; Takes in two values and mult them together
(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "one argument was not a number")]))

; Takes in two values and / them together
(define (num/ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? r) (= (numV-n r) 0)) (error 'num/ "Division by zero")]
    [(and (numV? l) (numV? r))
     (numV (/ (numV-n l) (numV-n r)))]
    [else
     (error 'num/ "one argument was not a number")]))

; Takes in two values and - them together
(define (num- [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (- (numV-n l) (numV-n r)))]
    [else
     (error 'num- "one argument was not a number")]))

; Takes in two values and <= them together
(define (num<= [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (booleanV (<= (numV-n l) (numV-n r)))]
    [else
     (error 'num- "one argument was not a number")]))

; Takes in two values and eq? them together
(define (numeq? [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (booleanV (eq? (numV-n l) (numV-n r)))]
    [(and (booleanV? l) (booleanV? r))
     (booleanV (eq? (booleanV-b l) (booleanV-b r)))]
    [else (booleanV false)]))

; Searches through a list of symbols to see if a symbol s is in the list.
; returns true if it is, false otherwise.
(define (check-in-list [s : symbol] [lst : (listof symbol)]) : boolean
  (cond [(empty? lst) false]
        [(symbol=? s (first lst)) true]
        [else (check-in-list s (rest lst))]))

; defines a list of symbols
(define sym-list (list 'a 'b 'c 'd 'e 'f 'g 'h))

; defines a list of expression
(define exp-list (list 'binop
                       'lamC
                       'if
                       'appC))

; defines a list of expression
(define base-list (list 'numC
                       'booleanC
                       'idC
                       'true
                       'false))

; 3.6 Appends two list together
(define (append-list [l1 : (listof 'a)] [l2 : (listof 'a)]) : 'a
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else (cons (first l1) (append-list (rest l1) l2))]))

(define (randomSymbol [s : (listof symbol)]) : symbol
  (get-single-element s (random (length s))))

(define (get-single-element [s : (listof symbol)] [n : number]) : symbol
  (cond [(eq? 0 n) (first s)]
        [else (get-single-element (rest s) (sub1 n))]))


(randomSymbol sym-list)
(randomSymbol sym-list)
(randomSymbol sym-list)
(randomSymbol sym-list)

; Returns finds a random closed term
(define (randomClosedTerm [ss : (listof symbol)]
                          [bound : (listof symbol)]) : ExprC
  (let ([s (get-single-element ss (random (length ss)))]
        [rands (random 1)])
  (cond [(eq? s 'numC) (numC (random 100))]
        [(eq? s 'booleanC) (cond [(eq? rands 0) (booleanC true)]
                                 [else (booleanC false)])]
        [(and (not (empty? bound)) (eq? s 'idC)) (idC (randomSymbol bound))]
        [(eq? s 'true) (booleanC true)]
        [else (booleanC false)])))

(randomClosedTerm base-list sym-list)
(randomClosedTerm base-list sym-list)
(randomClosedTerm base-list empty)
(randomClosedTerm base-list empty)
 
; Finds a random term
(define (randomTerm [maxDepth : number] [bound : (listof symbol)]) : ExprC
  (cond [(eq? 0 maxDepth) (randomClosedTerm base-list bound)]
        [else (let ([rand (random (length exp-list))])
           (cond [(eq? rand 0) (binop (binops-syms)
                                      (randomTerm (sub1 maxDepth) bound)
                                      (randomTerm (sub1 maxDepth) bound))]
                 [(eq? rand 1) 
                  (let ([symbols (get-lam-symbols sym-list)])
                    (lamC symbols (randomTerm (sub1 maxDepth) 
                                              (append-list bound symbols))))]
                 [(eq? rand 2) (if (randomTerm (sub1 maxDepth) bound)
                                   (randomTerm (sub1 maxDepth) bound)
                                   (randomTerm (sub1 maxDepth) bound))]
                 [(eq? rand 3) (appC (randomTerm (sub1 maxDepth) bound) 
                                     (get-appc-expr maxDepth bound))]))]))

; get symbols to be used in lamda call
(define (get-lam-symbols [syms : (listof symbol)]) : (listof symbol)
  (let ([rand (random 3)])
    (cond [(eq? 0 rand) (list (randomSymbol syms))]
          [(eq? 1 rand) (list (randomSymbol syms) (randomSymbol syms))]
          [(eq? 2 rand) (list (randomSymbol syms)
                              (randomSymbol syms)
                              (randomSymbol syms))])))
; returns a random list of expressions for appc
(define (get-appc-expr [maxDepth : number]
                       [bound : (listof symbol)]) : (listof ExprC)
  (let ([rand (random 3)])
    (cond [(eq? 0 rand) (list (randomTerm (sub1 maxDepth) bound))]
          [(eq? 1 rand) (list (randomTerm (sub1 maxDepth) bound)
                              (randomTerm (sub1 maxDepth) bound))]
          [(eq? 2 rand) (list (randomTerm (sub1 maxDepth) bound)
                              (randomTerm (sub1 maxDepth) bound)
                              (randomTerm (sub1 maxDepth) bound))])))
 
; defining symbols list to be used for binop
(define syms-list (list '+ '- '* '/))
; call to get  a random symbol
(define (binops-syms)
  (get-single-element syms-list (random (length syms-list))))

(randomTerm (random 3) sym-list)
(randomTerm (random 3) sym-list)
(randomTerm (random 3) sym-list)
(randomTerm (random 3) empty)
(randomTerm (random 3) empty)

; runs a number of trials and returns the results
(define (runTrials [numTrials : number] [depth : number]) : number
  (/ (run-trials numTrials 0 depth) numTrials))

; help function for run trials which does the counting.
(define (run-trials [numTrials : number] [success : number] [depth : number]) : number
  (cond [(eq? numTrials 0) success]
        [else (let [(truth (try (begin (eval (randomTerm depth empty) empty) true)
                                (lambda () false)))]
                (cond [truth (run-trials (sub1 numTrials) (add1 success) depth)]
                      [else (run-trials (sub1 numTrials) success depth)]))]))

 
(runTrials 50 3)
