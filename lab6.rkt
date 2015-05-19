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
(define exp-list (list `{binop +}
                       `{binop /}
                       `{binop *}
                       `{binop -}
                       `{lamC}
                       `{if}
                       `{appC}))

; defines a list of expression
(define exp-base-list (list `{numC}
                       `{booleanC}
                       `{idC}
                       `{true}
                       `{false}))


(define (randomSymbol [s : (listof symbol)]) : symbol
  (get-single-element s (modulo (random (length s)) (length s))))

; helper function that returns the randon symbol to randomSymbol
(define (get-single-element [s : (listof 'a)] [n : number]) : 'a
  (cond [(eq? n 0) (first s)]
        [else (get-single-element (rest s) (sub1 n))]))

; helper function that returns the randon symbol to randomSymbol
(define (get-element [s : (listof 'a)] 
                     [n : number]
                     [maxDepth : number]
                     [syms : (listof symbol)]) : 'a
  (cond [(eq? n 0) (create-s-expr (s-exp->list (first s)) maxDepth syms)]
        [else (get-element (rest s) (sub1 n) maxDepth syms)]))

; Joins pieces into an s expression
(define (create-s-expr [s : (listof s-expression)] 
                       [maxDepth : number]
                       [syms : (listof symbol)]) : ExprC
  (let ([chooser (random 3)])
    (cond
      [(and (<= 2 chooser)(and (not (eq? 0 maxDepth)) (s-exp-match? `if (first s))))
       (if (booleanC true) 
           (get-element exp-list (modulo (random (length exp-list)) 
                                         (length exp-list))
                        (sub1 maxDepth) syms)
           (get-element exp-list (modulo (random (length exp-list)) 
                                         (length exp-list))
                        (sub1 maxDepth) syms))]
      [(and (not (eq? 0 maxDepth)) (s-exp-match? `if (first s)))
       (if (booleanC false) 
               (get-element exp-list (modulo (random (length exp-list)) 
                                             (length exp-list))
                            (sub1 maxDepth) syms)
               (get-element exp-list (modulo (random (length exp-list)) 
                                             (length exp-list))
                            (sub1 maxDepth)syms))]
          [(and (not (eq? 0 maxDepth)) (s-exp-match? `binop (first s)))
           (binop (s-exp->symbol (second s)) 
                  (get-element exp-list (modulo (random (length exp-list)) 
                                                (length exp-list)) 
                               (sub1 maxDepth) syms)
                  (get-element exp-list (modulo (random (length exp-list)) 
                                                (length exp-list)) 
                               (sub1 maxDepth) syms))]
          [(and (not (eq? 0 maxDepth)) (s-exp-match? `appC (first s)))
           (appC  (get-element exp-list (modulo (random (length exp-list)) 
                                                (length exp-list)) 
                               (sub1 maxDepth) syms) 
           (random-symbols syms))]
          [(and (not (eq? 0 maxDepth)) (s-exp-match? `(lamC) (first s)))
           (lamC (random-symbols-lam syms)  (get-element exp-list 
                                                       (modulo 
                                                        (random 
                                                         (length exp-list)) 
                                                        (length exp-list)) 
                                                       (sub1 maxDepth) syms))]
          [(or (eq? chooser 0)  (s-exp-match? `numC (first s)))
           (numC chooser)]
          [(or (eq? chooser 1)  (s-exp-match? `booleanC (first s)))
               (booleanC true)]
          [(or (eq? chooser 2)  (s-exp-match? `idC (first s)))
           (idC (randomSymbol syms))]
          [(or (eq? chooser 2) (s-exp-match? `true (first s)))
           (booleanC true)]
          [(or (eq? chooser 1) (s-exp-match? `false (first s)))
           (booleanC false)]
          [(or (eq? chooser 2)  (s-exp-match? `true (first s)))
           (booleanC true)]
          [(or (eq? chooser 1) (s-exp-match? `false (first s)))
           (booleanC false)]
          [(or (eq? chooser 2) (or (eq? 0 maxDepth) (s-exp-match? `true (first s))))
           (booleanC true)]
          [(or (eq? chooser 0) (or (eq? 0 maxDepth) (s-exp-match? `true (first s))))
           (booleanC false)])))

; gets a random symbols and turns them into sexpressions for appC
(define (random-symbols [syms : (listof symbol)]) : (listof ExprC)
  (let ([randomNumber (+ 1 (random 3))])
    (cond [(eq? randomNumber 1) (list (idC (randomSymbol syms)))]
          [(eq? randomNumber 2) (list (idC (randomSymbol syms))
                                      (idC  (randomSymbol syms)))]
          [(eq? randomNumber 3) (list (idC (randomSymbol syms))
                                      (idC  (randomSymbol syms))
                                      (idC  (randomSymbol syms)))])))

; gets a random symbols and turns them into sexpressions for appC
(define (random-symbols-lam [syms : (listof symbol)]) : (listof symbol)
  (let ([randomNumber (+ 1 (random 3))])
    (cond [(eq? randomNumber 1) (list (randomSymbol syms))]
          [(eq? randomNumber 2) (list (randomSymbol syms)
                                        (randomSymbol syms))]
          [(eq? randomNumber 3) (list  (randomSymbol syms)
                                        (randomSymbol syms)
                                        (randomSymbol syms))])))


; takes in a list of symbols and returns a random expression
(define (randomClosedTerm [s : (listof s-expression)] 
                          [syms : (listof symbol)]) : ExprC
  
  (get-element s (modulo (random (length s)) (length s)) 0 syms))


; takes in a list of symbols and returns a random expression
(define (randomTerm [s : (listof s-expression)] [maxDepth : number]) : ExprC
  (get-element s (modulo (random (length s)) (length s)) maxDepth sym-list))


; Defines run trials with sees how many trials work and dont work.
#;(define (runTrials [numTrials : number] [maxDepth : number]) : Value
  (cond [(eq? 0 maxDepth) (eval (randomTerm exp-list maxDepth) empty)]
        [try (eval (randomTerm exp-list) empty) (lambda ())]))



(randomSymbol sym-list)
(randomSymbol sym-list)
(randomSymbol sym-list)
(randomSymbol sym-list)

(randomClosedTerm exp-base-list (list 'a 'b 'f 'e))
(randomClosedTerm exp-base-list (list 'a 'b 'f 'e))
(randomClosedTerm exp-base-list (list 'a 'b 'f 'e))

(randomTerm exp-list (random 3))
(randomTerm exp-list (random 3))
(randomTerm exp-list (random 3))