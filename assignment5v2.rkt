#lang plai-typed
(require plai-typed/s-exp-match)
(print-only-errors true)

; Defines Exprc
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [stringC (s : string)]
  [booleanC (b : boolean)]
  [appC (fun : ExprC) (arg : (listof ExprC))]
  [if (t : ExprC) (iff : ExprC) (ffi : ExprC)]
  [binop (sym : symbol) (l : ExprC) (r : ExprC)]
  [lamC (arg : (listof symbol)) (body : ExprC)]
  [recC (name : symbol) (value : ExprC) (body : ExprC)])

; Defines Values
(define-type Value
  [numV (n : number)]
  [booleanV (b : boolean)]
  [closV (arg : (listof symbol)) (body : ExprC) (env : Env)]
  [stringV (s : string)])

 
; Defines the Binding Type which takes a symbol
; maps it to a number
(define-type Binding
  [bind (name : symbol) (val : (boxof Value))])

; An Alias for a list of bindings
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; defines mutable hashmap
(define my-hash (make-hash empty))
(hash-set! my-hash 'Object (list))

; Defines the Interpreter. Takes in an ExprC and a list of functions
; and then evalutes the ExperC recursively and returns a number.
(define (eval [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
    [numC (n) (numV n)]
    [stringC (s) (stringV s)]
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
                      [else (eval ffi env)])]
    [recC (name rhs body)
          (cond [(check-if-reserved-symbol name) (error 'recC "error")]
                [else 
                 (local [(define newbox (box (numV 2779287)))
                         (define newenv (extend-env (bind name newbox) env))
                         (define rhsval (eval rhs newenv))]
                   (begin (set-box! newbox rhsval)
                          (eval body newenv)))])]))

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
        [else (cons (bind (first syms) (box (first a))) (get-list-binding (rest syms) (rest a) env))]))


; Looks for a symbol in an env and returns the number that is
; represented by the symbol.
(define (lookup [for : symbol] [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (unbox (bind-val (first env)))]
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
    [(and (stringV? l) (stringV? r)) (booleanV (equal? (stringV-s l) (stringV-s r)))]
    [else (booleanV false)]))

; Searches through a list of symbols to see if a symbol s is in the list.
; returns true if it is, false otherwise.
(define (check-in-list [s : symbol] [lst : (listof symbol)]) : boolean
  (cond [(empty? lst) false]
        [(symbol=? s (first lst)) true]
        [else (check-in-list s (rest lst))]))

; 3.11 Defining a parser that takes an s expression and convert it to an ExperC.
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-string? s) (stringC (s-exp->string s))]
    [(s-exp-boolean? s) (booleanC (s-exp->boolean s))]
    [(s-exp-symbol? s)
     (cond
       [(check-if-boolean (s-exp->symbol s)) (get-boolean-value (s-exp->symbol s))]
       [(check-if-reserved-symbol (s-exp->symbol s)) (error 'parse "invalid input :(")]
       [else (idC (s-exp->symbol s))])]
    [(s-exp-match? '{class SYMBOL extends SYMBOL ANY ...} 
                   (first (s-exp->list s)))
     (parse (create-final-program (s-exp->list s)))]
        [(s-exp-match? '{send ANY SYMBOL ANY ...} s)
     (parse (create-send (second (s-exp->list s))
                         (third (s-exp->list s))
                         (rest 
                          (rest 
                           (rest (s-exp->list s))))))]
    [(s-exp-match? '{new SYMBOL ANY ...} s)
     (appC (parse (second (s-exp->list s))) 
           (get-list-of-expr-all (rest (rest (s-exp->list s)))))]
    [(s-exp-match? '{rec {SYMBOL = ANY} ANY} s)
     (recC (s-exp->symbol (first (s-exp->list (second (s-exp->list s))))) 
           (parse (third (s-exp->list (second (s-exp->list s))))) 
           (parse (third (s-exp->list s))))]
    
    [(s-exp-match? '{with {SYMBOL = ANY}... ANY} s)
     (let ([args (list->s-exp (rest(s-exp->list s)))]
           [params (get-params (list->s-exp (rest(s-exp->list s))) empty)]
           [truth (or (not-valid-symbols?
                       (get-params (list->s-exp (rest(s-exp->list s))) empty))
                      (not (list-has-no-dups
                            (get-params (list->s-exp          
                                         (rest(s-exp->list s))) empty))))])
       (cond [truth (error 'with "dups")]
             [else
              (appC (lamC
                     params
                     (get-values args))
                    (get-list-exprC args empty))]))]
    
    [(s-exp-match? '{if ANY ANY ANY} s) (if
                                         (parse (second (s-exp->list s)))
                                         (parse (third (s-exp->list s)))
                                         (parse (fourth (s-exp->list s))))]
    
    [(s-exp-match? '{fn {SYMBOL ...} ANY} s)
     (let ([truth (or (not-valid-symbols? (create-list s))
                      (not (list-has-no-dups (create-list s))))])
       (cond [truth (error 'fn "duplicate symbols")]
             [else
              (lamC
               (create-list s)
               (parse (third (s-exp->list s))))]))]
    [(s-exp-match? '{eq? ANY ANY} s)
     (cond [(or (and  (s-exp-symbol? (second (s-exp->list s)))
                      (check-if-reserved-symbol-excluding-true-false
                       (s-exp->symbol (second (s-exp->list s)))))
                (and (s-exp-symbol? (third (s-exp->list s)))
                     (check-if-reserved-symbol-excluding-true-false
                      (s-exp->symbol (third (s-exp->list s))))))
            (error 'eq? "bad symbol yo")]
           [else
            (binop (s-exp->symbol (first (s-exp->list s)))
                   (parse (second (s-exp->list s)))
                   (parse (third (s-exp->list s))))])]
    [(s-exp-match? '{SYMBOL ANY ANY} s)
     (cond [(test-of-operators s second) (error 'parse "invalid input :(")]
           [(test-of-operators s third) (error 'parse "invalid input :(")]
           [(check-if-binop(s-exp->symbol (first (s-exp->list s))))  
            (binop (s-exp->symbol (first (s-exp->list s)))
                   (parse (second (s-exp->list s)))
                   (parse (third (s-exp->list s))))]
           [else (create-appC s)])]
    [(s-exp-match? '{true} s) (booleanC true)]
    [(s-exp-match? '{false} s) (booleanC false)]
    [(s-exp-match? '{ANY ANY ...} s)
     (create-appC s)]))

; Gets a list of parse exprC
(define (get-list-of-expr-all [s : (listof s-expression)]) : (listof ExprC)
  (cond [(empty? s) empty]
        [else (cons (parse (first s)) (get-list-of-expr-all (rest s)))]))

; creates the class s expression to be parse... still need to figure out
; how parent class stuff works
(define (create-class-s-expression [class : symbol]
                                   [parent : symbol]
                                   [fields : (listof s-expression)] 
                                   [methodNames : (listof s-expression)] 
                                   [methodBodies : (listof s-expression)]
                                   [methodParams : (listof s-expression)]) : s-expression
  (let ([ifChain (build-if-chain methodNames methodBodies methodParams)]
        [set-class (set-class-map class fields)])
  `{fn {,@fields} {with {parent = {new ,(symbol->s-exp parent) ,@(some-v 
                                                 (get-class-map-args parent))}}
                        {fn {mesg} ,ifChain}}}))

; Build the if chain for an object
(define (build-if-chain [methodNames : (listof s-expression)]  
                        [methodBodies : (listof s-expression)]
                        [methodParams : (listof s-expression)]) : s-expression
  (cond [(eq? (length methodNames) 1) `{if {eq? mesg ,(string->s-exp 
                                                (symbol->string 
                                                 (s-exp->symbol (first methodNames))))}
                                     {fn {this ,@(s-exp->list (first methodParams))} 
                                               ,(first methodBodies)} 
                                     {parent mesg}}]
        [else `{if {eq? mesg ,(string->s-exp 
                               (symbol->string 
                                (s-exp->symbol (first methodNames))))}
                   {fn {this ,@(s-exp->list (first methodParams))} ,(first methodBodies)}
                   ,(build-if-chain (rest methodNames) (rest methodBodies) (rest methodParams))}]))

; gets the method names associated with a class
(define (get-method-details [s :  (listof s-expression)] 
                            [position : ((listof 'a) -> 'a)]) : (listof s-expression)
  (cond [(empty? s) empty]
        [else (cons (position (s-exp->list (first s))) 
                    (get-method-details (rest s) position))]))

; 3.6 Appends two list together
(define (append-list [l1 : (listof 'a)] [l2 : (listof 'a)]) : 'a
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else (cons (first l1) (append-list (rest l1) l2))]))

; gets the fields associated with the class
(define (get-fields [s :  (listof s-expression)] [parent : symbol]) : (listof s-expression)
  (let ([parentArgs (get-class-map-args parent)])
  (cond [(s-exp-match? '{ANY ...} (first s))
         (append-list (some-v parentArgs) (s-exp->list (first s)))]
        [else (error 'get-fields (to-string s))])))

; creates the send s expression
(define (create-send [p1 : s-expression] [sym : s-expression] [elist : (listof s-expression)]) : s-expression
  (local [(define a p1)
          (define b (string->s-exp (symbol->string (s-exp->symbol sym))))
          (define c elist)]
  `{with {obj = ,a} {{obj ,b} obj ,@c}}))

; Creates the recursive tree from section 6.2
(define (create-final-program [s : (listof s-expression)]) : s-expression
  (cond 
    [(empty? s) `{}]
    [(and 
          (not (s-exp-match? '{class SYMBOL extends SYMBOL ANY ...} (first s))) 
          (equal? (length s) 1)) (first s)]
        [(s-exp-match? '{class SYMBOL extends SYMBOL ANY ...} (first s))
         (let 
             ([class (s-exp->symbol (first (rest (s-exp->list (first s)))))]
              [parent (s-exp->symbol (first 
                                      (rest
                                       (rest
                                        (rest (s-exp->list (first s)))))))]
              [fields  (get-fields (rest 
                                    (rest 
                                     (rest 
                                      (rest 
                                       (s-exp->list (first s)))))) 
                                   (s-exp->symbol (first 
                                                   (rest
                                                    (rest
                                                     (rest (s-exp->list (first s))))))))]
              [methodNames (get-method-details (rest
                                                (rest
                                                 (rest
                                                  (rest
                                                   (rest (s-exp->list (first s))))))) 
                                               first)]
              [methodBodies (get-method-details (rest
                                                 (rest
                                                  (rest
                                                   (rest
                                                    (rest (s-exp->list (first s)))))))
                                                third)]
              [methodParams (get-method-details (rest
                                                 (rest
                                                  (rest
                                                   (rest
                                                    (rest (s-exp->list (first s)))))))
                                                second)])
           `{rec {Object = {fn {}
                               {fn {mesg}
                                   "NO METHODS"}}} 
              {rec {,(symbol->s-exp class) = ,(create-class-s-expression class 
                                                                         parent 
                                                                         fields 
                                                                         methodNames
                                                                         methodBodies
                                                                         methodParams)}  
                ,(create-final-program (rest s))}})]
        [else (error 'final-program "I think something is wrong...")]))
; Takes in a list of symbols and returns true if a symbol in the list
; is the same as the reserved symbol. False otherwise.
(define (not-valid-symbols? [syms : (listof symbol)]) : boolean
  (cond [(empty? syms) false]
        [(check-if-reserved-symbol (first syms)) (error 'not-valid "Invalid input")]
        [else (not-valid-symbols? (rest syms))]))

; Checks if a symbol is a boolean
(define (check-if-boolean [s : symbol])
  (cond [(or (equal? s 'true) (equal? s 'false)) true]
        [else false]))

; Checks returns the correct booleanC
(define (get-boolean-value [s : symbol]) : ExprC
  (cond [(equal? s 'true) (booleanC true)]
        [(equal? s 'false) (booleanC false)]))

; Gets the parameters from a with statement
(define (get-params [args : s-expression] [syms : (listof symbol)]) : (listof symbol)
  (cond [(empty? (s-exp->list args)) syms]
        [(s-exp-match? '{SYMBOL = ANY} (first (s-exp->list args)))
         (cons (s-exp->symbol (first (s-exp->list (first (s-exp->list args)))))
               (get-params (list->s-exp (rest (s-exp->list args))) syms))]
        [else (get-params (list->s-exp (rest (s-exp->list args))) syms)]))

; Gets the expression to be evalueated from a with statement
(define (get-values [args : s-expression]) : ExprC
  (cond
    [(empty? (s-exp->list args)) (error 'get-values "Invalid input :(")]
    [(not (s-exp-match? '{SYMBOL = ANY} (first (s-exp->list args)))) (parse (first (s-exp->list args)))]
    [else (get-values (list->s-exp (rest (s-exp->list args))))]))

; Gets and parses the rhs of the varaible declarations in a with statement
(define (get-list-exprC [args : s-expression] [exprClist  :(listof ExprC)]) : (listof ExprC)
  (cond [(empty? (s-exp->list args)) exprClist]
        [(s-exp-match? '{SYMBOL = ANY} (first (s-exp->list args)))
         (cons (parse (third (s-exp->list (first (s-exp->list args))))) (get-list-exprC (list->s-exp (rest (s-exp->list args))) exprClist))]
        [else (get-list-exprC (list->s-exp (rest (s-exp->list args))) exprClist)]))

; Test to see if symbol is being used that is reserve symbol. Takes in an
; s expression and a position to operate on and then check if it is a
; reserve symbol.
(define (test-of-operators [s : s-expression]
                           [position : ((listof 'a) -> 'a)]) : boolean
  (and (s-exp-symbol? (position (s-exp->list s)))
       (check-if-reserved-symbol (s-exp->symbol (position (s-exp->list s))))))

;Creates and appC by using the map operator on an S expression
(define (create-appC [s : s-expression]) :  ExprC
  (appC (parse (first (s-exp->list s)))
        (map parse (rest (s-exp->list s)))))

; Creates a List of symbols from an s-expresssion to be used in fdC
(define (create-list [s : s-expression]) : (listof symbol)
  (map s-exp->symbol (s-exp->list (second (s-exp->list s)))))

; Check to see if a list of symbols has duplicets.
; True => no dupes
(define (list-has-no-dups [l : (listof symbol)]) : boolean
  (cond [(empty? l) true]
        [else (check-dupes (first l) (rest l))]))
 
; Takes a symbol and a list and checkes whether that symbol is in the list.
; returns true if the symbol is not in the list.
(define (check-dupes [s : symbol] [l : (listof symbol)]) : boolean
  (cond [(empty? l) true]
        [else (and (and (not (eq? s (first l))) (check-dupes s (rest l)))
                   (list-has-no-dups (rest l)))]))

; Parses the s expression and the list of function expressions and then
; calls eval
(define (top-eval [program : s-expression]) : string
  (serialize (eval (parse (create-final-program (s-exp->list program))) empty)))

; Parses a program and returns an ExprC
(define (parse-prog [program : s-expression]) : ExprC
  (parse (create-final-program (s-exp->list program))))

; Takes a Value and outputs the string version of it
(define (serialize [e : Value]) : string
  (cond [(numV? e) (to-string (numV-n e))]
        [(booleanV? e)
         (cond [(booleanV-b e) "true"]
               [else "false"])]
        [(closV? e) "#<procedure>"]
        [(stringV? e) (stringV-s e)]))


; Sets the hash map which takes a class name and maps to arguments
(define (set-class-map [class : symbol] [args : (listof s-expression)]) : void
  (hash-set! my-hash class args))

; Gets the argument associated with a class
(define (get-class-map-args [class : symbol]) : (optionof (listof s-expression))
  (hash-ref my-hash class))

;===============TEST CASES=======================
(test (parse '"string") (stringC "string"))
;(test/exn (get-fields (list '{i}) 'a) "get")
(test/exn (top-eval '{{rec (with = 34) 3}}) "error")
(test/exn (create-final-program (list '{i} '{p})) "final")

(test (parse-prog '{{+ 3 5}}) (binop '+ (numC 3) (numC 5)))
(test (top-eval '{{rec {fact = {fn {n} {if {eq? 0 n} 1 {* n {fact {- n 1}}}}}} 
                   {fact 3}}}) "6")
(test (top-eval '{{rec {fact = {fn {n} {if {eq? 0 n} 1 {* n {fact {- n 1}}}}}} 
                   {fact 4}}}) "24")


(test (create-send (second (s-exp->list '{send p1 set-x 999 123}))
                   (third (s-exp->list '{send p1 set-x 999 123}))
                   (rest 
                    (rest 
                     (rest (s-exp->list '{send p1 set-x 999 123}))))) 
      '(with (obj = p1) ((obj "set-x") obj 999 123)))

(test (create-final-program (s-exp->list '{{class Point extends Object
                                             {x y}
                                             {get-x {} x}
                                             {get-y {} y}}
                                           
                                           {class 3DPoint extends Point
                                             {z}
                                             {get-z {} z}
                                             {dist-from-zero {}
                                                             {with {y = {send this get-y}}
                                                                   {with {x = {send this get-x}}
                                                                         {expt {+ {* z z} {+ {* y y} {* x x}}} 1/2}}}}}}))
      '(rec
  (Object = (fn () (fn (mesg) "NO METHODS")))
  (rec
   (Point
    =
    (fn
     (x y)
     (with
      (parent = (new Object))
      (fn (mesg) (if (eq? mesg "get-x") (fn (this) x) (if (eq? mesg "get-y") (fn (this) y) (parent mesg)))))))
   (rec
    (Object = (fn () (fn (mesg) "NO METHODS")))
    (rec
     (3DPoint
      =
      (fn
       (x y z)
       (with
        (parent = (new Point x y))
        (fn
         (mesg)
         (if (eq? mesg "get-z")
           (fn (this) z)
           (if (eq? mesg "dist-from-zero")
             (fn (this) (with (y = (send this get-y)) (with (x = (send this get-x)) (expt (+ (* z z) (+ (* y y) (* x x))) 1/2))))
             (parent mesg)))))))
     ())))))

(test (top-eval '{{class Point extends Object
                    {x y}
                    {get-x {} x}
                    {get-y {} y}
                    {set-x {new-x} {x <- new-x}}}
                  {with {point = {new Point 5 15}}
                        {send point get-x}}}) "5")

(test (top-eval '{{class Point extends Object
                    {x y}
                    {get-x {} x}
                    {get-y {} y}
                    {set-x {new-x} {x <- new-x}}}
                  {with {point = {new Point 5 15}}
                        {send point get-y}}}) "15")


(test (top-eval '{{class Point extends Object
                    {x y}
                    {get-x {} x}
                    {get-y {} y}
                    {set-x {new-x} {x <- new-x}}}
                  {class 3DPoint extends Point
                    {z}
                    {get-z {} z}
                    { dist-from-zero {}
                            {with {y = {send this get-y}}
                                  {with {x = {send this get-x}}
                                        {expt {+ {* z z} 
                                                 {+ {* y y} 
                                                    {* x x}}} 
                                              1/2}}}}}
                  {with {point = {new 3DPoint 5 15 20}}
                         {send point get-x}}}) "5")

(test (top-eval '{{class Point extends Object
                    {x y}
                    {get-x {} x}
                    {get-y {} y}
                    {create-point {} {new Point 10 100}}
                    {set-x {new-x} {x <- new-x}}}
                  {class 3DPoint extends Point
                    {z}
                    {get-z {} z}
                    {dist-from-zero {}
                            {with {y = {send this get-y}}
                                  {kk = {send this create-point}}
                                  {with {x = {send this get-x}}
                                        {+ {* z z} 
                                                 {+ {send kk get-y} 
                                                    {* x x}}} 
                                              }}}}
                  {with {point = {new 3DPoint 5 15 20}}
                         {send point dist-from-zero}}}) "525")


(test (top-eval '{{class Point extends Object
                    {x y}
                    {get-x {e} {+ x e}}
                    {get-y {} y}
                    {create-point {} {new Point 10 100}}
                    {set-x {new-x} {x <- new-x}}}
                  {class 3DPoint extends Point
                    {z}
                    {get-z {} z}
                    {dist-from-zero {}
                            {with {y = {send this get-y}}
                                  {kk = {send this create-point}}
                                  {with {x = {send this get-x 5}}
                                        {x} 
                                              }}}}
                  {with {point = {new 3DPoint 5 15 20}}
                         {send point dist-from-zero}}}) "525")

(test (top-eval
       '{{class Rider extends Object
           {}
           {one-if-by-land {}
                           {if {eq? {send this travel-mode} "by land"}
                               1
                               2}}}
         
         {class LandRider extends Rider
           {}
           {travel-mode {} "by land"}}
         
         {class SeaRider extends Rider
           {}
           {travel-mode {} "by sea"}} 
         {with {r = {new LandRider}}
               {send r travel-mode}}}) "by land")

(test (top-eval
 '{{class Rider extends Object
     {}
     {one-if-by-land {}
             {if {eq? {send this travel-mode} "by land"}
                 1
                 2}}}
   
   {class LandRider extends Rider
     {}
     {travel-mode {} "by land"}}
   
   {class SeaRider extends Rider
     {}
     {travel-mode {} "by sea"}} 
   {with {r = {new SeaRider}}
         {send r travel-mode}}}) "by sea")


;================OLD==============================
(test (serialize (booleanV true)) "true")
(test (serialize (booleanV false)) "false")
(test (parse '{true}) (booleanC true))
(test (parse '{false}) (booleanC false))
(test (eval (parse '{{fn {seven} (seven)}
                     {{fn {minus} {fn {} (minus (+ 3 10) (* 2 3))}}
                      {fn {x y} (+ x (* -1 y))}}}) empty) (numV 7))
(test (eval (parse '{eq? true false}) empty) (booleanV false))
(test (eval (parse '{eq? true true}) empty) (booleanV true))
(test (eval (parse '{eq? true 3}) empty) (booleanV false))
(test (parse `3) (numC 3))
(test (parse `xxx) (idC 'xxx))
(test (parse '{+ 5 2}) (binop '+ (numC 5) (numC 2)))
(test (parse '{- 5 2}) (binop '- (numC 5) (numC 2)))
(test (parse '{* 5 2}) (binop '* (numC 5) (numC 2)))
(test (parse '{/ 5 2}) (binop '/ (numC 5) (numC 2)))
(test (parse '{eq? 5 2}) (binop 'eq? (numC 5) (numC 2)))
(test (parse '{<= 5 2}) (binop '<= (numC 5) (numC 2)))
(test (parse '{if {<= 3 5} 3 5})
      (if (binop '<= (numC 3) (numC 5)) (numC 3) (numC 5)))
(test (parse '{if true 3 5}) (if (booleanC #t) (numC 3) (numC 5)))
(test (parse '{if false 3 5}) (if (booleanC #f) (numC 3) (numC 5)))

(test (get-list-exprC '{{z = {+ 9 14}} {y = 98}} empty) (list (binop '+ (numC 9) (numC 14)) (numC 98)))
(test (get-params '{{z = {+ 9 14}} {y = 98}} empty) (list 'z 'y))
(test (parse '{fn {x y} {+ x y}}) (lamC (list 'x 'y) (binop '+ (idC 'x) (idC 'y))))
(test (eval (binop '+ (numC 10) (appC (lamC (list 'const5 '_) (numC 5)) (list (numC 10))))
            mt-env)
      (numV 15))

(test/exn (eval (appC (lamC (list 'f1 'x) (appC (lamC (list 'f2 'y) (binop '+ (idC 'x) (idC 'y)))
                                                (list (numC 4))))
                      (list (numC 3)))
                mt-env)
          "name not found")

(test (eval (parse '{+ x 3}) (list (bind 'x (box (numV 5))))) (numV 8))
(test (eval (parse '{+ x y}) (list (bind 'x (box(numV 5))) (bind 'y (box (numV 12))))) (numV 17))
(test (eval (parse'{+ 1 1}) empty) (numV 2))
(test (get-params '{{z = {+ 9 14}} {y = 98}} empty) (list 'z 'y))
(test (get-params '{} empty) empty)
(test (eval (parse '{eq? true {if {<= 5 3} 5 true}}) empty) (booleanV true))
(test (parse `#t) (booleanC true))
(test (eval (parse `#t) empty) (booleanV true))

(test (num<= (numV 5) (numV 10)) (booleanV true))
(test (num<= (numV 10) (numV 1)) (booleanV false))
(test/exn (num<= (numV 10) (booleanV true)) "one argument was not a number")

(test (numeq? (booleanV true) (booleanV true)) (booleanV true))
(test (numeq? (booleanV false) (booleanV true)) (booleanV false))
(test (numeq? (numV 7) (numV 8)) (booleanV false))
(test (numeq? (numV 7) (numV 7)) (booleanV true))
(test (numeq? (numV 7) (booleanV true)) (booleanV false))
(test (parse '{eq? 2 true}) (binop 'eq? (numC 2) (booleanC #t)))
(test/exn (parse '{eq? 2 if}) "bad")
(test (eval (parse '{eq? 3 5}) empty) (booleanV false))
(test (parse '{if #t 3 5}) (if (booleanC #t) (numC 3) (numC 5)))
(test (eval (parse '{if true 3 5}) empty) (numV 3))
(test (eval (parse '{if #f 3 5}) empty) (numV 5))
(test (serialize (numV 3)) "3")
(test (serialize (booleanV true)) "true")
(test (parse '{with {z = {+ 9 14}} {y = 98} {+ z y}})
      (appC (lamC (list 'z 'y) (binop '+ (idC 'z) (idC 'y)))
            (list (binop '+ (numC 9) (numC 14)) (numC 98))))
(test (parse `true) (booleanC true))
(test/exn (parse '{fn {x x} 3}) "dup")
(test (top-eval `1) "1")
(test (parse '{{+ 3 5} {<= 5 3}}) (appC (binop '+ (numC 3) (numC 5)) (list (binop '<= (numC 5) (numC 3)))))
(test (parse '{if false 3 4}) (if (booleanC false) (numC 3) (numC 4)))
(test (parse '{fn {x} {+ 5 x}}) (lamC (list 'x) (binop '+ (numC 5) (idC 'x))))
(test (check-if-binop '+) true)
(test (check-if-binop '*) true)
(test (check-if-binop '-) true)
(test (check-if-binop '/) true)
(test (check-if-binop 'eq?) true)
(test (check-if-binop 'f) false)
(test (parse '{eq? true {fn {} {+ 3 5}}})
      (binop 'eq? (booleanC #t)
             (lamC empty (binop '+ (numC 3) (numC 5)))))

(test (parse '{eq? {fn {} {+ 3 5}} true})
      (binop 'eq? (lamC empty (binop '+ (numC 3) (numC 5)))
             (booleanC #t)))
(test (check-in-list 'd empty) false)
(test (check-in-list 'a (list 'b 'c)) false)
(test (check-in-list 'a (list 'b 'a)) true)

(test (get-binop '+) num+)
(test (get-binop '*) num*)
(test (get-binop '-) num-)
(test (get-binop '/) num/)
(test (get-binop '<=) num<=)
(test/exn (get-binop 'f) "NOT A BINOP")

(test (list-has-no-dups (list 'a 'b 'c)) true)
(test (list-has-no-dups (list 'a 'a)) false)
(test/exn (parse '{+ / 3}) "invalid input :(")
(test/exn (parse '{+ 2 *}) "invalid input :(")
(test/exn (parse '{+ - 3}) "invalid input :(")
(test/exn (parse '{+ if *}) "invalid input :(")
(test/exn (parse '{+ + *}) "invalid input :(")
(test/exn (parse '{if}) "invalid input :(")
(test/exn (parse `if) "invalid input :(")
(test/exn (get-values '{}) "Invalid")
(test/exn (not-valid-symbols? (list '+)) "Invalid")

(test (num- (numV 10) (numV 10)) (numV 0))
(test (num* (numV 10) (numV 10)) (numV 100))
(test (num/ (numV 10) (numV 10)) (numV 1))


(test/exn (num/ (numV 10) (numV 0)) "Division")
(test/exn (num- (booleanV true) (numV 10)) "one")
(test/exn (num* (booleanV true) (numV 10)) "one")
(test/exn (num/ (booleanV true) (numV 10)) "one")
(test/exn (num+ (booleanV true) (numV 10)) "one")
(test/exn (parse '{fn {+ - p} 5}) "input")

(test/exn (parse '{+ / 3}) "invalid input :(")
(test/exn (parse '{+ 2 *}) "invalid input :(")
(test/exn (parse '{+ - 3}) "invalid input :(")
(test/exn (parse '{+ if *}) "invalid input :(")
(test/exn (parse '{+ + *}) "invalid input :(")
(test/exn (parse '{if}) "invalid input :(")
(test/exn (parse `if) "invalid input :(")
(test/exn (eval (if (binop '+ (numC 3) (numC 3)) (numC 5) (numC 10)) empty) "Not a boolean")
#;(
(test (check-if-reserved-symbol '<=) true)
(test (check-if-reserved-symbol 'if) true)
(test (check-if-reserved-symbol 'eq?) true)
(test (check-if-reserved-symbol 'true) true)
(test (check-if-reserved-symbol 'false) true)
(test (check-if-reserved-symbol 'with) true)
(test (check-if-reserved-symbol 'fn) true)
(test (check-if-reserved-symbol-excluding-true-false '<=) true)
(test (check-if-reserved-symbol-excluding-true-false 'if) true)
(test (check-if-reserved-symbol-excluding-true-false 'with) true)
(test (check-if-reserved-symbol-excluding-true-false 'fn) true)
(test (check-if-reserved-symbol-excluding-true-false 'ddd) false)
(test (check-if-reserved-symbol-excluding-true-false '+) true)
(test (check-if-reserved-symbol-excluding-true-false '*) true)
(test (check-if-reserved-symbol-excluding-true-false '/) true)
(test (check-if-reserved-symbol-excluding-true-false '-) true)
(test (check-if-reserved-symbol-excluding-true-false 'eq?) true)
)
(test (serialize (closV empty (numC 3) empty)) "#<procedure>")
(test/exn (parse '{with {z = {fn {} 3}} {z = 9} {z}}) "dups")
(test (top-eval '{(fn (minus) (minus 8 5)) (fn (a b) (+ a (* -1 b)))}) "3")

;; Captain Teach Test Cases
(test (parse `3) (numC 3))
(test (parse `xabth) (idC 'xabth))
(test (parse '{+ 4 5}) (binop '+ (numC 4) (numC 5)))
(test (parse '(z 4 5)) (appC (idC 'z) (list (numC 4) (numC 5))))
(test (parse '(z 4 5 6 7 8 9 (g 10) 11 12 13 14 15 16))
      (appC (idC 'z)
            (list (numC 4) (numC 5)
                  (numC 6) (numC 7) (numC 8) (numC 9) (appC (idC 'g)
                                                            (list (numC 10)))
                  (numC 11) (numC 12) (numC 13) (numC 14)
                  (numC 15) (numC 16))))
 

;; fundefs
(test (parse '{fn {} 3}) (lamC (list) (numC 3)))

;; with
(test (parse '{with {z = {fn {} 3}} {z}})
      (appC (lamC (list 'z) (appC (idC 'z) (list)))
            (list (lamC (list) (numC 3)))))
;; NEW FOR 2144:
(test (parse '{with {z = {fn {} 3}} {q = 9} {z}})
      (appC (lamC (list 'z 'q) (appC (idC 'z) (list)))
            (list (lamC (list) (numC 3))
                  (numC 9))))
;; if
(test (parse '{if 3 4 5})
      (if (numC 3) (numC 4) (numC 5)))

(test (eval (parse '{{fn {function} {function 6}} {fn {x} {- x 1}}}) empty) (numV 5))
;(test (eval (parse '{{fn {function} {function 6}} {fn {x} {if {<= x 0} 1 {function {- x 1}}}}}) empty) (numV 6))

;(top-eval (parse '{{fn {multi-larg} {multi-larg 1 5 4 2}} {fn {a b c d} {if {<= a b} {if {<= c d} {+ b d} {+ b c}}
;                                                                        {if {<= c d} {+ a d} {+ a c}}}}}))