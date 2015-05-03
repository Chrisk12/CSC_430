#lang plai-typed
(require plai-typed/s-exp-match)
(print-only-errors true)

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
                         (get-list-binding (closV-arg f-value) a (closV-env f-value))))]
    [lamC (param body) (closV param body env)]
    [binop (s l r) ((get-binop s) (eval l env) (eval r env))]
    [if (t iff ffi) (cond 
                      [(not (booleanV? (eval t env))) 
                       (error 'if "Not a boolean expression")]
                      [(booleanV-b (eval t env)) (eval iff env)]
                          [else (eval ffi env)])]))
 
; Creates a list of binding from teh fdC-Arg and teh argument to the function
(define (get-list-binding [syms : (listof symbol)]
                          [a : (listof ExprC)]
                          [env : Env]) : (listof Binding)
  (cond [(empty? a) env]
        ;[(not (= (length syms) (length a))) (error 'get-list-bing "Wrong Size)")]
        [else (cons (bind (first syms) (eval (first a) env)) (get-list-binding (rest syms) (rest a) env))]))

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

; 3.11 Defining a parser that takes an s expression and convert it to an ExperC. 
(define (parse [s : s-expression]) : ExprC
  (cond
    
    [(s-exp-number? s) (numC (s-exp->number s))]
    
    [(s-exp-boolean? s) (booleanC (s-exp->boolean s))]
    
    [(s-exp-symbol? s) 
     
     (cond
       [(check-if-boolean (s-exp->symbol s)) (get-boolean-value (s-exp->symbol s))]
       [(check-if-reserved-symbol (s-exp->symbol s)) (error 'parse "invalid input :(")]
       [else (idC (s-exp->symbol s))])]
    
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
   
    [(s-exp-match? '{SYMBOL ANY ANY} s)
     (cond [(test-of-operators s second) (error 'parse "invalid input :(")]
           [(test-of-operators s third) (error 'parse "invalid input :(")]
           [(check-if-binop(s-exp->symbol (first (s-exp->list s))))  
            (binop (s-exp->symbol (first (s-exp->list s)))
                   (parse (second (s-exp->list s))) 
                   (parse (third (s-exp->list s))))]
           [else (create-appC s)])]
    
    [(s-exp-match? '{ANY ANY ...} s)
     (create-appC s)]))

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
(define (top-eval [s : s-expression]) : string 
  (serialize (eval (parse s) empty)))

; Takes a Value and outputs the string version of it
(define (serialize [e : Value]) : string
  (cond [(numV? e) (to-string (numV-n e))]
        [(booleanV? e) 
         (cond [(booleanV-b e) "true"]
               [else "false"])]
        [(closV? e) "#<procedure>"]))

;===============TEST CASES=======================
(test (serialize (booleanV true)) "true")
(test (serialize (booleanV false)) "false")
(test (eval (parse '{{fn {seven} (seven)} 
                     {{fn {minus} {fn {} (minus (+ 3 10) (* 2 3))}} 
                      {fn {x y} (+ x (* -1 y))}}}) empty) (numV 7))

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

(test (eval (parse '{+ x 3}) (list (bind 'x (numV 5)))) (numV 8))
(test (eval (parse '{+ x y}) (list (bind 'x (numV 5)) (bind 'y (numV 12)))) (numV 17))
(test (eval (parse'{+ 1 1}) empty) (numV 2))
(test (get-params '{{z = {+ 9 14}} {y = 98}} empty) (list 'z 'y))
(test (get-params '{} empty) empty)

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
;(test (top-eval '{if (<= {+ 1 11} 10) 5 10}) (numV 10))
(test/exn (parse '{fn {x x} 3}) "dup")
(test (top-eval `1) "1")
(test (parse '{{+ 3 5} {<= 5 3}}) (appC (binop '+ (numC 3) (numC 5)) (list (binop '<= (numC 5) (numC 3)))))
;(test (eval (if (>= (numC 0) 0) (numC 5) (numC 10)) empty ) (numV 5))
;(test (eval (if (>= (numC 0) 1) (numC 10)) empty ) (numV 10))
(test (parse '{if false 3 4}) (if (booleanC false) (numC 3) (numC 4)))
(test (parse '{fn {x} {+ 5 x}}) (lamC (list 'x) (binop '+ (numC 5) (idC 'x))))
(test (check-if-binop '+) true)
(test (check-if-binop '*) true)
(test (check-if-binop '-) true)
(test (check-if-binop '/) true)
(test (check-if-binop 'f) false)

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

(test (check-if-reserved-symbol '<=) true)
(test (check-if-reserved-symbol 'if) true)
(test (check-if-reserved-symbol 'eq?) true)
(test (check-if-reserved-symbol 'true) true)
(test (check-if-reserved-symbol 'false) true)
(test (check-if-reserved-symbol 'with) true)
(test (check-if-reserved-symbol 'fn) true)
(test (serialize (closV empty (numC 3) empty)) "#<procedure>")
(test/exn (parse '{with {z = {fn {} 3}} {z = 9} {z}}) "dups")
(test (top-eval '{(fn (minus) (minus 8 5)) (fn (a b) (+ a (* -1 b)))}) "3") 

; ======= DEPRECATED CODE =============

; ==== TEST ====
#;( 
    (test (subst (list (numC 3)) (list 'x) (numC 3)) (numC 3))
    (test (subst (list (numC 3)) (list 'x) (idC 'x)) (numC 3))
    (test (subst (list (numC 3)) (list 'y) (idC 'x)) (idC 'x))
    (test (subst (list (numC 5)) (list 'x) (appC 'f (list (numC 5))))
          (appC 'f (list (numC 5))))
    (test (subst (list (numC 6)) (list 'x 'y) (ifleq0 (numC 3) (numC 5) (numC 10)))
          (ifleq0 (numC 3) (numC 5) (numC 10)))
    (test (parse-fundef '{fn twice (x) (realtwice x)}) (fdC 'twice (list 'x) (appC 'realtwice (list (idC 'x)))))
    (test (parse-fundef '{fn minus (x y) (+ x (* -1 y))}) (fdC 'minus (list 'x 'y) (binop '+ (idC 'x) (binop '* (numC -1) (idC 'y)))))
    (test (parse-fundef '{fn realtwice (x) (+ x x)}) (fdC 'realtwice (list 'x) (binop '+ (idC 'x) (idC 'x))))
    (test (eval (parse '{twice 15}) empty  
                (list (parse-fundef '{fn realtwice (x) (+ x x)}) 
                      (parse-fundef '{fn twice (x) (realtwice x)}))) 30)
    (test (parse-fundef '{fn p {x} {+ x 1}}) (fdC 'p (list 'x) (binop '+ (idC 'x) (numC 1))))
    (test (eval (parse '{f 1}) empty (list (parse-fundef '{fn p {x} {+ x 1}})
                                           (parse-fundef '{fn f {x} {+ x 1}}))) (numV 2))
    
    (test (eval (parse '{f 1}) empty (list (parse-fundef '{fn p {x} {+ x 1}})
                                           (parse-fundef '{fn f {x} {+ x 1}}))) 2)
    
    (test (eval (parse '{f 1}) empty (list (parse-fundef '{fn f {x} {+ x 1}}))) 2)
    (test (eval (parse '{+ {f} {f}}) empty (list (parse-fundef '{fn f {} 5}))) 10)
    (test/exn (eval (parse '{f 1}) empty (list (parse-fundef '{fn f {x y} {+ x y}})))
              "wrong arity")
    (test/exn (parse-fundef '{f 0 1 5 2 4 8}) "Function is of the wrong type")
    
    (test/exn (get-fundef 'a empty) "reference to undefined function")
    
    (test (eval (parse '{minus 8 5}) empty  
                (list (parse-fundef '{fn minus (x y) (+ x (* -1 y))}))) 3)
    (test (eval (parse '{minus 5}) empty  
                (list (parse-fundef '{fn minus (y) (* -1 y)}))) -5)
    (test (eval (parse '{seven}) empty  
                (list (parse-fundef '{fn seven () (minus (+ 3 10) (* 2 3))}) 
                      (parse-fundef '{fn minus (x y) (+ x (* -1 y))}))) 7)
    (test/exn (parse-fundef '{fn + () 13}) "invalid input :(")
    ) 

; ==== CODE ====

#;(
; DEPRECATED
; Defines the substition function from the book with a modification to it to
; allow for variable number of parameters
#;(define (subst [what : (listof ExprC)] [for : (listof symbol)] [in :  ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond [(look-for-symbol s for) (find-experc-to-replace s for what)]
                   [else in])]
    [appC (f a) (appC f  (do-sub-for-arguments what for in a))]
    [ifleq0 (t iff fii) (ifleq0 (subst what for t)
                                (subst what for iff)
                                (subst what for fii))]
    [binop (s iff fii) (binop s (subst what for iff) (subst what for fii))]))

; DEPRECATED
; Preforms the substition on all the arguments of a function and returns
; a list of the substituded variable in the ExprC
#;(define (do-sub-for-arguments [what : (listof ExprC)] 
                              [for : (listof symbol)] 
                              [in :  ExprC] 
                              [a : (listof ExprC)]) : (listof ExprC)
  (cond [(or (empty? a) (empty? for)) a]
        [else (cons 
               (subst what for (first a)) 
               (do-sub-for-arguments what for in (rest a)))]))

; DEPRECATED
; Returns the ExperC for the symbol we are trying to find in look-for-symbol.
; The needle is the symbol we are looking for in the haystack.
(define (find-experc-to-replace [needle : symbol] 
                                [haystack : (listof symbol)] 
                                [value : (listof ExprC)]) : ExprC
  (cond [(symbol=? needle (first haystack)) (first value)]
        [else (find-experc-to-replace needle (rest haystack) (rest value))]))

; DEPRECATED
; Tries to find the symbol in the symbol list for subst. 
; The needle is the symbol we are looking for in the haystack.
; returns true if the symbol was found.
(define (look-for-symbol [needle : symbol] [haystack : (listof symbol)]) : boolean
  (cond [(empty? haystack) false]
        [(symbol=? needle (first haystack)) true]
        [else (look-for-symbol needle (rest haystack))]))

;DEPRECATED
; Gets the fundefC in fds associated with a symbol n and returns the FundefC
#;(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond 
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

;DEPRECATED
; Parses an  s expression and creates a funDef out of it
#;(define (parse-fundef [s : s-expression]) : FunDefC
    (cond
      [(s-exp-match? '{fn SYMBOL ANY ANY} s)
       (cond [(test-of-operators s second) (error 'parse "invalid input :(")]
             [else (fdC 
                    (create-list s)
                    (parse (fourth (s-exp->list s))))])]
      [else (error 'parse "Function is of the wrong type :(")]))
)


#;( CAPTIAN TEACH
            
            (test (parse `3) (numC 3))
(test (parse `xabth) (idC 'xabth))
(test (parse '{+ 4 5}) (binopC '+ (numC 4) (numC 5)))
(test (parse '(z 4 5)) (appC (idC 'z) (list (numC 4) (numC 5))))
(test (parse '(z 4 5 6 7 8 9 (g 10) 11 12 13 14 15 16)) 
      (appC (idC 'z)
           (list (numC 4) (numC 5)
                 (numC 6) (numC 7) (numC 8) (numC 9) (appC (idC 'g) 
                                                      (list (numC 10)))
                 (numC 11) (numC 12) (numC 13) (numC 14) 
                 (numC 15) (numC 16))))
(test/exn (parse `+) "illegal variable name")
(test/exn (parse `+) "illegal variable name")
(test/exn (parse `{+ if with}) "illegal variable name")
(test/exn (parse '((()))) "illegal expression")
;; bools
(test (parse `true) (boolC #t))
;; fundefs
(test (parse '{fn {} 3}) (lamC (list) (numC 3)))
;; NEW FOR 2144:
(test/exn (parse '{fn {x x} 3}) "unique parameter names")
;; with
(test (parse '{with {z = {fn {} 3}} {z}})
      (appC (lamC (list 'z) (appC (idC 'z) (list)))
            (list (lamC (list) (numC 3)))))
;; NEW FOR 2144:
(test (parse '{with {z = {fn {} 3}} {q = 9} {z}})
      (appC (lamC (list 'z 'q) (appC (idC 'z) (list)))
            (list (lamC (list) (numC 3))
                  (numC 9))))
(test/exn (parse '{with {z = {fn {} 3}} {z = 9} {z}})
      "unique parameter")
;; if
(test (parse '{if 3 4 5})
      (ifC (numC 3) (numC 4) (numC 5)))

)