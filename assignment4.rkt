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
  [newArrayC (size : ExprC) [value : ExprC]]
  [lamC (arg : (listof symbol)) (body : ExprC)]
  [refC (sym : ExprC) (location : ExprC)]
  [setArrayC (sym : ExprC) (location : ExprC) (expression : ExprC)]
  [mutateC (sym : symbol) (value : ExprC)]
  [beginC (elist : (listof ExprC))  (sym : ExprC)])


; Defines Values
(define-type Value
  [numV (n : number)]
  [booleanV (b : boolean)]
  [closV (arg : (listof symbol)) (body : ExprC) (env : Env)]
  [arrayV (location : Location) (length : number)])

; Defines the Binding Type which takes a symbol
; maps it to a number
(define-type Binding
  [bind (name : symbol) (val : Location)])

; An Alias for a list of bindings
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; An Alias for a Location
(define-type-alias Location number)

; A Type that maps a location to a value
(define-type Storage
  [cell (location : Location) (val : Value)])

; An Alias for a list of storage
(define-type-alias Store (listof Storage))
(define mt-store empty)
(define override-store cons)

; A type that has a value and a store
(define-type Result
  [v*s (v : Value) (s : Store)])

; A type that has a baselocation and a store
(define-type N*S
  [n*s (base : number) (store : Store)])

; A type that has a baselocation and a store
(define-type ENV*S
  [env*s (env : Env) (store : Store)])

; A type that has a list of values and a store
(define-type LV*S
  [lv*s (values : (listof Value)) (store : Store)])

; Defines the Interpreter. Takes in an ExprC and a list of functions
; and then evalutes the ExperC recursively and returns a number.
(define (eval [e : ExprC] [env : Env] [store : Store]) : Result
  (type-case ExprC e
    [numC (n) (v*s (numV n) store)]
    [idC (n) (v*s (fetch (lookup n env) store) store)]
    [booleanC (b) (v*s (booleanV b) store)]
    [refC (sym loc) (let ([update (type-case Result (eval sym env store)
                                    [v*s (v-sym s-sym)
                                         (v*s v-sym s-sym)])])
                      (let ([arrayIndex (type-case Result (eval loc env (v*s-s update))
                                          [v*s (v-loc s-loc)
                                               (v*s v-loc s-loc)])])
                        (cond [(and (arrayV? (v*s-v update))
                                    (> (arrayV-length (v*s-v update)) 
                                       (numV-n (v*s-v arrayIndex))))
                               (v*s (fetch (+ (arrayV-location (v*s-v update))
                                              (numV-n (v*s-v arrayIndex))) 
                                           (v*s-s arrayIndex)) 
                                    (v*s-s arrayIndex))]  
                                [else (error 'refC 
                                             (string-append (string-append (string-append "Not an array or not long enough   " 
                                                                           (to-string sym))
                                                             "   ") (to-string update)))])))]
    [setArrayC (s loc exp) 
               (type-case Result (eval s env store)
                 [v*s (v-s s-s)
                      (type-case Result (eval loc env s-s)
                        [v*s (v-loc s-loc)
                             (type-case Result (eval exp env s-loc)
                               [v*s (v-exp s-exp)
                                      (cond [(and (arrayV? v-s) 
                                                  (> (arrayV-length v-s) 
                                                     (numV-n v-loc)))
                                             (v*s v-exp (update-store 
                                                         (+ (arrayV-location v-s) 
                                                            (numV-n v-loc)) 
                                                         v-exp 
                                                         s-exp))] 
                                            [else 
                                             (error 'refC "Not an array or not long enough")])])])])]
    [mutateC (s expr) (let ([update (type-case Result (eval expr env store)
                                      [v*s (v-expr s-expr)
                                           (v*s v-expr s-expr)])])
                        (v*s (v*s-v update) 
                             (update-store (lookup s env) 
                                           (v*s-v update) 
                                           (v*s-s update))))]
    [newArrayC (size value) 
               (type-case Result (eval size env store)
                           [v*s (v-size s-size)
                                (type-case Result (eval value env s-size)
                                  [v*s (v-value s-value)
                                       (let [(all (allocate s-value 
                                                            (numV-n v-size) 
                                                            v-value))]
                                          (v*s  (arrayV (n*s-base all) (numV-n v-size)) 
                                                (n*s-store all)))])])]
    [beginC (l sym)
            (let ([extendStore (evaluate-list l env store)])
              (type-case Result (eval sym env extendStore)
            [v*s (v-sym s-sym)
                 (v*s v-sym s-sym)]))]
    [lamC (param body) (v*s (closV param body env) store)]
    [binop (s l r) (type-case Result (eval l env store) 
                      [v*s (v-l s-l)
                           (type-case Result (eval r env s-l)
                             [v*s (v-r s-r) 
                                  (v*s ((get-binop s) v-l v-r) s-r)])])]
    [if (t iff ffi) (type-case Result (eval t env store)
                      [v*s (v-t s-t)
                           (cond [(not (booleanV? v-t)) 
                                  (error 'if "not a boolean")]
                                 [(booleanV-b v-t) (type-case Result (eval iff env s-t)
                                                     [v*s (v-iff s-iff)
                                                          (v*s v-iff s-iff)])]
                                 [else (type-case Result (eval ffi env s-t)
                                         [v*s (v-ffi s-ffi)
                                              (v*s v-ffi s-ffi)])])])]
    [appC (f a)
          (type-case Result (eval f env store)
            [v*s (v-f s-f)
                 (let ([lvs (get-values-eval a env s-f)])
                   (let ([update (update-store-bind-symbols (closV-arg v-f) 
                                                            (lv*s-values lvs)
                                                            (closV-env v-f)
                                                            (lv*s-store lvs))])
                     (eval (closV-body v-f)  
                           (env*s-env update) 
                           (env*s-store update))))])]))

; Evalues a list of experC  

(define (evaluate-list [elist : (listof ExprC)] [env : Env] [store : Store]) : Store
  (cond [(empty? elist) store]
        [else (type-case Result (eval (first elist) env store)
                        [v*s (v-value s-store)
                             (evaluate-list (rest elist) env s-store)])]))
  

; Updates the store env and binds the symbosl to the env.
; The work horse function
(define (update-store-bind-symbols [args : (listof symbol)] 
                                   [values : (listof Value)]
                                   [env : Env]
                                   [store : Store]) : ENV*S
  (let 
      ([nextLocation (add1 (get-last-location (cond [(empty? store) 
                                                (list (cell -1 (numV -1)))] 
                                               [else store])))])
    (cond [(not (equal? (length args) (length values))) 
           (error 'update "not the same length")]
      [(empty? args) (env*s env store)]
          
      [else (let 
                ([extendStore (allocate store 1 (first values))]
                 [extendEnv (cons (bind (first args) nextLocation) env)])
              (let ([update 
                     (update-store-bind-symbols (rest args)
                                                (rest values)
                                                extendEnv
                                                (n*s-store extendStore))]) 
                
                (env*s (env*s-env update) (env*s-store update))))])))

; 3.11 Defining a parser that takes an s expression and convert it to an ExperC.
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-match? '{true} s) (booleanC true)]
    [(s-exp-match? '{false} s) (booleanC false)]
    [(s-exp-symbol? s)
     (cond
       [(check-if-boolean (s-exp->symbol s)) (get-boolean-value (s-exp->symbol s))]
       [(check-if-reserved-symbol (s-exp->symbol s)) 
        (error 'parse (string-append (to-string s) "invalid input :("))]
       [else (idC (s-exp->symbol s))])]
    [(s-exp-match? '{new-array ANY ANY} s)
                   (newArrayC (parse (second (s-exp->list s)))
                           (parse (third (s-exp->list s))))]
    [(s-exp-match? '{ref ANY [ANY]} s)
     (refC (parse (second (s-exp->list s))) 
           (parse (first (s-exp->list (third (s-exp->list s))))))]
    [(s-exp-match? '{ANY [ANY] <- ANY} s) 
     (setArrayC (parse (first (s-exp->list s)))
                (parse (first (s-exp->list (second (s-exp->list s)))))
                (parse (fourth (s-exp->list s))))]
    [(s-exp-match? '{SYMBOL <- ANY} s) 
     (mutateC (s-exp->symbol (first (s-exp->list s)))
              (parse (third (s-exp->list s))))]
    [(s-exp-match? '{begin ANY ...} s)
     (beginC (get-list-of-expr (rest (s-exp->list s))) 
             (parse (first (reverse (s-exp->list s)))))]
     [(s-exp-match? '{with {SYMBOL = ANY}... ANY} s)
     (let ([args (list->s-exp (rest (s-exp->list s)))]
           [params (get-params (list->s-exp (rest (s-exp->list s))) empty)]
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
    [(s-exp-match? '{ANY ANY ...} s)
     (create-appC s)]))

; Gets a list of parse exprC
(define (get-list-of-expr [s : (listof s-expression)]) : (listof ExprC)
    (cond [(or (empty? s) (= 1 (length s))) empty]
          [else (cons (parse (first s)) (get-list-of-expr (rest s)))]))

; Preforms the look up for the symbol in the envirnment and 
; returns the location
(define (lookup [for : symbol] [env : Env]) : Location
  (cond
    [(empty? env) (error  'lookup 
                          (string-append 
                           (string-append "name not found:   " 
                                          (to-string for)) 
                           (to-string env)))]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))

; Fetches the value from the location in the store
(define (fetch [loc : Location] [sto : Store]) : Value
  (cond
    [(empty? sto) (error 'lookup "name not found")]
    [else (cond
            [(equal? loc (cell-location (first sto)))
             (cell-val (first sto))]
            [else (fetch loc (rest sto))])]))


; Update Store
(define (update-store [pointer : number] [new-value : Value] [store : Store]) : Store
  (cond [(or (< pointer 0) (empty? store)) (error 'update-Storage "index out of bounds")]
        [(equal? (cell-location (first store)) pointer) 
         (cons (cell (cell-location (first store)) new-value) (rest store))]
        [else (cons (first store) (update-store pointer new-value (rest store)))]))

; Allocates a number of Values onto the store
(define (allocate [store : Store] [extend-by : number] [value : Value]) : N*S
  (let ([baseLocation (add1 (get-last-location (cond [(empty? store) 
                                                (list (cell -1 (numV -1)))] 
                                               [else store])))])
  (n*s baseLocation (append-list store 
                                      (create-store extend-by 
                                                          value 
                                                          baseLocation)))))
 
; Get the last location in the store
(define (get-last-location [store : Store]) : Location
  (cell-location (first (reverse store))))

; Creates a n cells with a given value and returns the new store
(define (create-store [extend-by : number] 
                      [value : Value] 
                      [last-location : number]) : Store
  (cond [(<= extend-by 0) empty]
        [else (cons (cell last-location value) 
                    (create-store (sub1 extend-by) 
                                  value 
                                  (add1 last-location)))]))

; 3.6 Appends two list together
(define (append-list [l1 : (listof 'a)] [l2 : (listof 'a)]) : 'a
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else (cons (first l1) (append-list (rest l1) l2))]))

;Gets the function associated with the binary operator and returns that
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
    [(and (arrayV? l) (arrayV? r)) (booleanV (and (equal? (arrayV-location l)
                                          (arrayV-location r))
                                          (equal? (arrayV-length l)
                                          (arrayV-length r))))]
    [else (booleanV false)]))


; Takes a list of exprc and returns a list of values. To be used in eval.
(define (get-values-eval [exprs : (listof ExprC)]
                         [env : Env]
                         [store : Store]) : LV*S
           (cond [(empty? exprs) (lv*s empty store)]
                 [else
                    (type-case Result (eval (first exprs) env store)
                      [v*s (v-exp s-exp)
                           (let ([others (get-values-eval (rest exprs) env s-exp)])
                  (lv*s (cons v-exp (lv*s-values others)) (lv*s-store others)))])]))
    
; Creates a list of binding from teh fdC-Arg and teh argument to the function
#;(define (get-list-binding [syms : (listof symbol)]
                          [a : (listof Value)]
                          [env : Env]) : (listof Binding)
  (cond [(empty? a) env]
        [else (cons (bind (first syms) (first a)) (get-list-binding (rest syms) (rest a) env))]))

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
        [(equal? s '<-) true]
        [(equal? s '=) true]
        [(equal? s 'new-array) true]
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
        [(equal? s '<-) true]
        [(equal? s '=) true]
        [(equal? s 'new-array) true]
        [(equal? s 'fn) true]
        [else false]))

; Searches through a list of symbols to see if a symbol s is in the list.
; returns true if it is, false otherwise.
(define (check-in-list [s : symbol] [lst : (listof symbol)]) : boolean
  (cond [(empty? lst) false]
        [(symbol=? s (first lst)) true]
        [else (check-in-list s (rest lst))]))

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
    [(not (s-exp-match? '{SYMBOL = ANY} (first (s-exp->list args)))) 
     (parse (first (s-exp->list args)))]
    [else (get-values (list->s-exp (rest (s-exp->list args))))]))

; Gets and parses the rhs of the varaible declarations in a with statement
(define (get-list-exprC [args : s-expression] [exprClist  :(listof ExprC)]) : (listof ExprC)
  (cond [(empty? (s-exp->list args)) exprClist]
        [(s-exp-match? '{SYMBOL = ANY} (first (s-exp->list args)))
         (cons (parse (third (s-exp->list (first (s-exp->list args))))) 
               (get-list-exprC (list->s-exp (rest (s-exp->list args))) exprClist))]
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
  (serialize (eval (parse s) empty empty)))

; Takes a Value and outputs the string version of it
(define (serialize [e : Result]) : string
  (cond [(numV? (v*s-v e)) (to-string (numV-n (v*s-v e)))]
        [(booleanV? (v*s-v e))
         (cond [(booleanV-b (v*s-v e)) "true"]
               [else "false"])]
        [(closV? (v*s-v e)) "#<procedure>"]
        [(arrayV? (v*s-v e)) "#<array>"]))

; Defines a simple while function
(define (while) : s-expression
  '{fn {a b} {with {while = 123}
                   {temp = {fn {guard body fun} 
                               {with {x = body}
                                     {if guard {fun guard body fun} x}}}}
                   {begin {while <- temp}
                          {if a {while a b while}} false}}})
  
; Defines an inorder function
(define (in-order) : s-expression
  '{fn {a size} {with {in-order = 123}
                         {temp = {fn {a size cur last fun}
                             {if {eq? size cur} true        
                                 {if {<= {ref a[last]} {- {ref a[cur]} 1}}
                                     {fun a size {+ 1 cur} {+ 1 last} fun}
                                     false}}}}
                         {begin {in-order <- temp}
                         {in-order a size 1 0 in-order}}}})










; Used for testing, defines store 
(define test-store (list (cell 0 (numV 5)) 
                         (cell 1 (numV 6)) 
                         (cell 2 (booleanV true)) 
                         (cell 3 (numV 8))))

;Used for testing, defines store 
(define test-store-2 (list (cell 0 (numV 5)) 
                         (cell 1 (numV 6)) 
                         (cell 2 (booleanV true))
                         (cell 3 (numV 8))
                         (cell 4 (numV 5))
                         (cell 5 (numV 5))
                         (cell 6 (numV 0))))

;Used for testing, defines store 
(define test-store-array (list
        (cell 0 (numV 5))
        (cell 1 (numV 6))
        (cell 2 (booleanV #t))
        (cell 3 (numV 8))
        (cell 4 (arrayV 7 5))
        (cell 5 (numV 5))
        (cell 6 (numV 0))
        (cell 7 (numV 7))
        (cell 8 (numV 8))
        (cell 9 (numV 9))
        (cell 10 (numV 10))
        (cell 11 (numV 11))))

;Used for testing, defines binding
(define test-binding (list (bind 'p 4) (bind 'x 0)))
;===============TEST CASES=======================

(test (parse '{+ 5 2}) (binop '+ (numC 5) (numC 2)))
(test (parse '{- 5 2}) (binop '- (numC 5) (numC 2)))
(test (parse '{* 5 2}) (binop '* (numC 5) (numC 2)))
(test (parse '{/ 5 2}) (binop '/ (numC 5) (numC 2)))
(test (parse '{eq? 5 2}) (binop 'eq? (numC 5) (numC 2)))
(test (parse '{<= 5 2}) (binop '<= (numC 5) (numC 2)))

(test (eval (parse '{+ 5 2}) empty empty) (v*s (numV 7) empty))
(test (eval (parse '{- 5 2}) empty empty) (v*s (numV 3) empty))
(test (eval (parse '{* 5 2}) empty empty) (v*s (numV 10) empty))
(test (eval (parse '{/ 5 2}) empty empty) (v*s (numV 5/2) empty))
(test (eval (parse '{eq? 5 2}) empty empty) (v*s (booleanV false) empty))
(test (eval (parse '{<= 5 2}) empty empty) (v*s (booleanV false) empty))
(test (eval (parse '{new-array 2 5}) empty empty) 
      (v*s (arrayV 0 2) (list (cell 0 (numV 5)) (cell 1 (numV 5)))))

(test (parse '{new-array 5 10}) (newArrayC (numC 5) (numC 10)))

(test (allocate test-store 2 (numV 5)) (n*s 4 (list (cell 0 (numV 5)) 
                                                    (cell 1 (numV 6)) 
                                                    (cell 2 (booleanV true)) 
                                                    (cell 3 (numV 8))
                                                     (cell 4 (numV 5))
                                                      (cell 5 (numV 5)))))

(test (update-store 2 (numV 100) test-store) 
      (list (cell 0 (numV 5)) 
            (cell 1 (numV 6)) 
            (cell 2 (numV 100)) 
            (cell 3 (numV 8))))

(test (fetch 3 test-store) (numV 8)) 
(test (get-last-location test-store) 3)
(test (eval (parse '{new-array 2 8}) empty 
            (list (cell 0 (numV 5)) 
                  (cell 1 (numV 5)))) 
      (v*s (arrayV 2 2) 
           (list (cell 0 (numV 5)) 
                 (cell 1 (numV 5)) 
                 (cell 2 (numV 8)) 
                 (cell 3 (numV 8)))))

(test (eval (parse '{p <- {new-array 5 10}}) test-binding test-store-2) 
      (v*s
       (arrayV 7 5)
       (list
        (cell 0 (numV 5))
        (cell 1 (numV 6))
        (cell 2 (booleanV #t))
        (cell 3 (numV 8))
        (cell 4 (arrayV 7 5))
        (cell 5 (numV 5))
        (cell 6 (numV 0))
        (cell 7 (numV 10))
        (cell 8 (numV 10))
        (cell 9 (numV 10))
        (cell 10 (numV 10))
        (cell 11 (numV 10)))))

(test/exn (eval (parse '{ref p [15]}) test-binding test-store-array) "Not")
(test (eval (parse '{ref p [4]}) test-binding test-store-array) 
      (v*s
       (numV 11)
       (list
        (cell 0 (numV 5))
        (cell 1 (numV 6))
        (cell 2 (booleanV #t))
        (cell 3 (numV 8))
        (cell 4 (arrayV 7 5))
        (cell 5 (numV 5))
        (cell 6 (numV 0))
        (cell 7 (numV 7))
        (cell 8 (numV 8))
        (cell 9 (numV 9))
        (cell 10 (numV 10))
        (cell 11 (numV 11)))))
(test/exn (eval (parse '{p [15] <- 50}) test-binding test-store-array) "Not")
(test (eval (parse '{p [4] <- 50}) test-binding test-store-array) 
      (v*s
       (numV 50)
       (list
        (cell 0 (numV 5))
        (cell 1 (numV 6))
        (cell 2 (booleanV #t))
        (cell 3 (numV 8))
        (cell 4 (arrayV 7 5))
        (cell 5 (numV 5))
        (cell 6 (numV 0))
        (cell 7 (numV 7))
        (cell 8 (numV 8))
        (cell 9 (numV 9))
        (cell 10 (numV 10))
        (cell 11 (numV 50)))))

(define list-exprs (list (numC 1) (numC 2) (numC 3) (newArrayC (numC 2) (numC 8))))
(define get-values-eval-store (list (cell 3 (numV 5))))
(test (get-values-eval list-exprs empty get-values-eval-store) 
      (lv*s (list (numV 1) 
                  (numV 2) 
                  (numV 3) 
                  (arrayV 4 2)) 
            (list (cell 3 (numV 5))
                  (cell 4 (numV 8)) 
                  (cell 5 (numV 8)))))

(test (update-store-bind-symbols (list 'a 'b) 
                                 (list (numV 1) (numV 2)) 
                                 empty 
                                 empty) 
      (env*s (list (bind 'b 1) 
                   (bind 'a 0)) 
             (list (cell 0 (numV 1)) 
                   (cell 1 (numV 2)))))

(test (update-store-bind-symbols (list 'a 'b) 
                                 (list (numV 1) 
                                       (numV 2)) 
                                 empty 
                                 (list (cell 2 (numV 5))))
      (env*s (list (bind 'b 4) 
                   (bind 'a 3)) 
             (list (cell 2 (numV 5)) 
                   (cell 3 (numV 1)) 
                   (cell 4 (numV 2)))))

(test (serialize (v*s (booleanV true) empty)) "true")
(test (serialize (v*s (booleanV false) empty)) "false")
(test (parse '{true}) (booleanC true))
(test (parse '{false}) (booleanC false))
(test (eval (parse '{{fn {seven} (seven)}
                     {{fn {minus} {fn {} (minus (+ 3 10) (* 2 3))}}
                      {fn {x y} (+ x (* -1 y))}}}) empty empty) 
      (v*s
       (numV 7)
       (list
        (cell 0 (closV (list 'x 'y) (binop '+ (idC 'x) (binop '* (numC -1) (idC 'y))) empty))
        (cell 1 (closV empty (appC (idC 'minus) (list (binop '+ (numC 3) (numC 10)) 
                                                    (binop '* (numC 2) (numC 3)))) 
                       (list (bind 'minus 0))))
        (cell 2 (numV 13))
        (cell 3 (numV 6)))))
(test (eval (parse '{eq? true false}) empty empty) (v*s (booleanV false) empty))
(test (eval (parse '{eq? true true}) empty empty) (v*s (booleanV true) empty))
(test (eval (parse '{eq? true 3}) empty empty) (v*s (booleanV false) empty))
(test (parse `3) (numC 3))
(test (parse `xxx) (idC 'xxx))
(test (parse '{if {<= 3 5} 3 5})
      (if (binop '<= (numC 3) (numC 5)) (numC 3) (numC 5)))
(test (parse '{if true 3 5}) (if (booleanC #t) (numC 3) (numC 5)))
(test (parse '{if false 3 5}) (if (booleanC #f) (numC 3) (numC 5)))
(test/exn (top-eval '{{fn {} 9} 17}) "update")

(test (append-list (list 'a) empty) (list 'a))
(test (get-list-exprC '{{z = {+ 9 14}} {y = 98}} empty) (list (binop '+ (numC 9) (numC 14)) (numC 98)))
(test (get-params '{{z = {+ 9 14}} {y = 98}} empty) (list 'z 'y))
(test (parse '{fn {x y} {+ x y}}) (lamC (list 'x 'y) (binop '+ (idC 'x) (idC 'y))))

(test (eval (parse'{+ 1 1}) empty empty) (v*s (numV 2) empty))
(test (get-params '{{z = {+ 9 14}} {y = 98}} empty) (list 'z 'y))
(test (get-params '{} empty) empty)
(test (eval (parse '{eq? true {if {<= 5 3} 5 true}}) empty empty) (v*s (booleanV true) empty))
(test (parse `true) (booleanC true))
(test (eval (parse `true) empty empty) (v*s (booleanV true) empty))

(test (num<= (numV 5) (numV 10)) (booleanV true))
(test (num<= (numV 10) (numV 1)) (booleanV false))
(test/exn (num<= (numV 10) (booleanV true)) "one argument was not a number")
(test/exn (lookup 'a empty) "name")
(test/exn (fetch -5 empty) "name")
(test/exn (update-store -5 (numV 5) empty) "index")

(test (numeq? (booleanV true) (booleanV true)) (booleanV true))
(test (numeq? (booleanV false) (booleanV true)) (booleanV false))
(test (numeq? (numV 7) (numV 8)) (booleanV false))
(test (numeq? (numV 7) (numV 7)) (booleanV true))
(test (numeq? (numV 7) (booleanV true)) (booleanV false))
(test (parse '{eq? 2 true}) (binop 'eq? (numC 2) (booleanC #t)))
(test/exn (parse '{eq? 2 if}) "bad")
(test (eval (parse '{eq? 3 5}) empty empty) (v*s (booleanV #f) empty))
(test (parse '{if true 3 5}) (if (booleanC #t) (numC 3) (numC 5)))
(test (eval (parse '{if true 3 5}) empty empty) (v*s (numV 3) empty))
(test (eval (parse '{if false 3 5}) empty empty) (v*s (numV 5) empty))
(test (serialize (v*s (numV 3) empty)) "3")
(test (serialize (v*s (booleanV true) empty)) "true")
(test (parse '{with {z = {+ 9 14}} {y = 98} {+ z y}})
      (appC (lamC (list 'z 'y) (binop '+ (idC 'z) (idC 'y)))
            (list (binop '+ (numC 9) (numC 14)) (numC 98))))
(test (parse `true) (booleanC true))
(test/exn (parse '{fn {x x} 3}) "dup")
(test (top-eval `1) "1")
(test (parse '{{+ 3 5} {<= 5 3}}) (appC (binop '+ (numC 3) (numC 5)) 
                                        (list (binop '<= (numC 5) (numC 3)))))
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
(test/exn (eval (if (binop '+ (numC 3) (numC 3)) (numC 5) (numC 10)) empty empty) "if:")

(test (check-if-reserved-symbol '<=) true)
(test (check-if-reserved-symbol 'if) true)
(test (check-if-reserved-symbol 'eq?) true)
(test (check-if-reserved-symbol 'true) true)
(test (check-if-reserved-symbol 'false) true)
(test (check-if-reserved-symbol 'with) true)
(test (check-if-reserved-symbol 'fn) true)
(test (check-if-reserved-symbol '<-) true)
(test (check-if-reserved-symbol '=) true)
(test (check-if-reserved-symbol 'new-array) true)
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
(test (check-if-reserved-symbol-excluding-true-false '<-) true)
(test (check-if-reserved-symbol-excluding-true-false '=) true)
(test (check-if-reserved-symbol-excluding-true-false 'new-array) true)

(test (serialize (v*s (closV empty (numC 3) empty) empty)) "#<procedure>")
(test/exn (parse '{with {z = {fn {} 3}} {z = 9} {z}}) "dups")
(test (top-eval '{(fn (minus) (minus 8 5)) (fn (a b) (+ a (* -1 b)))}) "3")
(test (top-eval '{{fn {multi-larg} {multi-larg 1 5 4 2}} 
                  {fn {a b c d} {if {<= a b} {if {<= c d} 
                                                 {+ b d} 
                                                 {+ b c}}                        
                                    {if {<= c d} 
                                        {+ a d} 
                                        {+ a c}}}}}) "9")

(test (top-eval '{new-array 5 3}) "#<array>") 

(test (eval (parse '{+ x y}) 
            (list (bind 'x 1) (bind 'y 0)) 
            (list (cell 0 (numV 10)) (cell 1 (numV 7)))) 
      (v*s (numV 17) (list (cell 0 (numV 10)) (cell 1 (numV 7)))))

(test (top-eval '{{fn {seven} (seven)}
                     {{fn {minus} {fn {} (minus (+ 3 10) (* 2 3))}}
                      {fn {x y} (+ x (* -1 y))}}}) "7") 

(test (eval (parse '{begin {+ 3 5} {* 9 8} {/ 5 8} p}) 
            (list (bind 'p 8)) 
            (list (cell 8 (numV 5))))
      (v*s (numV 5) 
           (list (cell 8 (numV 5)))))
      
(test (eval (parse '{begin {new-array 2 5} {* 9 8} {/ 5 8} p}) 
            (list (bind 'p 8)) (list (cell 8 (numV 5))))
      (v*s (numV 5) (list (cell 8 (numV 5)) 
                          (cell 9 (numV 5)) 
                          (cell 10 (numV 5)))))

(test (eval (parse '{eq? p p}) (list (bind 'p 4) (bind 'x 0)) (list
        (cell 0 (numV 5))
        (cell 1 (numV 6))
        (cell 2 (booleanV #t))
        (cell 3 (numV 8))
        (cell 4 (arrayV 7 5))
        (cell 5 (numV 5))
        (cell 6 (numV 0))
        (cell 7 (numV 7))
        (cell 8 (numV 8))
        (cell 9 (numV 9))
        (cell 10 (numV 10))
        (cell 11 (numV 11)))) 
      
      (v*s
       (booleanV #t)
       (list
        (cell 0 (numV 5))
        (cell 1 (numV 6))
        (cell 2 (booleanV #t))
        (cell 3 (numV 8))
        (cell 4 (arrayV 7 5))
        (cell 5 (numV 5))
        (cell 6 (numV 0))
        (cell 7 (numV 7))
        (cell 8 (numV 8))
        (cell 9 (numV 9))
        (cell 10 (numV 10))
        (cell 11 (numV 11)))))

(test (eval (parse '{{fn {seven} (seven)}
                     {{fn {minus} {fn {} (minus (+ 3 10) (* 2 3))}}
                      {fn {x y} (+ {x <- 20} (* -1 y))}}}) empty empty)
      (v*s
       (numV 14)
       (list
        (cell 0 (closV (list 'x 'y) 
                       (binop '+ (mutateC 'x (numC 20)) (binop '* (numC -1) (idC 'y))) empty))
        (cell 1 (closV empty (appC (idC 'minus) 
                                 (list (binop '+ (numC 3) (numC 10)) 
                                       (binop '* (numC 2) (numC 3)))) 
                       (list (bind 'minus 0))))
        (cell 2 (numV 20))
        (cell 3 (numV 6)))))
      
(test (eval (parse '{{new-array 50 5} [15] <- -100}) 
            (list (bind 'p 4) (bind 'x 0)) 
            (list
             (cell 0 (numV 5))
             (cell 1 (numV 6))
             (cell 2 (booleanV #t))
             (cell 3 (numV 8))
             (cell 4 (arrayV 7 5))
             (cell 5 (numV 5))
             (cell 6 (numV 0))
             (cell 7 (numV 7))
             (cell 8 (numV 8))
             (cell 9 (numV 9))
             (cell 10 (numV 10))
             (cell 11 (numV 11)))) (v*s
                                    (numV -100)
                                    (list
                                     (cell 0 (numV 5))
                                     (cell 1 (numV 6))
                                     (cell 2 (booleanV #t))
                                     (cell 3 (numV 8))
                                     (cell 4 (arrayV 7 5))
                                     (cell 5 (numV 5))
                                     (cell 6 (numV 0))
                                     (cell 7 (numV 7))
                                     (cell 8 (numV 8))
                                     (cell 9 (numV 9))
                                     (cell 10 (numV 10))
                                     (cell 11 (numV 11))
                                     (cell 12 (numV 5))
                                     (cell 13 (numV 5))
                                     (cell 14 (numV 5))
                                     (cell 15 (numV 5))
                                     (cell 16 (numV 5))
                                     (cell 17 (numV 5))
                                     (cell 18 (numV 5))
                                     (cell 19 (numV 5))
                                     (cell 20 (numV 5))
                                     (cell 21 (numV 5))
                                     (cell 22 (numV 5))
                                     (cell 23 (numV 5))
                                     (cell 24 (numV 5))
                                     (cell 25 (numV 5))
                                     (cell 26 (numV 5))
                                     (cell 27 (numV -100))
                                     (cell 28 (numV 5))
                                     (cell 29 (numV 5))
                                     (cell 30 (numV 5))
                                     (cell 31 (numV 5))
                                     (cell 32 (numV 5))
                                     (cell 33 (numV 5))
                                     (cell 34 (numV 5))
                                     (cell 35 (numV 5))
                                     (cell 36 (numV 5))
                                     (cell 37 (numV 5))
                                     (cell 38 (numV 5))
                                     (cell 39 (numV 5))
                                     (cell 40 (numV 5))
                                     (cell 41 (numV 5))
                                     (cell 42 (numV 5))
                                     (cell 43 (numV 5))
                                     (cell 44 (numV 5))
                                     (cell 45 (numV 5))
                                     (cell 46 (numV 5))
                                     (cell 47 (numV 5))
                                     (cell 48 (numV 5))
                                     (cell 49 (numV 5))
                                     (cell 50 (numV 5))
                                     (cell 51 (numV 5))
                                     (cell 52 (numV 5))
                                     (cell 53 (numV 5))
                                     (cell 54 (numV 5))
                                     (cell 55 (numV 5))
                                     (cell 56 (numV 5))
                                     (cell 57 (numV 5))
                                     (cell 58 (numV 5))
                                     (cell 59 (numV 5))
                                     (cell 60 (numV 5))
                                     (cell 61 (numV 5)))))
; In order test case


(test (top-eval '{{fn {a size} {with {in-order = 123}
                         {temp = {fn {a size cur last fun}
                             {if {eq? size cur} true        
                                 {if {<= {ref a[last]} {- {ref a[cur]} 1}}
                                     {fun a size {+ 1 cur} {+ 1 last} fun}
                                     false}}}}
                         {begin {in-order <- temp}
                         {in-order a size 1 0 in-order}}}} {with {a = {new-array 5 0}}
                                                                 {begin {a[0] <- 1}
                                                                        {a[1] <- 12}
                                                                        {a[2] <- 13}
                                                                        {a[3] <- 14}
                                                                        {a[4] <- 15}
                                                                        a}} 5}) "true")

(test (top-eval '{{fn {a size} {with {in-order = 123}
                         {temp = {fn {a size cur last fun}
                             {if {eq? size cur} true        
                                 {if {<= {ref a[last]} {- {ref a[cur]} 1}}
                                     {fun a size {+ 1 cur} {+ 1 last} fun}
                                     false}}}}
                         {begin {in-order <- temp}
                         {in-order a size 1 0 in-order}}}} {with {a = {new-array 3 0}}
                                                                 {begin {a[0] <- 1}
                                                                        {a[1] <- 12}
                                                                        {a[2] <- 13}
                                                                        a}} 3}) "true")

(test (top-eval '{{fn {a size} {with {in-order = 123}
                         {temp = {fn {a size cur last fun}
                             {if {eq? size cur} true        
                                 {if {<= {ref a[last]} {- {ref a[cur]} 1}}
                                     {fun a size {+ 1 cur} {+ 1 last} fun}
                                     false}}}}
                         {begin {in-order <- temp}
                         {in-order a size 1 0 in-order}}}} {with {a = {new-array 5 0}}
                                                                 {begin {a[0] <- 1}
                                                                        {a[1] <- 1}
                                                                        {a[2] <- 13}
                                                                        {a[3] <- 14}
                                                                        {a[4] <- 15}
                                                                        a}} 5}) "false")
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

(test (top-eval '{{fn {function} {function 6}} {fn {x} {- x 1}}})  "5")


#;( Stephen Play Ground
            
{fn {guard body} {with {f = 12345}
                       {g = {fn {a b fun} {{if a {begin b {fun a b fun}} 'done}}}}
                       {begin {f <- g}
                              {f a b f}}}}
            
            
            

{{fn {while} {fn {} {while {<= x 5} {x <- {+ x 1}}}}}{fn {guard body} {with {f = 12345}
                                     {g = {fn {a b fun} {{if a {begin body {new-array 1 x} {fun a b fun}} 'done}}}}
                                     {begin {f <- g}
                                            {f a b f}}}}}
            
            
            
      (eval (parse '{{fn {while} {fn {} {while {<= x 5} {x <- {+ x 1}}}}}{fn {guard body} {with {f = 12345}
                                     {g = {fn {a b fun} {{if a {begin body {new-array 1 x} {fun a b fun}} 'done}}}}
                                     {begin {f <- g}
                                            {f a b f}}}}}) empty empty)      
            
     (eval (parse '{fn {guard body} {with {f = 12345}
                                     {g = {fn {a b fun} {{if a {begin body {new-array 1 x} {fun a b fun}} 'done}}}}
                                     {begin {f <- g}
                                            {f a b f}}}} {<= x 5} {x <- {+ x 1}}) empty empty)         
     
     (eval (parse '{fn {guard body} {with {f = 12345}
                                     {g = {fn {a b fun} {{if a {begin body {new-array 1 x} {fun a b fun}} 'done}}}}
                                     {begin {f <- g}
                                            {f a b f}}} {<= x 5} {x <- {+ x 1}}}) (list (bind 'x 0)) (list (cell 0 (numV 0))))
     
     '{fn {a b} {with {while = 123}
                       {temp = {fn {guard body fun} 
                                   {if guard 
                                       {begin body {fun guard body fun}}
                                       {true}}}}
                       {junkVariable = while <- temp}
                       {while a b while}}}
                  
     (parse '{fn {a b} {with {while = 123}
                       {temp = {fn {guard body fun} 
                                   {if guard 
                                       {begin body {fun guard body fun}}
                                       {true}}}}
                       {junkVariableWhile = {while <- temp}}
                       {while a b while}}})
     
     '{fn {a size} {with {inorder = 123}
                         {temp = {fn {a size cur last fun}
                             {if {<= {ref a[last]} {ref a[cur]}}
                                 {fun  a size {+ 1 cur} {+ 1 last} fun}
                                 {false}}}}
                         {junkVaiableInorder = {inorder <- temp}}
                         {inorder a s 1 0 fun}}}
                                             
)
