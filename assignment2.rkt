#lang plai-typed
(require plai-typed/s-exp-match)
(print-only-errors true)

; Defines Exprc
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : (listof ExprC))]
  [ifleq0 (t : ExprC) (iff : ExprC) (ffi : ExprC)]
  [binop (sym : symbol) (l : ExprC) (r : ExprC)])

; Defines the Interpreter. Takes in an ExprC and a list of functions
; and then evalutes the ExperC recursively and returns a number.
(define (eval [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'eval "shouldn't get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (cond [(not (= (length (fdC-arg fd)) (length a)))
                         (error 'eval "wrong arity")]
                        [else 
                         (eval  (subst a (fdC-arg fd) (fdC-body fd)) fds)]))]
    
    [binop (s l r) ((get-binop s) (eval l fds) (eval r fds))]
    [ifleq0 (t iff ffi) (cond [(<= (eval t fds) 0) (eval iff fds)]
                              [else (eval ffi fds)])]))

; Checks to see if a symbol is a binary operator and returns true if it is
(define (check-if-binop [s : symbol]) : boolean
  (cond [(eq? s '+) true]
        [(eq? s '*) true]
        [(eq? s '-) true]
        [(eq? s '/) true]
        [else false]))

; Checks to see if a symbol is a reserved symbol and returns true if it is
(define (check-if-reserved-symbol [s : symbol]) : boolean
  (cond [(eq? s '+) true]
        [(eq? s '*) true]
        [(eq? s '-) true]
        [(eq? s '/) true]
        [(eq? s 'ifleq0) true]
        [else false]))

; Gets the function associated with the binary operator and returns that 
; operator
(define (get-binop [s : symbol]) : (number number -> number)
  (cond [(eq? s '+) +]
        [(eq? s '*) *]
        [(eq? s '-) -]
        [(eq? s '/) /]
        [else (error 'get-binop "NOT A BINOP")]))

; Searches through a list of symbols to see if a symbol s is in the list.
; returns true if it is, false otherwise.
(define (check-in-list [s : symbol] [lst : (listof symbol)]) : boolean
  (cond [(empty? lst) false]
        [(symbol=? s (first lst)) true]
        [else (check-in-list s (rest lst))]))

; Defines the substition function from the book with a modification to it to
; allow for variable number of parameters
(define (subst [what : (listof ExprC)] [for : (listof symbol)] [in :  ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond [(look-for-symbol s for) (find-experc-to-replace s for what)]
                   [else in])]
    [appC (f a) (appC f  (do-sub-for-arguments what for in a))]
    [ifleq0 (t iff fii) (ifleq0 (subst what for t)
                                (subst what for iff)
                                (subst what for fii))]
    [binop (s iff fii) (binop s (subst what for iff) (subst what for fii))]))

; Preforms the substition on all the arguments of a function and returns
; a list of the substituded variable in the ExprC
(define (do-sub-for-arguments [what : (listof ExprC)] 
                              [for : (listof symbol)] 
                              [in :  ExprC] 
                              [a : (listof ExprC)]) : (listof ExprC)
  (cond [(or (empty? a) (empty? for)) a]
        [else (cons 
               (subst what for (first a)) 
               (do-sub-for-arguments what for in (rest a)))]))

; Returns the ExperC for the symbol we are trying to find in look-for-symbol.
; The needle is the symbol we are looking for in the haystack.
(define (find-experc-to-replace [needle : symbol] 
                                [haystack : (listof symbol)] 
                                [value : (listof ExprC)]) : ExprC
  (cond [(symbol=? needle (first haystack)) (first value)]
        [else (find-experc-to-replace needle (rest haystack) (rest value))]))

; Tries to find the symbol in the symbol list for subst. 
; The needle is the symbol we are looking for in the haystack.
; returns true if the symbol was found.
(define (look-for-symbol [needle : symbol] [haystack : (listof symbol)]) : boolean
  (cond [(empty? haystack) false]
        [(symbol=? needle (first haystack)) true]
        [else (look-for-symbol needle (rest haystack))]))

; Gets the fundefC in fds associated with a symbol n and returns the FundefC
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond 
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

; 3.11 Defining a parser that takes an s expression and convert it to an ExperC. 
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) 
     (cond [(check-if-reserved-symbol (s-exp->symbol s)) 
            (error 'parse "invalid input :(")]
           [else (idC (s-exp->symbol s))])]
    [(s-exp-match? '{ifleq0 ANY ANY ANY} s) (ifleq0
                                             (parse (second (s-exp->list s)))
                                             (parse (third (s-exp->list s)))
                                             (parse (fourth (s-exp->list s))))]
    [(and (s-exp-symbol? (first (s-exp->list s)))
          (not(check-if-reserved-symbol (s-exp->symbol (first (s-exp->list s))))))
     (create-appC  s)]
    [(s-exp-match? '{SYMBOL ANY ANY} s)
     (cond [(test-of-operators s second) (error 'parse "invalid input :(")]
           [(test-of-operators s third) (error 'parse "invalid input :(")]
           [else 
            (binop (s-exp->symbol (first (s-exp->list s)))
                   (parse (second (s-exp->list s))) 
                   (parse (third (s-exp->list s))))])]
    [else (error 'parse "invalid input :(")]))

; Test to see if symbol is being used that is reserve symbol. Takes in an
; s expression and a position to operate on and then check if it is a
; reserve symbol.
(define (test-of-operators [s : s-expression] 
                           [position : ((listof 'a) -> 'a)]) : boolean
  (and (s-exp-symbol? (position (s-exp->list s))) 
       (check-if-reserved-symbol (s-exp->symbol (position (s-exp->list s)))))) 

;Creates and appC by using the map operator on an S expression
(define (create-appC [s : s-expression]) :  ExprC
  (appC (s-exp->symbol(first (s-exp->list s)))
        (map parse (rest (s-exp->list s)))))

; Defines a datatype for functions to be represented as
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol)) (body : ExprC)])

; Creates a List of symbols from an s-expresssion to be used in fdC
(define (create-list [s : s-expression]) : (listof symbol)
  (map s-exp->symbol (s-exp->list (third (s-exp->list s)))))

; Parses an  s expression and creates a funDef out of it
(define (parse-fundef [s : s-expression]) : FunDefC
  (cond
    [(s-exp-match? '{fn SYMBOL ANY ANY} s)
     (cond [(test-of-operators s second) (error 'parse "invalid input :(")]
           [else (fdC (s-exp->symbol (second (s-exp->list s)))
                      (create-list s)
                      (parse (fourth (s-exp->list s))))])]
    [else (error 'parse "Function is of the wrong type :(")]))

;Check to see if a list of symbols has duplicets.
(define (list-has-no-dups [l : (listof symbol)]) : boolean
  (cond [(empty? l) true]
        [else (check-dupes (first l) (rest l))]))

; Takes a symbol and a list and checkes whether that symbol is in the list.
; returns true if the symbol is not in the list.
(define (check-dupes [s : symbol] [l : (listof symbol)]) : boolean
  (cond [(empty? l) true]
        [else (and (and (not (eq? s (first l))) (check-dupes s (rest l)))
                   (list-has-no-dups (rest l)))]))

; Used for testing because of lab 2
(define (parse-eval [s : s-expression]) : number
  (eval (parse s) empty))

; Parses the s expression and the list of function expressions and then 
; calls eval
(define (top-eval [s : s-expression] [fun-sexps : (listof s-expression)]) : number
  (eval (parse s) (map parse-fundef fun-sexps)))

;===============TEST CASES=======================

(test (top-eval '{+ 1 1} empty) 2)
(test (top-eval '{ifleq0 {+ 1 1} 5 10} empty) 10)
(test (top-eval '{f 1} (list '{fn f {x} {+ x 1}})) 2)

(test (parse-eval '{+ {* 1 2} {+ 2 3}}) 7)
(test (parse-eval '{+ 3 {+ {* 1 2} {+ 2 3}}}) 10)
(test (parse-eval '{+ {* 1 2} {+ 2 3}}) 7)

(test (eval (parse '{f 1}) (list (parse-fundef '{fn p {x} {+ x 1}})
                                 (parse-fundef '{fn f {x} {+ x 1}}))) 2)

(test (eval (parse '{f 1}) (list (parse-fundef '{fn f {x} {+ x 1}}))) 2)
(test (eval (parse '{+ {f} {f}}) (list (parse-fundef '{fn f {} 5}))) 10)
(test/exn (eval (parse '{f 1}) (list (parse-fundef '{fn f {x y} {+ x y}})))
          "wrong arity")
(test/exn (parse-eval '{+}) "invalid input :(")
(test (eval (numC 1) empty) 1)

(test/exn (eval (idC 'a) empty) "shouldn't get here")
(test (eval (ifleq0 (numC 0) (numC 5) (numC 10)) empty) 5)
(test (eval (ifleq0 (numC 5) (numC 5) (numC 10)) empty) 10)
(test (parse '{ifleq0 2 3 4}) (ifleq0 (numC 2) (numC 3) (numC 4)))

(test (subst (list (numC 3)) (list 'x) (numC 3)) (numC 3))
(test (subst (list (numC 3)) (list 'x) (idC 'x)) (numC 3))
(test (subst (list (numC 3)) (list 'y) (idC 'x)) (idC 'x))
(test (subst (list (numC 5)) (list 'x) (appC 'f (list (numC 5))))
      (appC 'f (list (numC 5))))
(test (subst (list (numC 6)) (list 'x 'y) (ifleq0 (numC 3) (numC 5) (numC 10)))
      (ifleq0 (numC 3) (numC 5) (numC 10)))

(test (check-if-binop '+) true)
(test (check-if-binop '*) true)
(test (check-if-binop '-) true)
(test (check-if-binop '/) true)
(test (check-if-binop 'f) false)

(test (check-in-list 'd empty) false)
(test (check-in-list 'a (list 'b 'c)) false)
(test (check-in-list 'a (list 'b 'a)) true)

(test (get-binop '+) +)
(test (get-binop '*) *)
(test (get-binop '-) -)
(test (get-binop '/) /)
(test/exn (get-binop 'f) "NOT A BINOP")

(test/exn (parse-fundef '{f 0 1 5 2 4 8}) "Function is of the wrong type")
(test (parse-eval '{+ 5 5}) 10)

(test (list-has-no-dups (list 'a 'b 'c)) true)
(test (list-has-no-dups (list 'a 'a)) false)
(test/exn (get-fundef 'a empty) "reference to undefined function")

(test (eval (parse '{minus 8 5}) 
            (list (parse-fundef '{fn minus (x y) (+ x (* -1 y))}))) 3)
(test (eval (parse '{minus 5}) 
            (list (parse-fundef '{fn minus (y) (* -1 y)}))) -5)
(test (eval (parse '{seven}) 
            (list (parse-fundef '{fn seven () (minus (+ 3 10) (* 2 3))}) 
                  (parse-fundef '{fn minus (x y) (+ x (* -1 y))}))) 7)

(test (eval (parse '{twice 15}) 
            (list (parse-fundef '{fn realtwice (x) (+ x x)}) 
                  (parse-fundef '{fn twice (x) (realtwice x)}))) 30)
(test (parse-fundef '{fn p {x} {+ x 1}}) (fdC 'p (list 'x) (binop '+ (idC 'x) (numC 1))))
(test/exn (parse '{+ / 3}) "invalid input :(") 
(test/exn (parse '{+ 2 *}) "invalid input :(") 
(test/exn (parse '{+ - 3}) "invalid input :(") 
(test/exn (parse '{+ ifleq0 *}) "invalid input :(") 
(test/exn (parse '{+ + *}) "invalid input :(") 
(test/exn (parse-fundef '{fn + () 13}) "invalid input :(")
(test/exn (parse '{ifleq0}) "invalid input :(")
(test/exn (parse `ifleq0) "invalid input :(")
(test (parse-fundef '{fn twice (x) (realtwice x)}) (fdC 'twice (list 'x) (appC 'realtwice (list (idC 'x)))))
(test (parse-fundef '{fn minus (x y) (+ x (* -1 y))}) (fdC 'minus (list 'x 'y) (binop '+ (idC 'x) (binop '* (numC -1) (idC 'y)))))
(test (parse-fundef '{fn realtwice (x) (+ x x)}) (fdC 'realtwice (list 'x) (binop '+ (idC 'x) (idC 'x))))
   

#;(
(test (parse '1)
      (numC 1))

(test (parse `x)
      (idC 'x))

(test (parse '{+ 1 2})
      (binopC  '+ (numC 1) (numC 2)))

(test (parse '{* x 3})
      (binopC  '* (idC 'x) (numC 3)))

(test (parse '{% 2 3})
      (appC '% (list (numC 2) (numC 3))))

(test/exn (parse '{2 + 3})
          "invalid expression")

(test/exn (parse '{+})
          "invalid number of arguments")

(test/exn (parse '{+ / 3})
          "invalid usage of keyword")

(test/exn (parse '{ifleq0 1 2 3 4})
          "invalid usage of keyword")

;; parse-fundef tests
(test (parse-fundef '{fn five {} 5})
      (fdC 'five (list) (numC 5)))

(test (parse-fundef '{fn f {x} {+ x 5}})
      (fdC 'f (list 'x) (binopC '+ (idC 'x) (numC 5))))

(test (parse-fundef '{fn f {x y z} {+ {* z y} x}})
      (fdC 'f (list 'x 'y 'z)
           (binopC '+ (binopC '* (idC 'z) (idC 'y)) (idC 'x))))

(test/exn (parse-fundef '{not-function f {x} {+ x 5}})
          "invalid function definition")

(test/exn (parse-fundef '{fn + {} 13}) "invalid usage of keyword")

)