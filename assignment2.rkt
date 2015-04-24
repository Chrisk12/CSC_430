#lang plai-typed
(require plai-typed/s-exp-match)

; Defines Experc
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [if0 (t : ExprC) (iff : ExprC) (ffi : ExprC)]
  [binop (sym : symbol) (l : ExprC) (r : ExprC)])

; Defines the Interpreter / OUR EVAL
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst a (fdC-arg fd) (fdC-body fd)) fds))]
    [binop (s l r) (cond [(eq? s '+) (+ (interp l fds) (interp r fds))]
                         [(eq? s '*) (* (interp l fds) (interp r fds))])]
    [if0 (t iff ffi) (cond [(<= (interp t fds) 0) (interp iff fds)]
                           [else (interp ffi fds)]
                           )]))

; Searches through a list of symbols to see if a symbol is in the list.
(define (check-in-list [s : symbol] [lst : (listof symbol)]) : boolean
  (cond [(empty? lst) false]
        [(symbol=? s (first lst)) true]
        [else (check-in-list s (rest lst))]))

(test (check-in-list 'a (list 'b 'c)) false)
(test (check-in-list 'a (list 'b 'a)) false)

; Defines the substition
(define (subst [what : ExprC] [for : (listof symbol)] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in];THIS IS BROKEN NOW IDC
    [idC (s) (cond [(symbol=? s (first for)) what]
                   [else in])]
    [appC (f a) (appC f (subst what for a))]
    [if0 (t iff fii) (error 'sust "error")]
    [binop (t iff fii) (cond [(eq? t '+) (error 'subst "hi")])]))
 
; Get Funds ===EXPLAIN BETTER===
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))

; 3.11 Defining a parser
(define (parse [s : s-expression]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
   
    [(s-exp-match? '{SYMBOL ANY ANY} s)
     (binop (s-exp->symbol (first (s-exp->list s))) (parse (second (s-exp->list s))) (parse (third (s-exp->list s))))]

    [(s-exp-match? '{if0 ANY ANY ANY} s) (if0
                                          (parse (second (s-exp->list s)))
                                          (parse (third (s-exp->list s)))
                                          (parse (fourth (s-exp->list s))))]
    [else (error 'parse "invalid input :(")]))


 
(define (parse-eval [s : s-expression]) : number
  (interp (parse s) empty))

#;(test (parse-eval '{if0 3 10 {+ 2 3}}) 5)
#;(test (parse-eval '{+ 5 5}) 10)

; Defines a datatype for functions to be represented as
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol)) (body : ExprC)])

(define (create-list [s : s-expression]) : (listof symbol)
  (map s-exp->symbol (s-exp->list (third (s-exp->list s)))))

  
(define (parse-fundef [s : s-expression]) : FunDefC
  (cond
    [(s-exp-match? '{fn ANY ANY ANY} s)
     (fdC (s-exp->symbol (second (s-exp->list s))) (create-list s) (parse (fourth (s-exp->list s))))]
    [else (error 'parse "wrong arity")]))
 

(test (interp (parse '{f 1})
            (list (parse-fundef '{fn f {x} {+ 1 1}}))) 3)
(test (interp (parse '{+ {f} {f}})
            (list (parse-fundef '{fn f {} 5}))) 10)
(test/exn (interp (parse '{f 1})
                (list (parse-fundef '{fn f {x y} {+ x y}}))) "wrong arity")
;======================================
#;(define (top-eval [s : s-expression] [fun-sexps : (listof s-expression)]) : number
    (eval (parse s) (map parse-fundef fun-sexps)))


