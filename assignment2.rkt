#lang plai-typed
(require plai-typed/s-exp-match)

; Defines a datatype for functions to be represented as
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ExprC)])

; Defines Experc
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ExprC)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [if0 (t : ExprC) (iff : ExprC) (ffi : ExprC)])

; Defines the Interpreter / OUR EVAL
(define (interp [e : ExprC] [fds : (listof FunDefC)]) : number
  (type-case ExprC e
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (interp (subst a (fdC-arg fd) (fdC-body fd)) fds))]
    
    [plusC (l r) (+ (interp l fds) (interp r fds))]
    [multC (l r) (* (interp l fds) (interp r fds))]
    [if0 (t iff ffi) (cond [(<= (interp t fds) 0) (interp iff fds)]
                           [else (interp ffi fds)]
                            )]))

; Defines the substition 
(define (subst [what : ExprC] [for : symbol] [in : ExprC]) : ExprC
  (type-case ExprC in
    [numC (n) in]
    [idC (s) (cond [(symbol=? s for) what]
                   [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]
    [if0 (t iff fii) (error 'sust "error")]))

 
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
    [(s-exp-match? '{+ ANY ANY} s)
     (plusC (parse (second (s-exp->list s))) (parse (third (s-exp->list s))))]
    [(s-exp-match? '{* ANY ANY} s)
     (multC (parse (second (s-exp->list s))) (parse (third (s-exp->list s))))]
    [(s-exp-match? '{if0 ANY ANY ANY} s) (if0 
                                          (parse (second (s-exp->list s))) 
                                          (parse (third (s-exp->list s)))
                                          (parse (fourth (s-exp->list s))))]
    [else (error 'parse "invalid input :(")]))

(define (parse-eval [s : s-expression]) : number
  (interp (parse s) empty))
 
(test/exn (parse-eval '{if0  2 10 {+ 2 3}}) "I found the if")



;======================================
#;(define (top-eval [s : s-expression] [fun-sexps : (listof s-expression)])  : number  
  (eval (parse s) (map parse-fundef fun-sexps)))
 
