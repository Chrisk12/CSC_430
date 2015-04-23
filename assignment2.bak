#lang plai-typed

; 3.8 Defining ArithC from Chap 3
(define-type ArithC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : ArithC)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

; 3.9 Defining eval for the ArithC 
(define (eval [a : ArithC] [fds : (listof FunDefC)]) : number
  (type-case ArithC a
    [numC (n) n]
    [idC (_) (error 'interp "shouldn't get here")]
    [appC (f a) (local ([define fd (get-fundef f fds)])
                  (eval (subst a (fdC-arg fd) (fdC-body fd)) fds))]
    
    [plusC (l r) (+ (eval l fds) (eval r fds))]
    [multC (l r) (* (eval l fds) (eval r fds))]))

(test (eval (numC 3) empty) 3)
(test (eval (plusC (numC 3) (numC 3)) empty) 6)
(test (eval (multC (numC 3) (numC 3)) empty) 9)

; 3.11 Defining a parser

(define (parse [s : s-expression]) : ArithC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusC (parse (second sl)) (parse (third sl)))]
         [(*) (multC (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input :(")]))



; 3.12 Accepts and S express and call parser and then the eval functions
(define (parse-eval [s : s-expression]) : number
  (eval (parse s) empty))

(test (parse-eval '{+ {* 1 2} {+ 2 3}}) 7)
(test (parse-eval '{+ 3 {+ {* 1 2} {+ 2 3}}}) 10)
(test/exn (parse-eval '{+ 3 {+ {* 1 "d"} {+ 2 3}}}) "invalid input :(")

(test (parse-eval '{+ {* 1 2} {+ 2 3}}) 7)
(test/exn (parse-eval '{% 3 {+ {* 1 2} {+ 2 3}}}) "invalid input :(")
(test/exn (parse-eval '{+ {% 1 2} {^ 2 3}}) "invalid input :(")
(test/exn (parse-eval '{+ 3 3 3 3 3 3}) "invalid input :(")
(test/exn (parse-eval '{+}) "invalid input :(")

; Assignment 2 Code
#;(define (top-eval [s : s-expression] [fun-sexps : (listof s-expression)])  : number  
  (eval (parse s) (map parse-fundef fun-sexps)))

; A function definition
(define-type FunDefC
  [fdC (name : symbol) (arg : symbol) (body : ArithC)])

; Defining substition
(define (subst [what : ArithC] [for : symbol] [in : ArithC]) : ArithC
  (type-case ArithC in
    [numC (n) in]
    [idC (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appC (f a) (appC f (subst what for a))]
    [plusC (l r) (plusC (subst what for l)
                        (subst what for r))]
    [multC (l r) (multC (subst what for l)
                        (subst what for r))]))

; Defining get function
(define (get-fundef [n : symbol] [fds : (listof FunDefC)]) : FunDefC
  (cond
    [(empty? fds) (error 'get-fundef "reference to undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdC-name (first fds))) (first fds)]
                   [else (get-fundef n (rest fds))])]))