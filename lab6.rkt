#lang plai-typed

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
  [newArrayC (size : ExprC) [value : ExprC]]
  [lamC (arg : (listof symbol)) (body : ExprC)]
  [refC (sym : ExprC) (location : ExprC)]
  [setArrayC (sym : ExprC) (location : ExprC) (expression : ExprC)]
  [mutateC (sym : symbol) (value : ExprC)]
  [beginC (elist : (listof ExprC))  (sym : ExprC)])

; defines a list of symbols
(define sym-list (list 'a 'b 'c 'd 'e 'f 'g 'h))

; defines a list of expression
(define exp-list (list '{numC} 
                       '{booleanC} 
                       '{eq?} 
                       '{appC} 
                       '{binop} 
                       '{lamC}))


(define (randomSymbol [s : (listof symbol)]) : symbol
  (get-element s (modulo (random 3) (length s))))

; helper function that returns the randon symbol to randomSymbol
(define (get-element [s : (listof 'a)] [n : number]) : 'a
  (cond [(eq? n 0) (create-s-expr (first s) 
                                  (randomSymbol sym-list) 
                                  (randomSymbol sym-list))]
        [else (get-element (rest s) (sub1 n))]))

; Joins pieces into an s expression
(define (create-s-expr [s : s-expression] 
                       [sym1 : symbol] 
                       [sym2 : symbol]) : s-expression
  )

(randomSymbol sym-list)
(randomSymbol sym-list)
(randomSymbol sym-list)


; takes in a list of symbols and returns a random expression
(define (randomExpression [s : (listof s-expression)]) : s-expression
  (get-element s (modulo (random 3) (length s))))

(randomExpression exp-list)
(randomExpression exp-list)
(randomExpression exp-list)