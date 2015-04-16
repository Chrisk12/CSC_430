#lang plai-typed

; 1.6 AutoTabbing
#;(define (show-example b)
  (begin
    ( "magnitude of sum of elements: ~s\n"
            (array-map magnitude (array-axis-fold (my-fft b) 0 + 0)))
    
    (printf "sum of magnitude of elements: ~s\n"
            (array-axis-fold (array-map magnitude (my-fft b)) 0 + 0))
    
    (plot (points (in-array (array->plottable (my-fft b))))
          #:y-max 1500
          #:x-max 1024
          #:height 300)))
#;printf

; 1.7

#;((define (srl:map-append func lst)
  (if (null? lst)
      lst
      (append (func (car lst))
              (srl:map-append func (cdr lst))))))

; Lab 2 Exercises

; 3 Actual Brain Exercise

; 3.1 Creates a string in reverse of the items in a list.
(define (rev-str-app [l : (listof string)]) : string
  (cond [(empty? l) ""]
  [else
   (string-append (rev-str-app (rest l)) (first l))]))

(test (rev-str-app (list "ball" "juice" "frog")) "frogjuiceball")
(test (rev-str-app empty) "")

; 3.2 Defines a representation for a processor
(define-type Processor
  [Intel (i : number)]
  [AMD (i : number)]
  [ARM (i : number)])

; 3.3 Takes a list of prcessors and returns a list containing only Intels
(define intels (Intel 5))
(define l (list intels (AMD 6) (ARM 10)))

(define (onlyIntels [processList : (listof Processor)]) : (listof Processor)
  (cond [(empty? processList) empty]
        [else 
         (type-case Processor (first processList)
           [Intel(i) (cons (first processList) (onlyIntels (rest processList)))]
           [AMD(i) (onlyIntels (rest processList))]
           [ARM(i) (onlyIntels (rest processList))])]))

(test (onlyIntels l) (list intels))
(test (onlyIntels empty) empty)

; 3.4 Takes a list of prcessors and returns a list containing only AMD

(define (onlyAMDs [processList : (listof Processor)]) : (listof Processor)
  (cond [(empty? processList) empty]
        [else 
         (type-case Processor (first processList)
           [Intel(i) (onlyAMDs (rest processList))]
           [AMD(i) (cons (first processList) (onlyAMDs (rest processList)))]
           [ARM(i) (onlyAMDs (rest processList))])]))

(test (onlyAMDs l) (list (AMD 6)))
(test (onlyAMDs empty) empty)

; 3.5 Generalizes 3.4 and 3.3. consumes a list of processors and a particular processor 
; predicate and returns a list of only those elements.

(define (onlyThese [l : (listof Processor)] [f : (Processor -> boolean)]) : (listof Processor)
  (cond [(empty? l) empty]
        [(f (first l)) (cons (first l) (onlyThese (rest l) f))]
        [else (onlyThese (rest l) f)]))

(test (onlyThese l AMD?) (list (AMD 6)))
(test (onlyThese l Intel?) (list intels))
(test (onlyThese empty AMD?) empty)

(define l2 (list "d" "e" "f"))
(define l1 (list "a" "b" "c"))
; 3.6 Appends two list together
(define (my-append [l1 : (listof 'a)] [l2 : (listof 'a)]) : (listof 'a)
  (cond [(empty? l1) l2]
        [(empty? l2) l1]
        [else (cons (first l1) (my-append (rest l1) l2))]))

(test (my-append empty empty) empty)
(test (my-append (list "1" "2" "3") empty) (list "1" "2" "3"))
(test (my-append empty (list "1" "2" "3")) (list "1" "2" "3"))
(test (my-append (list "a" "b" "c") (list "d" "e" "f")) (list "a" "b" "c" "d" "e" "f"))

; 3.7 Drops the first n numbers from a list
(define (my-drop [l : (listof number)] [n : number]) : (listof number) 
  (cond [(empty? l) l]
        [(<= n 0) l] 
        [else (my-drop (rest l) (- n 1))]))

(test (my-drop empty 5) empty)
(test (my-drop (list 1 2 3) 0) (list 1 2 3))
(test (my-drop (list 1 2 3) 1) (list 2 3))

; 3.8 Defining ArithC from Chap 3
(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

; 3.9 Defining eval for the ArithC 
(define (eval [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n]
    [plusC (l r) (+ (eval l) (eval r))]
    [multC (l r) (* (eval l) (eval r))]))

(test (eval (numC 3)) 3)
(test (eval (plusC (numC 3) (numC 3))) 6)
(test (eval (multC (numC 3) (numC 3))) 9)

; 3.10 Counts how many numbers are in an ArithC
(define (num-nums [n : ArithC]) : number
  (type-case ArithC n
    [numC (i) 1]
    [plusC (i j) (+ (num-nums i) (num-nums j))]
    [multC (i j ) (+ (num-nums i) (num-nums j))]))

(test (num-nums (numC 3)) 1)
(test (num-nums (plusC (numC 3) (numC 3))) 2)
(test (num-nums (plusC (numC 3) (multC (numC 3) (numC 3)))) 3)

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
    [else (error 'parse "invalid input")]))



; 3.12 Accepts and S express and call parser and then the eval functions
(define (parse-eval [s : s-expression]) : number
  (eval (parse s)))

(test (parse-eval '(+ (* 1 2) (+ 2 3))) 7)
(test (parse-eval '(+ 3 (+ (* 1 2) (+ 2 3)))) 10)
(test (parse-eval '(^ (^ 1 2) (^ 2 3))) 0)
(test (parse-eval '(+ 3 pie)) 0)





; THIS MAY NOT BE NEEDED ANYMORE?
(define-type ArithS
  [numS (n : number)]
  [plusS (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])


(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]))