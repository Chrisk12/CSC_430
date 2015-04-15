#lang plai-typed
; Section 2

(+ 3 4)
(+ 4 5)

(test (* 4 13) 52)

; Section 3

true
false
(or false true)

(test (not true) false)
(test (or false true) true)

; Adds two number together
(define (add-nums [a : number] [b : number]) : number
  (+ a b))

(add-nums 5 10)
(test (add-nums 5 10) 15)
(test (add-nums -5 5) 0)
(test (add-nums -5 -5) -10)

; Ex. 2.2.2 Converts dollars to euros
(define (dollar->euro [a : number]) : number
  (* .92 a))

(test (dollar->euro 1) .92)
(test (dollar->euro 0) 0)
(test (dollar->euro -1) -.92)

; Ex. 2.3.2 Sums the number of pennys, nickles, dimes and quarters.
(define (sum-coins [pennys : number] [nickles : number] [dimes : number] [quarters : number]) : number
  (+ (+ (+ (* 1 pennys) (* 5 nickles)) (* 10 dimes)) (* 25 quarters)))

(test (sum-coins 1 0 0 0) 1)
(test (sum-coins 0 1 0 0) 5)
(test (sum-coins 0 0 1 0) 10)
(test (sum-coins 0 0 0 1) 25)
(test (sum-coins 1 1 1 1 ) 41)

; Ex 4.4.1 Takes a deposit amount and calculates interest the money earns in a year.
(define (interest [deposit : number]) : number
  (cond
     [(<= deposit 1000) (* deposit .04)]
     [(<= deposit 5000) (* deposit .045)]
     [(> deposit 5000) (* deposit .05)]
     ))

(test (interest 100) 4)
(test (interest 2000) 90)
(test (interest 10000) 500)


; Ex 4.4.4 Calculates the number of roots in quadratic formula.
(define (how-many [a : number] [b : number] [c : number]) : number
  (cond
    [( > (* b b) (* a (* c 4))) 2]
    [( = (* b b) (* a (* c 4))) 1]
    [( < (* b b) (* a (* c 4))) 0]))

(test (how-many 1 0 -1) 2)
(test (how-many 2 4 2) 1)
(test (how-many 1 1 1) 0)
(test (how-many 1 0 1) 0)
(test (how-many 1 5 1) 2)

; Ex 5.1.4 Returns how many solutions a quadratic equations has
(define (what-kind [a : number] [b : number] [c : number]) : symbol
  (cond
     [( = a 0) 'degenerate]
     [( > (* b b) (* a (* c 4))) 'two]
     [( = (* b b) (* a (* c 4))) 'one]
     [( < (* b b) (* a (* c 4))) 'none]))

(test (what-kind 1 2 1) 'one)
(test (what-kind 2 4 2) 'one)
(test (what-kind 1 0 -1) 'two)
(test (what-kind 1 5 1) 'two)
(test (what-kind 0 5 1) 'degenerate)
(test (what-kind 2 4 3) 'none)

; Defines a compound data type that represents a desk
(define-type Office-Furniture
  (desk [width : number] [height : number] [depth : number])
  (bookshelves [depth : number] [num-shelves : number] [shelves-width : number]))

; Example of a desk
(define d (desk 10 20 30))

; Calculates the area of a desk
(define (furniture-footprint o) : number
  (type-case Office-Furniture o
    [desk (w h d) (* w d)]
    [bookshelves (depth num-shelves shelves-width) (* depth shelves-width)]))

(test (furniture-footprint d) 300)
(test (furniture-footprint (desk 3 2 1)) 3)

; Example of a bookshelves
(define b (bookshelves 5 6 7))

(test (furniture-footprint b) 35)
(test (furniture-footprint (bookshelves 3 1 2)) 6)