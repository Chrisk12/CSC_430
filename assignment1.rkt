#lang plai-typed

; Ex 2.3.3 Calculates the total profit from a show from the number of people
(define (total-profit [numAttendees : number]) : number
  (- (- (* 5 numAttendees) (* .5 numAttendees)) 20))

(test (total-profit 0) -20)
(test (total-profit 100) 430)
(test (total-profit 2) -11)

;defining pi to be used for the next problem
(define PI 3.14)
; Ex 3.3.3 Calculates the area of a cylinder

(define (area-cylinder [r : number] [h : number]) : number
  (+ (* (* (* 2 PI) r) h) (* 2 (* PI (* r r)))))
  
(test (area-cylinder 5 10) (* 150 PI))
(test (area-cylinder 10 5) 942)
(test (area-cylinder 2 3) (* 20 PI))
(test (area-cylinder 3 4) (* 42 PI))

; 2.2
; Represent a writing implement
(define-type Writer
  ; ink volume in ml, how-full a number in the range 0.0 to 1.0
  [pen (ink-volume : number) (how-full : number)]
  ; length in cm
  [pencil (length : number)])

; Calulcates how far a pen and pencil can write.
(define (how-far-to-write [writer : Writer]) : number
   (type-case Writer writer
   [pen (ink full) (* 150 (* ink full))]
   [pencil (length)  (* length 56)]))

(test (how-far-to-write (pen 10 0)) 0)
(test (how-far-to-write (pen 5 1)) 750)
(test (how-far-to-write (pencil 0)) 0)
(test (how-far-to-write (pencil 1)) 56)

; 2.3 Define the Polynomial types.
(define-type Polynomial
  [linear (a : number) (b : number)]
  [quadratic (a : number) (b : number) (c : number)])

(define (eval [poly : Polynomial] [x : number]) : number
  (type-case Polynomial poly
    [linear (a b) (+ (* a x) b)]
    [quadratic (a b c) (+ (+ (+ a (* x x)) (* b x)) c)]))

(test (eval (linear 5 10) 3) 25)
(test (eval (quadratic 1 2 3) 4) 28)
(test (eval (quadratic 5 10 15) 20) 620)

; 2.4 Computes the Derivative of a Polynomial
(define (derivative [poly : Polynomial]) : Polynomial
    (type-case Polynomial poly
    [linear (a b) (linear 0 a)]
    [quadratic (a b c) (linear (* 2 a) b)]))
  
(test (derivative (linear 5 10)) (linear 0 5))
(test (derivative (quadratic 1 2 3)) (linear 2 2))
(test (derivative (quadratic 5 10 15)) (linear 10 10))

; 2.5 Defining a binary tree =================ASK==================
(define-type BTree
  [leaf (sym : symbol)]
  [node (left : BTree) (right : BTree)])

(define t1 (leaf 'left))
(define t2 (leaf 'right))
(define t3 (leaf 'stephen))
(define t4 (node t1 t2))
(define t5 (node t3 t4))

; 2.6 Mirroring a Binary Tree
(define (mirror [tree : BTree]) : BTree
   (type-case BTree tree
     [leaf (leav) (leaf leav)]
     [node (left right) (node (mirror right) (mirror left))]))

(test (mirror t1) (leaf 'left))
(test (mirror t4) (node t2 t1))
(test (mirror t5) (node (node t2 t1) t3))


; 2.7 Calulates the minimum depth of a tree
(define (min-depth [root : BTree]) : number
  (type-case BTree root
    [leaf (leav) 0]
    [node (left right) (+ 1 (min (min-depth left) (min-depth right)))]))

(test (min-depth t1) 0)
(test (min-depth t4) 1)
(test (min-depth t5) 1)