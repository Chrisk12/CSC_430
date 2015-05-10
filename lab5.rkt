#lang plai-typed

;Warm Up Applies a function to a value
(define one (lambda (f) (lambda (v) (f v))))

(test ((one sub1) 8) 7)
(test ((one sub1) 7) 6)

; 2.1 Two Function that applies the function
; to the value twice
(define two (lambda (f) (lambda (v) (f (f v)))))

(test ((two sub1) 8) 6)
(test ((two sub1) 7) 5)

; 2.2 defines a function that returns the argument
(define zero (lambda (f) (lambda (v) v)))
(test ((zero sub1)  1234) 1234)
(test ((zero sub1)  123) 123)

; 2.3 defines a function called add1
(define add1 (lambda (f) (lambda (g) (lambda (v) (g ((f g) v))))))
(test (((add1 zero) sub1) 0) -1)
(test (((add1 two) sub1) 0) -3)

; 2.4 defines and add function that takes one function and applies it
; the number of time to the second function
(define add (lambda (f1) (lambda (f2) (lambda (f) 
                                        (lambda (v) ((f1 f) ((f2 f) v)))))))
(test ((((add zero) zero) sub1) 0) 0)
(test ((((add zero) one) sub1) 0) -1)

; 2.5 Take two arguments and returns the first one
(define tru (lambda (f1) (lambda (f2) f1)))
(test ((tru 3) 4) 3)
(test ((tru 5) 7) 5)

; 2.6 Take two arguments and returns the first one
(define fals (lambda (f1) (lambda (f2) f2)))
(test ((fals 3) 4) 4)
(test ((fals 5) 7) 7)

; 2.7 defines a function that that acts like an if statement
(define if (lambda (f1) (lambda (iff) (lambda (ffi) ((f1 iff) ffi)))))
(test (((if fals) 4 ) 5) 5)
(test (((if tru) 4 ) 5) 4)
