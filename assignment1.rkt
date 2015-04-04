#lang plai-typed

; Ex 2.3.3 Calculates the total profit from a show from the number of people
(define (total-profit [numAttendees : number]) : number
  (- (- (* 5 numAttendees) (* .5 numAttendees)) 20))

(test (total-profit 0) -20)
(test (total-profit 100) 430)
(test (total-profit 2) -11)

; Ex 3.3.5 Calculates the height of a rocket after a given amount of time

(define (height [a : number] [t : number]) : number
  (* t( * a (* .5 t))))

(test (height 10 10) 500)
(test (height 5 10) 250)
(test (height 10 5) 125)