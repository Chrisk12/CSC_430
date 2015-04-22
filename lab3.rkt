#lang plai-typed
map

; Takes in a function and list and applies the function to each element
; of the list twice.
(define (doublemap [f : ('a -> 'b)] [l : (listof 'a)]) : (listof 'b)
 (map f (map f l)))

(test (doublemap sub1 (list 1 2 3)) (list -1 0 1))
(test (doublemap add1 (list 1 2 3)) (list 3 4 5))

; Takes two lists and creates a list of list where each element of each 
; list is contains both elements. AKA zip function
(define (zip [l1 : (listof 'a)] [l2 : (listof 'a)]) : (listof (listof 'a))
  (cond [(empty? l1) empty]
        [else (cons (list (first l1) (first l2)) (zip (rest l1) (rest l2)))]))

(test (zip (list 1 2 3) (list 4 5 6)) (list (list 1 4) (list 2 5) (list 3 6))) 
(test (zip empty empty) empty)

; Consumes alist of numbers and does quicksort
(define (quicksortt [l : (listof number)]) : (listof number)
  (cond [(empty? l) empty]
        [else (append (quicksortt (getList <= (first l) (rest l ))) 
                      (cons (first l) (quicksortt (getList > (first l) (rest l)))))]))

; Defines a concatention function that will concatenate the 3 list together
(define (getList [f : (number number -> boolean)] [pivot : number] [l : (listof number)])
  (cond [(empty? l) empty]
        [(f (first l) pivot) (cons (first l) (getList f pivot (rest l)))]
        [else (getList f pivot (rest l))]))

(test (quicksortt (list 1 3 2 4)) (list 1 2 3 4))
(test (quicksortt empty) empty)
(test (quicksortt (list 1 2 )) (list 1 2))


  