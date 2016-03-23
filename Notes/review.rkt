#lang plai-typed


(define (string-list-length-fold [los : (listof string)]) : number 
  (foldl (lambda (str acc) (+ (string-length str) acc)) 0 los ))

;; foldl should be used as the iterator not recursive call
;; using functions that can be composed ('making small functions that can do good things')

(test (string-list-length-fold empty) 0)
(test (string-list-length-fold empty) 0)



;; Fold : <function> <Accumulator> <
(define (string-list-length-fold-map los)
  (foldl + 0
         (map string-length los)))

(define (string-list-length-fold-map-filter los)
  (foldl + 0 (filter (lambda (str-len) (> str-len 3))
                     (map string-length los))))
;; Filter :: keeps things that meet the condition, ignores them if they fail.


;; Define your own type: 
(define-type Room ;; This is a room type! 
  [lecture-hall (room-num : number) (capacity : number)  (type-of-chairs : string) ] ;; These are types of rooms! 
  [computer-lab (num-computers : number) (room-num : number) (computer-type : string)] ;; Almost like interfaces bridging objects
  [classroom (room-num : number) (capacity : number) (board-type : string) ]
  [office (room-num : number) (professor : string)])


(define kfisler-office (office 130 "Kathi Fisler")) ;; Constructor of a variant is it's name
(define fl320 (classroom 320 60 "whiteboard"))
(define sl115 (lecture-hall 115 120 "uncomfortable"))
(define sl120 (computer-lab 30 120 "dells"))


(define (get-room-nums lor)
  (cond [(empty? lor) empty]
        [(cons? lor)
         (cons ( let([first-room (first lor)])
                  (cond [(lecture-hall? (first lor)) (lecture-hall-room-num (first lor ))]
                        [(computer-lab? first-room) (computer-lab-room-num first-room)]
                        [(classroom? first-room)(classroom-room-num first-room)]
                        [(office? first-room)(office-room-num first-room)]
                        ))
               (get-room-nums (rest lor)))]))




(begin (display "hello world\n\r"))




(test (string-list-length-fold empty) 0)
(test (string-list-length-fold (list "fred" "ted")) 7)
(test (string-list-length-fold-map (list "fred" "ted")) 7)
(test (string-list-length-fold-map-filter (list "fred" "ted")) 4)

(test (get-room-nums (list kfisler-office fl320 sl115 sl120)) (list 130 320 115 120))
