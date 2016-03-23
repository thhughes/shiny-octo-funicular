#lang plai-typed



;; list-of-numbers -> number
;; Sums the numbers in a list of numbers
(define (sum [lon : (listof number)]) : number
	(foldl + 0 lon)
	)

;; List-of-numbers -> number
;; Sums the negative numbers in a list of numbers
(define (sum-neg [lon : (listof number)]) : number
	(foldl + 0 
		(filter (lambda (x) (< x 0))
                        lon)
		)
	)

;; List-of-numbers -> list-of-numbers 
;; Removes negative numbers and replaces them with 0. 
(define (raise [lon : (listof number)]) : (listof number)
  (map (lambda (x) (cond[(> x 0 ) x]
                        [(< x 0 ) 0]))
       lon)
  )


;; list-of-any-type -> list-of-input-type
;; removes odd indexed values
(define (alternating some-list)
  (cond [(empty? some-list)(list )]
        [(cons? some-list) (append (list (first some-list)) (cond [(> (length some-list) 2) (alternating (rest (rest some-list)))]
                                                                  [else (list )]
                                                                  ))]
        )
  )




(define-type Scores
  [scores (midterm-exam : number) (final-exam : number)(course-grade : string)]
  )


(define-type Students
  [undergrad (name : string) (grades : Scores) (grad-year : number)]
  [graduate (name : string) (grades : Scores) (deg-prog : string)]
  )

;; empty-score -> score
;; takes in a score without a final grade and returns a score with a final grade.
;; NOTE: This assumes grade >= 85 == high pass, 85 > grade >= 65 == pass, grade < 65 fail
(define (grade-score [some-score : Scores]) : Scores
  (let ([midterm (scores-midterm-exam some-score)]
        [final (scores-final-exam some-score)])
    (let ([average (/ (+ midterm final) 2)])
      (scores midterm final (cond [(>= average 85) "high pass"]
                                  [(and (>= average 65)(< average 85)) "pass"]
                                  [(< average 65) "fail"]
                                  [else "error"])
            )
    ))
  )

;; list-of-students-without-final-grades -> list-of-students-with-final-grades
;; Takes in an empty-list-of-students (meaning they have no final grades) and returns a list-of-students (with final grades)
(define (assign-grades [los : (listof Students)]) : (listof Students)
  (append (map (lambda (x) (undergrad (undergrad-name x)(grade-score (undergrad-grades x))(undergrad-grad-year x)))
               (filter undergrad? los))
          (map (lambda (x) (graduate (graduate-name x)(grade-score (graduate-grades x))(graduate-deg-prog x)))
               (filter graduate? los)
          )
  ))



;; list-of-students -> boolean
;; Determines if all the PhD students in the given list pass
(define (all-phd-pass? (los : (listof Students))) : boolean
  (foldl (lambda (a b) (and a b))
         true
         (map (lambda (grade) (cond[(string=? grade "high pass") true] ;; Maps list of score-grades to true and false list
                            [(string=? grade "pass") true]
                            [(string=? grade "fail") false]))
              (map (lambda (stud) (scores-course-grade (graduate-grades stud))) ;; maps los to list of score-grades
                   (filter (lambda (grad-stud) (string=? (graduate-deg-prog grad-stud) "PhD")) (filter graduate? los)))
              )
  )
  )


;; list-of-numbers split-number-> list-of-numbers-cut-at-value
;; splits a list on the first instance of the <split-number> and returns the list up to that point
(define (split-list-at [lon : (listof number)][split-num : number]) : (listof number)
  (cond [(empty? lon)(list )]
        [(cons? lon) (append (list (first lon))
                             (cond [(= 1 (length lon))(list )]
                                   [(= split-num (second lon)) (list )]
                                   [else (split-list-at (rest lon) split-num)])
                             )]
        )
  )



;; list-of-numbers -> number
;; returns the average of all of the positive values from the 0th' place in the lon to the first instance of
;;     -999. If -999 is not in the list then the average is of all the positive numbers in the whole list. 
(define (rainfall [lon : (listof number)]) : number
  (let* ([clean-list (split-list-at (filter (lambda (x) (or (> x 0)(= -999 x))) lon) -999)])
    (/ (foldl + 0 clean-list)
       (length clean-list))
    )
  )
            


(define-type CartItem
     [item (name : string) (price : number)])



;; list-of-items, item-name -> number
;; finds the sum price of a specific item from a list of items.  
(define (gross-price [items : (listof CartItem)]) : number
  (foldl + 0 (map (lambda (itm) (item-price itm)) items))
  )

;; number -> number
;; Takes in the total price of the number of shoes and returns the amount of money
;;    to take off the final bill.
(define (discount-shoes [gross-price : number]) : number
  (cond [(>= gross-price 100)(* gross-price -0.2)]
        [else 0])
        )
;; number -> number
;; Takes in the total number of hats and returns a discount for the final bill.
(define (discount-hats [num-hats : number]) : number
  (cond [(>= num-hats 2) -10]
        [else 0])
        )
  

;; list-of-items -> number
;; Takes in a list of CartItems and returns the price of the cart after discounts. 
(define (checkout [items : (listof CartItem)]) : number
  (let*([shoes-only (filter (lambda (itm) (string=? "shoes" (item-name itm))) items)]
        [hats-only (filter (lambda (itm) (string=? "hat" (item-name itm))) items)]
        [num-hats (length hats-only)]
        [gross-price-shoes (gross-price shoes-only)]
        [shoe-adjustment (discount-shoes gross-price-shoes)]
        [hat-adjustment (discount-hats num-hats)]
        [total-cost (gross-price items)])
    (foldl + 0 (list total-cost hat-adjustment shoe-adjustment))
    )
  )
















;; sum and sum neg test
(test (sum (list 1 2 3 4 5 6 )) 21)
(test (sum (list 1 -1 2 -2 3 -3)) 0)
(test (sum-neg (list 1 -1 2 -2 3 -3)) -6)
(test (sum-neg (list 1 2 3 4 5 6)) 0 )
(test (sum-neg (list -1 -2 -3 -4 -5 -6)) -21)

;; raise test
(test (raise (list 69 -10 54)) (list 69 0 54))
(test (raise (list -1 -2 -3)) (list 0 0 0))

;; alternating test
(test (alternating  (list "Hello" "World" "!!")) (list "Hello" "!!"))
(test (alternating  (list 1 2 3 4 5 6 7 8))(list 1 3 5 7))

; Score Data:
;; Filled Data
(define score-1 (scores 0 0 "fail"))
(define score-2 (scores 100 100 "high pass"))
(define score-3 (scores 80 90 "high pass"))
(define score-4 (scores 70 70 "pass"))
(define score-5 (scores 60 70 "pass"))
(define score-6 (scores 60 60 "fail"))
;; Empty Data
(define empty-score-1 (scores 0 0 ""))
(define empty-score-2 (scores 100 100 ""))
(define empty-score-3 (scores 80 90 ""))
(define empty-score-4 (scores 70 70 ""))
(define empty-score-5 (scores 60 70 ""))
(define empty-score-6 (scores 60 60 ""))

; Student Data:
;; Filled Students
(define student-1 (undergrad "John" score-2 2016))
(define student-2 (undergrad "Jacob" score-1 2017))
(define student-3 (undergrad "Jingle" score-4 2018))
(define student-4 (graduate "Kate" score-3 "PhD"))
(define student-5 (graduate "Samie" score-5 "PhD"))
(define student-6 (graduate "Liz" score-6 "MS"))
(define student-7 (graduate "Troy" score-1 "PhD"))
(define student-8 (graduate "Guillermo" score-6 "PhD"))

;; Empty Students
(define empty-student-1 (undergrad "John" empty-score-2 2016))
(define empty-student-2 (undergrad "Jacob" empty-score-1 2017))
(define empty-student-3 (undergrad "Jingle" empty-score-4 2018))
(define empty-student-4 (graduate "Kate" empty-score-3 "PhD"))
(define empty-student-5 (graduate "Samie" empty-score-5 "PhD"))
(define empty-student-6 (graduate "Liz" empty-score-6 "MS"))


; list of students
(define all-phd (list student-4 student-5 student-7 student-8))
(define all-grad (list student-4 student-5 student-6))
(define all-grad-empty (list empty-student-4 empty-student-5 empty-student-6))
(define all-undergrad (list student-1 student-2 student-3))
(define all-undergrad-empty (list empty-student-1 empty-student-2 empty-student-3))
(define mix-1 (list student-1 student-2 student-3 student-4 student-5 student-6))
(define mix-1-empty (list empty-student-1 empty-student-2 empty-student-3 empty-student-4 empty-student-5 empty-student-6))
(define mix-2 (list student-1 student-4 student-5))
(define mix-3 (list student-1 student-4 student-6))


;; Testing : grade-score ::
(test (grade-score empty-score-1) score-1)
(test (grade-score empty-score-2) score-2)
(test (grade-score empty-score-3) score-3)
(test (grade-score empty-score-4) score-4)
(test (grade-score empty-score-5) score-5)
(test (grade-score empty-score-6) score-6)

;; Number 7 test
(test (assign-grades mix-1-empty) mix-1)


;; Number 8 test
(test (all-phd-pass? mix-1) true)
(test (all-phd-pass? mix-2) true)
(test (all-phd-pass? mix-3) true)
(test (all-phd-pass? all-phd) false)

;; Number 9 test
; -- split-list-at test:
(test (split-list-at (list 1 2 3 4 5 6 7 8 9 0) 5) (list 1 2 3 4))
(test (split-list-at (list 1 2 3 4 5 6 7 8 9 0) 10) (list 1 2 3 4 5 6 7 8 9 0))
(test (split-list-at (list 1 2 3 4 5 6 10 7 8 9 0) 10) (list 1 2 3 4 5 6))
; -- rainfall test
(test (rainfall (list 1 2 3 4 5 6)) 3.5)
(test (rainfall (list 1 2 3 -4 -5 -6)) 2)
(test (rainfall (list 1 2 3 -4 -5 -6 -999 999)) 2)


;; Number 10 data
(define cart-1 (list (item "shoes" 25)(item "shoes" 85)(item "bag" 50)(item "hat" 15)))
(define cart-2 (list (item "hat" 5)(item "hat" 5)(item "hat" 5)(item "hat" 5)))
(define cart-3 (list (item "shoes" 25)(item "shoes" 25)(item "shoes" 25)(item "shoes" 25)))
;; Number 10 tests
(test (checkout cart-1) 153)
(test (checkout cart-2) 10)
(test (checkout cart-3) 80)


