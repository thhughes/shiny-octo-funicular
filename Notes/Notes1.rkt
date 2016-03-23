#lang plai-typed


(define-type ArithC
  [numC (n : number)]
  [plusC (l : ArithC)(r : ArithC)]
  [multC (l : ArithC)(r : ArithC)])

(numC 4)
(plusC (numC 3)(numC 0))

; interp : ArithC -> number
; NOTES:: By using the [a : ArithC] or the : number annotation you can do type checking
; interpreters an ArithC progrma into it's resulting answer
(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC (n) n ]
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    )
  )


(test (interp (numC 4)) 4)
(test (interp (plusC (numC 3)(numC 4)))
      7)



(define (parse [s : s-expression]) : ArithC
  (cond [(s-exp-number? s) (numC (s-exp->number s))]
        [(s-exp-list? s)
         (let ([sl (s-exp->list s)])
           (case (s-exp->symbol (first sl))
             [(+) (plusC (parse (second sl)) (parse (third sl))) ]
             [(*) ... ]
             ...)
           )
         ]
        )
  )




(test (parse '(+ 3 4)) (plusC (numC 3) (numC 4)))
(test (parse '5) (numC 5))
