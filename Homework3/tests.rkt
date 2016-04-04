#lang Plai-typed

;; These are tests for the Mutation assignment
;; Goals of the assignment: Write tests to check the language
;                         : Your tests may be limited to mutation related things
;                         : peer review will only occur above the 10 line.

;;; Your tests should not:
;   - rely on the specific address locations in expected answers (because locations could be different)
;   - If you run something that returns a box it can be:
;     - (test (boxV? (run '(box 5))) true )

;;; Standard Error Messages:
;   - "unbound"  :: unbound identifier
;   - "multiple" :: same identifier multiple times in the same with or parameter list
;   - "expected" :: grammar violation
;   - "memory"   :: reference to unused memory/store location



;; Language Grammar:
;; <Expr> ::= number
 ;; ;; | (+ <Expr> <Expr>)
 ;; ;; | (* <Expr> <Expr>)
 ;; ;; | (- <Expr> <Expr>)
 ;; ;; | id                                ;; ID's or Variables
 ;; ;; | (fun (id ...) <Expr>)             ;; lambda
 ;; ;; | (if0 <Expr> <Expr> <Expr>)        ;; <if is 0> <do this> <else> 
 ;; ;; | (with ((id <Expr>) ... ) <Expr>)  ;; with in racket
 ;; ;; | (<Expr> <Expr> ...)               ;; Run Expressions
 ;; ;; | (box <Expr>)                      ;; Create box
 ;; ;; | (unbox <Expr>)                    ;; Get value from box
 ;; ;; | (setbox <Expr> <Expr>)            ;; Assign value in box
 ;; ;; | (setq <Expr> <Expr>)              ;; Begin Equivelant
 ;; ;; | (set id <Expr>)                   ;; Variable assignment


; Top Ten Tests:
"Test that mutation is passed across function calls"
(define mutated-math-of-box '(with ([b (box 1)])
                                  (+ (seq (setbox b (* 4 (unbox b)))
                                          (unbox (seq (setbox b 100)
                                                      b)))
                                     (seq (setbox b (- 10 (unbox b)))
                                          (unbox (seq (setbox b 100)
                                                      b)))
                                     ))) ;; Should return 200
(test (run mutated-math-of-box) (numV 200))


"Test that variables function and can be mutated"
(define basic-variable-test '(with ([x 1][y 3])
                                   (+ (seq (set x (* x 4))
                                           x) ;; 4
                                      (seq (set y (- y 2))
                                           (set y (+ 50 (set y 50))));;4
                                   )) ;;  104
(test (run basic-variable-test) (numV 104))


"Test that mutation will only occur in the evaluated branch of the if0 clause"
(define with-if-mutation-passing '(with ([x 0][b (box 1)])
                                        (if0 (+ x (unbox b))          ;; Will fail (0 + 1) > 0
                                             (seq (set x 9001)        ;; This should't get run
                                                  x)                  ;; x should stay at 0
                                             (seq (setbox b 0)        ;; changes b to 0
                                                  (+ x (unbox b)))))) ;; should be 0
(test (run with-if-mutation-passing) (numV 0))

"Test the scoping of mutation within with statements"
;; This tests to make sure that when the with scope changes the values that are edited in the
;; store stay edited, as the store is passed along.
(define with-scoping-mutation-1 '(with ([x 0])
                                       (seq (with ([x 4])
                                                  (set x 100))
                                            (set x (+ x x))
                                            x)))
(test (run with-scoping-mutation-1) (numV 200))

"Test the scoping of mutation within a function"
(define scoping-mutation-in-functions '(with ([x 4])
                                             (seq (with ([f (y) (set x y)])
                                                        (f 5))
                                                  x)))
(test (run scoping-mutation-in-functions)(numV 5))


;; TODO :: Create variable in internal scope and reference it in external
;;      :: This should error because the variable doesn't exist in the internal scope anymore (in the environment)


;; TODO :: Make a variable name the same as a paramater name?
;;      :: Low priority test, wuld be similar to last suite. 


;; TODO :: Test that ensures that mutation is passed from one side of the sequence to the other
;;      :: although I think that the first test in this list checks that too.


;; TODO :: Test to make sure that boxC is using the coorect store's in all it's calls
;;      :: If it were to use the interp-sto instead of the returned s-a then the mutations would stop existing.
;;      :: so this test needs to have a seq inside of a (box ...) that changes the scoped values of something else.


;; TODO :: Test that there is an order to addition subtraction etc, such that the mutation enforces an order we woudl expect.






; --------------------------------------------------------------------
; Other Tests








; ---------------------------------------------------------------------
; thoughts:
;; [appC (f a)
;;       (type-case Result (interp f env sto)
;;                  [v*s (v-f s-f)
;;                       (type-case Result (interp a env s-f)
;;                                  [v*s (v-a s-a)
;;                                       (let ([where (new-loc)])
;;                                         (interp (closV-body v-f)
;;                                                 (extend-env (bind (closV-arg v-f)
;;                                                                   where)
;;                                                             (closV-env v-f))
;;                                                 (override-store (cell where v-a) s-a)))])])]
;; What happens if the wrong stores and environments are passed along on this?
