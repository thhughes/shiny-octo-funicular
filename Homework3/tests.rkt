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



; --------------------------------------------------------------------
; Main Tests











; --------------------------------------------------------------------
; Other Tests

;; Basic Tests:
"Test making a box successfully creates a box"
(define making-box-works '(with ([b (box 1)])
                                (boxV? b)))
(test (run making-box-works) true)

"Test that making a box and unboxing it returns the value set to the box"
(test (run '(with ([b (box 1)])
                  (unbox b))) (numV 1))

"Test that mutating a box while unboxing it returns the correct box"
(test (run '(with ([b (box 1)])
                  (unbox (seq (setbox b 10)
                              b))))
      (numV 10))
"Test that mutating a box that doesn't exist errors"
(test/exn (run '(unbox b)) "unbound")

"Test that mutating within a different scope impacts the errors produced"
(test/exn (run '(with ([x 4])
                      (+ (seq (with ([b (box 6)])) 4)
                         (unbox b)))) "unbound")


"Test correct scoping and store passing in nested with statements"
(define nested-with-scoping-and-store-passing '(with ([b (box 1)])
                                                     (seq (with ([b (box 3)])
                                                                (setbox b 10000))
                                                          (unbox b))))
(test (run nested-with-scoping-and-store-passing) (numV 1))

"Test functions returning proper store"
(define functions-returning-proper-store '(with ([b (box 0)])
                                                (with ([f (fun (x) (setbox b (+ (unbox b) 10)))])
                                                      (unbox (seq (f 10)
                                                                  (f 10))))))
(test (run functions-returning-proper-store) (numV 20))


"Test Seq executes in the correct order"
(define seq-exec-order '(with ([b (box 4)])
                              (unbox (seq (setbox b (* (unbox b) 0))
                                          (setbox b (+ (unbox b) 1))))))
(test (run seq-exec-order) (numV 1))

"Addition carries store across sides"
(define addition-carries-sto '(with ([b (box 1)])
                                    (+ (seq (setbox b 2)
                                            b)
                                       (seq (setbox b 3)
                                            b))))
(test (run addition-carries-sto) (numV 5))

"Subtraction carries store across sides"
(define subtraction-carries-sto '(with ([b (box 1)])
                                       (* (seq (setbox b 2)
                                               b)
                                          (seq (setbox b 3)
                                               b))))
(test (run addition-carries-sto) (numV 6))


"Multiplication carries store across sides"
(define multiplication-carries-sto '(with ([b (box 1)])
                                          (- (seq (setbox b 3)
                                                  b)
                                             (seq (setbox b 2)
                                                  b))))
(test (run addition-carries-sto) (numV 1))


"Testing boxing a box"
(test (run '(with ([b (box 1)])
                  (with ([c (box b)])
                        (boxV? (unbox c))))) true)


"Test that variables function and can be mutated"
(define basic-variable-test '(with ([x 1][y 3])
                                   (+ (seq (set x (* x 4))
                                           x) ;; 4
                                      (seq (set y (- y 2))
                                           (set y (+ 50 (set y 50))));; 100
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
