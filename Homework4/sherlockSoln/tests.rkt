;; 1
;; Test for Pass by Reference or Pass by Value:
;; If this test is passed, then it will be pass by reference
;; If this test is failed, then it will be pass by value
(define testPBR '(with ([x 1][y 2])
                       (seq (with ([f (fun (z) (set z 10))])
                                  (f y))
                            (+ x y))))
(test (run testPBR) (numV 11))

;; 2
;; Test for Static Scope or Dynamic Scoping:
;; If this test is passed, then it will be a statically scoped language
;; If this test is failed, then it will be a dynamically scoped language
(define testScope '(with ([x 1])
                         (with ([foo (fun (y) (+ x y))])
                               (with ([x 2])
                                     (foo 10)))))

(test (run testScope) (numV 11))

;;3
;; Test Lazy or Eager evaluation
;; If this test passes, then it is eager evaluation
;; If this test fails, then it is an lazy evaluation
(define testLazy '(with ([x 1])
                        (with ([simpMath (fun (x) (+ 1 2))])
                              (with ([mutator (fun (y) (set x 100))])
                                    (seq (simpMath (mutator 0))
                                         x)))))
(test (run testLazy) (numV 100))


;;4
;; Test for Mutation Information:
;; These tests are designed to figure out how mutation is handled for boxes.

;; Testing if x -> box and y -> x, if you setbox y, x should change too.
(define mutationTestOne '(with ([x (box 4)])
                               (with ([y x])
                                     (seq (setbox y 10)
                                          (unbox x)))))
;; If this acts like our language this will pass.
(test (run mutationTestOne) (numV 10))

;;5
;; Testing if x -> box and y -> x, if you setbox y, x should change too.
(define mutationTestOneRetY '(with ([x (box 4)])
                               (with ([y x])
                                     (seq (setbox y 10)
                                          (unbox y)))))
;; If this acts like our language this will pass.
(test (run mutationTestOne) (numV 10))

;;6
;; Test if mutation is saved from one function to another
(define mutationTestTwo '(with ([b (box 3)])
                               (unbox (seq (setbox b 4)
                                           b))))
;; If this acts like our language this will pass
(test (run mutationTestTwo) (numV 4))

;;7
;; Test that setbox is actually setting b. 
(define testSetboxReturnsCorrectly '(with ([b (box 5)])
                                          (setbox b 100)))
;; This test should return 100 to ensure that the box is actually being set
(test (run testSetboxReturnsCorrectly) (numV 100))


8
;; This is testing to see how mutation changes from one scope to another.
(define mutationTestingWithScopeChange '(with ([x (box 10)])
                                              (seq ( with([y 10])
                                                         (setbox x 100))
                                                   (unbox x))))
;; This test should return 100 to ensure that mutation holds across scope.
(test (run mutationTestingWithScopeChange) (numV 100))
