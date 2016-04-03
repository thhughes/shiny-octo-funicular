#lang plai-typed

;--------------------------------------------------------------------------------
;; Tests

; Tests;; 14 Total
"Testing simple math and embedded functions"
(test (run '(+ 3 4)) (numV 7))
(test (run '(- 4 2)) (numV 2))
(test (run '(* 2 4)) (numV 8))
(test (run '(+ (- (* 1 2) (* 1 1)) 1)) (numV 2))

"Testing if statements"
(test (run '(if0 (* 1 0) 1 2)) (numV 1))

"Test single parameter functions"
(test (run '(with ([f (fun (x) (* x x))]) (f 3))) (numV 9))

"Test multiple parameter functions"
(test (run '(with ([f (fun (x y) (* y x))])(f 3 5))) (numV 15))

"Testing functions being declared and run simultaneously"
(test (run '((fun (x) 5) 5)) (numV 5))                                   

"Testing multiple functions being called in functions"
(test (run '(with ([f (fun (x y) (* x y))][g (fun (z) (* z z))]) (f 1 (g 2)))) (numV 4))

"Testing multiple declerations in with"
(test (run '(with ([x 1][y 2]) (+ x y))) (numV 3))

"Testing Static Scoping"
(test (run '(with ([x 1]) (with ([f (fun (y) (+ y x))]) (with ([x 2]) (f 10))))) (numV 11))


"Testing Errors"
"- Unbound by Undeclared Variable"
(test/exn (run '(with ((x 3)(y 4)) (+ x (* z y)))) "unbound")

"- Multiple by multiple definition"
(test/exn (run '(+ 2 (with ((x 3)(x 4)) (+ x (* x 1))))) "multiple")

"- Type by improper passing to function"
(test/exn (run '(+ (fun (x) x) 5)) "type")

