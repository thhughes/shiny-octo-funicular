#lang plai-typed

#|
; Example tests from the thing online:
(define p1 '(+ 3 4))

(test (run p1) (numV 7))
(test/exn (run '(+ x 4)) "unbound")
|#

 ;; Things to remember:
 #|System needs to handle: 
 # (+ <Expr> <Expr>)                   ;; Addition
 # (* <Expr> <Expr>)                   ;; Multiplication
 # (- <Expr> <Expr>)                   ;; Subtraction
 # id                                  ;; Symbol other than: + - * fun if0 with
 # (fun (id ...) <Expr>)               ;; This is lambda
 # (if0 <Expr> <Expr> <Expr>)          ;; if0 : conditional : (if this is 0)(do this)(else)
 # (with ((id <Expr>) ...) <Expr>)     ;; This is a let synonym
 # (<Expr> <Expr> ...)                 ;; Not sure what this does:
 |#



;; Tests;;
(test (run '(+ 3 4)) (numV 7))
(test (run '(- 4 2)) (numV 2))
(test (run '(* 2 4)) (numV 8))
(test (run '(+ (- (* 1 2) (* 1 1)) 1)) (numV 2))

(test (run '(if0 (* 1 0) 1 2)) (numV 1))
(test (run '(+ 2 (with ((x 3)(y 4)) (+ x (* x y))))) (numV 17))
(test (run '(with ((x 3)(y 4)) (+ x (* z y)))) (numV 3))

;; Test single parameter things
(test (run '(with ([f (fun (x) (* x x))] (f 3)))) (numV 9))     ;; Result
;; Test multiple parameter things
(test (run '(with ([f (fun (x y) (* y x))](f 3 5)))) (numV 15)) ;; Result
(test (run '((fun (x) 5))) 5)                                   ;; I belive this should be legal 

(test/exn (run '(with ((x 3)(y 4)) (+ x (* z y)))) "unbound")
(test/exn (run '(+ 2 (with ((x 3)(x 4)) (+ x (* x y))))) "multiple")
(test/exn (run '(+ (fun (x) x) 5)) "type")
