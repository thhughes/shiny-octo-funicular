#lang plai-typed

; Example tests from the thing online:
(define p1 '(+ 3 4))

(test (run p1) (numV 7))
(test/exn (run '(+ x 4)) "unbound")


;; Things to remember:
#|System needs to handle: 
# (+ <Expr> <Expr>)                   ;; Addition
# (* <Expr> <Expr>)                   ;; Multiplication
# (- <Expr> <Expr>)                   ;; Subtraction
# id                                  ;; Symbol other than: + - * fun if0 with
# (fun (id ...) <Expr>)               ;; 
# (if0 <Expr> <Expr> <Expr>)          ;; if0 : conditional : (if this is 0)(do this)(else)
# (with ((id <Expr>) ...) <Expr>)     ;; This is lambda expression
# (<Expr> <Expr> ...)                 ;; Not sure what this does:
|#

(define p-1 '(+ 3 4))
(define s-1 '(- 4 2))
(define m-1 '(* 2 4))

(define psm-1 '(+ (- (* 1 2) (* 1 1)) 1))

(define cond-1 '(if0 (* 1 0)(1)(2)))
(define with-1 '(+ 2 ( with ((x 3)(y 4)) (+ x (* x y)))))
(define with-2 '( with ((x 3)(y 4)) (+ x (* z y))))              ;; Unbound
(define with-3 '(+ 2 ( with ((x 3)(x 4)) (+ x (* x y)))))        ;; multiple




;; Tests;;
(test (run p-1) 7)
(test (run s-1) 2)
(test (run m-1) 8)

(test (run psm-1) 2)

(test (run cond-1) 1)
(test (run with-1) 17)

(test/enx (run with-2) "unbound")
(test/enx (run with-3) "multiple")
