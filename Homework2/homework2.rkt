#lang plai-typed

;; starter file for the extended basic interpreter assignment

;---------------------------------------------------------------------------------
;; surface syntax and parser : you should NOT need to edit this section

; type used to capture a with-binding
(define-type DefS
  [defS (name : symbol) (val : ExprS)])

; surface syntax for expressions
(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [appS (f : ExprS) (args : (listof ExprS))]
  [if0S (c : ExprS) (t : ExprS) (e : ExprS)]
  [funS (params : (listof symbol)) (body : ExprS)]
  [withS (bindings : (listof DefS)) (body : ExprS)]
  )

; parses s-expressions into surface syntax
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond [(s-exp-symbol? (first sl)) ;; built-in construct or calling function through an identifier
              (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(-) (bminusS (parse (second sl)) (parse (third sl)))]
                [(if0) (if0S (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(fun) (funS (map s-exp->symbol (s-exp->list (second sl))) 
                                (parse (third sl)))]
                [(with) (withS (map (lambda (b) 
                                      (let ([bl (s-exp->list b)])
                                        (defS (s-exp->symbol (first bl)) (parse (second bl)))))
                                    (s-exp->list (second sl)))
                               (parse (third sl)))]
                [else ;; must be a function call using function name
                 (appS (idS (s-exp->symbol (first sl)))
                       (map parse (rest sl)))])]
             [(s-exp-list? (first sl)) ;; function call with complex expression in function position
              (appS (parse (first sl))
                    (map parse (rest sl)))]
             [else (error 'parse "expected symbol or list after parenthesis")]))]
    [else (error 'parse "unexpected input format")]))
     
;---------------------------------------------------------------------------------
;; abstract syntax and desugar
     
(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (i : symbol)]
  [appC (f : ExprC) (arg : (listof ExprC))]
  [if0C (c : ExprC) (t : ExprC) (e : ExprC)]
  [funC (param : (listof symbol)) (body : ExprC)]
  )

;; desugar -- returning a default/dummy value so file can be run
;; Desugar With's into funC :: It's equivelant to a function that's being defined and is imediately called. 
(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
             [numS (n) (numC n)]
             [idS (s) (idC s)]
             [plusS (l r) (plusC (desugar l)(desugar r))]
             [multS (l r) (multC (desugar l)(desugar r))]
             [bminusS (l r) (plusC (desugar l) (multC (numC -1)
                                                      (desugar r)))]
             [if0S (c t e) (if0C (desugar c)(desugar t)(desugar e))]
             [appS (f arg) (appC (desugar f)(map desugar arg))]
             [funS (params body) (funC params (desugar body))]
             [withS (bindings body) (appC (funC (map defS-name bindings) (desugar body))
                                          (map desugar (map defS-val bindings)))]
             )
  )
  

  
;---------------------------------------------------------------------------------
;; output values

(define-type Value
  [numV (n : number)]
  [closV (arg : (listof symbol)) (body : ExprC) (env : Env)])

;---------------------------------------------------------------------------------
;; Environments

;; binding an identifier to a value
(define-type Binding
  [bind (name : symbol) (val : Value)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)


;---------------------------------------------------------------------------------
;; interp -- returning a default/dummy value so file can be run
(define (interp [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
             [numC (n) (numV n)]
             [idC (n) (lookup n env)]
             [plusC (l r) (numCMath '+ (interp l env) (interp r env))]
             [multC (l r) (numCMath '* (interp l env) (interp r env))]
             [if0C (c t e) (cond [(is-if0C-zero (interp c env))(interp t env)]
                                 [else (interp e env)])]
    ;; appC takes an f and arg: exprC and listof ExprC
    ;; We interp f to get the closure of what to do:
    ;; Then we have to evaluate that:
    ;; -- Interp the body with the environment being formed from the
    ;; -- Closure Environment + Bindings from: (closure(args)) bound to: (appC(args))
             [appC (f arg)(let ([closure (interp f env)])
                            (interp (closV-body (valid-closV closure))
                                    (append (new-env-for-interp (closV-arg (valid-closV closure))
                                                                arg
                                                                env)
                                            (closV-env (valid-closV closure)))))]
             [funC (param body)(closV param body env)]
             )
  )
;;(interp (closV-body f-val)
;;                                    (cons (bind (closV-arg f-val)
;;                                                (interp arg env))
;;                                          (closV-env f-val)))
;; (3 4 5) will choke on closV-body

;; Interp helpers:
;; valid-closV :: Value -> listof symbols or error
;; Checks if the thing is a closure and returns it if it is :: errors if not
(define (valid-closV [val : Value]) : Value
  (type-case Value val
              [numV (n) (error 'interp "got numV instead of closV")]
              [closV (arg body env) val]
    ))


;; new-env-for-interp :: listof symbol,passed Env, listof exprC -> listof binding
;; Takes in: closV args, environment, appC args
(define (new-env-for-interp [c-arg : (listof symbol)][a-arg : (listof ExprC)][env : Env]) : Env
  (cond [(= (length c-arg)(length a-arg))(error 'appC "incorrect number of args")]
        [else (map2 (lambda (c a) (bind c (interp a env))) c-arg a-arg)]
        )
  )



;; lookup symbols in environment:
(define (lookup [n : symbol][env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "Symbol was not found in the envirionment")]
    [else (let* ([envF (first env)]
                 [name (bind-name envF)]
                 [body (bind-val envF)])
            (cond [(symbol=? n name) body]
                  [else (lookup n (rest env))])
            )]
    ))


;; Math function
(define (numCMath [s : symbol][l : Value][r : Value]) : Value
  (cond [(and (numV? l) (numV? r))
         (cond [(symbol=? s '+)(numV (+ (numV-n l)(numV-n r)))]
               [(symbol=? s '*)(numV (* (numV-n l)(numV-n r)))]
               [else (error 'numCMath "Invalid Operator")])]
         [else (error 'numCMath "One argument is not a number")]
        ))

;; If Case:
(define (is-if0C-zero [condition : Value]) : boolean
  (cond [(numV? condition)(= 0 (numV-n condition))]
        [else (error 'if0C "Type Error: Condition should evaluate to NumV")])
  )

   

;---------------------------------------------------------------------------------
;; API for running programs

; evaluates a program starting with a pre-populated environment
; (this can be helpful in testing)
(define (run/env sexp env)
  (interp (desugar (parse sexp)) env))

; evaluates a program in the empty environment
(define (run sexp)
  (run/env sexp mt-env))



;--------------------------------------------------------------------------------
;; Tests

;; Tests;; 14 Total
(test (run '(+ 3 4)) (numV 7))
(test (run '(- 4 2)) (numV 2))
(test (run '(* 2 4)) (numV 8))
(test (run '(+ (- (* 1 2) (* 1 1)) 1)) (numV 2))

(test (run '(if0 (* 1 0) 1 2)) (numV 1))
;;(test (run '(if0 "Hello WOrld" 1 2)) "type")                    ;; Some Error 
;; Test single parameter things
(test (run '(with ([f (fun (x) (* x x))] (f 3)))) (numV 9))     ;; Result
;; Test multiple parameter things
(test (run '(with ([f (fun (x y) (* y x))](f 3 5)))) (numV 15)) ;; Result
(test (run '((fun (x) 5))) (numV 5))                                   ;; I belive this should be legal 
;; Function Calling a function:
(test (run '(with ([f (fun (x y) (*x y))][g (fun (z) (* z z))]) (f 1 (g 2))) ) (numV 4))



(test/exn (run '(with ((x 3)(y 4)) (+ x (* z y)))) "unbound")
(test/exn (run '(+ 2 (with ((x 3)(x 4)) (+ x (* x y))))) "multiple")
(test/exn (run '(+ (fun (x) x) 5)) "type")
(test/exn (run '(if0 (+ 1 0) 2)) "expected");; Tests;; 14 Total
(test (run '(+ 3 4)) (numV 7))
(test (run '(- 4 2)) (numV 2))
(test (run '(* 2 4)) (numV 8))
(test (run '(+ (- (* 1 2) (* 1 1)) 1)) (numV 2))

(test (run '(if0 (* 1 0) 1 2)) (numV 1))
(test (run '(if0 "Hello WOrld" 1 2)) "type")                    ;; Some Error 
;; Test single parameter things
(test (run '(with ([f (fun (x) (* x x))] (f 3)))) (numV 9))     ;; Result
;; Test multiple parameter things
(test (run '(with ([f (fun (x y) (* y x))](f 3 5)))) (numV 15)) ;; Result
(test (run '((fun (x) 5))) 5)                                   ;; I belive this should be legal 
;; Function Calling a function:
(test (run '(with ([f (fun (x y) (*x y))][g (fun (z) (* z z))]) (f 1 (g 2))) ) (numV 4))



(test/exn (run '(with ((x 3)(y 4)) (+ x (* z y)))) "unbound")
(test/exn (run '(+ 2 (with ((x 3)(x 4)) (+ x (* x y))))) "multiple")
(test/exn (run '(+ (fun (x) x) 5)) "type")
(test/exn (run '(if0 (+ 1 0) 2)) "expected")

