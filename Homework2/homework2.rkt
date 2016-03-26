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
  [appC (f : ExprC) (arg : ExprC)]
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
             [appS (f arg) (error 'desugar "appS Not Implented yet")]
             [funS (params body) (error 'desugar "funS Not Implented yet")]
             [withS (bindings body) (error 'desugar "withS Not Implented yet")]
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
             [appC (f arg) (error 'interp "appC not implemented yet")]
             ;; [appC (f arg)(let ([f-val (interp f env)]
             ;;                    (interp (closV-body f-val)
             ;;                            (extend-env (bind (closV-arg f-val)
             ;;                                              (interp arg env))
             ;;                                        (closV-env f-val))) 
             ;;                   ))]
             [funC (param body)(closV param body env)]
             )
  )


;; Interp helpers:
;; lookup symbols in environment:
(define (lookup [n : symbol][env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup "Symbol was not found in the envirionment")]
    [else (let ([envF (first env)])
            (cond [(symbol=? n (bind-name envF))(bind-name envF)]
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

; Check if plus minus or multiplication work:
"Checking Simple Addition, Subtraction, and Multiplication"
(test (run '(+ 1 1)) (numV 2))
(test (run '(- 1 1)) (numV 0))
(test (run '(* 1 4)) (numV 4))
(test (run '(* 0 (+ 1 (- 4 1)))) (numV 0))

"Checking Conditional Tests"
(test (run '(if0 0 1 0)) (numV 1))
(test (run '(if0 1 1 0)) (numV 0))
(test (run '(if0 (+ 1 0) 0 (* 2 3))) (numV 6))
