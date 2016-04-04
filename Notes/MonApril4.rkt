RECURSION STUFF:

(with ([fact (fun (n)
                  (if0 n 1 (* n (fact (- n 1)))))])
      (fact 10))
;; With the current language:
; You cannot do recursion with this because the closure for fact
; does not contain itself in its environment. Instead, it only exists
; after the function is defined, but that cannot happend

;; But what if we tried to use mutation:
;; (with ([fact 0])
;;       (seq (set fact (fun (n) ... ))
;;            (fact 10)))
; In this case we're using sugar to mutate fact into a function so that fact exists
; in it's own envirionment. 


;; ; #################### HASCAL #############################
;; ;; >3+4
;; ;; 7
;; ;; >let ones = 1:ones
;; ;; 1,1,1,1,1,1,1..... 
;; ;; >take 5 ones
;; ;; 1,1,1,1,1            ;; This is lazy evaluation. "Only going to computer as much as I need"
;; ;; ;; When you ask for ones:: I'm asking for everything, try and get everything
;; ;; ;; when you run take    :: I'm asking for 5 of ones, so only calculate 5
;; ;; > let nats = 1 : (map (+1 nats)
;; ;; > take 9 nats
;; ;; > zip ["Larry", "Curley", "mo"] nats ;; Given me a fianite list and zip with infinate
;; ;; [("Larry",1),("Curley",2),("mo",3)]
;; ;; > let lights = cycle ["red","yellow","green"]
;; ;; > take 7 lights
;; ;; ["red","yellow","green","red","yellow","green","red"]
;; ;; ;; lazy language will go through and manage infinity for you. 

; ################ Where in this did we make the decision not to be lazy #################
; ################ Where in this did we make the decision not to be lazy #################
; ################ Where in this did we make the decision not to be lazy #################

#lang plai-typed

;; language with functions and arithmetic, static scope

;; surface syntax and parser 

(define-type ExprS
  [numS (n : number)]
  [plusS (l : ExprS) (r : ExprS)]  
  [multS (l : ExprS) (r : ExprS)]
  [idS (i : symbol)]
  [funS (arg : symbol) (body : ExprS)]
  [if0S (q : ExprS) (th : ExprS) (el : ExprS)]
  [appS (f : ExprS) (arg : ExprS)]
  [withS (var : symbol) (val : ExprS) (body : ExprS)]
  )

;; the core language
(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [plusC (l : ExprC) (r : ExprC)]  
  [multC (l : ExprC) (r : ExprC)]
  [if0C (q : ExprC) (th : ExprC) (el : ExprC)]
  [funC (arg : symbol) (body : ExprC)]  
  [appC (fun : ExprC) (arg : ExprC)])

;; parser : s-expressions -> ExprS
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
                [(fun) (funS (s-exp->symbol (first (s-exp->list (second sl)))) (parse (third sl)))]
                [(if0) (if0S (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(with) (let ([binding (s-exp->list (first (s-exp->list (second sl))))])
                          (withS (s-exp->symbol (first binding))
                                 (parse (second binding))
                                 (parse (third sl))))]
                [else ;; must be a function application
                 (appS (idS (s-exp->symbol (first sl)))
                       (parse (second sl)))])]
             [else (appS (parse (first sl))
                         (parse (second sl)))]))]
    [else (error 'parse "unexpected input format")]))
     
;; desugar

(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l)
                        (desugar r))]
    [multS (l r) (multC (desugar l)
                        (desugar r))]
    [idS (i) (idC i)]
    [funS (arg body) (funC arg (desugar body))]
    [if0S (q t e) (if0C (desugar q) (desugar t) (desugar e))]
    [appS (f arg) (appC (desugar f) (desugar arg))]
    [withS (var val body) (appC (funC var (desugar body)) (desugar val))]
    ))

;; output values

(define-type Value
  [numV (n : number)]
  [closV (param : symbol) (body : ExprC) (env : Env)]
  [experV (e:ExperC)(en: Env)]) ;; Often you're caching the value so you don't have to recompute it.

;; Environments

(define-type Binding
  [bind (name : symbol) (val : Value)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

; return first value bound to id in env, or raise error if id is not found
(define (lookup [id : symbol] [env : Env]) : Value
  (cond [(empty? env) (error 'lookup (string-append "unbound identifier " (to-string id)))]
        [(cons? env) (if (symbol=? id (bind-name (first env)))
                         (bind-val (first env))
                         (lookup id (rest env)))]))

; operators on numVs

(define (num+ [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (+ (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "type error: one argument was not a number")]))

(define (num* [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (* (numV-n l) (numV-n r)))]
    [else
     (error 'num* "type error: one argument was not a number")]))

(define (num0? [v : Value]) : boolean
  (if (numV? v) 
      (zero? (numV-n v))
      (error 'num0? "type error: argument was not a number")))
(define (evalNow e)
  (type-case Value e
             [experV (interp (experV-e e)
                             (experV-env e))]
             [else e]))


;; interpreter

(define (interp [a : ExprC] [env : Env]) : Value
  (type-case ExprC a
    [numC (n) (numV n)]
    [plusC (l r) (num+ (evalNow (interp l env)) (evalNow (interp r env)))]
    [multC (l r) (num* (evalNow (interp l env)) (evalNow (interp r env)))]
    [idC (i) (lookup i env)] 
    [if0C (q th el) (if (num0? (evalNow (interp q env)))
                        (interp th env)
                        (interp el env))]
    [funC (arg body) (closV arg body env)]
    ;; [appC (f arg) 
    ;;       (local ([define f-value (interp f env)]
    ;;               [define param (closV-param f-value)])
    ;;         (interp (closV-body f-value)
    ;;                 (extend-env (bind param (interp arg env))    ;; This is where we made the decision about lazyness.
    ;;                             ;; Why? Because we could just hang onto the info in case we need it later. 
    ;;                             (closV-env f-value))))]
    [appC (f arg) 
          (local ([define f-value (interp f env)]
                  [define param (closV-param f-value)])
            (interp (closV-body f-value)
                    (extend-env (bind param (experV arg env)) ;; Invent a different value to store it in the bind
                                (closV-env f-value))))]
    ))

;; putting it all together

; evaluates a program starting with a pre-populated environment
; (this can be helpful in testing)
(define (run/env sexp env)
  (interp (desugar (parse sexp)) env))

; evaluates a program in the empty environment
(define (run sexp)
  (run/env sexp mt-env))
