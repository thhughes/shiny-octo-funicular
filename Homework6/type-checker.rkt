#lang plai-typed

(require (typed-in racket/sandbox [call-with-limits : (number boolean (-> 'a) -> 'a)]))

;; starter file for type checker assignment 

;;;;;;;;;; Surface Syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type ExprS
  [numS (n : number)]                                                         ;; Done
  [boolS (b : boolean)]                                                       ;; Done
  [nemptyS]                                                                   ;; Done
  [plusS (l : ExprS) (r : ExprS)]                                             ;; Done
  [bminusS (l : ExprS) (r : ExprS)]                                           ;; Done
  [multS (l : ExprS) (r : ExprS)]                                             ;; Done
  [idS (i : symbol)]                                                          ;; Maybe done
  [appS (f : ExprS) (arg : ExprS)]                                            ;; Nope
  [iszeroS (e : ExprS)]                                                       ;; Done
  [bifS (c : ExprS) (t : ExprS) (e : ExprS)]                                  ;; Done
  [lamS (param : symbol) (paramtype : Type) (rettype : Type) (body : ExprS)]  ;; Nope
  [withS (var : symbol) (val : ExprS) (body : ExprS)]                         ;; Nope
  [nconsS (e : ExprS) (l : ExprS)]                                            ;; Done
  [nisEmptyS (e : ExprS)]                                                     ;; Done
  [nfirstS (e : ExprS)]                                                       ;; Done
  [nrestS (e : ExprS)]                                                        ;; Done
  )

(define-type Type
  [numT]
  [boolT]
  [nlistT]
  [funT (input : Type) (return : Type)])

;;;;;;;;;;;; Parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; parses s-expressions into surface syntax
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) 
     (case (s-exp->symbol s)
       [(true) (boolS true)]
       [(false) (boolS false)]
       [(nempty) (nemptyS)]
       [else (idS (s-exp->symbol s))])]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond [(s-exp-symbol? (first sl)) ;; built-in construct or calling function through an identifier
              (case (s-exp->symbol (first sl))
                [(+) (plusS (parse (second sl)) (parse (third sl)))]
                [(*) (multS (parse (second sl)) (parse (third sl)))]
                [(-) (bminusS (parse (second sl)) (parse (third sl)))]
                [(iszero) (iszeroS (parse (second sl)))]
                [(bif) (bifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
                [(fun) (if (= (length sl) 5)
                           (let ([pspec (s-exp->list (second sl))])
                             (begin (unless (= (length pspec) 3)
                                      (error 'parse (string-append "parse error: Malformed parameter specification (expected 3 parts)" (to-string pspec))))
                                    (unless (eq? ': (s-exp->symbol (third sl)))
                                      (error 'parse (string-append "parse error: Missing : for output type " (to-string sl))))
                                    (unless (eq? ': (s-exp->symbol (second pspec)))
                                      (error 'parse (string-append "parse error: Expected : as second element of parameter syntax in " (to-string pspec))))
                                    (let ([pname (s-exp->symbol (first pspec))]
                                          [ptype (parse-type (third pspec))])
                                      (lamS pname ptype
                                            (parse-type (fourth sl))
                                            (parse (fourth (rest sl)))))))
                           (error 'parse (string-append "parse error: Malformed lambda expression (expected 5 parts)" (to-string s))))]
                [(with) (let ([bindings (s-exp->list (second sl))]
                              [body (third sl)])
                          (begin (unless (= 1 (length bindings))
                                   (error 'parse (string-append "parse error: with expects list containing one binding but got " (to-string bindings))))
                                 (let ([binding (s-exp->list (first bindings))])
                                   (withS (s-exp->symbol (first binding))
                                          (parse (second binding))
                                          (parse body)))))]
                [(ncons) (nconsS (parse (second sl)) (parse (third sl)))]
                [(nempty?) (nisEmptyS (parse (second sl)))]
                [(nfirst) (nfirstS (parse (second sl)))]
                [(nrest) (nrestS (parse (second sl)))]
                [else ;; must be a function application
                 (appS (idS (s-exp->symbol (first sl)))
                       (parse (second sl)))])]
             [else (appS (parse (first sl))
                         (parse (second sl)))]))]
    [else (error 'parse "parse error: unexpected syntax")]))

; parses type annotations in the surface syntax grammar
;     type ::= number
;            | boolean
;            | nlist
;            | (type -> type)
(define (parse-type [t : s-expression]) : Type
  (cond [(s-exp-symbol? t)
         (case (s-exp->symbol t)
           [(number) (numT)]
           [(boolean) (boolT)]
           [(nlist) (nlistT)]
           [else (error 'parse-type (string-append "parse error: unrecognized type name " (to-string t)))])]
        [(s-exp-list? t) ; must be a function type
         (let ([tl (s-exp->list t)])
           (if (= (length tl) 3)
               (let ([tin (parse-type (first tl))]
                     [tarrow (s-exp->symbol (second tl))]
                     [tout (parse-type (third tl))])
                 (if (eq? tarrow '->)
                     (funT tin tout)
                     (error 'parse-type (string-append "parse error: Malformed type syntax " (to-string tl)))))
               (error 'parse-type  (string-append "parse error: Malformed type syntax " (to-string tl)))))]
        [else (error 'parse-type  (string-append "parse error: Malformed type syntax " (to-string t)))]))

;;;;;;;;;;;;;; type checker ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; type environment

(define-type tBinding
  [tbind (name : symbol) (type : Type)])
(define-type-alias typeEnv (listof tBinding))
(define mt-tenv empty)

; normally, typecheck would take an ExprC, but we will use the output of parse directly
; for this assignment since no constructs are being desugared
(define (type-of [e : ExprS] [t : typeEnv]) : Type
  (type-case ExprS e
    ;; Basic Building Blocks
    [numS (n) (numT)]   ;; numT
    [boolS (b) (boolT)]  ;; bool
    [nemptyS () (nlistT)] ;; nList
    [idS (s) (lookup-type s t)]
    ;; MATH 
    [plusS (l r) (mathTCheck-helper l r t)]
    [multS (l r) (mathTCheck-helper l r t)]
    [bminusS (l r) (mathTCheck-helper l r t)]
    [iszeroS (n) (if (equal? (type-of n t) (numT)) (boolT) (error 'type-check "isZero only works on numT"))]
    ;; List Functions
    [nconsS (e l) (let ([et (type-of e t)]
                        [lt (type-of l t)])
                    (if (and (equal? et (numT))
                             (equal? lt (nlistT)))
                        (nlistT)
                        (error 'type-check "ncons must be with a number and a list of numbers")))]
    [nisEmptyS (l) (if (equal? (type-of l t) (nlistT))
                       (boolT)
                       (error 'type-check "nisEmpty only works when called on a list"))]
    [nfirstS (l) (if (equal? (type-of l t) (nlistT))
                     (numT)
                     (error 'type-check "nfirst only works when called on a list"))]
    [nrestS (l) (if (equal? (type-of l t) (nlistT))
                    (nlistT)
                    (error 'type-check "nrest only works when called on a list"))]
    ;; if
    [bifS (c tr e) (tc-bifs c tr e t)]
    ;; With's and functions and appc
    [withS (var val body) (let ([new-tenv (build-tenv var val t)])
                            (type-of body new-tenv))]
    [lamS (param paramtype rettype body)(boolT)]
    [appS (f arg)(boolT)]
    ))

(define (build-tenv [var : symbol][val : ExprS][tenv : typeEnv]) : typeEnv
  (cons (tbind var (type-of val tenv))
          tenv))

(define (tc-bifs [c : ExprS][tr : ExprS][e : ExprS][tenv : typeEnv]) : Type
  (let ([ct (type-of c tenv)]
        [trt (type-of tr tenv)]
        [et (type-of e tenv)])
    (if (and (equal? et trt)
             (equal? ct (boolT)))
        trt
        (error 'type-check "If statements must have a boolen condition and matching type between if and else statements"))))

(define (mathTCheck-helper [l : ExprS][r : ExprS][tenv : typeEnv]) : Type
  (let ([lt (type-of l tenv)]
        [rt (type-of r tenv)])
    (if (and (equal? lt (numT))
             (equal? rt (numT)))
        (numT)
        (error 'tc-math "both arguments are not nubers"))))


;; lookup a type name in the type environment
(define (lookup-type [name : symbol] [env : typeEnv])
  (cond [(empty? env) (error 'lookup-type (string-append "unbound identifier " (to-string name)))]
        [(cons? env) (if (eq? name (tbind-name (first env)))
                         (tbind-type (first env))
                         (lookup-type name (rest env)))]))

;;;;;;;;;;;;; API for type checking programs ;;;;;;;;;;;

; have "type error" be a substring in all type errors raised by your code

(define (typecheck sexp)
  (call-with-limits 
   10 #f
   (lambda () (type-of (parse sexp) mt-tenv))))

;;;;;;;;;;;;;; TESTS ;;;;;;;;;;;;;;;;;
;; Test that a number correclty returns the type of NumT
(test (typecheck '5) (numT))

;; Test that boolean if works with a boolean value and returns numT for
;; numbers in the than and else
(test (typecheck '(bif true 10 0)) (numT))

;; Test that boolean if works with a boolean value and returns boolT for
;; booleans in the than and else
(test (typecheck '(bif true true false)) (boolT))

;; Check that all of the math functions return numT
(test (typecheck '(+ (- (* 1 1) 0) 1)) (numT))

;; Test that isZero returns a boolean
(test (typecheck '(bif (iszero 0) true false)) (boolT))

;; Test that ncons returns an nlisT, implicitly testing if nempty returns nlistT as well
(test (typecheck '(ncons 1 nempty)) (nlistT))

;; Tes that first returns numT
(test (typecheck '(nfirst (ncons 1 (ncons 2 (ncons 3 nempty))))) (numT))

;; Test that nrest returns nlistT
(test (typecheck '(nrest (ncons 1 (ncons 2 (ncons 3 nempty))))) (nlistT))

;; Test that with correctly places numT a in type environment and recalls it
(define numTwith '(with ([a 10])
                       a))
(test (typecheck numTwith) (numT))

;; Test that with correctly places boolT a in type environment and recalls it
(define boolTwith '(with ([a true])
                         a))
(test (typecheck boolTwith) (boolT))

;; Test that stored ID's via withs properly work in math.
(define withNumTMath '(with ([a 10])
                         (+ a a)))
(test (typecheck withNumTMath) (numT))
