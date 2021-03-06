#lang plai-typed

;; We're going to add box:
	;; This has 1 value inside of it
	;; (Think of it as an object with one field/instance var)
	;; we can get/set

;; We are going to add to ExprC:
;; [boxC (arg: ExprC)] ;; This is an initialization field.
;; [unboxC (b: ExprC)] ;; This is the getter
;; [setboxC (b : ExprC)(v : ExprC)] ;; Setter
;; [seqC (e1 : ExprC)(e2 : ExprC)] ;; begin

;; Need to change Value:
(define-type Value
	[numV ...]
	[cloV ...]
	[boxV (v : Value)])


(define (interp (e:ExprC)(env:Env)) : Value
	[boxC (arg) (boxV (interp arg env))]
	[unboxC (b) (boxV-v (interp b env))]
	[setC (e1 e2) (begin (interp e1 env)
						 (interp e2 env))]
	[setboxC (b v) ...]
	)



; ; :: Examples
; (with ([b (box 0)])
; 	(+ (setq (setbox b (+ 1 (unbox b)))
; 		(unbox b))
; 	   (setq (setbox b (+ 1 (unbox b)))
; 	   	 (unbox b))))
; What does this return? :: 3

;; How much do you borrow from your host language? 
; ; ; We are going to do this without assignment in our interpreter
; ; ; You don't wnat to steal from your host language too much otheriwise
; ; ; What you're doing wouldn't work in a different host language

;; Proposal:
	; on setbox, update the envirionment and pass the new 
	; environment along to right side of setq + etc. 
(+ (with ([b (box 0)])
	1)
	(unbox b))
;; If we're passing the environment along this will evaluate
;; Instead of throwing an unbound error becasue passing 
;; along the environent destroys scope


;; What about: 
(with ([a (box 1)])
	(with ([f (fun (x) (+ x (unbox a))]))
		(setq
			(setbox a 2)
			(f 10)))

;; This is going to return (12)
;; We want the scope to be static
;; but we want dynamic extent :: which value associate with binding

;; The environment's job is to manaje scope

(define-type Binding 
	(bind (name: symbol) (loc : Location)))

(define-type-alias Location number)
(define-type Storage
	[cell (loc :location)(val :Value)]
	)

;; pictorally: x -> [100] -> [numV 5]




;; Interp is going to change: 
(define (interp (e : ExprC)(env : Env)(sto : Store))
	;;[plusC (l r)(numV+ (interp l env)(interp r env))] OLD PlusC
	[plusC (l r)
		(type-case Result (interp l env sto)
			[v*s (val-r newSto) (type-case Result (interp r env newSto)
									[v*s (val-l sto-r) (v*s (numV+ val-l val-r) sto-r)])])])

	(interp r env sto))] ;; New plusC needs to return the value AND the store
	)


(define-type Result
	[v*s (v : value)(s : store)])





;; What are the differences between? 
(with ([b (box 6)])
	(unbox b))
;; and
(with ([b 6])
	b)
	






















