;; abstract syntax and desugar


;; by removing the type : FunDefC we can replace it as part of the ExperC
;; and place it as part of appC. 

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (i : symbol)]
  [appC (f : ExprC) (arg : ExprC)]
  )
;; desugar -- returning a default/dummy value so file can be run
;; (define (desugar [e : ExprS]) : ExprC
;;   (numC 0))

;---------------------------------------------------------------------------------
;; output values

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])

;---------------------------------------------------------------------------------
;; Environments

;; binding an identifier to a value
(define-type Binding
  [bind (name : symbol) (val : Value)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)

;---------------------------------------------------------------------------------
;; interp -- returning a default/dummy value so file can be run
(define (interp [e : ExprC] [fds : (listofFunDefC)][env : Env]) : number
  (type-case ExprC e
             [numC (n) n]
             [idC (s) (lookup s env)]
             [plusC (l r) (+ (interp l env fds)(interp r env fds))]
             [multC (l r) (* (interp l env fds)(interp r env fds))]
             [appC (fun arg) (local ([define fd (get-fundef fun fds)])
                               (interp (fdC-body fd)
                                       (extend-env (bind (fdC-param fd)
                                                         (interp arg env fds))
                                                   mt-env)
                                       fds))]
             ))



(define (run sexp fds)
  (interp (desugar (parse sexp)) mt-env fds))
