;; abstract syntax and desugar


;; by removing the type : FunDefC we can replace it as part of the ExperC
;; and place it as part of appC. 

(define-type ExprC
  [numC (n : number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [idC (i : symbol)]
  [appC (fun : ExprC) (arg : ExprC)]
  [fdC (param : symbol) (body : ExprC)]
  )
;; desugar -- returning a default/dummy value so file can be run
;; (define (desugar [e : ExprS]) : ExprC
;;   (numC 0))

;---------------------------------------------------------------------------------
;; output values

(define-type Value
  [numV (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)])
;; -- Closure: When you define a function, you save the envirionement in effect when the function was defined.
;; -- -- THis is the statis scope guardian. 

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
             [idC (s) (lookup s env)]
             [plusC (l r) (numV+ (interp l env)(interp r env))]
             [multC (l r) (numV* (interp l env)(interp r env))]
             [appC (fun arg) (local ([define fd (interp fun env)])         ;; Running a function
                               (interp (fdC-body fd)
                                       (extend-env (bind (fdC-param fd)
                                                         (interp arg env))
                                                   mt-env)))]              ;; Need to take the closure's env
             [fdC (param body) (funV param body)]                          ;; Calling a function
             ))



;; TYPE CASE from Class:
 ;; This does Dynamic Scoping because we're changing the env as we go down
 ;; [appC (f arg)
 ;;       (let ([fundef (interp f env)]
 ;;             (interp (funV-body fundef)
 ;;                     (extend-env (bind (funV-param fundef))
 ;;                                 env))))]

;; NumV? Is this a numV?
;; TYPE ERRRORRRRR

;; Static Scope: Use the binding in effect when you make the function
;; Dynamic Scope: Use the binding in effect when you call the function

;; With and Function info:
 ;; (with (([x 3])
 ;;        (+ 2 x)))
 ;; -------------------- Is equivelant to:
 ;; ((fun (x) (+ 2 x)) 3) :: note how it's a function being defined and called




;; NOTE: same deferred subst = environment





(define (run sexp fds)
  (interp (desugar (parse sexp)) mt-env fds))
