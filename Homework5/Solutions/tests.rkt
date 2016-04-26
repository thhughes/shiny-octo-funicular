#lang plai-typed


(define classOne '(class ClassOne (w)
                 (parent Object)
                 (private (one-val w))
                 (public (c1-pub-1 6) (c1-pub-2 5))
                 (c1-add-val-pub-1 (fun (x) (+ x c1-pub-1)))
                 (c1-add-val-pub-2 (fun (x) (- x c1-pub-2)))
                 (c1-add-both (fun () (+ c1-pub-1 c1-pub-2)))
                 (c1-use-private (fun () (+ 1 one-val)))
                 ))

(define classTwo '(class ClassTwo (w)
                 (parent ClassOne 10)
                 (private (two-val 2))
                 (public (c2-pub-1 -6) (c2-pub-2 -5))
                 (c2-add-val-pub-1 (fun (x) (+ x c2-pub-1)))
                 (c2-add-two-pub-2 (fun () (+ 2 c2-pub-2)))
                 ))



;; Tests that we can get from a class 
(test (run/classes '(with ((obj-1 (new ClassOne 100)))
                          (send obj-1 get-c1-pub-1))
                      (list classOne))
         (numV 6))

;; Tests that we can use private variables
(test (run/classes '(with ((obj-1 (new ClassOne 100)))
                          (send obj-1 c1-use-private))
                      (list classOne))
         (numV 101))

;; Test Set public variables
(test (run/classes '(with ((myobj (new ClassOne 100)))
                          (seq (send myobj set-c1-pub-1 100)
                               (send myobj get-c1-pub-1)))
                      (list classOne))
         (numV 100))

;; Test getting public variables of function that has parent object. 
(test (run/classes '(with ((obj-1 (new ClassTwo 100)))
                          (send obj-1 get-c2-pub-1))
                      (list classOne classTwo))
         (numV -6))

;; Test getting public variables from the parent function.
(test (run/classes '(with ((obj-1 (new ClassTwo 100)))
                          (send obj-1 get-c1-pub-1))
                      (list classOne classTwo))
         (numV 6))

;; Test running a method that has arguments
(test (run/classes '(with ((obj-1 (new ClassTwo 100)))
                          (send obj-1 c2-add-val-pub-1 1234))
                      (list classOne classTwo))
         (numV 1228))

;; Test Set public variables of the parent
(test (run/classes '(with ((myobj (new ClassTwo 100)))
                          (seq (send myobj set-c1-pub-1 100)
                               (send myobj get-c1-pub-1)))
                      (list classOne classTwo))
         (numV 100))

;; Test using a function that doesn't exist
(test/exn (run/classes '(with ((obj-1 (new ClassOne 100)))
                          (send obj-1 c3-use-private))
                      (list classOne))
         "unbound")

;; Test failing to get from a private variable
(test/exn (run/classes '(with ((obj-1 (new ClassOne 100)))
                          (send obj-1 get-one-val))
                      (list classOne))
         "unbound")


;; Test failing to set a private variable
(test/exn (run/classes '(with ((obj-1 (new ClassOne 100)))
                          (send obj-1 set-one-val 123))
                      (list classOne))
         "unbound")


;; Test calling a function with bad number of arguments
(test/exn (run/classes '(with ((obj-1 (new ClassOne 100)))
                          (send obj-1 set-c1-pub-1 123 33))
                      (list classOne))
         "number")