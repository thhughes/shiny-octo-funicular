;; Testing Pass by value vs. Pass by reference:
(with ([x 1][y 2])
      (seq (with ([f (fun (z) (set z 10))])
                 (f y))
           (+ x y)))
;; If this is PFV : Returns 3
;; If this is PBR : Returns 11


;; Attempt at figuring out dynamic vs. static scope:
(with ([f2 (fun (y) (+ x y))])
      (with ([f1 (fun (x))])
            (f1 3)))

(with ([x 1])
      (with ([foo (fun (y) (+ x y))])
            (with ([x 2])
                  (foo 10))))




;; Sherlock Tests:
(with ([x (box 4)])
      (with ([y x])
            (seq (setbox y 10)
                 (unbox x))))
;; sherlock returns 10, marple returns 4.
;; This tells us that sherlock is maintaining the environment and store correctly
;; and marple is not (how it's maintianing the store is different)



;; This doesnt work in marple.... It does in sherloc
(with ([b (box 3)])
      (unbox (seq (setbox b 4)
                  b)))

;; This returns 0 in marple... and 20 in sherloc
(with ([x (box 0)])
      (+ (seq (setbox x 10)
              (unbox x))
         (unbox x)))

;; Check if mutation occurs on internal store if it will
;; stay on external - e.g. dynamic store.
(with ([x (box 10)])
      (seq ( with([y 10])
                 (setbox x 100))
           (unbox x)))

;; Checks If the thing evaluates lazily 
(with ([x 1])
      (with ([foo (fun (x) (+ 1 2))])
            (with ([setter (fun (y) (set x 100))])
                  (seq (foo (setter 0))
                       x))))


