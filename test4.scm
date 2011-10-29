
(define (fun x) 
    (cond [(< x 0) (throw 'neg x)]
          [(= x 0) (throw 'zero x)]
          [(= (remainder x 2) 0) (catch (fun (- x 3)) 'neg (λ (x) (+ x 1)))]
          [else (fun (- x 3))]))

(fun 4) ; -1
(fun 5) ; 0
(fun 6) ; unhandled exception: zero
