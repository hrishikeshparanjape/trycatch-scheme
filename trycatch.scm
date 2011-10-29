; stack for all pending catch clauses
(define catch-stack null)

; stack for skipped catch clauses that may need to
; be restored in retry
(define saved-catch-stack null)

; unhandled-exception is a continuation that returns
; to the command prompt

(define unhandled-exception null)

(call/cc (lambda (cont) (set! unhandled-exception (lambda (x) (cont x)))))

; macro for try block with catch clause
; @param expr the expression to evaluate
; @param cond the predicate that tests whether the handler matches
; @param handler the exception handler function
; @return the value of expr, if no exception, or the 
; result of the handler function, if there was an exception
(define-syntax catch
  (syntax-rules ()
    ((catch expr sym handler)
     (call/cc 
      (lambda (cont)
        (set! catch-stack 
              (cons 
               (list sym cont handler)
               catch-stack))
        (let ((result expr))
          (set! catch-stack 
              (rest catch-stack))
          result))))))

; throw an exception
; @param sym the symbol signifying the exception type
; @param arg the argument for the handler function
(define (throw sym arg)
  (if  
   (empty? catch-stack) 
   (begin
     (display "Unhandled exception: ")
     (display sym)
     (newline)
     (display arg)
     (newline)
     (unhandled-exception null))
   (let 
       ((catch (first catch-stack)))
     (set! catch-stack (rest catch-stack))
     (if 
      (eq? sym (first catch))
      ((first (rest catch)) ((first (rest (rest catch))) arg))
      (throw sym arg)))))

