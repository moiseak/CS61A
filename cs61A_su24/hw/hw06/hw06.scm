(define (square n) (* n n))

(define (pow base exp) 'YOUR-CODE-HERE)

(define (repeatedly-cube n x)
  (if (zero? n)
      x
      (let (_________________)
        (* y y y))))

(define (cddr s) (cdr (cdr s)))

(define (cadr s) 'YOUR-CODE-HERE)

(define (caddr s) 'YOUR-CODE-HERE)

(define (ascending? s) 'YOUR-CODE-HERE)

(define (my-filter pred s) 'YOUR-CODE-HERE)

(define (no-repeats s) 'YOUR-CODE-HERE)

; helper function
; returns the values of lst that are bigger than x
; e.g., (larger-values 3 '(1 2 3 4 5 1 2 3 4 5)) --> (4 5 4 5)
(define (larger-values x lst)
  ______________________________________________)

(define (longest-increasing-subsequence lst)
  (if (null? lst)
      nil
      (begin (define first (car lst))
             (define rest (cdr lst))
             (define large-values-rest
                     (larger-values first rest))
             (define with-first
                     ______________________________________________)
             (define without-first
                     ______________________________________________)
             (if ______________________________________________
                 with-first
                 without-first))))
