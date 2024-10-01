(define (square n) (* n n))

(define (pow base exp)
  (cond
    ((= exp 0) 1)
    ((even? exp) (square (pow base (/ exp 2))))
    (else (* base (square (pow base (/ (- exp 1) 2)))))
    )
  )

(define (repeatedly-cube n x)
  (if (zero? n)
      x
      (let ([y (repeatedly-cube (- n 1) x)]) (* y y y))))

(define (cddr s) (cdr (cdr s)))

(define (cadr s) (car (cdr s)))

(define (caddr s) (car (cdr (cdr s))))

(define (ascending? s)
  (cond
    ((or (null? s) (null? (cdr s))) #t)
    ((< (cadr s) (car s)) #f)
    (else (ascending? (cdr s)))
    )
  )

(define (my-filter pred s)
  (cond
    ((null? s) '())
    ((pred (car s)) (cons (car s) (my-filter pred (cdr s))))
    (else (my-filter pred (cdr s)))
    )
  )

(define (no-repeats s)
  (if (null? s)
    s
    (cons (car s)
      (no-repeats (my-filter (lambda (x) (not (= (car s) x))) (cdr s)))
      )
    )
  )

; helper function
; returns the values of lst that are bigger than x
; e.g., (larger-values 3 '(1 2 3 4 5 1 2 3 4 5)) --> (4 5 4 5)
(define (larger-values x lst)
    (filter (lambda (v) (> v x)) lst))

(define (longest-increasing-subsequence lst)
    ; the following skeleton is optional, remove if you like
    (if (null? lst)
        nil
        (begin
            (define first (car lst))
            (define rest (cdr lst))
            (define large-values-rest
                (larger-values first rest))
            (define with-first
                (cons
                    (car lst)
                    (longest-increasing-subsequence large-values-rest)))
            (define without-first
                (longest-increasing-subsequence rest))
            (if (> (length with-first) (length without-first))
                with-first
                without-first))))
