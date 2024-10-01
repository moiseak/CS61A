(define (over-or-under num1 num2) (if (<= num1 num2) (if (< num1 num2) -1 0) 1))

(define (make-adder num) (lambda (inc) (+ num inc)))

(define (composed f g) (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 0)
  (lambda (x) x)
  (composed f (repeat f (- n 1))
    )))

(define (max a b)
  (if (> a b)
      a
      b))

(define (min a b)
  (if (> a b)
      b
      a))

(define (gcd a b) (if (zero? (modulo (max a b) (min a b)))
                    (min a b)
                    (gcd (min a b) (modulo (max a b) (min a b)))
                    )
  )

(define (duplicate lst)
  (if (null? lst)
  '()
  (cons (car lst) (cons (car lst) (duplicate (cdr lst))))
    )
  )

(expect (duplicate '(1 2 3)) (1 1 2 2 3 3))

(expect (duplicate '(1 1)) (1 1 1 1))

(define (deep-map fn s)
  (map (lambda (x)
         (if (list? x)
           (deep-map fn x)
           (fn x))
         )
    s)
  )
