(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
    (car (cdr (cdr s)))
)


(define (sign num)
    (cond ((< num 0) (- 1)) ((> num 0) 1) ((= num 0) 0))
)


(define (square x) (* x x))

(define (pow x y)
    (if (= y 0)
        1
        (if (even? y)
            (square (pow x (/ y 2)))        
            (* x (square (pow x (/ (- y 1) 2))))))
)

(define (remove item lst)
    (filter (lambda (x) (not (equal? x item))) lst)
)
(define (unique s)
    (if (null? s)
        nil
        (cons (car s) (remove (car s) (unique (cdr s))))
        )
    
)


(define (replicate x n)
    (define (replicate-tail lst n)
        (if (= n 0)
            lst
            (replicate-tail (cons x lst) (- n 1))
            )
        )   
    (replicate-tail nil n)
)


(define (accumulate combiner start n term)
    (if (= 0 n)
        start
        (combiner (accumulate combiner start (- n 1) term) (term n)))
)


(define (accumulate-tail combiner start n term)
    (define (accumulate-tail-iter c ans)
        (if (= n c)
            ans
            (accumulate-tail-iter (+ c 1) (combiner ans (term (+ c 1))))
            )
        )
    (accumulate-tail-iter 0 start)
)


(define-macro (list-of map-expr for var in lst if filter-expr)
  'YOUR-CODE-HERE
)

