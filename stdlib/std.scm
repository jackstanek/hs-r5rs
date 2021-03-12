(define (not b)
  (if b #f #t))

(define (and l r)
  (if l r #f))

(define (or l r)
  (if l #t r))

(define (null? l)
  (eqv? l '()))

(define (equal? l r)
  (or (eqv? l r)
      (and (eqv? (car l) (car r))
           (equal? (cdr l) (cdr r)))))

(define (length l)
  (if (empty? l)
      0
      (+ 1 (length (cdr l)))))

(define (append l r)
  (if (null? l)
      r
      (cons (car l) (append (cdr l) r))))

(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l))
              (list (car l)))))

(define (map f xs)
  (if (null? xs)
      '()
      (cons (f (car xs))
            (map f (cdr xs)))))

(define (filter f xs)
  (if (null? xs)
      '()
      (if (f (car xs))
          (cons (car xs) (filter f (cdr xs)))
          (filter f (cdr xs)))))

(define (reduce f i xs)
  (if (null? xs)
      i
      (reduce f (f i (car xs)) (cdr xs))))

(define (list-tail l k)
  (if (eqv? k 0)
      l
      (list-tail (cdr l) (- k 1))))

(define (memv m l)
  (if (null? l)
      #f
      (if (eqv? m (car l))
          l
          (memv m (cdr l)))))

(define (member m l)
  (if (null? l)
      #f
      (or (equal? m (car l))
          (member m (cdr l)))))
