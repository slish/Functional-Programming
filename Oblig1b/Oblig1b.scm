#|(cons 42 11)

(cons 42 '())

(list 42 11)

'(42 (11 12))

(cdr (cdr (car (cdr '(42 (11 12))))))


(define foo '(1 2 3))
(define foo1 (cons foo foo))
(define foo2 (list foo foo))
"fooene"
foo1
foo2
"car av fooene"
(car foo1)
(car foo2)
|#

"1f"
(car (cdr '(0 42 #t bar)))

; Må bruke en ekstra car for å ikke få en liste som inneholder
; 42 og en tom liste
"1g"
(car (cdr (car '((0 42) (#t bar)))))

"1h"
(car (car (cdr '((0) (42 #t) (bar)))))

(newline)
"1i"
"Bare ved bruk av cons"
(define my-cons
  (cons (cons 0
              (cons 42
                    '()))
        (cons (cons #t
                    (cons 'bar
                          '()))
              '())))
my-cons

"Bare ved bruk av list"
(define my-list
  (list (list 0 42) (list #t 'bar)))
my-list

#|
Bruker en or for å sjekke både om n er lik null og om listen er tom.
Om listen er tom har vi kommet til slutten og trenger ikke å lete mer.
|#
(newline)
"2a"
(define (take n items)
  (if (or (= n 0)
          (null? items))
      '()
      (cons (car items) (take (- n 1) (cdr items)))))
      

(take 3 '(a b c d e f))
(take 1 '(a b c d e f))
(take 4 '(a b))
(take 4 '())

(newline)
"2b"
(define (take n items)
  (define (take-iter counter in out)
    (if (or (= counter 0)
            (null? in))
        (reverse out)
        (take-iter (- counter 1)
                   (cdr in)
                   (cons (car in) out))))
  (take-iter n items '()) )
  
(take 3 '(a b c d e f))
(take 1 '(a b c d e f))
(take 4 '(a b))
(take 4 '())

(newline)
"2c"
(define (take-while pred items)
  (cond ((null? items)
         '())
        ((pred (car items))
         (cons (car items)
               (take-while pred (cdr items))))
        (else '())))

(take-while even? '(2 32 42 75 88 103 250))
(take-while odd? '(2 32 42 75 88 103 250))
(take-while (lambda (x) (< x 100)) '(2 34 42 75 88 103 250))

(newline)
"2d"
(define (map2 proc ls1 ls2)
  (if (or (null? ls1)
          (null? ls2))
      '()
      (cons (proc (car ls1) (car ls2))
            (map2 proc (cdr ls1) (cdr ls2)))))

(map2 + '(1 2 3 4) '(3 4 5))
(map2 + '(1 2 3) '(4 5 6 7))

(newline)
"2e"
(map2 (lambda (x y) (/ (+ x y) 2)) '(1 2 3 4) '(3 4 5))
(map2 (lambda (x y) (/ (+ x y) 2)) '(5 6 7 8) '(10 12 14))



