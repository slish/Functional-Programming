(load "prekode3a.scm")

"1a/b"

(define old-procs (make-table))

(define (mem message proc)
  (let ((my-table (make-table))
        (old-proc proc))
    (cond ((equal? message 'memoize)
           (let ((new-proc
                  (lambda args
                    (let ((previously-computed-result
                           (lookup args my-table)))
                      ;; Hent enten forrige beregnet resultat
                      ;; eller la resultatet komme av å kjøre
                      ;; prosedyren på argumentet og lagre
                      ;; resultatet i my-table. 
                      (or previously-computed-result
                          (let ((result (apply proc args)))
                            (insert! args result my-table)
                            result))))))
             ;; Lagrer gammel prosedyre for å kunne unmemoize senere
             (insert! new-proc old-proc old-procs)
             new-proc))
          ((equal? message 'unmemoize)
           (lookup proc old-procs))
          (else
           ;; Viser feilmelding og returnerer samme
           ;; prosedyre ved udefinert message
           (display "Unrecognized message\n")
           proc))))

(display "\n**Testing memoized fib** \n\n")

(set! fib (mem 'memoize fib))
(fib 3)
(fib 3)
(fib 2)
(fib 4)

(display "\n**Testing undefined command** \n\n")

(set! fib (mem 'hei fib))

(display "\n**Unmemoizing** \n\n")

(set! fib (mem 'unmemoize fib))
(fib 3)
(fib 2)

(display "\n**Testing other procedure** \n\n")
(set! test-proc (mem 'memoize test-proc))
(test-proc)
(test-proc 40 41 42 43 44)
(test-proc 40 41 42 43 44)

(newline)
"1c forklart i fil"
(define mem-fib (mem 'memoize fib))
(mem-fib 3)
(mem-fib 3)
(mem-fib 2)
#| Jeg er ikke helt stødig på svaret her, men intuisjonen peker på
at (set! fib (mem 'memoize fib)) kjører gjennom (mem 'memoize fib)
og så endrer verdien av fib til å være prosedyren vi får som resultat,
mens (define mem-fib (mem 'memoize fib)) peker på kjøringen av
(mem' memoize fib). (mem-fib 3) vil da kjøre gjennom hele
(mem' memoize fib) og lagre resultater underveis, men resultatene
blir da utenfor skopet til rekursive kjøringer som gjør at fib of 1
blir beregnet to ganger. Ved en ny kjøring av (mem-fib 3) er derimot
disse lagret via forrige kjøring og kan nås direkte. (mem-fib 2)
vil kjøre en ny prosedyre som ikke har tilgang til de lagrede verdiene
fra (mem-fib 3) og må derfor beregne verdiene om igjen. 

Jeg tar gjerne imot ekstra feedback på denne oppgaven, for jeg
kjenner at dette er litt vagt for meg. 
|#

(newline)
"2a"

(define (list-to-stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list) (list-to-stream (cdr list)))))

(define (stream-to-list stream . end)
  (if (stream-null? stream)
      '()
      (if (not (null? end))
          (if (> (car stream) (car end))
              '()
              (cons
               (car stream)
               (stream-to-list (stream-cdr stream) (car end))))
          (cons
           (car stream)
           (stream-to-list (stream-cdr stream))))))

(list-to-stream '(1 2 3 4 5))
(stream-to-list (stream-interval 10 20))
(show-stream nats 15)
(stream-to-list nats 10)

(newline)
"2b"
(define (stream-take n stream)
  (let ((low (car stream)))
    (if (> low n)
        the-empty-stream
        (cons-stream
         (car stream)
         (stream-take n (stream-cdr stream))))))

(define foo (stream-take 10 nats))
foo
(show-stream foo 5)
(show-stream foo 20)
(show-stream (stream-take 15 nats) 10)

(newline)
"2c forklart i fil"
#| Det første problemet jeg trodde jeg så var følgende:
   ((not (memq (stream-car lst) (stream-cdr lst)))
   Jeg mistenkte at koden over bare ville sjekke om det første i listen
   var medlem av det neste elementet i listen.
   Den nye memq vil derimot stream-cdr'e seg gjennom hele strømmen slik
   at dette faktisk vil fungere, ref. implementasjonen min under. 

   Et potensielt problem kan derimot være at strømmer kan være uendelige,
   slik at remove-duplicates aldri vil terminere.
   (remove-duplicates nats) for eksempel vil aldri terminere, da den vil
   sjekke om 1 er medlem av den resterende uendelige rekken av naturlige
   tall.
|#

(define doublestream (list-to-stream '(1 1 1 2 3 4 5 6 1 2 3 4 8 7 6)))

(define (my-remove-duplicates lst)
  (cond ((stream-null? lst) the-empty-stream)
        ((not (my-memq (stream-car lst) (stream-cdr lst)))
         (cons-stream (stream-car lst) (my-remove-duplicates (stream-cdr lst))))
        (else (my-remove-duplicates (stream-cdr lst)))))

(define (my-memq item x)
  (cond ((null? x)
         #f)
        ((eq? item (stream-car x))
         x)
        (else
         (my-memq item (stream-cdr x)))))

(show-stream (my-remove-duplicates doublestream))

(newline)
"2d"
(define (remove-duplicates stream)
  (if (stream-null? stream)
      the-empty-stream
      (cons-stream (stream-car stream)
                   (remove-duplicates
                    (stream-filter
                     ;; Predikatet sjekker om det som kommer senere allerede
                     ;; har vært i listen, istedenfor å sjekke fremover. 
                     (lambda(x) (not (eq? x (stream-car stream))))
                     (stream-cdr stream))))))

(show-stream (remove-duplicates doublestream))
