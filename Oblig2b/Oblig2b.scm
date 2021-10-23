"1a"
(define make-counter
  ;; make-counter returnerer en prosedyre via lambda
  ;; som har en lokal variabel count
  (lambda ()
    (let ((count 0))
      ;; Denne prosedyren kjøres igjen ved kall på
      ;; "foreldre"-prosedyren, som øker count med 1
      (lambda ()
        (set! count (+ count 1))
        count))))

(define count 42)
(define c1 (make-counter))
(define c2 (make-counter))

(c1)
(c1)
(c1)
count
(c2)

(newline)
"1b"
"Løsning i vedlagt fil"

(newline)
"2a"

(define (make-stack list)
  (let ((stack list))
    (lambda (message . rest)
      (cond
        ((equal? message 'pop!)
         (if (null? stack)
             (set! stack stack) ;; Do nothing
             (set! stack (cdr stack))))
        ((equal? message 'push!)
         ;; Konverter stack til å være seg selv der
         ;; vi har appendet den reverserte listen i
         ;; argumentet inn i stacken
         (if (list? (car rest)) ;; Måtte gjøre et krumspring for å sjekke
                                ;; om rest er en liste i en liste, som den
                                ;; blir om jeg sender den via (push! stack)
             (set! stack (append (reverse (car rest)) stack))
             (set! stack (append (reverse rest) stack))))
        ((equal? message 'stack)
         stack)
        (else
         "Error: Invalid message")))))

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))

(s1 'pop!)
(s1 'stack);;bar

(s2 'pop!)
(s2 'push! 1 2 3 4)
(s2 'stack);;(4 3 2 1)

(s1 'push! 'bah)
(s1 'push! 'zap 'zip 'baz)
(s1 'stack);;(baz zip zap bah bar)

(newline)
"2b"
(define (pop! st)
  (st 'pop!))

(define (stack st)
  (st 'stack))

(define (push! st . rest)
  (st 'push! rest))

(pop! s1)
(stack s1);;(zip zap bah bar)
(push! s1 'foo 'faa)
(stack s1);;(faa foo zip zap bah bar)

(newline)
"3a"
"Tegning i vedlagt fil"
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))
(list-ref bar 0)
(list-ref bar 3)
(list-ref bar 4)
(list-ref bar 5)
#| Setter cdr av cdddr (e) lik cdr av listen. Siden
   elementet vi endrer på også er en del av cdr av
   listen får vi en evig loop.
   Siden bcd gjentar seg får vi b og c på listekall
   4 og 5, respektive. Et listekall 6 ville ha gitt
   d, mens et listekall 7 ville ha gitt b igjen,
   og slik fortsetter det i evig.
|#
bar

(newline)
"3b"
"Tegning i vedlagt fil"
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
bah
(set-car! (car bah) 42)
bah
#| Grunnen til at vi får ((42 towel) 42 towel) etter
   det siste kallet på set-car! er fordi det første
   elementet i bah nå er listen (a towel). Når vi da
   spør om å endre car av det første elementet blir det
   a vi endrer på, som nå blir til 42. Cdr av bah peker
   også på denne listen, slik at a blir endret til 42 for
   både car og cdr, eller rettere sagt a blir endret til
   42 for listen som både car og cdr peker på. 
|#

(newline)
"3c"
(define (cycle? list)
  ;; Hjelpefunksjon som sjekker om første element er
  ;; likt elementet som kommer etter
  (define (cycle-checker first last)
    (cond
      ;; Om vi kommer til en ende kan vi ikke ha en sykel
      ((null? last) #f)
      ;; Om vi finner et element etter siste liste har
      ;; vi heller ingen sykel
      ((null? (cdr last)) #f)
      ;; Om første og siste liste er et og samme objekt
      ;; i minnet har vi en sykel
      ((eq? first last) #t)
      ;; Kjør ellers videre for å sjekke alle element i
      ;; listen.
      (else (cycle-checker (cdr first) (cddr last)))))
  (if (null? list)
      #f
      (cycle-checker list (cdr list))))

(cycle? '(hey ho))
(cycle? '(la la la))
(cycle? bah)
(cycle? bar)

(newline)
"3d"
(list? bar)
(list? bah)
#| Kort forklart er bar ikke en liste fordi en liste per
   definisjon skal avsluttes med den tomme listen, eller nil.
   En liste er en sekvens med par, der cdr i det siste paret
   er den tomme listen (SICP s. 135)
   Per dokumentasjonen til Scheme kan vi også lese at alle lister
   har en endelig lengde og er terminert av den tomme listen.
https://groups.csail.mit.edu/mac/ftpdir/scheme-reports/r5rs-html/r5rs_14.html#SEC88
|#


