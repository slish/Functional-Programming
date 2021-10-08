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

#|(define levelup
  (lambda ()
    (let ((strength 10)
          (agility 15))
      (lambda()
        (set! strength (+ strength 1))
        (set! agility (+ agility 2))
        (list strength agility)))))

(define orc (levelup))
(orc)
(orc)|#

(define (make-stack list)
  (let ((stack list)) ;; Lokal variabel som holder på stack
                      ;; og lagrer listen i argumentet
    (lambda(message . list)
      (cond
        ((equal? message 'pop!)
         (if (null? stack)
             (set! stack stack);; Do nothing
             (set! stack (cdr stack))))
             ;; Endre stack til å være cdr av stack,
             ;; altså stack uten sin opprinnelige car.
             ;; Ergo det samme som å fjerne det første
             ;; elementet.
        ((equal? message 'stack)
         stack)
        ;;((equal? message 'push!)
             
        (else
         "Error: invalid message")))))

(define s1 (make-stack (list 'foo 'bar)))
(define s2 (make-stack '()))
(s1 'pop!)
(s1 'stack)

(newline)
"2b"
"Løsning i vedlagt fil"

(newline)
"3a"
(define bar (list 'a 'b 'c 'd 'e))
(set-cdr! (cdddr bar) (cdr bar))
(list-ref bar 0)
(list-ref bar 3)
(list-ref bar 4)
(list-ref bar 5)
;; Setter cdr av cdddr (e) lik cdr av listen. Siden
;; elementet vi endrer på også er en del av listen
;; får vi en evig loop.
;; Siden bcd gjentar seg får vi b og c på listekall
;; 4 og 5, respektive. 
bar

(newline)
"3b"
(define bah (list 'bring 'a 'towel))
(set-car! bah (cdr bah))
bah
(set-car! (car bah) 42)
bah

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
;; Kort forklart er bar ikke en liste fordi en liste per
;; definisjon skal avsluttes med den tomme listen, eller nil.
;; En liste er en sekvens med par, der cdr i det siste paret
;; er den tomme listen (SICP s. 135)


