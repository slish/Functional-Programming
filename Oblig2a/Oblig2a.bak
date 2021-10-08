(load "huffman.scm")
(#%require racket/trace)

"1a"

(define (p-cons x y)
  (lambda (proc) (proc x y)))

(define (p-car proc)
  (proc (lambda (x y) x)))

(define (p-cdr proc)
  (proc (lambda (x y) y)))

;;p-cons forventer en prosedyre som velger x eller y.
;;p-car og p-cdr er prosedyrer som velger første eller siste av to elementer,
;;respektivt. 

(p-cons "foo" "bar")
(p-car (p-cons "foo" "bar"))
(p-cdr (p-cons "foo" "bar"))
(p-car (p-cdr (p-cons "zoo"
                      (p-cons "foo"
                              "bar"))))

(newline)
"1b"

(define foo 42)
;; x blir different fordi den ikke "kjenner" til den lokale foo enda
"Med let"
(let ((foo 5)
      (x foo))
  (if (= x foo)
      'same
      'different))

(newline)
"Med lambda"
((lambda (foo x)
   (if (= x foo)
       'same
       'different))
 5 foo)

(newline)

#| Den indre let'en kjenner nå til 'bar' og 'baz', som er knyttet til
   foo(som igjen er knyttet globalt til 42) og 'towel'.
   Den indre 'bar' blir dermed en liste av 42 og 'towel'.
   Den ytre 'bar' kjenner derimot ikke til den indre definisjonen
   av foo, og er dermed fortsatt knyttet til 42 når vi kjører
   (list bar baz)

   Evalueringen vil da se slik ut for det "spesielle" caset: 
   (let ((bar (list bar baz))))
   (let ((bar (list foo 'towel)))) <- fra ytre definisjon
   (let ((bar (list 42 'towel))))  <- fra global definisjon for foo
|#
"Med let"
(let ((bar foo)
      (baz 'towel))
  (let ((bar (list bar baz))
        (foo baz))
    (list foo bar)))

(newline)
"Med lambda"
((lambda (bar baz)
   ((lambda (bar foo)
      (list foo bar))
    (list bar baz) baz))
 foo 'towel)
 
(newline)
"1c"
;; Henter ut operand og operator fra input-liste med car og cdr-kall
(define (infix-eval exp)
  ((cadr exp) (car exp) (caddr exp)))

(define foo (list 21 + 21))
(define baz (list 21 list 21))
(define bar (list 84 / 2))

(infix-eval foo)
(infix-eval baz)
(infix-eval bar)

(newline)
"1d beskrevet i fil"
(define bah '(84 / 2))
#| I det nye tilfellet har vi et "'"-tegn, som er en snarvei for quote.
   Dette gjør at Scheme evaluerer nøyaktig det som står inne i parantesene
   og "/" tegnet blir da returnert som et tegn, og ikke som en prosedyre.
   (infix-eval bah) vil derfor returnere en feilmelding da den mangler en
   prosedyre
|#

(newline)
"2a"
#| Jeg har valgt å løse oppgaven med å bygge opp en returliste som appender
   hvert symbol den finner i løvnodene. Siden en slik liste blir bygget opp
   baklengs returnerer jeg til slutt det reverse av listen jeg har bygget opp.
|#
(define (decode bits tree)
  (define (decode-iter returnlist bits current-branch)
    (if (null? bits)
        (reverse returnlist)
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (decode-iter (append
                            (list (symbol-leaf next-branch))
                            returnlist) ;; Bygger opp returliste med append
                           (cdr bits) tree)
              (decode-iter returnlist (cdr bits) next-branch)))))
  (decode-iter '() bits tree))

(newline)
"2b beskrevet i fil"
#| Denne dekodingen gir 'samurais fight ninjas by night'
   Har satt opp treet under og ser på hvordan vi følger koden
   1 0   - samurais
   0 0   - fight
   0 1   - ninjas
   1 1 1 - by
   1 1 0 - night       O
                     0/ \1
              O                  O
            0/ \1              0/ \1
        fight   ninjas  samurais   O
                                 0/ \1
                             night   by
|#
(decode sample-code sample-tree)

(newline)
"2c"
(define (encode message tree)
  (define (encode-symbol symbol tree) ;; Hjelpefunksjon for å kode et symbol
    (define (encode-iter return-bit current-branch)
      (cond ((null? current-branch) ;; Returner tom liste om treet er tomt
             '())
            ((leaf? current-branch) ;; Om nåværende løv-node ikke er lik
             ;; symbolet vi ser etter, returner tom liste. Om den er lik,
             ;; returner return-bit slik den har blitt bygd opp til nå
             (if (not (equal? symbol (symbol-leaf current-branch)))
                 '()
                 return-bit))
            (else
             ;;Legger på 0 om vi går til venstre og leter i venstre side
             ;;Om vi ikke finner symbolet på denne siden vil tom liste bli
             ;;returnert
             (let ((left-side (encode-iter (cons 0 return-bit)
                                           (left-branch current-branch)))
                   ;;Legger på 1 om vi går til høyre og leter i høyre side
                   (right-side (encode-iter (cons 1 return-bit)
                                            (right-branch current-branch))))
               ;;Om venstre side returnerer tom liste, gå heller til høyre
               (if (null? left-side)
                   (if (null? right-side)
                       '() ;;Returnerer tom liste om ikke symbolet er hverken
                       right-side) ;; til høyre eller venstre
                   left-side)))))
    (reverse (encode-iter '() tree)))
  (if (null? message)
      '()  ;; Skifter ut et og et symbol med sin kode-versjon
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Tester
(encode '(ninjas fight ninjas) sample-tree)
(decode (encode '(ninjas fight ninjas) sample-tree) sample-tree)
(encode '(samurais fight ninjas by night) sample-tree)
(decode (encode '(samurais fight ninjas by night) sample-tree) sample-tree)

(newline)
"2d"
(define (grow-huffman-tree symb-freq-list)
  ;; Hjelpefunksjon for å konvertere liste med par til et løv-sett
  (define (grow-1 ordered-list)
    ;; Med bare en node igjen har alle blitt kombinert, returner denne
    (if (= (length ordered-list) 1)
        (car ordered-list)
        ;; Tilordner venstre og høyre side første og andre element av
        ;; ordnet liste
        (let ((left-side (car ordered-list))
              (right-side (cadr ordered-list)))
          ;; Rekursivt kall på grow-1 der vi nå omdefinerer ordered-list.
          ;; Ved å bruke adjoin-set sorterer vi det nye treet skapt av
          ;; første og andre element inn i det resterende av ordered-list.
          ;; Resterende av ordered-list er det tredje elementet (cddr)
          ;; siden vi har tatt ut to element for å kombinere dem.
          ;; (display ordered-list)(newline)(newline) <- Brukte kode
          ;; for å se hvordan listen ble bygd opp, for forståelse
          (grow-1 (adjoin-set (make-code-tree left-side
                                              right-side)
                              (cddr ordered-list))))))
  (grow-1 (make-leaf-set symb-freq-list)))

;Tester
(define freqs '((a 2) (b 5) (c 1) (d 3) (e 1) (f 3)))
(define codebook (grow-huffman-tree freqs))
(decode (encode '(a b c) codebook) codebook)
(decode (encode '(a b b c b d e f) codebook) codebook)

(newline)
"2e besvart i fil"
(define alphabet '((samurais 57)
                   (ninjas 20)
                   (fight 45)
                   (night 12)
                   (hide 3)
                   (in 2)
                   (ambush 2)
                   (defeat 1)
                   (the 5)
                   (sword 4)
                   (by 12)
                   (assassin 1)
                   (river 2)
                   (forest 1)
                   (wait 1)
                   (poison 1)))

(define ninjacode (grow-huffman-tree alphabet))

(define message
  (encode '(ninjas fight ninjas fight ninjas
          ninjas fight samurais
          samurais fight
          samurais fight ninjas
          ninjas fight by night) ninjacode))

(length message)
#| Vi bruker 43 bits på å kode meldingen, som man kan se ut fra
   lengden av variabelen message.
|#
(newline)
(/ 43 17)
#| Den gjennomsnittlige lengden på hvert kodeord er 43/17 som er 
   omtrent 2.53. Lengden av bits delt på antall tegn i meldingen.
|#

#| En fixed-length code trenger log2n bits per symbol for å skille mellom
   n symboler. Med 17 symboler trenger vi da log2(17)=4.09 rundet opp til 5
   bits per symbol. Hele meldingen trenger da 17*5=85 bits for å kodes.
   Vi kan også se på det som at 4 bits gir oss muligheten til å representere
   2^4=16 ulike tekn, mens alfabetet vårt består av 17 tegn. 4 bits er altså
   ikke nok til å representere alle de ulike tegnene, så vi trenger 5 bits
|#
(newline)
"2f"
#| Bruker lignende metode som i encode-funksjonen for å lete i alle greiner
   av treet etter symbolet. 
|#
(define (huffman-leaves tree)
  (define (leaf-picker symbol tree) ;;Hjelpefunksjon for å finne løvnoden til
    (define (leaf-iter return-pair current-branch) ;; et symbol
      (cond ((null? current-branch)
             '())
            ((leaf? current-branch)
             (if (not (equal? symbol (symbol-leaf current-branch)))
                 '()
                 (append (list (symbol-leaf current-branch)
                               (weight-leaf current-branch))
                         return-pair)))
            (else
             (let ((left-side (leaf-iter '()
                                         (left-branch current-branch)))
                   (right-side (leaf-iter '()
                                          (right-branch current-branch))))
               (if (null? left-side)
                   (if (null? right-side)
                       '()
                       right-side)
                   left-side)))))
    (leaf-iter '() tree))
  (define (huff-iter symbol-list return-pairs)
    (if (null? symbol-list)
        return-pairs ;;Returnerer funnet par når symbol-listen er tom.
        ;;Rekursivt kall leter etter gjenstående i symbol-listen og conser
        ;;funnet løvnode til symbol i return-pairs
        (huff-iter (cdr symbol-list)
                   (cons (leaf-picker (car symbol-list) tree)
                           return-pairs))))
  (reverse (huff-iter (symbols tree) '())))

;; Tester
(huffman-leaves sample-tree)
(huffman-leaves ninjacode)

;;; TESTS brukt for å visualisere hva som skjer
#|"Testing for å se oppbyggingen av et tre"
(define test-tree
  (make-code-tree '(leaf a 2) '(leaf b 3)))
(display test-tree)
(newline)
(symbols test-tree)
(weight test-tree)
(leaf? test-tree)
(leaf? (left-branch test-tree))
(symbol-leaf (left-branch test-tree))
(symbol-leaf (right-branch test-tree))
(weight-leaf (left-branch test-tree))|#

#|(newline)
"Forgrening ut til løvnodene i sample-tree"
(define l (left-branch sample-tree))
(define r (right-branch sample-tree))
"l"
(display l)(newline)
"r"
(display r)(newline)

(define ll (left-branch l))
(define lr (right-branch l))
(define rr (right-branch r))
(define rl (left-branch r))
"ll"
(display ll)(newline)
"lr"
(display lr)(newline)
"rl"
(display rl)(newline)
"rr"
(display rr)(newline)

;(define lll (left-branch ll))
;(define llr (right-branch ll))
;(define lrl (left-branch lr))
;(define lrr (right-branch lr))
;(define rll (left-branch rl))
;(define rlr (right-branch rlr))
(define rrl (left-branch rr))
(define rrr (right-branch rr))

;"lll"
;(display lll)(newline)
;"llr"
;(display llr)(newline)
;"lrl"
;(display lrl)(newline)
;"lrr"
;(display lrr)(newline)
;"rll"
;(display rll)(newline)
;"rlr"
;(display rlr)(newline)
"rrl"
(display rrl)(newline)
"rrr"
(display rrr)(newline)|#