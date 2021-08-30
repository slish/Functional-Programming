;;012345678901234567890123456789012345678901234567890123456789012345678901234567

;;3 Rekursjon, iterasjon, og blokkstruktur
;;a)
(define (add1 add-number)
  (+ add-number 1))

(define (sub1 subtract-number)
  (- subtract-number 1))

#|b)
Vi legger til rekursivt 1 til det første tallet mens vi trekker fra 1 fra
det andre tallet. Når det andre tallet har nådd null er det første tallet
summen av tallene, og vi kan returnere det.
|#
(define (plus number1 number2)
  (if (zero? number2)
      number1
      (plus (add1 number1) (sub1 number2))))

#|
Prosedyren over er veldig lite effektiv om det ene tallet er mye større enn det
andre, så jeg lagde også en mer effektiv versjon som sjekker hvilket tall som
er størst. Den trekker så fra det minste tallet, slik at vi har en kortere vei
til 0 og færre operasjoner som må utføres.
|#
(define (efficient-plus number1 number2)
  (if (> number1 number2)
      (if (zero? number2)
          number1
          (efficient-plus (add1 number1) (sub1 number2)))
      (if (zero? number1)
          number2
          (efficient-plus (add1 number2) (sub1 number1)))))

#|c)
Prosedyren over gir opphav til en iterativ prosess da vi vedlikeholder et
løpende tall helt til det andre tallet når 0. Når vi når basistallet er
den endelige returverdien ferdig beregnet og kan returneres. 
Setter opp en lignende prosedyre som kjører en rekursiv prosedyre under.
Den "bygger" nå opp en mengde med add1-prosedyrer som venter på å legge til 1
til det som kommer ut av base-caset. 
|#
(define (recursive-plus number1 number2)
  (if (= number2 0)
      number1
      (add1 (recursive-plus number1 (sub1 number2)))))

#|d)
Prøvde å gjenskape denne prosedyren både iterativt og rekursivt, tror absolutt
den blir mer elegant som rekursiv. power-iter blir også litt forenklet da den nå
ikke lenger trenger å ta inn b og n som argumenter, bare en e-variabel som vi
kan legge til. b og n er nå allerede i skopet til prosedyren så vi kan kalle på
de også i power-iter. 
|#
(define (power-close-to1 b n)
  (define (power-iter e)
    (define (iter b e)
      (if (> (expt b e) n)
          e
          (iter b (add1 e))))
    (iter b e))
  (power-iter 1))

(define (power-close-to2 b n)
  (define (power-iter e)
    (if (> (expt b e) n)
        e
        (power-iter (add1 e))))
  (power-iter 1))

#|e)
I blokkstrukturen av Fibonacci-prosedyren klarer jeg ikke å forenkle antall
variabler i hjelpeprosedyren da nettopp n er telleren som prosedyren må ha. 
|#
(define (fib n)
  (define (fib-iter a b n)
    (if (= n 0)
        b
        (fib-iter (+ a b) a (- n 1))))
  (fib-iter 1 0 n))