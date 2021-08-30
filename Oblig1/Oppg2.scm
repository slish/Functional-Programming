;;2 Kontrollstrukturer og egendefinerte prosedyrer
;;a)
(or (= 1 2)
    "paff!"
    "piff!"
    (zero? (1 - 1)))
;; Dette evaluerer til "paff!" da or-prosedyren vil returnere det første som er 
;; sant. Dette er fordi så fort noe er sant i et or-uttrykk vil uttrykket bli
;; sant, og man trenger ikke da å sjekke resten. Alt annet enn #f er sant, og 
;; "paff!" vil da være sant, og bli returnert

(and (= 1 2)
     "paff!"
     "piff!"
     (zero? (1 - 1)))
;; Dette evaluerer til #f av lik men motsatt grunn som i over, så fort noe er
;; usant i et and-uttrykk er hele uttrykket usant. 1 er ikke lik 2 og #f blir da
;; returnert ved evalueringen av dette.

(if (positive? 42)
    "poff!"
    (i-am-undefined))
;; Dette evaluerer til "poff!" da et if-uttrykk kan returnere to ting, basert på om
;; inputen er sann eller ikke. 42 er et positivt tall og inputen er dermed sann.
;; Den første linjen etter er det som blir returnert om det er sant, og det er "poff!"
;; Dermed returneres dette.

;; Alle disse bryter med evalueringsreglene som Scheme vanligvis bruker der man 
;; evaluerer det innerste uttrykket for å jobbe seg utover. For if-uttrykket kunne man
;; tenke seg at man burde få en error siden "i-am-undefined" er udefinert og kompilatoren
;; ikke vil skjønne hvordan det skal evalueres. Siden if-uttrykket er sant derimot prøver
;; aldri kompilatoren å evaluere dette uttrykket. Det samme gjelder for if og and som
;; stopper evaluering ved respektive første sanne og falske uttrykk og ikke evaluerer
;; det resterende. 

;;b)
;; Definerer først sign med if. Siden if bare kan ha to verdier å returnere bruker
;; jeg en nøstet if-prosedyre som slår inn om x ikke er mindre enn 0.
(define (sign1 x)
  (if (< x 0)
      -1
      (if (> x 0)
          1
          0)))

;; Med cond kan jeg sjekke for og returnere alle verdier, men igjen velger jeg å ikke
;; eksplisitt sjekke at x = 0 da det er eneste gjenværende mulighet om x ikke er større
;; eller mindre enn 0.
(define (sign2 x)
  (cond ((< x 0) -1)
        ((> x 0) 1)
        (else 0)))

;;c)
;; Jeg bygger på at or-prosedyren returnerer det første sanne uttrykket den finner og at
;; and-prosedyren returnerer den siste sanne verdien om uttrykket er sant.
;; Inne i and-prosedyrene sjekker jeg dermed om tallet er positivt og så om det er negativt,
;; og så manipulerer jeg x til å bli 1, -1 eller 0 basert på sjekken. Dette
;; manipulerte tallet vil alltid bli sant og om and-sjekken er sann så vil dette bli returnert,
;; og det blir den første sanne verdien som or-prosedyren finner og returnerer totalt. Om den
;; første sjekken i and-prosedyren er usann blir usann returnert til or-prosedyren som da fortsetter
;; til neste and-prosedyre. Om tallet ikke er større eller mindre enn 0 returneres bare den sanne
;; verdien 0. 
(define (sign3 x)
  (or (and (> x 0)
          (/ x x))
      (and (< x 0)
           (/ x (- x)))
      0))

;; Tester av prosedyrene
#|
"sign1"
(sign1 5)
(sign1 -5)
(sign1 0)
"sign2"
(sign2 5)
(sign2 -5)
(sign2 0)
"sign3"
(sign3 5)
(sign3 -5)
(sign3 0)
|#