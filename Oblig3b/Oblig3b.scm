(load "evaluator.scm")

(set! the-global-environment (setup-environment))
(define global the-global-environment)

;;(read-eval-print-loop)
(newline)
(display "1a forklart i fil\n")
#|
(foo 2 square) => 0
For (foo 2 square) har vi definert en prosedyre som tar inn to
parametere - cond og else.
I (cond ((= cond 2) 0) evalueres den første cond som den vanlige
conditional som vi er vant med, mens cond nr 2 evalueres som
parameteret vi har sendt inn i foo-prosedyren. Vi sendte jo inn
nettopp 2, som tilsier at dette cond-utsagnet blir sant og
returnerer da 0.

(foo 4 square) => 16
Igjen som i over kommer vi til cond-sjekken, men nå sender vi ikke
inn 2. Vi kommer da til else-uttrykket (else (else cond))
Den første else evalueres som vanlig, mens else nr 2 er det
andre parameteret vi sendte inn. Vi har jo definert else
utenfor skopet til prosedyren, men det er vår definerte funksjon
square som blir sendt inn som parameter. (else cond) vil da
kjøre prosedyren fra det andre parameteret på tallet som ble
sendt inn i det første parameteret, altså (square 4) = (* 4 4) = 16

(cond ((= cond 2) 0)
      (else (else cond))) => 2
Igjen evalueres den første cond og den første else som vanlig,
mens (= cond 2) sjekker om vår egendefinerte cond er 2. Vi
har definert cond til å være lik 3 og går dermed til else-uttrykket.
Vi kjører så prosedyren else på tallet 4, som blir (else 4) = (/ 4 2) = 2
|#

(newline)
(display "2a\n")
"Test av '1+' og '1-'"
(mc-eval '(1+ 2) the-global-environment)
(mc-eval '(1- 2) the-global-environment)

(newline)
(display "2b\n")
(display "Primitive prosedyrer og objekt før innsetting\n")
(primitive-procedure-names)
(newline)
(primitive-procedure-objects)
(newline)

(install-primitive! 'square (lambda (x) (* x x)))
(display "Primitive prosedyrer og objekt etter innsetting av square\n")
(primitive-procedure-names)
(newline)
(primitive-procedure-objects)
(newline)

"Test av square etter innsettelse"
(display "'(square 2) = ")
(mc-eval '(square 2) the-global-environment)
(display "'(square 6) = ")
(mc-eval '(square 6) the-global-environment)

(newline)
(display "3a\n")
(define and-checker
  '(and
    (= 2 2)
    (= 2 3)))

(define or-checker
  '(or
    (= 3 2)
    (= 4 2)
    (= 2 2)))

(display "Test av and-prosedyre\n")
(display "Hvordan prosedyren ser ut\n")
and-checker

(display "Evaluering av prosedyren, burde bli #f\n")
(mc-eval and-checker the-global-environment)

(display "Sjekk om prosedyren er en and-prosedyre via 'and?'\n")
(mc-eval (and? and-checker) the-global-environment)

(display "Sjekk om prosedyren er en special form via 'special-form?'\n")
(mc-eval (special-form? and-checker) the-global-environment)

(newline)
(display "Test av or-prosedyre\n")
(display "Hvordan prosedyren ser ut\n")
or-checker

(display "Evaluering av prosedyren, burde bli #t\n")
(mc-eval or-checker the-global-environment)

(display "Sjekk om prosedyren er en or-prosedyre via 'or?'\n")
(mc-eval (or? or-checker) the-global-environment)

(display "Sjekk om prosedyren er en special form via 'special-form?'\n")
(mc-eval (special-form? or-checker) the-global-environment)

(newline)
(display "3b\n")

(define newif-checker
  '(if (= 2 3)
       then (+ 1 0)
       elsif (= 2 3)
       then (+ 2 0)
       elsif (= 2 3)
       then (+ 3 0)
       else (+ 4 0)))

(display "Test av ny if-prosedyre\n")
(display "Hvordan prosedyren ser ut\n")
newif-checker

(display "Evaluering av prosedyren, burde bli 4\n")
(mc-eval newif-checker the-global-environment)

(display "Sjekk om prosedyren er en if-prosedyre via 'if?'\n")
(mc-eval (if? newif-checker) the-global-environment)

(display "Sjekk om prosedyren er en special form via 'special-form?'\n")
(mc-eval (special-form? newif-checker) the-global-environment)

