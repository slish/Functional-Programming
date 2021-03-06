(load "evaluator.scm")

(set! the-global-environment (setup-environment))

;;(read-eval-print-loop)
(newline)
(display "***1a*** forklart i fil\n")
#|
Oppgave 1a fungerer ikke i REPL'et slik filen er initielt, da
jeg har redefinert IF-funksjonen i Evaluator.scm. 

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
(display "***2a***\n")
(display "Test av '1+' og '1-'\n")
(mc-eval '(1+ 2) the-global-environment)
(mc-eval '(1- 2) the-global-environment)

(newline)
(display "***2b***\n")
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
(display "***3a***\n")
(define and-checker
  '(and
    (= 2 2)
    (= 2 3)))

(define and-checker2
  '(and
    (= 2 2)
    (= 3 3)))

(define or-checker
  '(or
    (= 3 2)
    (= 4 2)
    (= 2 2)))

(define or-checker2
  '(or
    (= 3 2)
    (= 4 2)
    (= 5 2)))

(display "Test av and-prosedyrene\n")
(display "Hvordan prosedyrene ser ut\n")
and-checker
and-checker2

(display "Evaluering av prosedyren, burde bli #f, #t\n")
(mc-eval and-checker the-global-environment)
(mc-eval and-checker2 the-global-environment)

(display "Sjekk om prosedyrene er en and-prosedyre via 'and?'\n")
(mc-eval (and? and-checker) the-global-environment)
(mc-eval (and? and-checker2) the-global-environment)

(display "Sjekk om prosedyrene er en special form via 'special-form?'\n")
(mc-eval (special-form? and-checker) the-global-environment)
(mc-eval (special-form? and-checker2) the-global-environment)

(newline)
(display "Test av or-prosedyrene\n")
(display "Hvordan prosedyren ser ut\n")
or-checker
or-checker2

(display "Evaluering av prosedyren, burde bli #t, #f\n")
(mc-eval or-checker the-global-environment)
(mc-eval or-checker2 the-global-environment)

(display "Sjekk om prosedyren er en or-prosedyre via 'or?'\n")
(mc-eval (or? or-checker) the-global-environment)
(mc-eval (or? or-checker2) the-global-environment)

(display "Sjekk om prosedyren er en special form via 'special-form?'\n")
(mc-eval (special-form? or-checker) the-global-environment)
(mc-eval (special-form? or-checker2) the-global-environment)

(newline)
(display "***3b***\n")

(define newif-checker
  '(if (= 2 3)
       then (+ 1 0)
    elsif (= 2 3)
       then (+ 2 0)
    elsif (= 2 3)
       then (+ 3 0)
    else (+ 4 0)))

(define newif-checker2
  '(if (= 2 3)
       then 1
       else 2))

(display "Test av ny if-prosedyre\n")
(display "Hvordan prosedyrene ser ut\n")
newif-checker
newif-checker2

(display "Evaluering av prosedyrene, burde bli 4, 2\n")
(mc-eval newif-checker the-global-environment)
(mc-eval newif-checker2 the-global-environment)

(display "Sjekk om prosedyrene er en if-prosedyre via 'if?'\n")
(mc-eval (if? newif-checker) the-global-environment)
(mc-eval (if? newif-checker2) the-global-environment)

(display "Sjekk om prosedyrene er en special form via 'special-form?'\n")
(mc-eval (special-form? newif-checker) the-global-environment)
(mc-eval (special-form? newif-checker2) the-global-environment)

(newline)
(display "***3c***\n")

(define let-checker
  '(let ((first 1)
         (second 2)
         (third 3))
     (+ first second third)))

(define let-checker2
  '(let ((x 2)
         (y 3))
     (display (cons x y))
     (+ x y)))

(display "Test av ny let-prosedyre\n")
(display "Hvordan prosedyrene ser ut\n")
let-checker
let-checker2

(display "Hvordan prosedyrene ser ut i lambda-ifisert versjon\n")
(append-expressions-to-lambda
 (let-to-lambda let-checker)
 (let-expressions let-checker))
(append-expressions-to-lambda
 (let-to-lambda let-checker2)
 (let-expressions let-checker2))

(display "Evaluering av prosedyrene, burde vise 6, (2 . 3)5\n")
(mc-eval let-checker the-global-environment)
(mc-eval let-checker2 the-global-environment)

(display "Sjekk om prosedyrene er en let-prosedyre via 'let?'\n")
(mc-eval (let? let-checker) the-global-environment)
(mc-eval (let? let-checker2) the-global-environment)

(display "Sjekk om prosedyren er en special form via 'special-form?'\n")
(mc-eval (special-form? let-checker) the-global-environment)
(mc-eval (special-form? let-checker2) the-global-environment)

(newline)
(display "***3d***\n")
"Se notat om oppgave i evaluator.scm på linje 341"

;; Kode under fungerer bare om man først fjerner kommentering av relevant
;; kode i evaluator.scm

#|(define let-checker2
  '(let x = 2 and
        y = 3 and
        z = 4 in
     (display (cons x
                    (cons y z)))
     (+ x y z)))

(display "Test av ny let-prosedyre\n")
(display "Hvordan prosedyren ser ut\n")
let-checker2

(display "Bygger opp grunnsteinene\n")
"parameter"
(define params (let-parameters let-checker2))
params
"expressions"
(define exps (let-expressions let-checker2))
exps
"kropp"
(define letbody (let-body let-checker2))
letbody
"lambda"
(define letlambda (let-to-lambda let-checker2))
letlambda
"lambda with expressions"
(define lambexp (append-expressions-to-lambda letlambda exps))
lambexp
(display "Evaluerer det vi har bygd opp fra grunnsteiner\n")
(mc-eval lambexp the-global-environment)
(display "Evaluerer grunnuttrykket 'let-checker2'\n")
(mc-eval let-checker2 the-global-environment)|#

#|(car let-checker2)
(cdr let-checker2)
(cadddr (cdr let-checker2))|#

(newline)
(display "Åpner REPL til testing\n")

(read-eval-print-loop)