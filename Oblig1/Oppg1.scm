;;1 Grunnleggende syntaks og semantikk i Scheme
;;a)
(* (+ 4 2) 5) ;; Dette evaluerer til 30. Man evaluerer innenfra og får 4+2 = 6, og så utover som gir 6*5 = 30

;;b)
;;(* (+ 4 2) (5))  Dette gir en feilmelding da man forventer en ny prosedyre når man lager en ny parantes.
                ;; Hadde man endret (5) til (+ 5) hadde det vært en korrekt prosedyre og ville da gitt samme
                ;; svar som i a)

;;c)
;;(* (4 + 2) 5) Dette gir en feilmelding da man ikke bruker prefiksnotasjonen på pluss-prosedyren.
             ;; Scheme tror da at den skal utføre "4" på + og 2, noe som ikke er definert.

;;d)
(define bar (/ 44 2))  ;; Her blir først "bar" definert som 44/2=22, og så evaluerer vi bar i neste linje.
bar                    ;; "bar" blir da evaluert til 22.                 
                       
;;e)
(- bar 11) ;; bar er fortsatt definert som 22 og vi kjører så minusprosedyren på dette og får at uttrykket
           ;; evaluerer til 22-11 = 11

;;f)
(/ (* bar 3 4 1) bar) ;; Prosedyren kjører her først innerst og gir oss produktet av 22, 3, 4 og 1 som er 264
                    ;; Så vil det ytterste evalueres og vi får da 264 / 22 = 12
                    ;; Et menneske ville nok fort ha oppdaget at vi både multipliserer og dividerer med 22,
                    ;; og så ha strøket disse fra regnestykket. Da trenger vi bare å regne 3*4*1 = 12,
                    ;; men slik logikk finnes vel dessverre ikke i kompilatorene.