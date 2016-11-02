;  /---------------------------------------------------------------------------/
; /--------------------------------PAVIMENTO----------------------------------/
;/---------------------------------------------------------------------------/
(defrule domanda_presenza_massetto_pavimento
	(preparazione_utente ?)
	(pavimento)
	(not (presenza_massetto ?))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "È presente un massetto?"))
	(assert (presenza_massetto ?risposta)))

(defrule domanda_presenza_pavimento_pavimento
	(preparazione_utente ?)
	(pavimento)
	(not (presenza_pavimento ?))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "E' presente già un pavimento?"))
	(assert (presenza_pavimento ?risposta)))

(defrule domanda_interno_esterno_pavimento
	(preparazione_utente ?)
	(and (not interno)
		 (not esterno))
	(pavimento)
	=>
	(bind ?*help* "Rispondere 'interno' se il lavoro deve essere effettuato in una stanza che non sarà soggetta alle intemperie (bagno, cucina, stanza da %nletto, etc), 'esterno' in caso contrario (balcone, terrazzo).")
	(bind ?risposta (ask_question "Il lavoro riguarda l'interno o l'esterno? (esterno/interno" interno esterno))
	(if (eq ?risposta interno)
		then (assert (interno))
		else (assert (esterno))))

(defrule domanda_posa_sopra
	(preparazione_utente ?)
	(pavimento)
	(not (posa_sopra ?))
	(presenza_pavimento TRUE)
	(paviment)
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Vuoi effettuare la posa sopra il pavimento esistente"))
	(assert (posa_sopra ?risposta)))



;------------------------------------------------------------------------------------------------------------
(defrule controllo_condizioni_posa_sopra
	(preparazione_utente ?)
	(posa_sopra TRUE)
	(pavimento)
	=>
	(bind ?*help* "")
	()
	)

(defrule controllo_condizioni_massetto
	)






























