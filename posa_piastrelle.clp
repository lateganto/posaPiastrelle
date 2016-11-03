(defglobal ?*highest_priority* = 1000)
(defglobal ?*high_priority* = 100)
(defglobal ?*low_priority* = -100)
(defglobal ?*lowest_priority* = -1000)
(defglobal ?*help* = "")

;  /---------------------------------------------------------------------------/
; /---------------------------------TEMPLATES---------------------------------/
;/---------------------------------------------------------------------------/
(deftemplate esperienza
	(slot esperto)
	(slot principiante))

(deftemplate domande_poste
	(slot numero))

(deftemplate domanda
	(slot valore))

(deftemplate car
	(slot nome)
	(slot valore))

;  /---------------------------------------------------------------------------/
; /---------------------------------FUNCTIONS---------------------------------/
;/---------------------------------------------------------------------------/
(deffunction ask_question (?question $?allowed_values)
	(format t (str-cat "%n" ?question))

	(bind ?i 1)
	(progn$ (?o ?allowed_values)
		(format t (str-cat "%n (%d) %s" ) ?i (nth$ ?i ?allowed_values))
		(bind ?i (+ 1 ?i)))
	(format t "%n (%d) help%n" ?i)

	(format t "Inserire scelta: ")
	(bind ?answer (read))

	(while (or (not (numberp ?answer)) (< ?answer 1) (> ?answer (length$ ?allowed_values)))
		(if (eq ?answer (+ 1 (length$ ?allowed_values)))
			then (if (eq (length$ ?*help*) 0)
				  	then (printout t "Non è presente alcun help!" crlf)
				  	else (format t (str-cat ?*help* "%n"))))

		(format t "Inserire scelta: ")
		(bind ?answer (read)))

	(nth ?answer ?allowed_values))

(deffunction ask_question1 (?question $?allowed-values)
  (printout t ?question)
  (bind ?answer (read))
  (if (lexemep ?answer) ;; TRUE is ?answer is a STRING or SYMBOL
      then (bind ?answer (lowcase ?answer)))
     (while (not (member ?answer ?allowed-values)) do
	    (printout t ?question)
	    (bind ?answer (read))
	    (if (lexemep ?answer) 
		then (bind ?answer (lowcase ?answer))))
     ?answer)

(deffunction yes_or_no_p (?question)
	(bind ?allowed_values (create$ si no s n))
  	(format t (str-cat "%n" ?question " (si/s/no/n/help/h): "))
  	(bind ?answer (read))

  	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

  	(while (not (member ?answer ?allowed_values)) do
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "Non è presente alcun help!" crlf)
		  				else (format t (str-cat ?*help* "%n"))))
		(format t (str-cat "%n" ?question " (si/s/no/n/help/h): "))
	    (bind ?answer (read))
	    (if (lexemep ?answer) 
			then (bind ?answer (lowcase ?answer))))

  	(if (or (eq ?answer si) (eq ?answer s))
         then TRUE 
         else FALSE))

(deffunction ask_number (?question)
	(format t (str-cat "%n" ?question " (help/h): "))
	(bind ?answer (read))

	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

	(while (not (numberp ?answer)) do  ;check if answer is a NUMBER
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "Non è presente alcun help!" crlf)
		  				else (format t (str-cat ?*help* "%n"))))
		(format t (str-cat "%n" ?question " (help/h): "))
	    (bind ?answer (read)))
	 ?answer)

;FUNZIONI PER RITRATTAZIONE

(deffunction get-all-facts-by-names
  ($?template-names)
  (bind ?facts (create$))
  (progn$ (?f (get-fact-list))
	   (if (member$ (fact-relation ?f) $?template-names)
	       then (bind ?facts (create$ ?facts ?f))))
  ?facts)

(deffunction stampa_scelte_lavoro ()
	(bind ?i 1)
	(progn$ (?f (get-all-facts-by-names car))
		(if (numberp (fact-slot-value ?f valore))
			then (format t "(%d) %s: %d%n" ?i (fact-slot-value ?f nome) (fact-slot-value ?f valore))
			else (format t "(%d) %s: %s%n" ?i (fact-slot-value ?f nome) (fact-slot-value ?f valore)))
		(bind ?i (+ 1 ?i))))

(deffunction cambia_scelta_da_indice
	(?indice)
	(bind ?f (nth$ ?indice (get-all-facts-by-names car)))
	(retract ?f))

(deffunction gen-int-list
  (?max-n)
  (bind ?int-list (create$))
  (loop-for-count (?i 1 ?max-n)
		  (bind ?int-list (create$ ?int-list ?i)))
  ?int-list)

(deffunction chiedi_cambio_scelte_lavoro
	(?question)
	(printout t "Vuoi cambiare qualche scelta?" crlf)
	(stampa_scelte_lavoro)

	(bind ?num_scelte (length$ (get-all-facts-by-names car)))
	(bind ?response (ask_question1 ?question (create$ (gen-int-list ?num_scelte) c t)))
	(printout t crlf crlf)
	(switch ?response 
		(case t 
			then (printout t "Bye bye!" crlf)
	         	 (halt))
		(case c
			then (return))
		(default (cambia_scelta_da_indice ?response)
				 (chiedi_cambio_scelte_lavoro ?question))))


;  /---------------------------------------------------------------------------/
; /----------------------------PROFILAZIONE UTENTE----------------------------/
;/---------------------------------------------------------------------------/
(defrule inizio
	(declare (salience ?*highest_priority*))
	=>
	(set-strategy random)
	(assert (esperienza (esperto 0) (principiante 0)))
	(assert (domande_poste (numero 0)))
	(printout t crlf "*** Un sistema per la posa di pavimenti e rivestimenti in gres porcellanato ***" crlf crlf))

;Domanda 7: sai usare un livello?

(defrule domanda_anni
	(not (domanda (valore one)))
	?f1 <- (esperienza (esperto ?val_esp) (principiante ?val_princ))
	?f2 <- (domande_poste (numero ?x))
	=>
	(bind ?*help* "Indicare la propria età.")
	(bind ?risposta (ask_number "Quanti anni hai?"))
	(assert (domanda (valore one)))
	(modify ?f2 (numero (+ ?x 1)))
	(if (< ?risposta 14) 
		then (printout t crlf "Forse non hai l'età per lavorare!" crlf) 
	 		 (halt))
	(if (and (>= ?risposta 14) (<= ?risposta 20)) 
		then (modify ?f1 (principiante (+ ?val_princ 3))))
	(if (and (>= ?risposta 21) (<= ?risposta 30)) 
		then (modify ?f1 (principiante (+ ?val_princ 2))))
	(if (and (>= ?risposta 31) (<= ?risposta 50)) 
		then (modify ?f1 (esperto (+ ?val_esp 1)))
			(modify ?f1 (principiante (+ ?val_princ 1))))
	(if (and (>= ?risposta 51) (<= ?risposta 60)) 
		then (modify ?f1 (esperto (+ ?val_esp 2))))
	(if (and (>= ?risposta 61) (<= ?risposta 70)) 
		then (modify ?f1 (esperto (+ ?val_esp 3))))
	(if (> ?risposta 70)
		then (printout t "Forse non hai più l'età per fare certi lavori!" crlf)
			 (halt)))

(defrule domanda_fai_da_te
	(not (domanda (valore two)))
	?f1 <- (esperienza (esperto ?val_esp) (principiante ?val_princ))
	?f2 <- (domande_poste (numero ?x))
	=>
	(bind ?*help* "Rispondere affermativamente se si è realizzato qualche volta un piccolo lavoro in casa o qualche tipo riparazione, %nnegativamente in caso contrario.")
	(bind ?risposta (yes_or_no_p "Se si rompe qualcosa in casa, cerchi di aggiustarla da solo?"))
	(assert (domanda (valore two)))
	(modify ?f2 (numero (+ ?x 1)))
	(if ?risposta 
		then (modify ?f1 (esperto (+ ?val_esp 3)))
		else (modify ?f1 (principiante (+ ?val_princ 3)))))

(defrule domanda_piastrellista
	(not (domanda (valore three)))
	?f1 <- (esperienza (esperto ?val_esp) (principiante ?val_princ))
	?f2 <- (domande_poste (numero ?x))
	=>
	(bind ?*help* "Rispondere affermativamente se nella propria vita si è mai lavorato come piastrellista professionalmente.")
	(bind ?risposta (yes_or_no_p "Sei un piastrellista o hai mai lavorato come piastrellista?"))
	(assert (domanda (valore three)))
	(modify ?f2 (numero (+ ?x 1)))
	(if ?risposta 
		then (modify ?f1 (esperto (+ ?val_esp 10)))
		else (modify ?f1 (principiante (+ ?val_princ 3)))))

(defrule domanda_utilità_sistema
	(not (domanda (valore four)))
	?f1 <- (esperienza (esperto ?val_esp) (principiante ?val_princ))
	?f2 <- (domande_poste (numero ?x))	
	=>
	(bind ?*help* "Rispondere affermativamente nel caso in cui si sia già effettuata la posa di un pavimento o di un rivestimento.")
	(bind ?risposta (yes_or_no_p "Hai mai realizzato prima d'ora la posa di un pavimento?"))
	(assert (domanda (valore four)))
	(modify ?f2 (numero (+ ?x 1)))
	(if ?risposta
		then (modify ?f1 (esperto (+ ?val_esp 3)))
		else (modify ?f1 (principiante (+ ?val_princ 3)))))

(defrule domanda_lavoro
	(not (domanda (valore five)))
	?f1 <- (esperienza (esperto ?val_esp) (principiante ?val_princ))
	?f2 <- (domande_poste (numero ?x))
	=>
	(bind ?*help* "Rispondere affermativamente nel caso in cui si abbia mai svolto nella propria vita un lavoro di tipo manuale (l'operaio ad esempio).")
	(bind ?risposta (yes_or_no_p "Hai mai svolto un lavoro di tipo manuale nella tua vita?"))
	(assert (domanda (valore five)))
	(modify ?f2 (numero (+ ?x 1)))
	(if ?risposta
		then (modify ?f1 (esperto (+ ?val_esp 3)))
		else (modify ?f1 (principiante (+ ?val_princ 3)))))

(defrule esperto
	(declare (salience ?*high_priority*))
	(esperienza (esperto ?val_esp&:(>= ?val_esp 10)))
	=>
	(assert (preparazione_utente alta)))

(defrule principiante
	(declare (salience ?*high_priority*))
	(esperienza (principiante ?val_princ&: (> ?val_princ 10)))
	=>
	(assert (preparazione_utente bassa)))

(defrule determina_esperienza
	(declare (salience ?*high_priority*))
	(domande_poste (numero 5))
	(esperienza (esperto ?val_esp) (principiante ?val_princ))
	=>
	(if (> ?val_esp ?val_princ)
		then (assert (preparazione_utente alta))
		else (assert (preparazione_utente bassa))))

(defrule pulizia
	(declare (salience ?*high_priority*))
	(preparazione_utente ?)
	?f1 <- (domande_poste (numero ?))
	?f2 <- (esperienza (esperto ?) (principiante ?))
	=>
	(do-for-all-facts ((?domanda domanda)) TRUE (retract ?domanda))  ;elimina tutti i fatti di tipo "domanda"
	(retract ?f1 ?f2)
	(printout t crlf crlf)
	(set-strategy depth))


;  /---------------------------------------------------------------------------/ 
; /----------------------------------STEP 1-----------------------------------/		Capire cosa l'utente vuole fare
;/---------------------------------------------------------------------------/
(defrule domanda_interno_esterno
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(and (not (car (nome luogo) (valore interno)))
		 (not (car (nome luogo) (valore esterno))))
	=>
	(bind ?*help* "Rispondere 'interno' se il lavoro deve essere effettuato in una stanza che non sarà soggetta alle intemperie (bagno, cucina, stanza da %nletto, etc), 'esterno' in caso contrario (balcone, terrazzo).")
	(bind ?risposta (ask_question "Il lavoro riguarda l'interno o l'esterno?" interno esterno))
	(if (eq ?risposta interno)
		then (assert (car (nome luogo) (valore interno)))
		else (assert (car (nome luogo) (valore esterno)))))

(defrule domanda_tipo_stanza
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(not (car (nome tipo_stanza) (valore ?)))
	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "Indicare a quale tipo tra quelli elencati corrisponde la stanza in cui deve essere fatto il lavoro. Nel caso in cui ci sia più di una %nrisposta, allora effettuare la scelta di una stanza e continuare, poi riavviare il sistema e procedere con la successiva scelta.")
	(bind ?risposta (ask_question "Quale stanza riguarda il lavoro?" bagno cucina altro))
	(assert (car (nome tipo_stanza) (valore ?risposta))))

(defrule domanda_presenza_pavimento
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(not (car (nome presenza_pavimento) (valore ?)))
	=>
	(bind ?*help* "Rispondere 'si' se è già presente un pavimento nella stanza in cui si intende lavorare, 'no' altrimenti.")
	(bind ?risposta (yes_or_no_p "È già presente un pavimento?"))
	(assert (car (nome presenza_pavimento) (valore ?risposta))))

(defrule domanda_presenza_rivestimento
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(not (car (nome presenza_rivestimento) (valore ?)))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	=>
	(bind ?*help* "Rispondere 'si' se è già presente un rivestimento, cioè le pareti della stanza sono ricoperte con piastrelle, 'no' altrimenti.")
	(bind ?risposta (yes_or_no_p "È già presente un rivestimento?"))
	(assert (car (nome presenza_rivestimento) (valore ?risposta))))

(defrule domanda_presenza_massetto
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(not (car (nome presenza_massetto) (valore ?)))
	(car (nome presenza_pavimento) (valore FALSE))
	=>
	(bind ?*help* "Il massetto è quello strato di cemento la cui presenza è fondamentale perché sopra di esso verranno poste le piastrelle.")
	(bind ?risposta (yes_or_no_p "È presente un massetto?"))
	(assert (car (nome presenza_massetto) (valore ?risposta))))

(defrule domanda_condizioni_pavimento_presente
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(car (nome presenza_pavimento) (valore TRUE))
	(not (car (nome condizioni_pavimento) (valore ?)))
	(not (car (nome ristrutturazione_pavimento) (valore TRUE)))
	=>
	(bind ?*help* "Rispondere 'si' se il pavimento in questione presenta segni di usura come piastrelle scheggiate, consumate o non aderenti.")
	(bind ?risposta (yes_or_no_p "Il pavimento esistente presenta molte piastrelle consumate o non perfettamente aderenti?"))
	(if ?risposta
		then (assert (car (nome condizioni_pavimento) (valore cattive)))
		else (assert (car (nome condizioni_pavimento) (valore buone)))))

(defrule domanda_pavimento_presente_rinnovo
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(car (nome presenza_pavimento) (valore TRUE))
	(not (car (nome ristrutturazione_pavimento) (valore ?)))
	=>
	(bind ?*help* "Rispondere decidendo se si vuole sostituire il pavimento presente con uno nuovo oppure no.")
	(bind ?risposta (yes_or_no_p "Vuoi ristrutturare il pavimento esistente?"))
	(if ?risposta
		then (assert (car (nome ristrutturazione_pavimento) (valore TRUE))) ;chiedi se deve fare fughe o battiscopa o solo aggiustare una piastrella scheggiata
		else (assert (car (nome ristrutturazione_pavimento) (valore FALSE))))) ;deve rimuovere il pavimento

(defrule domanda_condizioni_rivestimento_presente
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(car (nome presenza_rivestimento) (valore TRUE))
	(not (car (nome condizioni_rivestimento) (valore ?)))
	(not (car (nome ristrutturazione_rivestimento) (valore TRUE)))
	=>
	(bind ?*help* "Rispondere 'si' se il rivestimento in questione presenta segni di usura come piastrelle scheggiate, consumate o non aderenti.")
	(bind ?risposta (yes_or_no_p "Il rivestimento presenta molte piastrelle non aderenti, mancanti, scheggiate o consumate?"))
	(if ?risposta 
		then (assert (car (nome condizioni_rivestimento) (valore cattive)))
		else (assert (car (nome condizioni_rivestimento) (valore buone)))))

(defrule domanda_rivestimento_presente_rinnovo
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(car (nome presenza_rivestimento) (valore TRUE))
	(not (car (nome ristrutturazione_rivestimento) (valore ?)))
	=>
	(bind ?*help* "Rispondere decidendo se si vuole sostituire il rivestimento presente con uno nuovo oppure no.")
	(bind ?risposta (yes_or_no_p "Vuoi ristrutturare il rivestimento presente?"))
	(if ?risposta
		then (assert (car (nome ristrutturazione_rivestimento) (valore TRUE))) 
		else (assert (car (nome ristrutturazione_rivestimento) (valore FALSE)))))

(defrule domanda_anni_presenza_pavimento
	(preparazione_utente ?)
	(not (continua))
	(not (lavoro ?))

	(car (nome presenza_pavimento) (valore TRUE))
	(not (car (nome anni_pavimento) (valore ?)))
	=>
	(bind ?*help* "Rispondere indicando (se si conoscono) gli anni che ha il pavimento presente.")
	(bind ?risposta (ask_number "Quanti anni sono che il pavimento non viene sostituito (-1 nel caso non si sappia)?"))
	(while (< ?risposta -1)
		(printout t crlf "Inserisci un numero da 0 in poi, o -1 nel caso tu non conosca gli anni.")
		(bind ?risposta (ask_number "Quanti anni sono che il pavimento non viene sostituito (-1 nel caso non si sappia)?")))
	(assert (car (nome anni_pavimento) (valore ?risposta))))

;---------------------------------------------------------------------------------------------------------------------------------------------------------

(defrule massetto
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_massetto) (valore FALSE))
	=>
	(assert (lavoro massetto)))

(defrule fughe_pavimento ;domanda fughe in caso di pavimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore TRUE))
	(car (nome condizioni_pavimento) (valore buone))
	(car (nome ristrutturazione_pavimento) (valore FALSE))
	=>
	(assert (lavoro fughe)))

(defrule fughe_rivestimento ;domanda fughe in caso di rivestimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_rivestimento) (valore TRUE))
	(car (nome condizioni_rivestimento) (valore buone))
	(car (nome ristrutturazione_rivestimento) (valore FALSE))
	=>
	(assert (lavoro fughe)))

(defrule battiscopa1
	(declare (salience ?*high_priority*))
	(not (continua))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore TRUE))
	(car (nome condizioni_pavimento) (valore buone))
	(car (nome ristrutturazione_pavimento) (valore FALSE))
	=>
	(assert (lavoro battiscopa)))

(defrule battiscopa2
	(declare (salience ?*high_priority*))
	(not (continua))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore altro))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_pavimento) (valore TRUE))
	(car (nome condizioni_pavimento) (valore buone))
	(car (nome ristrutturazione_pavimento) (valore FALSE))
	=>
	(assert (lavoro battiscopa)))

;(defrule rattoppo
;	(declare (salience ?*high_priority*))
;	(not (continua))
;	(not (no_lavoro (nome rattoppo)))
;
;	(or (car (nome luogo) (valore interno) )
;		(car (nome luogo) (valore esterno) ))
;	(car (nome presenza_pavimento) (valore TRUE))
;	(car (nome condizioni_pavimento) (valore buone))
;	(anni_pavimento ?x)
;	(test (<= ?x 6))  ;se il pavimento è troppo vecchio non si fa il rattoppo perché ci sarà una differenza di colore tra la piastrella nuova e quella vecchia
;	=>
;	(bind ?*help* "Rispondere affermativamente se il lavoro che si deve fare è un rattoppo, come la sostituzione di una o più piastrelle scheggiate, %nalzate o usurate; ;rispondere negativamente in caso contrario")
;	(bind ?risposta (yes_or_no_p "Quello che vuoi realizzare è un rattoppo?"))
;	(if ?risposta
;		then (assert (continua))
;			 (assert (rattoppo))
;		else (assert (no_lavoro (nome rattoppo)))))

(defrule pavimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(or (car (nome condizioni_pavimento) (valore cattive))
		(car (nome ristrutturazione_pavimento) (valore TRUE))
		(car (nome presenza_pavimento) (valore FALSE))
		(car (nome presenza_massetto) (valore TRUE)))
	=>
	(assert (lavoro pavimento)))

(defrule rivestimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome condizioni_rivestimento) (valore cattive))
		(car (nome ristrutturazione_rivestimento) (valore TRUE))
		(car (nome presenza_rivestimento) (valore FALSE)))
	=>
	(assert (lavoro rivestimento)))

(defrule pavimento_rivestimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome condizioni_pavimento) (valore cattive))
		(car (nome ristrutturazione_pavimento) (valore TRUE))
		(car (nome presenza_pavimento) (valore FALSE))
		(car (nome presenza_massetto) (valore TRUE)))
	(or (car (nome condizioni_rivestimento) (valore cattive))
		(car (nome ristrutturazione_rivestimento) (valore TRUE))
		(car (nome presenza_rivestimento) (valore FALSE)))
	=>
	(assert (lavoro pavimento_rivestimento)))

;(defrule pavimento_rivestimento2  ;domanda nel caso in cui risponda no a pavimento, è molto probabile che voglia fare entrambi
;	(declare (salience ?*high_priority*))
;	(not (continua))
;
;	;(no_lavoro (nome pavimento))
;	(or (car (nome condizioni_rivestimento) (valore cattive))
;		(car (nome ristrutturazione_rivestimento) (valore TRUE))
;		(car (nome presenza_rivestimento) (valore FALSE)))
;	=>
;	(assert (lavoro pavimento_rivestimento)))
;
;(defrule pavimento_rivestimento3  ;domanda nel caso in cui risponda no a rivestimento è molto probabile che voglia fare entrambi
;	(declare (salience ?*high_priority*))
;	(not (continua))
;
;	;(no_lavoro (nome rivestimento))
;	(or (car (nome condizioni_pavimento) (valore cattive))
;		(car (nome ristrutturazione_pavimento) (valore TRUE))
;		(car (nome presenza_pavimento) (valore FALSE))
;		(car (nome presenza_massetto) (valore TRUE)))
;	=>
;	(assert (lavoro pavimento_rivestimento)))

;-----------------------------------------------------------------------------------------------------------------

(defrule lavoro_trovato
	(declare (salience ?*high_priority*))
	(lavoro ?lavoro)
	=>
	(printout t crlf ">>>>> Il lavoro che devi fare è: " ?lavoro crlf crlf)

	(if (yes_or_no_p "È quello che volevi?")
		then (assert (continua))
		else (assert (rivedi_scelte_lavoro))))

(defrule lavoro_non_trovato
	(declare (salience ?*lowest_priority*))
	(not (lavoro ?))
	=>
	(printout t crlf "Lavoro non trovato!" crlf)
	(if (yes_or_no_p "Vuoi rivedere le scelte fatte o cambiare qualcosa?")
		then (assert (rivedi_scelte_lavoro))
		else (printout t crlf "Prova a riavviare il sistema") 
			 (halt)))

(defrule rivedi_scelte_lavoro
	(declare (salience ?*high_priority*))
	?f2 <- (lavoro ?)
	?f1 <- (rivedi_scelte_lavoro)
	=>
	(retract ?f1 ?f2)
	(chiedi_cambio_scelte_lavoro "Inserisci il numero della scelta che vuoi modificare o 't' per terminare o 'c' per continuare: "))

(defrule rivedi_scelte_no_lavoro
	(declare (salience ?*high_priority*))
	(not (lavoro ?))
	?f <- (rivedi_scelte_lavoro)
	=>
	(retract ?f)
	(chiedi_cambio_scelte_lavoro "Inserisci il numero della scelta che vuoi modificare o 't' per terminare o 'c' per continuare: "))