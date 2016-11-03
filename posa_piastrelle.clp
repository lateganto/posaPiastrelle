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

(deftemplate no_lavoro
	(slot nome))

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

;rivedere scelte

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
	(if (< ?risposta 16) 
		then (printout t crlf "Forse non hai l'età per lavorare!" crlf) 
	 		 (halt))
	(if (and (>= ?risposta 16) (<= ?risposta 20)) 
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
	(printout t crlf crlf))


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

	(or (car (nome luogo) (valore interno) )
		(car (nome luogo) (valore esterno) ))
	(car (nome presenza_rivestimento) (valore TRUE))
	(car (nome condizioni_rivestimento) (valore buone))
	(car (nome ristrutturazione_rivestimento) (valore FALSE))
	=>
	(assert (lavoro fughe)))

(defrule battiscopa1
	(declare (salience ?*high_priority*))
	(not (continua))

	(car (nome luogo) (valore esterno) )
	(car (nome presenza_pavimento) (valore TRUE))
	(car (nome condizioni_pavimento) (valore buone))
	(car (nome ristrutturazione_pavimento) (valore FALSE))
	=>
	(assert (lavoro battiscopa)))

(defrule battiscopa2
	(declare (salience ?*high_priority*))
	(not (continua))

	(car (nome luogo) (valore interno) )
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
	(declare (salience ?*highest_priority*))
	(lavoro ?lavoro)
	=>
	(printout t crlf ">>>>> Il lavoro che devi fare è: " ?lavoro crlf crlf)

	(if (yes_or_no_p "Non è quello che volevi e vuoi rivedere qualcosa?")
		then (assert (rivedi_scelte_lavoro))
		else (assert (continua))))

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


;  /---------------------------------------------------------------------------/
; /----------------------------------MASSETTO---------------------------------/
;/---------------------------------------------------------------------------/

(defrule domanda_pavimento_da_raccordare
	(lavoro massetto)
	(continua)
	(not (car (nome pavimento_da_raccordare) (valore ?)))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Nello stesso piano ci sono altri pavimenti già posati?"))
	(assert (car (nome pavimento_da_raccordare) (valore ?risposta))))

(defrule domanda_dimensione_pavimento_esperto
	(preparazione_utente alta)
	(lavoro massetto)
	(continua)
	(not (car (nome dimensione_area) (valore ?)))
	=>
	(bind ?*help* "Indicare il numero che rappresenta la dimensione in metri quadri dell'area in cui si intende lavorare.")
	(bind ?risposta (ask_number "Fornire la dimensione in metri quadri dell'area in cui si deve lavorare: "))
	(assert (car (nome dimensione_area) (valore ?risposta))))

(defrule domanda_dimensione_pavimento_principiante
	(preparazione_utente bassa)
	(lavoro massetto)
	(continua)
	(not (car (nome dimensione_area) (valore ?)))
	=>
	(printout t "La misura dell'area da pavimentare non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un pavimento si" crlf 
				"effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella" crlf
				"dell'area da pavimentare, ma deve essere maggiore. Procedere individuando la forma di tale superficie, se questa può essere ricondotta" crlf 
				"ad una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, allora, per ottenere l'area, bisogna ricordare che:" crlf
				"	* se la superficie ha la forma di un quadrato, allora si calcola la lunghezza di un muro (che rappresenta il lato) e la si moltiplica" crlf
				"	  per se stesso" crlf
				"	* se la superficie ha la forma di un rettangolo, allora si moltiplica la dimensione del muro più lungo (che rappresenta la lunghezza)" crlf
				"	  per la dimensione del muro più piccolo (che rappresenta la larghezza)" crlf
				"	* se la superficie ha la forma di un triangolo, allora si trova la lunghezza del muro che rappresenta la base e quella del muro che" crlf
				"	  rappresenta l'altezza  del triangolo, si moltiplicando tra di loro le due misure e si divide per due il risultato" crlf
				"	* se la superficie ha la forma di un cerchio, allora si deve trovare la misura del raggio e si usa la formula 2πr, dove π = 3.14 e" crlf 
				"	  r = raggio calcolato (cioè si calcola il raggio, che è la metà del diametro e lo si moltiplica prima per due e poi per 3.14)" crlf
				"	* se la superficie ha la forma di una semicirconferenza, allora si procede come nel caso precedente (cerchio) a trovare la misura del" crlf
				"	  raggio della circonferenza e si usa la formula 2πr (dove π = 3.14 e r = raggio trovato) e si divide il risultato per due." crlf
				"Nel caso in cui la forma della superficie da pavimentare non fosse simile ad una delle precedenti, allora si suddivide l'area in parti più" crlf
				"piccole dalla forma riconducibile ad una di quelle precedenti, si calcola l'area di ogni parte e si sommano i vari risultati ottenuti" crlf
				"Le misure vanno espresse in metri al quadrato" crlf crlf)
	(bind ?*help* "Indicare il numero che rappresenta la dimensione in metri quadri dell'area in cui si intende lavorare.")
	(bind ?risposta (ask_number "Fornire quindi la dimensione in metri quadri dell'area in cui si deve lavorare?"))
	(assert (car (nome dimensione_area) (valore ?risposta))))

(defrule domanda_presenza_porte
	(preparazione_utente ?)
	(lavoro massetto)
	(continua)
	(car (nome pavimento_da_raccordare) (valore FALSE))
	(not (car (nome porte_da_raccordare) (valore ?)))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Sono presenti porte o balconi già montati?"))
	(assert (car (nome porte_da_raccordare) (valore TRUE))))

;------------------------------------------------------------------------
(defrule area_troppo_grande
	(declare (salience ?*high_priority*))
	(preparazione_utente ?)
	(lavoro massetto)
	(continua)
	(car (nome dimensione_area) (valore ?dim))
	(test (> ?dim 50))
	=>
	(printout t crlf "L'area in cui si deve fare il massetto è troppo ampia! Consulta un muratore!" crlf)
	(printout t crlf "Premi 'c' per chiudere il programma: ")
	(while (neq (read) c)
		(printout t crlf "Premi 'c' per chiudere il programma: "))
	(halt))

(defrule domanda_spessore_piastrella
	(preparazione_utente ?)
	(lavoro massetto)
	(continua)
	(or (car (nome pavimento_da_raccordare) (valore TRUE))
		(car (nome porte_da_raccordare) (valore TRUE)))
	(not (car (nome spessore_piastrella_pavimento) (valore ?)))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Hai già scelto la piastrella da usare?"))
	(if ?risposta 
		then (bind ?*help* "")
			 (bind ?risposta (ask_number "Indica lo spessore della piastrella in millimetri"))
			 (assert (car (nome spessore_piastrella_pavimento) (valore ?risposta)))
		else (printout t crlf "Senza lo spessore della piastrella non si può sapere quanto sarà alto il massetto!")
			(printout t crlf "Premi 'c' per chiudere il programma: ")
			(while (neq (read) c)
				(printout t crlf "Premi 'c' per chiudere il programma: "))
			(halt)))

(defrule guide_e_massetto_raccordo_interno
	(preparazione_utente ?)
	(lavoro massetto)
	(continua)
	(or (car (nome pavimento_da_raccordare) (valore TRUE))
		(car (nome porte_da_raccordare) (valore TRUE)))
	(car (nome spessore_piastrella_pavimento) (valore ?spessore_piastrella))
	(car (nome luogo) (valore interno))
	=> 
	(printout t crlf "Hai bisogno di:" crlf
					" * cazzuole (grande e piccola, a punta e piatta)" crlf
					" * 2-3 secchi per il cemento" crlf
					" * stadie di diverse lunghezze" crlf
					" * frattazzo in plastica" crlf
					" * sabbia"
					" * cemento"
					" * acqua"
					" * betoniera"
					" * livella" crlf crlf)
	(format t "%nIl massetto che si deve realizzare deve essere esattamente %d mm sotto il pavimento o le porte già presenti%n" (+ ?spessore_piastrella 3))
	(format t "%nRealizza in un secchio un po' di impasto mescolando un po' di sabbia e cemento con acqua.%n")
	(format t "%nRealizza un piccolo spessore con pezzi di piastrelle vecchie, da porre addossato al pavimento già presente (esattamente %d mm sotto)%n e fissalo con il cemento in modo che non faccia movimenti e sia ben saldo%n" (+ ?spessore_piastrella 3))
	(printout t crlf "Poni allo stesso modo un altro spessore alla distanza di circa 1,5 metri da quello precedente e poni le estremità di una stadia" crlf
					"di dimensione adeguata su questi due spessori vicini. Posa sopra la stadia una livella." crlf
					"Abbassa o alza il secondo spessore in base al posizionamento della bolla della livella (che deve essere in posizione centrale)." crlf
					"Realizza altri spessori coprendo tutto il perimetro della stanza." crlf crlf
					"Bisogna adesso realizzare l'impasto di sabbia e cemento con acqua miscelando 2 quintali di cemento per metro cubo di sabbia." crlf
					"Non si può sapere quanto materiale servirà poiché dipende dalle irregolarità del fondo sottostante" crlf
					"Partendo dal punto più lontano dall'uscita cominciare a riempire la parte vuota tra due spessori (riferimenti) e continuare così fino" crlf
					"a riempire tutta la superficie." crlf
					"Fare attenzione a lasciarsi sempre lo spazio per poter uscire, quindi il pezzo dell'ingresso va fatto per ultimo" crlf
					"Una volta completato un piccolo pezzo misurare se è a livello e lisciare con il frattazzo in plastica."crlf crlf))

(defrule guide_e_massetto_raccordo_esterno
	(preparazione_utente ?)
	(lavoro massetto)
	(continua)
	(or (car (nome pavimento_da_raccordare) (valore TRUE))
		(car (nome porte_da_raccordare) (valore TRUE)))
	(car (nome spessore_piastrella_pavimento) (valore ?spessore_piastrella))
	(car (nome luogo) (valore esterno))
	=>
	(printout t crlf "Hai bisogno di:" crlf
					" * cazzuole (grande e piccola, a punta e piatta)" crlf
					" * 2-3 secchi per il cemento" crlf
					" * stadie di diverse lunghezze" crlf
					" * frattazzo in plastica" crlf
					" * sabbia"
					" * cemento"
					" * acqua"
					" * betoniera"
					" * livella" crlf crlf)
	(format t "%nIl massetto che si deve realizzare deve essere esattamente %d mm sotto il pavimento o le porte già presenti%n" (+ ?spessore_piastrella 3))
	(format t "%nRealizza in un secchio un po' di impasto mescolando un po' di sabbia e cemento con acqua.%n")
	(format t "%nRealizza un piccolo spessore con pezzi di piastrelle vecchie, da porre addossato al pavimento già presente (esattamente %d mm sotto)%n e fissalo con il cemento in modo che non faccia movimenti e sia ben saldo%n" (+ ?spessore_piastrella 3))
	(printout t crlf "Bisogna individuare dove avverrà lo scolo dell'acqua; dopo averlo stabilito per creare la pendenza e permettere all'acqua di uscire" crlf
					 "poni allo stesso modo un altro spessore che sarà più basso di 1-1,5 cm alla distanza di circa 2 metri da quello precedente e poni le" crlf 
					 "estremità di una  stadia di dimensione adeguata su questi due spessori vicini. Posa sopra la stadia una livella e poni uno spessore" crlf
					 "di 1-1,5 per controllare che sia all'altezza giusta." crlf
					 "Abbassa o alza il secondo spessore in base al posizionamento della bolla della livella (che deve essere in posizione centrale)." crlf
					 "Realizza altri spessori coprendo tutto il perimetro della stanza." crlf crlf
					 "Bisogna adesso realizzare l'impasto di sabbia e cemento con acqua miscelando 3 quintali di cemento per metro cubo di sabbia." crlf
					 "Non si può sapere quanto materiale servirà poiché dipende dalle irregolarità del fondo sottostante" crlf
					 "Partendo dal punto più lontano dall'uscita cominciare a riempire la parte vuota tra due spessori (riferimenti) e continuare così fino" crlf
					 "a riempire tutta la superficie." crlf
					 "Fare attenzione a lasciarsi sempre lo spazio per poter uscire, quindi il pezzo dell'ingresso va fatto per ultimo" crlf
					 "Una volta completato un piccolo pezzo misurare se è a livello e lisciare con il frattazzo in plastica."crlf crlf))

(defrule guide_e_massetto_no_raccordo_interno
	(preparazione_utente ?)
	(lavoro massetto)
	(continua)
	(car (nome luogo) (valore interno))
	(or (car (nome pavimento_da_raccordare) (valore FALSE))
		(car (nome porte_da_raccordare) (valore FALSE)))
	=>
	(printout t crlf "Hai bisogno di:" crlf
					" * cazzuole (grande e piccola, a punta e piatta)" crlf
					" * 2-3 secchi per il cemento" crlf
					" * stadie di diverse lunghezze" crlf
					" * frattazzo in plastica" crlf
					" * sabbia"
					" * cemento"
					" * acqua"
					" * betoniera"
					" * livella" crlf crlf)
	(format t "%nRealizza in un secchio un po' di impasto mescolando un po' di sabbia e cemento con acqua.%n")
	(format t "%nRealizza un piccolo spessore con pezzi di piastrelle vecchie, da porre in un angolo e fissalo con il cemento in modo che non faccia %nmovimenti e sia ben saldo%n")
	(printout t crlf "Poni allo stesso modo un altro spessore alla distanza di circa 1,5 metri da quello precedente e poni le estremità di una stadia" crlf
					"di dimensione adeguata su questi due spessori vicini. Posa sopra la stadia una livella." crlf
					"Abbassa o alza il secondo spessore in base al posizionamento della bolla della livella (che deve essere in posizione centrale)." crlf
					"Realizza altri spessori coprendo tutto il perimetro della stanza." crlf crlf
					"Bisogna adesso realizzare l'impasto di sabbia e cemento con acqua miscelando 2 quintali di cemento per metro cubo di sabbia." crlf
					"Non si può sapere quanto materiale servirà poiché dipende dalle irregolarità del fondo sottostante" crlf
					"Partendo dal punto più lontano dall'uscita cominciare a riempire la parte vuota tra due spessori (riferimenti) e continuare così fino" crlf
					"a riempire tutta la superficie." crlf
					"Fare attenzione a lasciarsi sempre lo spazio per poter uscire, quindi il pezzo dell'ingresso va fatto per ultimo" crlf
					"Una volta completato un piccolo pezzo misurare se è a livello e lisciare con il frattazzo in plastica."crlf crlf))

(defrule guide_e_massetto_no_raccordo_esterno
	(preparazione_utente ?)
	(lavoro massetto)
	(continua)
	(car (nome luogo) (valore esterno))
	(or (car (nome pavimento_da_raccordare) (valore FALSE))
		(car (nome porte_da_raccordare) (valore FALSE)))
	=>
	(printout t crlf "Ecco tutto quello di cui hai bisogno:" crlf
					 " * cazzuole (grande e piccola, a punta e piatta)" crlf
					 " * 2-3 secchi per il cemento" crlf
					 " * stadie di diverse lunghezze" crlf
					 " * frattazzo in plastica" crlf
					 " * sabbia" crlf
					 " * cemento" crlf
					 " * acqua" crlf
					 " * betoniera" crlf
					 " * livella" crlf crlf)
	(format t "%nRealizza in un secchio un po' di impasto mescolando un po' di sabbia e cemento con acqua.%n")
	(format t "%nRealizza un piccolo spessore con pezzi di piastrelle vecchie, da porre in un angolo e fissalo con il cemento in modo che non faccia %nmovimenti e sia ben saldo%n")
	(printout t crlf "Bisogna individuare dove avverrà lo scolo dell'acqua; dopo averlo stabilito per creare la pendenza e permettere all'acqua di uscire" crlf
					 "poni allo stesso modo un altro spessore che sarà più basso di 1-1,5 cm alla distanza di circa 2 metri da quello precedente e poni le" crlf 
					 "estremità di una  stadia di dimensione adeguata su questi due spessori vicini. Posa sopra la stadia una livella e poni uno spessore" crlf
					 "di 1-1,5 per controllare che sia all'altezza giusta." crlf
					"Abbassa o alza il secondo spessore in base al posizionamento della bolla della livella (che deve essere in posizione centrale)." crlf
					"Realizza altri spessori coprendo tutto il perimetro della stanza." crlf crlf
					"Bisogna adesso realizzare l'impasto di sabbia e cemento con acqua miscelando 3 quintali di cemento per metro cubo di sabbia." crlf
					"Non si può sapere quanto materiale servirà poiché dipende dalle irregolarità del fondo sottostante" crlf
					"Partendo dal punto più lontano dall'uscita cominciare a riempire la parte vuota tra due spessori (riferimenti) e continuare così fino" crlf
					"a riempire tutta la superficie." crlf
					"Fare attenzione a lasciarsi sempre lo spazio per poter uscire, quindi il pezzo dell'ingresso va fatto per ultimo" crlf
					"Una volta completato un piccolo pezzo di 1 metro quadro, misurare se è a livello e lisciare con il frattazzo in plastica."crlf crlf))



;  /---------------------------------------------------------------------------/
; /-----------------------------------FUGHE-----------------------------------/
;/---------------------------------------------------------------------------/
(defrule fughe_interno_rivestimento
	(preparazione_utente ?)
	(lavoro fughe)
	(continua)
	=>
	(printout t crlf "Ecco tutto quello di cui hai bisogno:" crlf
					 " * stucco per fughe per interni" crlf
					 " * vasca di lavaggio per piastrellisti o un normale secchio" crlf
					 " * frattazzo in gomma (per spatolare lo stucco)" crlf
					 " * frattazzo in spugna (per la pulizia delle piastrelle)" crlf 
					 " * secchio e cazzuola piccola" crlf crlf)
	(printout t crlf "Metti un po' di stucco in polvere nel secchio e aggiungi l'acqua mescolando con la cazzuola fino ad ottenere un composto denso." crlf 
					 "Infatti se c'è un rivestimento deve essere tale da non colare nel momento in cui lo si pone sulle fughe." crlf
					 "Porre una piccola quantità sul frattazzo in gomma con la cazzuola e spalmarlo in corrispondenza delle fughe, ricordando che con movimenti" crlf 
					 "paralleli alla fuga si rimuove il composto in eccesso e si liscia, mentre con movimenti opposti al verso della fuga si riempie." crlf
					 "Dopo aver stuccato tutte le fughe, aspettare circa un'ora e proseguire alla pulizia." crlf crlf
					 "Riempire il secchio con acqua, inumidire il frattazzo in spugna e procedere a pulire l'intera area stuccata stando attenti a non scavare " crlf 
					 "troppo le fughe. Risciacquare spesso la spugna e cambiare l'acqua del secchio quando è troppo sporca." crlf
					 "Fare attenzione soprattutto alle piastrelle ruvide che possono nascondere lo sporco dato dallo stucco in eccesso." crlf crlf))

;  /---------------------------------------------------------------------------/
; /---------------------------------RATTOPPO----------------------------------/
;/---------------------------------------------------------------------------/





;  /---------------------------------------------------------------------------/
; /--------------------------------BATTISCOPA---------------------------------/
;/---------------------------------------------------------------------------/
(defrule chiedi_rivestimento_cucina
	(declare (salience ?*high_priority*))
	(preparazione_utente ?)
	(lavoro battiscopa)
	(continua)
	(car (nome tipo_stanza) (valore cucina))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "La cucina presenta un rivestimento che parte dal pavimento?"))
	(if ?risposta
		then (printout t crlf "Non si può apporre il battiscopa poiché è presente un rivestimento!" crlf)
			 (printout t crlf "Premi 'c' per chiudere il programma: ")
			 (while (neq (read) c)
			 	(printout t crlf "Premi 'c' per chiudere il programma: "))
			 (halt)))

(defrule battiscopa_interno_principiante
	(preparazione_utente bassa)
	(lavoro battiscopa)
	(continua)
	(car (nome luogo) (valore interno))
	=>
	(printout t crlf "Ecco tutto quello di cui hai bisogno:" crlf
					 " * colla per interni" crlf
					 " * acqua" crlf
					 " * livella" crlf
					 " * miscelatore elettrico (consigliato)" crlf
					 " * distanziatori" crlf 
					 " * secchio e cazzuola (piccola e grande)" crlf
					 " * smerigliatrice" crlf
					 " * matita da muratore" crlf crlf)

	(printout t crlf "Versa la colla in polvere nel secchio, aggiungi acqua in modo che tutta la polvere lo assorba e gira a mano o con il miscelatore." crlf
					 "L'impasto non deve essere molto liquido." crlf
					 "Parti nella posa da uno degli spigoli nella stanza (se ve ne sono) in modo che eventuali ritagli vadano a finire negli angoli, perché" crlf
					 "questi ultimi vengono generalmente coperti da elementi d'arredo. Se non vi sono spigoli partire da uno degli angoli." crlf
					 "Prendi un bel po' di colla e spalmala bene sul battiscopa; poi addossalo al muro. Prosegui nella posizione degli altri pezzi allo stesso modo" crlf
					 "In mezzo ad ogni battiscopa poni i distanziatori della dimensione desiderata." crlf
					 "Controlla con un livello o una stadia da 50 cm, dopo averne messi due o tre, che siano precisi e prosegui." crlf crlf

					 "Nel caso in cui non si possa inserire il pezzo intero del battiscopa occorrerà fare dunque un taglio, misurare la distanza tra il pezzo posato" crlf
					 "e il muro. Ci possono essere due casi:" crlf
					 " * nel caso in cui il taglio va effettuato all'ANGOLO, allora si toglie a tale misura della distanza, quella per la fuga (pari alla dimensione" crlf
					 "   del distanziatore) e un mezzo centimetro per non far incastrare il pezzo." crlf
					 " * nel caso in cui il taglio va effettuato allo SPIGOLO, allora bisogna fare in modo da aggiungere alla dimensione quella data dallo spessore" crlf 
					 "   del pezzo più la colla. In modo che un pezzo vada a filo con l'altro che verrà posto in maniera perpendicolare." crlf
					 "Segnare con la matita da muratore sulla piastrella il punto in cui deve effettuarsi il taglio e tracciare una linea più o meno dritta usando poi" crlf
					 "la smerigliatrice per effettuare il taglio vero e proprio." crlf crlf))

(defrule battiscopa_interno_esperto
	(preparazione_utente alta)
	(lavoro battiscopa)
	(continua)
	(car (nome luogo) (valore interno))
	=>
	(printout t crlf "Ecco tutto quello di cui hai bisogno:" crlf
					 " * colla per interni" crlf
					 " * acqua" crlf
					 " * livella" crlf
					 " * miscelatore elettrico (consigliato)" crlf
					 " * distanziatori" crlf
					 " * smerigliatrice" crlf 
					 " * secchio e cazzuola (piccola e grande)" crlf)
	(printout t crlf "Parti nella posa da uno degli spigoli nella stanza (se ve ne sono) in modo che eventuali ritagli vadano a finire negli angoli, perché" crlf
					 "questi ultimi vengono generalmente coperti da elementi d'arredo. Se non vi sono spigoli partire da uno degli angoli." crlf
					 "Prendi un bel po' di colla e spalmala bene sul battiscopa; poi addossalo al muro. Prosegui nella posizione degli altri pezzi allo stesso modo" crlf
					 "In mezzo ad ogni battiscopa poni i distanziatori della dimensione desiderata." crlf
					 "Controlla con un livello o una stadia da 50 cm, dopo averne messi due o tre, che siano precisi e prosegui." crlf crlf

					 "Nel caso in cui non si possa inserire il pezzo intero del battiscopa occorrerà fare dunque un taglio, misurare la distanza tra il pezzo posato" crlf
					 "e il muro. Ci possono essere due casi:" crlf
					 " * nel caso in cui il taglio va effettuato all'ANGOLO, allora si toglie a tale misura della distanza, quella per la fuga (pari alla dimensione" crlf
					 "   del distanziatore) e un mezzo centimetro per non far incastrare il pezzo." crlf
					 " * nel caso in cui il taglio va effettuato allo SPIGOLO, allora bisogna fare in modo da aggiungere alla dimensione quella data dallo spessore" crlf 
					 "   del pezzo più la colla. In modo che un pezzo vada a filo con l'altro che verrà posto in maniera perpendicolare." crlf
					 "Segnare con la matita da muratore sulla piastrella il punto in cui deve effettuarsi il taglio e tracciare una linea più o meno dritta usando poi" crlf
					 "la smerigliatrice per effettuare il taglio vero e proprio." crlf crlf))

(defrule battiscopa_esterno_principiante
	(preparazione_utente bassa)
	(lavoro battiscopa)
	(continua)
	(car (nome luogo) (valore esterno))
	=>
	(printout t crlf "Ecco tutto quello di cui hai bisogno:" crlf
					 " * colla per esterni" crlf
					 " * acqua" crlf
					 " * livella" crlf
					 " * miscelatore elettrico (consigliato)" crlf
					 " * distanziatori" crlf 
					 " * smerigliatrice" crlf 
					 " * secchio e cazzuola (piccola e grande)" crlf)
	(printout t crlf "Versa la colla in polvere nel secchio, aggiungi acqua in modo che tutta la polvere lo assorba e gira a mano o con il miscelatore." crlf
					 "L'impasto non deve essere molto liquido." crlf
					 "Parti nella posa da uno degli spigoli nella stanza (se ve ne sono) in modo che eventuali ritagli vadano a finire negli angoli, perché" crlf
					 "questi ultimi vengono generalmente coperti da elementi d'arredo. Se non vi sono spigoli partire da uno degli angoli." crlf
					 "Prendi un bel po' di colla e spalmala bene sul battiscopa; poi addossalo al muro. Prosegui nella posizione degli altri pezzi allo stesso modo" crlf
					 "In mezzo ad ogni battiscopa poni i distanziatori della dimensione desiderata." crlf
					 "Controlla con un livello o una stadia da 50 cm, dopo averne messi due o tre, che siano precisi e prosegui." crlf crlf

					 "Nel caso in cui non si possa inserire il pezzo intero del battiscopa occorrerà fare dunque un taglio, misurare la distanza tra il pezzo posato" crlf
					 "e il muro. Ci possono essere due casi:" crlf
					 " * nel caso in cui il taglio va effettuato all'ANGOLO, allora si toglie a tale misura della distanza, quella per la fuga (pari alla dimensione" crlf
					 "   del distanziatore) e un mezzo centimetro per non far incastrare il pezzo." crlf
					 " * nel caso in cui il taglio va effettuato allo SPIGOLO, allora bisogna fare in modo da aggiungere alla dimensione quella data dallo spessore" crlf 
					 "   del pezzo più la colla. In modo che un pezzo vada a filo con l'altro che verrà posto in maniera perpendicolare." crlf
					 "Segnare con la matita da muratore sulla piastrella il punto in cui deve effettuarsi il taglio e tracciare una linea più o meno dritta usando poi" crlf
					 "la smerigliatrice per effettuare il taglio vero e proprio." crlf crlf))

(defrule battiscopa_esterno_esperto
	(preparazione_utente alta)
	(lavoro battiscopa)
	(continua)
	(car (nome luogo) (valore esterno))
	=>
	(printout t crlf "Ecco tutto quello di cui hai bisogno:" crlf
					 " * colla per esterni" crlf
					 " * acqua" crlf
					 " * livella" crlf
					 " * miscelatore elettrico (consigliato)" crlf
					 " * distanziatori" crlf
					 " * smerigliatrice" crlf 
					 " * secchio e cazzuola (piccola e grande)" crlf)
	(printout t crlf "Parti nella posa da uno degli spigoli nella stanza (se ve ne sono) in modo che eventuali ritagli vadano a finire negli angoli, perché" crlf
					 "questi ultimi vengono generalmente coperti da elementi d'arredo. Se non vi sono spigoli partire da uno degli angoli." crlf
					 "Prendi un bel po' di colla e spalmala bene sul battiscopa; poi addossalo al muro. Prosegui nella posizione degli altri pezzi allo stesso modo" crlf
					 "In mezzo ad ogni battiscopa poni i distanziatori della dimensione desiderata." crlf
					 "Controlla con un livello o una stadia da 50 cm, dopo averne messi due o tre, che siano precisi e prosegui." crlf crlf

					 "Nel caso in cui non si possa inserire il pezzo intero del battiscopa occorrerà fare dunque un taglio, misurare la distanza tra il pezzo posato" crlf
					 "e il muro. Ci possono essere due casi:" crlf
					 " * nel caso in cui il taglio va effettuato all'ANGOLO, allora si toglie a tale misura della distanza, quella per la fuga (pari alla dimensione" crlf
					 "   del distanziatore) e un mezzo centimetro per non far incastrare il pezzo." crlf
					 " * nel caso in cui il taglio va effettuato allo SPIGOLO, allora bisogna fare in modo da aggiungere alla dimensione quella data dallo spessore" crlf 
					 "   del pezzo più la colla. In modo che un pezzo vada a filo con l'altro che verrà posto in maniera perpendicolare." crlf
					 "Segnare con la matita da muratore sulla piastrella il punto in cui deve effettuarsi il taglio e tracciare una linea più o meno dritta usando poi" crlf
					 "la smerigliatrice per effettuare il taglio vero e proprio." crlf crlf))

;  /---------------------------------------------------------------------------/
; /--------------------------------PAVIMENTO----------------------------------/
;/---------------------------------------------------------------------------/

;ritaglio?
;posa intero pavimento?
;partenza?


;(defrule domanda_presenza_massetto_pavimento
;	(preparazione_utente ?)
;	(pavimento)
;	(not (presenza_massetto ?))
;	=>
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "È presente un massetto?"))
;	(assert (presenza_massetto ?risposta)))
;
;(defrule domanda_presenza_pavimento_pavimento
;	(preparazione_utente ?)
;	(pavimento)
;	(not (presenza_pavimento ?))
;	=>
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "E' presente già un pavimento?"))
;	(assert (presenza_pavimento ?risposta)))
;
;(defrule domanda_pavimento_da_raccordare_pavimento
;	(preparazione_utente ?)
;	(not (pavimento_da_raccordare ?))
;	(pavimento)
;	=>
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Nello stesso piano ci sono altri pavimenti già posati?"))
;	(assert (pavimento_da_raccordare ?risposta)))
;
;
;;/------------posa_sopra------------/
;
;(defrule domanda_posa_sopra
;	(preparazione_utente ?)
;	(pavimento)
;	(not (posa_sopra ?))
;	(presenza_pavimento TRUE)
;	(pavimento_da_raccordare FALSE)
;	=>
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Vuoi effettuare la posa sopra il pavimento esistente?"))
;	(assert (posa_sopra ?risposta)))
;;TODO considerare che il pavimento si rialza
;
;(defrule controllo_condizioni_posa_sopra
;	(declare (salience ?*high_priority*))
;	(preparazione_utente ?)
;	?f <- (posa_sopra TRUE)
;	(pavimento)
;	=>
;	(bind ?*help* "")
;	(bind ?risposta1 (yes_or_no_p "Il pavimento presenta molte piastrelle alzate o non aderenti?"))
;
;	(bind ?*help* "")
;	(bind ?risposta2 (yes_or_no_p "Il pavimento è a livello?"))
;
;	(if (and (not (risposta1)) ?risposta2)
;		then (assert (inizio_posa))
;		else (assert (posa_sopra FALSE))))
;
;(defrule posa_sopra_no
;	(preparazione_utente ?)
;	(pavimento)
;	(posa_sopra FALSE)
;	=>
;	(printout t crlf "Procedi alla rimozione del pavimento e del materiale sottostante (massetto)." crlf)
;	(printout t crlf "Fai il massetto!" crlf) ;TODO collegamento con massetto (fai massetto) quindi il massetto sarà a livello
;	;(inizia posa) 
;	;TODO: nel modulo "massetto" verificare se presente il fatto "pavimento" cosicchè si ricollegherà alla parte di "inizio posa" 
;	)
;
;;/-------------massetto------------/
;
;(defrule controllo_condizioni_massetto_raccordo
;	(preparazione_utente ?)
;	(pavimento)
;	(pavimento_da_raccordare TRUE)
;	(spessore_piastrella_pavimento ?spessore_piastrella)
;	?f <- (presenza_massetto TRUE)
;	=>
;	(bind ?*help* "")
;	(bind ?risposta1 (yes_or_no_p "Il massetto è a livello?"))
;
;	(bind ?*help* "")
;	(format t "Considerando che la piastrella è %d mm più lo spessore della colla sarà di 3-4 mm%n" ?spessore_piastrella)
;	(bind ?risposta2 (yes_or_no_p "Il massetto si trova alla dimensione giusta sotto al pavimento presente?"))
;
;	(if (and ?risposta1 ?risposta2)
;		then (inizia_posa)
;		else (printout t crlf "Procedi alla rimozione del massetto." crlf)
;			 (printout t crlf "Fai il massetto!" crlf) ;TODO collegamento con massetto (fai massetto) quindi il massetto sarà a livello
;			;(inizia posa) 
;			;TODO: nel modulo "massetto" verificare se presente il fatto "pavimento" cosicchè si ricollegherà alla parte di "inizio posa" 
;			 ))
;
;(defrule controllo_condizioni_massetto
;	(preparazione_utente ?)
;	(pavimento)
;	(pavimento_da_raccordare FALSE)
;	?f <- (presenza_massetto TRUE)
;	=>
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Il massetto è a livello?"))
;	(if ?risposta
;		then (inizia_posa)
;		else (printout t crlf "Procedi alla rimozione del massetto." crlf)
;			 (printout t crlf "Fai il massetto!" crlf) ;TODO collegamento con massetto (fai massetto) quindi il massetto sarà a livello
;			;(inizia posa) 
;			;TODO: nel modulo "massetto" verificare se presente il fatto "pavimento" cosicchè si ricollegherà alla parte di "inizio posa" 
;			 ))
;
;(defrule massetto_non_presente
;	(preparazione_utente ?)
;	(presenza_massetto FALSE)
;	(pavimento)
;	=>
;	(printout t crlf "Fai il massetto!" crlf) ;TODO collegamento con massetto (fai massetto) quindi il massetto sarà a livello
;			;(inizia posa) 
;	)
;
;
;;/----------------inizia posa--------------/
;(defrule )
;scegli posa
;parti da entrata 
;fai una posa di prova
;mischia pacchi piastrelle
;smonta porte
;fai colla
;poni prima piastrelle intere e poi ritagli
;bagna piastrella e pavimento
;stendi colla
;vedi verso piastrella (freccia) e poni 
;continua ponendo 2 o 3 e lasciando lo spazio del distanziatore
;controlla livello
;prosegui 
;se lasci per troppo tempo la colla questa si asciuga
;togliere colla eccesso




;  /---------------------------------------------------------------------------/
; /------------------------------RIVESTIMENTO---------------------------------/
;/---------------------------------------------------------------------------/








;  /---------------------------------------------------------------------------/
; /-------------------------PAVIMENTO RIVESTIMENTO-----------------------------/
;/---------------------------------------------------------------------------/