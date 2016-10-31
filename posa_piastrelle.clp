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

;  /---------------------------------------------------------------------------/
; /---------------------------------FUNCTIONS---------------------------------/
;/---------------------------------------------------------------------------/
(deffunction ask_question (?question $?allowed_values)
	(insert$ ?allowed_values 1 help h)
	(format t (str-cat "%n" ?question "/help/h): "))
	(bind ?answer (read))

	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

	(while (not (member ?answer ?allowed_values)) do
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "Non è presente alcun help!" crlf)
		  				else (format t (str-cat ?*help* "%n"))))
		(format t (str-cat "%n" ?question "/help/h): "))
	    (bind ?answer (read))
	    (if (lexemep ?answer) 
			then (bind ?answer (lowcase ?answer))))
	 ?answer)

(deffunction yes_or_no_p (?question)
  (bind ?question (sym-cat ?question " (si/s/no/n"))
     (bind ?response (ask_question ?question si no s n))
     (if (or (eq ?response si) (eq ?response s))
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

;Domanda 6: Ti serve un consiglio specifico?

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
	(retract ?f1 ?f2))


;  /---------------------------------------------------------------------------/ 
; /----------------------------------STEP 1-----------------------------------/		Capire cosa l'utente vuole fare
;/---------------------------------------------------------------------------/
(defrule domanda_interno_esterno
	(preparazione_utente ?)
	(not (interno ?))
	(not (continua))
	=>
	(bind ?*help* "Rispondere 'interno' se il lavoro deve essere effettuato in una stanza che non sarà soggetta alle intemperie (bagno, cucina, stanza da %nletto, etc), 'esterno' in caso contrario (balcone, terrazzo).")
	(bind ?risposta (ask_question "Il lavoro riguarda l'interno o l'esterno? (esterno/interno" interno esterno))
	(if (eq ?risposta interno)
		then (assert (interno))
		else (assert (esterno))))

(defrule domanda_tipo_stanza
	(preparazione_utente ?)
	(not (tipo_stanza ?))
	(interno)
	(not (continua))
	=>
	(bind ?*help* "Indicare a quale tipo tra quelli elencati corrisponde la stanza in cui deve essere fatto il lavoro. Nel caso in cui ci sia più di una %nrisposta, allora effettuare la scelta di una stanza e continuare, poi riavviare il sistema e procedere con la successiva scelta.")
	(bind ?risposta (ask_question "Indicare in quale stanza si deve effettuare la posa? (bagno/cucina/altro" bagno cucina altro))
	(assert (tipo_stanza ?risposta)))

(defrule domanda_presenza_pavimento
	(preparazione_utente ?)
	(not (presenza_pavimento ?))
	(not (continua))
	=>
	(bind ?*help* "Rispondere 'si' se è già presente un pavimento nella stanza in cui si intende lavorare, 'no' altrimenti.")
	(bind ?risposta (yes_or_no_p "È già presente un pavimento?"))
	(assert (presenza_pavimento ?risposta)))

(defrule domanda_presenza_rivestimento
	(preparazione_utente ?)
	(not (presenza_rivestimento ?))
	(or (tipo_stanza bagno)
		(tipo_stanza cucina))
	(not (continua))
	=>
	(bind ?*help* "Rispondere 'si' se è già presente un rivestimento, cioè le pareti della stanza sono ricoperte con piastrelle, 'no' altrimenti.")
	(bind ?risposta (yes_or_no_p "È già presente un rivestimento?"))
	(assert (presenza_rivestimento ?risposta)))

(defrule domanda_presenza_massetto
	(preparazione_utente ?)
	(not (presenza_massetto ?))
	(presenza_pavimento FALSE)
	(not (continua))
	=>
	(bind ?*help* "Il massetto è quello strato di cemento la cui presenza è fondamentale perché sopra di esso verranno poste le piastrelle.")
	(bind ?risposta (yes_or_no_p "È presente un massetto?"))
	(assert (presenza_massetto ?risposta)))

(defrule domanda_condizioni_pavimento_presente
	(preparazione_utente ?)
	(presenza_pavimento TRUE)
	(not (condizioni_pavimento ?))
	(not (ristrutturazione_pavimento TRUE))
	(not (continua))
	=>
	(bind ?*help* "Rispondere 'si' se il pavimento in questione presenta segni di usura come piastrelle scheggiate, consumate o non aderenti.")
	(bind ?risposta (yes_or_no_p "Il pavimento esistente presenta molte piastrelle consumate o non perfettamente aderenti?"))
	(if ?risposta
		then (assert (condizioni_pavimento cattive))
		else (assert (condizioni_pavimento buone))))

(defrule domanda_pavimento_presente_rinnovo
	(preparazione_utente ?)
	(presenza_pavimento TRUE)
	(not (ristrutturazione_pavimento ?))
	(not (continua))
	=>
	(bind ?*help* "Rispondere decidendo se si vuole sostituire il pavimento presente con uno nuovo oppure no.")
	(bind ?risposta (yes_or_no_p "Vuoi ristrutturare il pavimento esistente?"))
	(if ?risposta
		then (assert (ristrutturazione_pavimento TRUE)) ;chiedi se deve fare fughe o battiscopa o solo aggiustare una piastrella scheggiata
		else (assert (ristrutturazione_pavimento FALSE)))) ;deve rimuovere il pavimento

(defrule domanda_condizioni_rivestimento_presente
	(preparazione_utente ?)
	(presenza_rivestimento TRUE)
	(not (condizioni_rivestimento ?))
	(not (ristrutturazione_rivestimento TRUE))
	(not (continua))
	=>
	(bind ?*help* "Rispondere 'si' se il rivestimento in questione presenta segni di usura come piastrelle scheggiate, consumate o non aderenti.")
	(bind ?risposta (yes_or_no_p "Il rivestimento presenta molte piastrelle non aderenti, mancanti, scheggiate o consumate?"))
	(if ?risposta 
		then (assert (condizioni_rivestimento cattive))
		else (assert (condizioni_rivestimento buone))))

(defrule domanda_rivestimento_presente_rinnovo
	(preparazione_utente ?)
	(presenza_rivestimento TRUE)
	(not (ristrutturazione_rivestimento ?))
	(not (continua))
	=>
	(bind ?*help* "Rispondere decidendo se si vuole sostituire il rivestimento presente con uno nuovo oppure no.")
	(bind ?risposta (yes_or_no_p "Vuoi ristrutturare il rivestimento presente?"))
	(if ?risposta
		then (assert (ristrutturazione_rivestimento TRUE)) 
		else (assert (ristrutturazione_rivestimento FALSE))))

(defrule domanda_anni_presenza_pavimento
	(preparazione_utente ?)
	(presenza_pavimento TRUE)
	(not (anni_pavimento ?))
	(not (continua))
	=>
	(bind ?*help* "Rispondere indicando (se si conosce) gli anni che ha il pavimento presente.")
	(bind ?risposta (ask_number "Quanti anni ha il pavimento presente (-1 nel caso non si sappia)?"))
	(while (and (< ?risposta -1) (> ?risposta 50))
		(printout t crlf "La risposta è sbagliata!")
		(bind ?risposta (ask_number "Quanti anni è che il pavimento non viene sostituito (-1 nel caso non si sappia)?")))
	(assert (anni_pavimento ?risposta)))

;---------------------------------------------------------------------------------------------------------------------------------------------------------

(defrule massetto
	(declare (salience ?*high_priority*))
	(not (continua))
	(not (no_lavoro (nome massetto)))
	(not (massetto))

	(or (interno)
		(esterno))
	(presenza_massetto FALSE)
	=>
	(bind ?*help* "Rispondere affermativamente se il lavoro che si deve fare è il massetto, negativamente in caso contrario.")
	(bind ?risposta (yes_or_no_p "Quello che vuoi realizzare è il massetto?"))
	(if ?risposta 
		then (assert (continua))
			 (assert (massetto))
		else (assert (no_lavoro (nome massetto)))))

(defrule fughe
	(declare (salience ?*high_priority*))
	(not (continua))
	(not (no_lavoro (nome fughe)))
	(not (fughe))

	(or (interno)
		(esterno))
	(presenza_pavimento TRUE)
	(condizioni_pavimento buone)
	(ristrutturazione_pavimento FALSE)
	=>
	(bind ?*help* "Rispondere affermativamente se il lavoro che si deve fare è il riempimento delle fughe, negativamente in caso contrario.")
	(bind ?risposta (yes_or_no_p "Quello che vuoi realizzare sono le fughe?"))
	(if ?risposta
		then (assert (continua))
			 (assert (fughe))
		else (assert (no_lavoro (nome fughe)))))

(defrule battiscopa 
	(declare (salience ?*high_priority*))
	(not (continua))
	(not (no_lavoro (nome battiscopa)))
	(not (battiscopa))

	(or (interno)
		(esterno))
	(not (or (tipo_stanza bagno)
			 (tipo_stanza cucina)))
	(presenza_pavimento TRUE)
	(condizioni_pavimento buone)
	(ristrutturazione_pavimento FALSE)
	=>
	(bind ?*help* "Rispondere affermativamente se il lavoro che si deve fare è il posizionamento del battiscopa, negativamente in caso contrario")
	(bind ?risposta (yes_or_no_p "Quello che vuoi realizzare è il battiscopa?"))
	(if ?risposta
		then (assert (continua))
			 (assert (battiscopa))
		else (assert (no_lavoro (nome battiscopa)))))

(defrule rattoppo
	(declare (salience ?*high_priority*))
	(not (continua))
	(not (no_lavoro (nome rattoppo)))
	(not (rattoppo))

	(or (interno)
		(esterno))
	(presenza_pavimento TRUE)
	(condizioni_pavimento buone)
	(anni_pavimento ?x)
	(test (<= ?x 6))  ;se il pavimento è troppo vecchio allora ci sarà una differenza di colore tra la piastrella nuova e quella vecchia
	=>
	(bind ?*help* "realizzare")
	(bind ?risposta (yes_or_no_p "Quello che vuoi realizzare è un rattoppo?"))
	(if ?risposta
		then (assert (continua))
			 (assert (rattoppo))
		else (assert (no_lavoro (nome rattoppo)))))

(defrule pavimento
	(declare (salience ?*high_priority*))
	(not (continua))
	(not (no_lavoro (nome pavimento)))
	(not (pavimento))
	(not (pavimento_rivestimento))

	(or (interno)
		(esterno))
	(or (condizioni_pavimento cattive)
		(ristrutturazione_pavimento TRUE)
		(presenza_pavimento FALSE)
		(presenza_massetto TRUE))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Quello che vuoi realizzare è il pavimento? %n(ATTENZIONE: Rispondi 'si' solo se devi realizzare unicamente il pavimento!) "))
	(if ?risposta
		then (assert (continua))
			 (assert (pavimento))
		else (assert (no_lavoro (nome pavimento)))))

(defrule rivestimento
	(declare (salience ?*high_priority*))
	(not (continua))
	(not (no_lavoro (nome rivestimento)))
	(not (rivestimento))

	(or (condizioni_rivestimento cattive)
		(ristrutturazione_rivestimento TRUE)
		(presenza_rivestimento FALSE))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Quello che vuoi realizzare è il rivestimento? %n(ATTENZIONE: Rispondi 'si' solo se devi realizzare unicamente il rivestimento!) "))
	(if ?risposta
		then (assert (continua))
			 (assert (rivestimento))
		else (assert (no_lavoro (nome rivestimento)))))

(defrule pavimento_rivestimento
	(declare (salience ?*high_priority*))
	(not (continua))
	(not (no_lavoro (nome pavimento_rivestimento)))

	(or (condizioni_pavimento cattive)
		(ristrutturazione_pavimento TRUE)
		(presenza_pavimento FALSE)
		(presenza_massetto TRUE))
	(or (condizioni_rivestimento cattive)
		(ristrutturazione_rivestimento TRUE)
		(presenza_rivestimento FALSE))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Quello che vuoi realizzare è sia il pavimento che il rivestimento?"))
	(if ?risposta
		then (assert (continua))
			 (assert (pavimento_rivestimento))
		else (assert (no_lavoro (nome pavimento_rivestimento)))))
;-----------------------------------------------------------------------------------------------------------------

(defrule lavoro_trovato
	(declare (salience ?*highest_priority*))
	(continua)
	=>
	(do-for-all-facts ((?no_lavoro no_lavoro)) TRUE (retract ?no_lavoro)))

(defrule lavoro_non_trovato
	(declare (salience ?*lowest_priority*))
	(not (continua))
	=>
	(printout t crlf "Non posso aiutarti! Premi 'c' per riprovare: ")
	(while (neq (read) c)
		(printout t crlf "Non posso aiutarti! Premi 'c' per riprovare: "))
	(reset)
	(run))


;  /---------------------------------------------------------------------------/
; /----------------------------------MASSETTO---------------------------------/
;/---------------------------------------------------------------------------/
(defrule domanda_pavimento_da_raccordare
	(massetto)
	(not (pavimento_da_raccordare ?))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Nello stesso piano ci sono altri pavimenti già posati?"))
	(assert (pavimento_da_raccordare TRUE)))

(defrule domanda_dimensione_pavimento_esperto
	(preparazione_utente alta)
	(massetto)
	(not (dimensione_area ?))
	=>
	(bind ?*help* "Indicare il numero che rappresenta la dimensione in metri quadri dell'area in cui si intende lavorare.")
	(bind ?risposta (ask_number "Fornire la dimensione in metri quadri dell'area in cui si deve lavorare: "))
	(assert (dimensione_area ?risposta)))

(defrule domanda_dimensione_pavimento_principiante
	(preparazione_utente bassa)
	(massetto)
	(not (dimensione_area ?))
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
	(assert (dimensione_area ?risposta)))

(defrule area_troppo_grande
	(declare (salience ?*high_priority*))
	(massetto)
	(preparazione_utente bassa)
	(dimensione_area ?dim)
	(test (> ?dim 50))
	=>
	(printout t crlf "L'area in cui si deve fare il massetto è troppo ampia! Consulta un muratore!" crlf)
	(printout t crlf "Premi 'c' per chiudere il programma: ")
	(while (neq (read) c)
		(printout t crlf "Premi 'c' per chiudere il programma: "))
	(halt))






































