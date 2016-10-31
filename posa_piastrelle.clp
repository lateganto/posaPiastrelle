(defglobal ?*highest_priority* = 1000)
(defglobal ?*high_priority* = 100)
(defglobal ?*low_priority* = -100)
(defglobal ?*lowest_priority* = -1000)
(defglobal ?*help* = "")

;  /-------------------------------/
; /-----------TEMPLATES-----------/
;/-------------------------------/
(deftemplate esperienza
	(slot esperto)
	(slot principiante))

(deftemplate domande_poste
	(slot numero))

(deftemplate domanda
	(slot valore))

(deftemplate caratteristica
	(slot nome)
	(slot valore))

;  /-------------------------------/
; /-----------FUNCTIONS-----------/
;/-------------------------------/
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


;  /-------------------------------/
; /------PROFILAZIONE UTENTE------/
;/-------------------------------/
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
	 		(assert (fine)))
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
			(assert (fine))))

(defrule domanda_fai_da_te
	(not (domanda (valore two)))
	?f1 <- (esperienza (esperto ?val_esp) (principiante ?val_princ))
	?f2 <- (domande_poste (numero ?x))
	=>
	(bind ?*help* "Rispondere affermativamente se si è realizzato qualche volta un piccolo lavoro in casa o qualche tipo riparazione, %nnegativamente in caso contrario.")
	(bind ?risposta (yes_or_no_p "Se si rompe qualcosa in casa, cerchi di aggiustarla da te?"))
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


;  /-------------------------------/ 
; /------------STEP 1-------------/		Capire cosa l'utente vuole fare
;/-------------------------------/
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
	(bind ?risposta (ask_number "Quanti anni ha il pavimento presente?"))
	(while (and (< ?risposta 1) (> ?risposta 50))
		(printout t crlf "La risposta è sbagliata!")
		(bind ?risposta (ask_number "Quanti anni ha il pavimento presente?")))
	(assert (anni_pavimento ?risposta)))


;---------------------------------------------------
(defrule massetto
	(not (no_massetto))
	(or (interno)
		(esterno))
	(presenza_massetto FALSE)
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi fare il massetto?"))
	(if ?risposta 
		then (assert (continua))
			 (assert (massetto))
		else (assert (no_massetto))))

(defrule fughe
	(not (no_fughe))
	(or (interno)
		(esterno))
	(presenza_pavimento TRUE)
	(condizioni_pavimento buone)
	(ristrutturazione_pavimento FALSE)
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi fare le fughe?"))
	(if ?risposta
		then (assert (continua))
			 (assert (fughe))
		else (assert (no_fughe))))

(defrule battiscopa 
	(not (no_battiscopa))
	(or (interno)
		(esterno))
	(not (or (tipo_stanza bagno)
			 (tipo_stanza cucina)))
	(presenza_pavimento TRUE)
	(condizioni_pavimento buone)
	(ristrutturazione_pavimento FALSE)
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi fare i battiscopa?"))
	(if ?risposta
		then (assert (continua))
			 (assert (battiscopa))
		else (assert (no_battiscopa))))

(defrule rattoppo
	(not (no_rattoppo))
	(or (interno)
		(esterno))
	(presenza_pavimento TRUE)
	(condizioni_pavimento buone)
	(anni_pavimento ?x)
	(test (<= ?x 6))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi fare un rattoppo?"))
	(if ?risposta
		then (assert (continua))
			 (assert (rattoppo))
		else (assert (no_rattoppo))))

(defrule pavimento
	(not (no_solo_pavimento))
	(or (interno)
		(esterno))
	(or (condizioni_pavimento cattive)
		(ristrutturazione_pavimento TRUE)
		(presenza_pavimento FALSE)
		(presenza_massetto TRUE))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi fare solo il pavimento?"))
	(if ?risposta
		then (assert (continua))
			 (assert (pavimento))
		else (assert (no_solo_pavimento))))

(defrule rivestimento
	(not (no_solo_rivestimento))
	(or (tipo_stanza bagno)
		(tipo_stanza cucina))
	(or (condizioni_rivestimento cattive)
		(ristrutturazione_rivestimento TRUE)
		(presenza_rivestimento FALSE))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi fare solo il rivestimento?"))
	(if ?risposta
		then (assert (continua))
			 (assert (rivestimento))
		else (assert (no_solo_rivestimento))))

(defrule pavimento_rivestimento
	(not (no_rivestimento_pavimento))
	(or (tipo_stanza bagno)
		(tipo_stanza cucina))
	(or (condizioni_pavimento cattive)
		(ristrutturazione_pavimento TRUE)
		(presenza_pavimento FALSE)
		(presenza_massetto TRUE))
	(or (condizioni_rivestimento cattive)
		(ristrutturazione_rivestimento TRUE)
		(presenza_rivestimento FALSE))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi fare sia il pavimento che il rivestimento?"))
	(if ?risposta
		then (assert (continua))
			 (assert (pavimento_rivestimento))
		else (assert (no_rivestimento_pavimento))))

;-------------------------------------------------------------
(defrule continua
	(declare (salience ?*high_priority*))
	(continua)
	=>
	(printout t crlf "sdfasfas" crlf)
	(halt))

(defrule no_soluzione
	(declare (salience ?*lowest_priority*))
	(not (continua))
	=>
	(printout t crlf "Non posso aiutarti! Riavvia il sistema e riprova!" crlf)
	(halt))






;considerare caso in cui si debba fare anche il rivestimento
;(defrule fare_pavimento  ;casi in cui si deve fare il pavimento
;	(or (condizioni_pavimento cattive)
;		(ristrutturazione_pavimento)
;		(presenza_pavimento FALSE))
;	=>
;	(assert (fare_pavimento)))
;
;(defrule fare_rivestimento
;	(or (presenza_rivestimento FALSE)
;		(condizioni_rivestimento cattive)
;		(ristrutturazione_rivestimento))
;	=>
;	(assert (fare_rivestimento)))
;
;(defrule fare_rivestimento_pavimento
;	(and (or (condizioni_pavimento cattive)
;			 (ristrutturazione_pavimento)
;			 (presenza_pavimento FALSE))
;		 (or (presenza_rivestimento FALSE)
;		 	 (condizioni_rivestimento cattive)
;		 	 (ristrutturazione_rivestimento)))
;	=>
;	(assert (fare_rivestimento_pavimento)))
;
;(defrule no_pavimento_presente
;	;chiedi se massetto presente altrimenti fallo)
;	)
;
;(defrule no_rivestimento_presente
;	;chiedi se vuole farlo
;	)

;(defrule interno_si_rivestimento ;se cucina o bagno allora si al rivestimento
;	(preparazione_utente ?)
;	(or (tipo_stanza bagno) 
;		(tipo_stanza cucina))
;	(or (not (pavimento ?))
;		(not (rivestimento ?)))
;	=>
;	(assert (pavimento TRUE) (rivestimento TRUE)))
;
;(defrule esterno_no_rivestimento ;se esterno allora nessun rivestimento
;	(interno)
;	(or (not (rivestimento ?))
;		(not (pavimento ?)))
;	=>
;	(assert (pavimento TRUE))
;	(assert (rivestimento FALSE)))
;
;(defrule stanze_no_rivestimento ;se stanza diversa da cucina o bagno allora niente rivestimento
;	(tipo_stanza altro)
;	(or (not (pavimento ?))
;		(not (rivestimento ?)))
;	=>
;	(assert (pavimento TRUE))
;	(assert (rivestimento FALSE)))


;---------------------------------------------------
;(defrule rimozione_rivestimento ;se non si deve fare un bagno o una cucina eliminare il rivestimento
;	(not (or (tipo_stanza bagno)
;			(tipo_stanza cucina)))
;	(presenza_rivestimento TRUE)
;	=>
;	(assert (rimozione_rivestimento)))
;
;(defrule domanda_dimensione_pavimento_esperto
;	(preparazione_utente alta)
;	(not (dimensione_pavimento))
;	=>
;	(bind ?*help* "Calcola la dimensione dell'area del pavimento e indicarla (in metri quadri).")
;	(bind ?risposta (ask_number "Indica la dimensione dell'area del pavimento: "))
;	(assert (dimensione_pavimento ?risposta)))
;
;(defrule domanda_dimensione_pavimento_principiante
;	(preparazione_utente bassa)
;	(not (dimensione_pavimento ?))
;	=>
;	(printout t "La misura dell'area da pavimentare non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un pavimento si" crlf 
;				"effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella" crlf
;				"dell'area da pavimentare, ma deve essere maggiore. Procedere individuando la forma di tale superficie, se questa può essere ricondotta" crlf 
;				"ad una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, allora, per ottenere l'area, bisogna ricordare che:" crlf
;				"	* se la superficie ha la forma di un quadrato, allora si calcola la lunghezza di un muro (che rappresenta il lato) e la si moltiplica" crlf
;				"	  per se stesso" crlf
;				"	* se la superficie ha la forma di un rettangolo, allora si moltiplica la dimensione del muro più lungo (che rappresenta la lunghezza)" crlf
;				"	  per la dimensione del muro più piccolo (che rappresenta la larghezza)" crlf
;				"	* se la superficie ha la forma di un triangolo, allora si trova la lunghezza del muro che rappresenta la base e quella del muro che" crlf
;				"	  rappresenta l'altezza  del triangolo, si moltiplicando tra di loro le due misure e si divide per due il risultato" crlf
;				"	* se la superficie ha la forma di un cerchio, allora si deve trovare la misura del raggio e si usa la formula 2πr, dove π = 3.14 e" crlf 
;				"	  r = raggio calcolato (cioè si calcola il raggio, che è la metà del diametro e lo si moltiplica prima per due e poi per 3.14)" crlf
;				"	* se la superficie ha la forma di una semicirconferenza, allora si procede come nel caso precedente (cerchio) a trovare la misura del" crlf
;				"	  raggio della circonferenza e si usa la formula 2πr (dove π = 3.14 e r = raggio trovato) e si divide il risultato per due." crlf
;				"Nel caso in cui la forma della superficie da pavimentare non fosse simile ad una delle precedenti, allora si suddivide l'area in parti più" crlf
;				"piccole dalla forma riconducibile ad una di quelle precedenti, si calcola l'area di ogni parte e si sommano i vari risultati ottenuti" crlf
;				"Le misure vanno espresse in metri al quadrato" crlf crlf)
;	(bind ?*help* "Indicare la dimensione in metri quadri dell'area da pavimentare.")
;	(bind ?risposta (ask_number "Qual è quindi la dimensione in metri quadri dell'area da pavimentare?"))
;	(assert (dimensione_pavimento ?risposta)))



;----------------2° STEP (Altre domande per capire il tipo di lavoro da fare)----------------
;(defrule pavimento_non_presente  ;regola relativa a quando il pavimento non è presente, quindi bisognerebbe farlo mentre l'utente ha deciso di non farlo.
;	?f <- (pavimento FALSE)
;	(presenza_pavimento FALSE)
;	=>
;	(printout t crlf "Il pavimento non è presente però hai scelto di non realizzarlo, forse dovresti fare anche il pavimento!" crlf)
;	(bind ?*help* "Scegliere si nel caso in cui si intende effettuare anche il pavimento, no altrimenti")
;	(bind ?risposta (yes_or_no_p "Vuoi quindi realizzare anche il pavimento?"))
;	(if ?risposta then (retract ?f) (assert (pavimento TRUE))))
;
;
;
;(defrule domanda_metri_quadri_pavimento
;	(pavimento TRUE)
;	(not (dimensioni_pavimento ?))
;	=>
;	(printout t crlf "Adesso si può procedere al calcolo della dimensione dell'area da pavimentare." crlf)
;	(printout t "La misura dell'area da pavimentare non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un pavimento si" crlf 
;				"effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella" crlf
;				"dell'area da pavimentare, ma deve essere maggiore. Procedere individuando la forma di tale superficie, se questa può essere ricondotta" crlf 
;				"ad una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, allora, per ottenere l'area, bisogna ricordare che:" crlf
;				"	* se la superficie ha la forma di un quadrato, allora si calcola la lunghezza di un muro (che rappresenta il lato) e la si moltiplica" crlf
;				"	  per se stesso" crlf
;				"	* se la superficie ha la forma di un rettangolo, allora si moltiplica la dimensione del muro più lungo (che rappresenta la lunghezza)" crlf
;				"	  per la dimensione del muro più piccolo (che rappresenta la larghezza)" crlf
;				"	* se la superficie ha la forma di un triangolo, allora si trova la lunghezza del muro che rappresenta la base e quella del muro che" crlf
;				"	  rappresenta l'altezza  del triangolo, si moltiplicando tra di loro le due misure e si divide per due il risultato" crlf
;				"	* se la superficie ha la forma di un cerchio, allora si deve trovare la misura del raggio e si usa la formula 2πr, dove π = 3.14 e" crlf 
;				"	  r = raggio calcolato (cioè si calcola il raggio, che è la metà del diametro e lo si moltiplica prima per due e poi per 3.14)" crlf
;				"	* se la superficie ha la forma di una semicirconferenza, allora si procede come nel caso precedente (cerchio) a trovare la misura del" crlf
;				"	  raggio della circonferenza e si usa la formula 2πr (dove π = 3.14 e r = raggio trovato) e si divide il risultato per due." crlf
;				"Nel caso in cui la forma della superficie da pavimentare non fosse simile ad una delle precedenti, allora si suddivide l'area in parti più" crlf
;				"piccole dalla forma riconducibile ad una di quelle precedenti, si calcola l'area di ogni parte e si sommano i vari risultati ottenuti" crlf
;				"Le misure vanno espresse in metri al quadrato" crlf crlf)
;	(bind ?*help* "Indicare il numero che rappresenta la dimensione in metri quadri dell'area da pavimentare.")
;	(bind ?risposta (ask_number "Qual è quindi la dimensione in metri quadri dell'area da pavimentare?"))
;	(assert (dimensioni_pavimento ?risposta)))
;
;(defrule domanda_metri_quadri_rivestimento
;	(not (dimensioni_rivestimento ?))
;	(rivestimento TRUE)
;	=>
;	(printout t crlf "Adesso si può procedere al calcolo della dimensione dell'area da rivestire." crlf)
;	(printout t "La misura dell'area da rivestire non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un rivestimento si" crlf
;				"effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella" crlf
;				"dell'area da rivestire, ma deve essere maggiore. Bisogna calcolare la dimensione dell'area delle pareti che andranno rivestite." crlf
;				"Generalmente la forma della parete sarà rettangolare o al massimo quadrata. Bisogna comunque eliminare dal calcolo dell'area eventuali" crlf
;				"elementi che non saranno interessati dalla posa del rivestimento, come ad esempio le finestre." crlf
;				"Procedere individuando la forma di tale superficie, se questa può essere ricondotta ad una forma semplice come quadrato, rettangolo," crlf
;				"triangolo, cerchio o semicerchio, allora, per ottenere l'area, bisogna ricordare che:" crlf
;				"	* se la superficie ha la forma di un quadrato, allora si calcola la lunghezza di un muro (che rappresenta il lato) e la si moltiplica" crlf
;				"	  per se stesso" crlf
;				"	* se la superficie ha la forma di un rettangolo, allora si moltiplica la dimensione del muro più lungo (che rappresenta la lunghezza)" crlf
;				"	  per la dimensione del muro più piccolo (che rappresenta la larghezza)" crlf
;				"	* se la superficie ha la forma di un triangolo, allora si trova la lunghezza del muro che rappresenta la base e quella del muro che" crlf
;				"	  rappresenta l'altezza  del triangolo, si moltiplicando tra di loro le due misure e si divide per due il risultato" crlf
;				"	* se la superficie ha la forma di un cerchio, allora si deve trovare la misura del raggio e si usa la formula 2πr, dove π = 3.14 e" crlf
;				"	  r = raggio calcolato (cioè si calcola il raggio, che è la metà del diametro e lo si moltiplica prima per due e poi per 3.14)" crlf
;				"	* se la superficie ha la forma di una semicirconferenza, allora si procede come nel caso precedente (cerchio) a trovare la misura del" crlf
;				"	  raggio della circonferenza e si usa la formula 2πr (dove π = 3.14 e r = raggio trovato) e si divide il risultato per due." crlf
;				"Nel caso in cui la forma della superficie non fosse simile ad una delle precedenti, allora si suddivide l'area in parti più piccole dalla" crlf
;				"forma riconducibile ad una di quelle precedenti, si calcola l'area di ogni parte e si sommano i vari risultati ottenuti" crlf
;				"Lo stesso procedimento si ripete per ogni parete e si sommano i risultati ottenuti." crlf
;				"Le misure vanno espresse in metri al quadrato" crlf crlf)
;	(bind ?*help* "Indicare il numero che rappresenta la dimensione in metri quadri dell'area da rivestire.")
;	(bind ?risposta (ask_number "Qual è quindi la dimensione in metri quadri dell'area da rivestire?"))
;	(assert (dimensioni_rivestimento ?risposta)))
;
;(defrule quantità_piastrelle_pavimento
;	;(declare (salience ?*high_priority*))
;	(disposizione_pavimento ?disp)
;	(dimensioni_pavimento ?dim)
;	=>
;	(printout t crlf "Per il pavimento ")
;	(switch ?disp
;		(case diagonale then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.15)))
;		(case sfalsata then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
;		(case spina_di_pesce_dritta then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
;		(case spina_di_pesce_obliqua then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
;		(case dritta then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))))
;
;(defrule quantità_piastrelle_rivestimento
;	;(declare (salience ?*high_priority*))
;	(disposizione_rivestimento ?disp)
;	(dimensioni_rivestimento ?dim)
;	=>
;	(printout t crlf "Per il rivestimento ")
;	(switch ?disp
;		(case diagonale then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.15)))
;		(case sfalsata then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
;		(case spina_di_pesce_dritta then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
;		(case spina_di_pesce_obliqua then (format t "devi prendere %d metri quadri di piastrelle di piastrelle!%n" (* ?dim 1.10)))
;		(case dritta then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))))
;
;(defrule attrezzi_necessari_rivestimento
;	;(declare (salience ?*high_priority*))
;	(rivestimento TRUE)
;	(interno ?valore)
;	=>
;	(printout t crlf crlf "Assicurati di procurarti tutti questi attrezzi: " crlf
;					"	* tagliapiastrelle (manuale o elettrica)" crlf
;					"	* smerigliatrice angolare (grande e piccola)" crlf
;					"	* tenaglia per piastrelle" crlf
;					"	* 2-3 cazzuole (almeno una piccola a punta)" crlf
;					"	* spatola liscia" crlf
;					"	* frattazzo dentellato" crlf
;					"	* martello pneumatico" crlf
;					"	* 2-3 secchi da muratore" crlf
;					"	* stadie di alluminio (varie dimensioni da 1 fino a 3 metri)" crlf
;					"	* mazza in gomma" crlf
;					"	* frattazzo in pugna" crlf
;					"	* secchio lavaggio per piastrellisti" crlf
;					"	* distanziatori" crlf
;					"	* squadra in acciaio per carpentieri" crlf
;					"	* livella" crlf
;					"	* matite in legno da muratori" crlf
;					"	* profili angolari (in numero pari agli angoli presenti nella stanza)" crlf
;					"	* filo a piombo" crlf )
;	(if (not ?valore) then (printout t   			;nel caso di lavoro esterno
;					"	* colla da esterno" crlf
;					"	* fugante da esterno (stucco per fughe)" crlf)
;			  else (printout t 
;					"	* colla" crlf
;					"	* fugante (stucco per fughe)" crlf))
;	printout t crlf)
;
;(defrule attrezzi_necessari
;	;(declare (salience ?*high_priority*))
;	(rivestimento FALSE)
;	(interno ?valore)
;	=>
;	(printout t crlf crlf "Assicurati di procurarti tutti questi attrezzi: " crlf
;					"	* tagliapiastrelle (manuale o elettrica)" crlf
;					"	* smerigliatrice angolare (grande e piccola)" crlf
;					"	* tenaglia per piastrelle" crlf
;					"	* 2-3 cazzuole (almeno una piccola a punta)" crlf
;					"	* spatola liscia" crlf
;					"	* frattazzo dentellato" crlf
;					"	* martello pneumatico" crlf
;					"	* 2-3 secchi da muratore" crlf
;					"	* stadie di alluminio (varie dimensioni da 1 fino a 3 metri)" crlf
;					"	* mazza in gomma" crlf
;					"	* frattazzo in pugna" crlf
;					"	* secchio lavaggio per piastrellisti" crlf
;					"	* distanziatori" crlf
;					"	* squadra in acciaio per carpentieri" crlf
;					"	* livella" crlf
;					"	* matite in legno da muratori" crlf)
;	(if (not ?valore) then (printout t 			;nel caso di esterno
;					"	* colla da esterno" crlf
;					"	* fugante da esterno (stucco per fughe)" crlf)
;			  else (printout t 
;					"	* colla" crlf
;					"	* fugante (stucco per fughe)" crlf))
;	printout t crlf)
;
;(defrule domanda_formato_piastrella_rivestimento
;	;(declare (salience ?*high_priority*))
;	(not (formato_piastrella_rivestimento ?))
;	(rivestimento TRUE)
;	=>
;	(bind ?*help* "In base al formato della piastrella alcuni tipi di posa non sono realizzabili.")
;	(bind ?risposta (ask_question "Qual è il formato della piastrella per il rivestimento? (quadrata/rettangolare" quadrata rettangolare))
;	(assert (formato_piastrella_rivestimento ?risposta)))
;
;(defrule domanda_formato_piastrella_pavimento
;	;(declare (salience ?*high_priority*))
;	(not (formato_piastrella_pavimento ?))
;	(pavimento TRUE)
;	=>
;	(bind ?*help* "In base al formato della piastrella alcuni tipi di posa non sono realizzabili.")
;	(bind ?risposta (ask_question "Qual è il formato della piastrella per il pavimento? (quadrata/rettangolare" quadrata rettangolare))
;	(assert (formato_piastrella_pavimento ?risposta)))
;
;(defrule domanda_spessore_piastrella_pavimento
;	(pavimento TRUE)
;	(not (spessore_piastrella_pavimento ?))
;	=>
;	(bind ?*help* "In genere la dimensione è di circa 1 cm (10 mm)")
;	(bind ?risposta (ask_number "Qual'è la dimensione in millimetri dello spessore della piastrella scelta per il pavimento?"))
;	(while (or (< ?risposta 5) (> ?risposta 15)) do 
;		(if (< ?risposta 5) then (printout t crlf "La dimensione è troppo piccola!" crlf))
;		(if (> ?risposta 15) then (printout t crlf "La dimensione è troppo grande!" crlf))
;		(bind ?risposta (ask_number "Qual'è la dimensione in millimetri dello spessore della piastrella scelta per il pavimento?")))
;	(assert (spessore_piastrella_pavimento ?risposta)))
;
;(defrule domanda_distanziatori_pavimento
;	(not (dim_distanziatori_pavimento ?))
;	(pavimento TRUE)
;	=>
;	(bind ?*help* "I distanziatori sono quei piccoli pezzi di plastica con forma a T o a croce che si pongono tra due piastrelle in modo da mantenere %nsempre la stessa ;distanza.")
;	(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori per il pavimento in millimetri?"))
;	(while (or (< ?risposta 1) (> ?risposta 10)) do 
;		(printout t crlf "La dimensione deve essere compresa tra 1 e 10!" crlf)
;		(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori per il pavimento in millimetri?")))
;	(assert (dim_distanziatori_pavimento ?risposta)))  ;prosegui alla successiva fase
;
;(defrule domanda_distanziatori_rivestimento
;	(not (dim_distanziatori_rivestimento ?))
;	(rivestimento TRUE)
;	=>
;	(bind ?*help* "I distanziatori sono quei piccoli pezzi di plastica con forma a T o a croce che si pongono tra due piastrelle in modo da mantenere %nsempre la stessa ;distanza.")
;	(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori per il rivestimento in millimetri?"))
;	(while (or (< ?risposta 1) (> ?risposta 10)) do 
;		(printout t crlf "La dimensione deve essere compresa tra 1 e 10!" crlf)
;		(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori per il rivestimento in millimetri?")))
;	(assert (dim_distanziatori_rivestimento ?risposta)))
;
;(defrule domanda_disposizione_piastrella_quadrata_pavimento
;	(formato_piastrella_pavimento quadrata)
;	(not (disposizione_pavimento ?))
;	=>
;	(bind ?*help* "") ;TODO far vedere immagini
;	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle scelta per il pavimento? (dritta/sfalsata/diagonale" dritta sfalsata diagonale))
;	(assert (disposizione_pavimento ?risposta)))
;
;(defrule domanda_disposizione_piastrella_rettangolare_pavimento
;	(formato_piastrella_pavimento rettangolare)
;	(not (disposizione_pavimento ?))
;	=>
;	(bind ?*help* "") ;TODO far vedere immagini
;	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle scelta per il pavimento? (dritta/sfalsata/spina_di_pesce_dritta%n/spina_di_pesce_obliqua" dritta ;sfalsata spina_di_pesce_dritta spina_di_pesce_obliqua))
;	(assert (disposizione_pavimento ?risposta)))
;
;(defrule domanda_disposizione_piastrella_quadrata_rivestimento
;	(formato_piastrella_rivestimento quadrata)
;	(not (disposizione_rivestimento ?))
;	=>
;	(bind ?*help* "") ;TODO far vedere immagini
;	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle scelta per il rivestimento? (dritta/sfalsata/diagonale" dritta sfalsata diagonale))
;	(assert (disposizione_rivestimento ?risposta)))
;
;(defrule domanda_disposizione_piastrella_rettangolare_rivestimento
;	(formato_piastrella_rivestimento rettangolare)
;	(not (disposizione_rivestimento ?))
;	=>
;	(bind ?*help* "") ;TODO far vedere immagini
;	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle scelta per il rivestimento? (dritta/sfalsata/spina_di_pesce_dritta/spina_di_pesce_obliqua" dritta ;sfalsata spina_di_pesce_dritta spina_di_pesce_obliqua))
;	(assert (disposizione_rivestimento ?risposta)))
;
;(defrule domanda_pavimento_da_raccordare  ;regola per capire se il pavimento che si deve realizzare è indipendente o deve essere raccordato con altri già presenti
;	(declare (salience ?*low_priority*))
;	(not (pavimento_da_raccordare ?))
;	(pavimento TRUE)
;	=>
;	(bind ?*help* "Se in una stanza adiacente a quella in cui si intende lavorare è presente un pavimento già posato che non si intende eliminare e%n con cui ci si deve ;raccordare (cioè il pavimento che si sta realizzando non dovrà essere né più alto e né più basso), allora tale pavimento deve essere%n realizzato in modo che, una ;volta completato, sia all'altezza giusta.")
;	(bind ?risposta (yes_or_no_p "È presente in una stanza adiacente a quella in cui si sta lavorando un pavimento con cui ci si dovrà raccordare %n(cioè, dopo aver posato il ;pavimento, esso dovrà essere alla stessa altezza del pavimento già presente)?"))
;	(assert (pavimento_da_raccordare ?risposta)))
;
;;------------3 STEP (Domande specifiche in base al tipo di lavoro scelto)----------------
;
;;DOMANDE SOLO RIVESTIMENTO NO PAVIMENTO
;(defrule domanda_presenza_massetto_solo_rivestimento
;	(rivestimento TRUE)
;	(pavimento FALSE)
;	(presenza_pavimento FALSE)
;	(not (presenza_massetto ?))
;	(muri_a_piombo)
;	=>
;	(printout t crlf "Il pavimento non è presente e non si intende farlo, tuttavia occorre che sia presente un massetto che sia a livello." crlf)
;	(bind ?*help* "Il massetto è uno strato di cemento la cui presenza è essenziale perché sopra esso verranno posate le piastrelle.")
;	(bind ?risposta (yes_or_no_p "È presente un massetto?"))
;	(assert (presenza_massetto ?risposta)))
;
;(defrule controllo_pavimento_livello_solo_rivestimento
;	(rivestimento TRUE)
;	(pavimento FALSE)
;	(presenza_pavimento TRUE)
;	(muri_a_piombo)
;	=>
;	(printout t crlf "Il pavimento è presente, ma per realizzare il rivestimento occorre controllare che sia a livello poiché vi si poggerà sopra il rivestimento." crlf)
;	(bind ?*help* "")
;    (printout t crlf "Posa una stadia sul pavimento da un angolo all'opposto facendo in modo che poggi bene. Poni su di essa un livello..." crlf
;            "Controlla se ci sono punti in cui la stadia si allontana dal pavimento di diversi centimetri..." crlf
;            "Controlla se la bolla d'aria sul livello si trova nella posizione centrale..." crlf
;            "Ripeti l'operazione diverse volte in modo da coprire tutta l'area da pavimentare e poi rispondi alle seguenti domande..." crlf)
;    (bind ?risposta1 (yes_or_no_p "Nelle varie misurazioni fatte ci sono stati casi in cui la stadia era lontana dal pavimento di diversi centimetri?"))
;    (bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. Non interessa il verso.")
;    (bind ?risposta2 (yes_or_no_p "Nelle varie misurazioni fatte la bolla d'aria sulla livella era sempre nella posizione centrale?"))
;	(if (and (not ?risposta1) ?risposta2) 
;		then 	;(assert (pavimento_livello TRUE))  
;				(assert (ok_inizio_rivestimento)) ;comincia il rivestimento
;		else 	(assert (pavimento_livello FALSE))))  ;il pavimento non è a livello; si collega con le regole per il pavimento non a livello
;
;;TODO spiegazione: spiegare come rimuovere pavimento 
;(defrule pavimento_non_livello_solo_rivestimento ;non si può fare la posa sopra il pavimento perché non a livello
;	?f1 <- (pavimento_livello FALSE)
;	(presenza_pavimento TRUE)
;	=>
;	(printout t crlf "Il pavimento non è a livello. Procedere alla rimozione e proseguire." crlf)
;	(assert (rimozione_pavimento))
;	(retract ?f1))
;
;;REGOLE GENERALI RIVESTIMENTO
;(defrule comincia_rivestimento
;	(rivestimento TRUE)
;	;(pavimento FALSE)
;	(massetto_livello TRUE)
;	(presenza_massetto TRUE)
;	=>
;	(assert (ok_inizio_rivestimento)))
;
;(defrule rimozione_rivestimento  ;se è presente un rivestimento e quello che voglio fare è il rivestimento, allora bisogna toglierlo
;	;(declare (salience ?*low_priority*))
;	?f <- (presenza_rivestimento TRUE)
;	(rivestimento TRUE)
;	=>
;	(printout t crlf "Procedi alla rimozione del rivestimento" crlf)
;	(bind ?*help* "Bisogna procedere alla rimozione delle piastrelle che compongono il rivestimento poiché non si può posare il nuovo rivestimento su %nquello già presente.")
;	(bind ?risposta (yes_or_no_p "Hai rimosso il rivestimento?"))
;	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rimosso il rivestimento?")))
;	(retract ?f)                                   ;il rivestimento
;	(assert (presenza_rivestimento FALSE)))        ;è stato tolto
;
;;TODO ampliamento: aggiungere come si aggiusta un muro?
;(defrule domanda_controllo_muri_rivestimento  ;il rivestimento non c'è e si è deciso di farlo, allora si controllano se i muri sono a piombo
;	;(declare (salience ?*low_priority*))
;	(presenza_rivestimento FALSE)
;	(rivestimento TRUE)
;	(not (muri_a_piombo))
;	=>
;	(printout t crlf "Controllo se i muri sono a piombo..." crlf
;			"Prendi il filo a piombo. Prendi la rocchetta di cui è dotato e poggiala sulla parete da misurare..." crlf
;			"Vedi se il piombo (il peso) alla fine del filo è lontano dal muro, troppo vicino oppure si muove liberamente..." crlf
;			"Ripeti la procedura per ogni parete della stanza su cui stai lavorando..." crlf)
;        (bind ?risposta (yes_or_no_p "Tutti i muri sono a piombo?"))
;        (while (not ?risposta) do 
;                (printout t crlf "Non puoi proseguire nella posa del rivestimento. Devi prima riparare i muri!" crlf)
;                (bind ?risposta (yes_or_no_p "Tutti i muri sono a piombo?")))
;        (assert (muri_a_piombo)))
;
;;TODO ampliamento: indicare anche la possibilità di rivestire solo la fascia di muro non coperta dalla cucina
;(defrule domanda_rivestimento_cucina  ;chiedere se fare il rivestimento di tutta la stanza o solo la parete dove sta la cucina o solo la fascia di parete visibile dietro la ;cucina
;	(declare (salience ?*low_priority*))
;	(not (rivestimento_cucina ?))
;	(tipo_stanza cucina)
;	(rivestimento TRUE)
;	=>
;	(printout t crlf "La stanza in cui fare il lavoro è una cucina ed è stato scelto di effettuare il rivestimento" crlf)
;	(bind ?*help* "Si può decidere di rivestire solo la fascia di muro che è possibile vedere una volta posta la cucina (viene scelto nel caso in %ncui non si vuole rivestire ;l'intera stanza) oppure realizzare il rivestimento di tutta la stanza (consigliato poiché la cucina è %nsempre un ambiente umido che potrebbe portare facilmente alla ;usura delle pareti).")
;	(bind ?risposta (ask_question "Vuoi rivestire tutta le pareti della cucina o solo la fascia di muro che si può vedere una volta posta %nla cucina? (tutta/solo_fascia" ;tutta solo_fascia))
;	(assert (rivestimento_cucina ?risposta)))
;
;;DOMANDE POSA SOPRA (PAVIMENTO PRESENTE)
;(defrule no_posa_sopra ;non è applicabile la posa sopra se è presente un rivestimento ed è stato stabilito di fare solo il pavimento
;	(declare (salience ?*high_priority*))
;	(presenza_rivestimento TRUE)
;	(rivestimento FALSE)
;	(pavimento TRUE)
;	(presenza_pavimento TRUE)
;	(not (posa_sopra_pavimento ?))
;	=>
;	(printout t crlf "È già presente un rivestimento ed è stato stabilito di fare solo il pavimento, dunque si dovrà procedere alla rimozione del," crlf
;					"pavimento esistente!" crlf)
;	(assert (posa_sopra_pavimento FALSE)))
;
;(defrule posa_sopra_pavimento_grandi_aree ;per aree da pavimentare molto grandi non è auspicabile fare la posa sopra
;	(declare (salience ?*high_priority*))
;	(not (posa_sopra_pavimento ?))
;	(pavimento TRUE)
;	(presenza_pavimento TRUE)
;	(dimensioni_pavimento ?dim)
;	=>
;	(if (> ?dim 50) 
;		then 	
;			(printout t crlf "Anche se si volesse fare una posa sopra il pavimento esistente, la sua dimensione sarebbe troppo grande per poter controllare" crlf
;							"bene il suo stato (piastrelle sollevate o non aderenti), conviene quindi procedere alla rimozione!" crlf)
;			(assert (posa_sopra_pavimento FALSE))))
;
;(defrule domanda_posa_sopra_pavimento_da_raccordare  ;se c'è un pavimento da raccordare non si può effettuare la posa sopra
;	;(declare (salience ?*low_priority*))
;	(not (posa_sopra_pavimento ?))
;	(pavimento_da_raccordare TRUE)  ;Il pavimento dovrà essere raccordato 
;	(pavimento TRUE)
;	(presenza_pavimento TRUE)
;	=>
;	(printout t crlf "Il pavimento deve essere raccordato con un altro già presente, non si può quindi effettuare la posa sopra!" crlf
;					"Bisogna rimuovere il pavimento esistente e procedere alla realizzazione del massetto!" crlf)
;	(assert (posa_sopra_pavimento FALSE)))
;
;(defrule domanda_posa_sopra_pavimento  ;se il pavimento è presente e si è scelto di porre un nuovo pavimento, chiedere se fare la posa sopra il pavimento esistente
;	;(declare (salience ?*low_priority*))
;	(not (posa_sopra_pavimento ?))
;	?f <- (pavimento_da_raccordare FALSE)  ;Il pavimento non dovrà essere raccordato 
;	(pavimento TRUE)
;	(presenza_pavimento TRUE)
;	(spessore_piastrella_pavimento ?spessore_piastrella)
;	=>
;	(printout t crlf "Considera che la posa sopra un pavimento già esistente rialzerà il piano, quindi non è da scegliere nel caso in cui ci si deve raccordare con un" crlf
;					"altro pavimento già esistente. Dalle scelte effettuate sembra che il pavimento non debba essere raccordato con nessun altro pavimento." crlf)
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "È cosi, cioè il pavimento non deve essere raccordato ad alcun altro pavimento?"))
;	(if (not ?risposta) 
;		then
;			(retract ?f)
;			(assert (pavimento_da_raccordare TRUE))
;		else
;			(format t "Il nuovo pavimento da porre sopra a quello già esistente avrà uno spessore di %d mm. %nOccorrerà effettuare delle modifiche anche alle porte (che ;dovranno essere ridotte).%n" (+ 3 ?spessore_piastrella))
;			(bind ?*help* "Scegliendo tale tipo di posa, il nuovo pavimento verrà realizzato posandolo sopra quello già esistente. Tuttavia occorre %nvalutare bene la scelta ;poiché si devono fare alcune modifiche alle porte, in quanto il piano verrà rialzato.")
;			(bind ?risposta (yes_or_no_p "Vuoi effettuare la posa sopra il pavimento esistente?"))
;			(assert (posa_sopra_pavimento ?risposta))))
;
;(defrule domanda_condizioni_pavimento  ;se si è scelti la posa sopra, verificare le condizioni del pavimento presente e se è a livello
;	(posa_sopra_pavimento TRUE)
;	=>
;	(printout t crlf "Guarda con attenzione in ogni punto il pavimento già presente e batti con il manico di un martello." crlf
;					"Se senti un rumore forte, questo vuol dire che la piastrella sotto è vuota (cioè non è più aderente)..." crlf)
;	(bind ?*help* "Controlla picchiettando con il manico si un martello le piastrelle in tutti i punti del pavimento, se senti un rumore cupo e forte, %nvuol dire che la ;piastrella non aderisce bene.")
;	(bind ?risposta (yes_or_no_p "Ci sono piastrelle rialzate o non perfettamente aderenti?"))
;	(assert (piastrelle_sollevate ?risposta))
;
;    (bind ?*help* "")
;    (printout t crlf "Posa una stadia sul pavimento da un angolo all'opposto facendo in modo che poggi bene. Poni su di essa un livello..." crlf
;            "Controlla se ci sono punti in cui la stadia si allontana dal pavimento di diversi centimetri..." crlf
;            "Controlla se la bolla d'aria sul livello si trova nella posizione centrale..." crlf
;            "Ripeti l'operazione diverse volte in modo da coprire tutta l'area da pavimentare e poi rispondi alle seguenti domande..." crlf)
;    (bind ?risposta1 (yes_or_no_p "Nelle varie misurazioni fatte ci sono stati casi in cui la stadia era lontana dal pavimento di diversi centimetri?"))
;    (bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. Non interessa il verso.")
;    (bind ?risposta2 (yes_or_no_p "Nelle varie misurazioni fatte la bolla d'aria sulla livella era sempre nella posizione centrale?"))
;	(if (and (not ?risposta1) ?risposta2) then (assert (pavimento_livello TRUE)) else (assert (pavimento_livello FALSE))))
;
;;TODO ampliamento: come si fa la malta cementizia e come riempire i buchi
;(defrule piastrelle_sollevate ;se ci sono piastrelle sollevate falle aggiustare e poi inizia la posa
;	?f <- (piastrelle_sollevate TRUE)
;	(pavimento_livello TRUE)  ;il pavimento deve essere a livello
;	=>
;	(printout t crlf "Procedere alla rimozione di tali piastrelle rialzate o non aderenti e riempire i buchi creati con malta cementizia." crlf)
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Hai rattoppato il vuoto creato dalle piastrelle eliminate?"))
;	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rattoppato il vuoto creato dalle piastrelle eliminate?")))
;	(retract ?f) ;piastrelle non più sollevate
;	(assert (piastrelle_sollevate FALSE)))
;
;(defrule piastrelle_non_sollevate ;se le piastrelle non sono sollevate e il pavimento è a livello procedi con la posa del pavimento
;	?f <- (piastrelle_sollevate FALSE)
;	?f1 <- (pavimento_livello TRUE)
;	=>
;	(retract ?f ?f1)
;	(assert (ok_inizio_pavimento)))
;
;(defrule pavimento_non_livello ;non si può fare la posa sopra il pavimento perché non a livello
;	?f1 <- (pavimento_livello FALSE)
;	(presenza_pavimento TRUE)
;	=>
;	(printout t crlf "Il pavimento non è in condizioni tali da potervi effettuare una posa sopra. Procedere alla rimozione e proseguire." crlf)
;	(assert (rimozione_pavimento))
;	(retract ?f1))
;
;;TODO spiegazione: spiegare come rimuovere pavimento 
;(defrule posa_sopra_pavimento_false ;se non si opta per la posa sopra si elimina il pavimento
;	?f1 <- (posa_sopra_pavimento FALSE)
;	(presenza_pavimento TRUE)
;	=>
;	(assert (rimozione_pavimento))
;	;(retract ?f1)
;	)
;
;
;;REGOLE GENERALI PAVIMENTO
;(defrule rimozione_pavimento ;regola per la rimozione del pavimento
;	?f1 <- (rimozione_pavimento)
;	?f2 <- (presenza_pavimento TRUE)
;	=>
;	(printout t crlf "Occorre procedere alla rimozione del pavimento..." crlf
;		"Procurati un martello pneumatico e procedi alla rimozione dell'intero pavimento togliendo piccole parti alla volta" crlf
;		"Quando si toglie il pavimento, si procede anche alla realizzazione del massetto, quindi non togliere solo le piastrelle e la colla attaccata" crlf
;		"ma togli anche qualche centimetro di massetto vecchio che poi si rifarà a nuovo." crlf)
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Hai rimosso il pavimento?"))
;	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rimosso il pavimento?")))
;	(retract ?f1 ?f2)
;	(assert (presenza_massetto FALSE))
;	(assert (presenza_pavimento FALSE)))
;
;(defrule domanda_presenza_massetto  ;regola per verificare la presenza del massetto
;	(declare (salience ?*lowest_priority*))
;	(presenza_pavimento FALSE)
;	(pavimento TRUE)
;	(not (presenza_massetto ?))
;	=>
;	(bind ?*help* "Il massetto è uno strato di cemento la cui presenza è essenziale perché sopra esso verranno posate le piastrelle.")
;	(bind ?risposta (yes_or_no_p "È presente un massetto?"))
;	(assert (presenza_massetto ?risposta)))
;
;(defrule rimozione_massetto_non_a_livello
;	?f1 <- (massetto_livello FALSE)
;	?f2 <- (rimozione_massetto)
;	?f3 <- (presenza_massetto TRUE)
;	=>
;	(printout t crlf "Occorre procedere alla rimozione del massetto nell'area in cui si intende lavorare..." crlf
;		"Procurati un martello pneumatico e comincia ad eliminare piccoli porzioni del massetto esistente." crlf
;		"Spostati in avanti e, di tanto in tanto, raccogli i pezzi del massetto vecchio che sono usciti e buttali." crlf
;		"Dopo aver finito tutto si procederà al rifacimento del nuovo massetto." crlf)
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Hai rimosso il massetto?"))
;	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rimosso il massetto?")))
;	(retract ?f1 ?f2 ?f3)
;	(assert (presenza_massetto FALSE)))
;
;(defrule rimozione_massetto ;rimozione del massetto
;	?f1 <- (rimozione_massetto)
;	?f2 <- (presenza_massetto TRUE)
;	(not (massetto_livello FALSE))
;	=>
;	(printout t crlf "Occorre procedere alla rimozione del massetto nell'area in cui si intende lavorare..." crlf
;		"Procurati un martello pneumatico e comincia ad eliminare piccoli porzioni del massetto esistente." crlf
;		"Spostati in avanti e, di tanto in tanto, raccogli i pezzi del massetto vecchio che sono usciti e buttali." crlf
;		"Dopo aver finito tutto si procederà al rifacimento del nuovo massetto." crlf)
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Hai rimosso il massetto?"))
;	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rimosso il massetto?")))
;	(retract ?f1 ?f2)
;	(assert (presenza_massetto FALSE)))
;
;;TODO ampliamento: spiegare come si fa il massetto
;;TODO remember: qui si collegano anche i rami riguardanti il massetto troppo alto o troppo basso nel caso di raccordo con pavimenti presenti e rivestimenti presenti
;(defrule fai_massetto_pavimento
;	;(declare (salience ?*low_priority*))
;	(interno ?val)
;	?f <- (presenza_massetto FALSE)
;	(pavimento_da_raccordare FALSE)
;	=>
;	(bind ?*help* "")
;	(printout t crlf "Devi fare il massetto tenendo conto anche di eventuali raccordi con pavimenti o rivestimenti già presenti!" crlf)
;	(if (not ?val) then (printout t "Devi tenere anche conto della pendenza!" crlf))
;	(bind ?risposta (yes_or_no_p "Hai fatto il massetto?"))
;	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai fatto il massetto?")))
;	(retract ?f)
;	(assert (presenza_massetto TRUE))
;	(assert (massetto_livello TRUE))
;	(assert (massetto_raccordo_rivestimento_livello TRUE))) ;quando si fa il massetto nuovo questo è sicuramente a livello
;
;;TODO ampliamento: spiegare come si fa il massetto
;;TODO remember: considera anche il raccordo del pavimento con un altro pavimento
;(defrule fai_massetto_pavimento_raccordo
;	;(declare (salience ?*low_priority*))
;	(interno ?val)
;	?f <- (presenza_massetto FALSE)
;	(pavimento_da_raccordare TRUE)
;	=>
;	(bind ?*help* "")
;	(printout t crlf "Devi fare il massetto tenendo conto anche di eventuali raccordi con pavimenti o rivestimenti già presenti!" crlf)
;	(if (not ?val) then (printout t "Devi tenere anche conto della pendenza!" crlf))
;	(bind ?risposta (yes_or_no_p "Hai fatto il massetto?"))
;	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai fatto il massetto?")))
;	(retract ?f)
;	(assert (presenza_massetto TRUE))
;	(assert (massetto_livello TRUE))
;	(assert (massetto_raccordo_livello TRUE))
;	(assert (massetto_raccordo_rivestimento_livello TRUE))) ;quando si fa il massetto nuovo questo è sicuramente a livello
;
;(defrule domanda_controllo_massetto_a_livello ;controllo se il massetto presente è a livello nel caso di pavimento da non raccordare
;	(presenza_massetto TRUE)
;	(not (massetto_livello ?))
;	=>
;	(printout t crlf "Controlliamo se il massetto è a livello..." crlf crlf
;					"Posa una stadia sul massetto da un angolo all'opposto facendo in modo che poggi bene. Poni su di essa un livello..." crlf
;	                "Controlla se ci sono punti in cui la stadia si allontana dal massetto di diversi centimetri..." crlf
;	                "Controlla se la bolla d'aria sul livello si trova nella posizione centrale..." crlf
;	                "Ripeti l'operazione diverse volte in modo da coprire da un alto all'altro tutta l'area da pavimentare e poi rispondi alle seguenti domande..." crlf)
;	(bind ?*help* "")
;	(bind ?risposta1 (yes_or_no_p "Nelle varie misurazioni fatte ci sono stati casi in cui la stadia era lontana dal massetto di diversi centimetri?"))
;	(bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. Non interessa il verso.")
;	(bind ?risposta2 (yes_or_no_p "Nelle varie misurazioni fatte la bolla d'aria sulla livella era sempre nella posizione centrale?"))
;
;	(bind ?massettoalivello (and (not ?risposta1) ?risposta2))
;	(if ?massettoalivello then (assert (massetto_livello TRUE)) else (assert (massetto_livello FALSE))))
;
;(defrule massetto_non_livello ;il massetto non è a livello, quindi deve essere tolto
;	(massetto_livello FALSE)
;	=>
;	(printout t crlf "Il massetto non è a livello, occorre procedere alla sua rimozione e al rifacimento..." crlf)
;	(assert (rimozione_massetto)))
;
;(defrule massello_a_livello ;massetto a livello e niente raccordo
;	(massetto_livello TRUE) 
;	(pavimento_da_raccordare FALSE)
;	(pavimento TRUE)
;	=>
;	(assert (ok_inizio_pavimento)))
;
;(defrule domanda_controllo_massetto_raccordo ;il massetto è a livello ma si deve controllare se è all'altezza giusta per il raccordo
;	(pavimento_da_raccordare TRUE)
;	(massetto_livello TRUE)
;	(pavimento TRUE)
;	(spessore_piastrella_pavimento ?spessore_piastrella)
;	(not (massetto_raccordo_livello ?))
;	=>
;	(printout t crlf "Il massetto è a livello, ma bisogna controllare che sia realizzato in modo tale che con la posa del pavimento esso si trovi" crlf
;					"allo stesso livello del pavimento in un'altra stanza con cui si andrà a raccordare" crlf)
;	(format t "%nLo spessore della piastrella è di %d mm%n" ?spessore_piastrella)
;	(printout t "Lo spessore della colla sarà di 3mm" crlf)
;	(bind ?spessore_pavimento (+ ?spessore_piastrella 3))
;	(format t "Il pavimento avrà uno spessore totale di %d mm%n" ?spessore_pavimento)
;
;	(printout t crlf "Il pavimento verrà raccordato con quello di un'altra stanza, quindi occorre procedere alla verifica dell'altezza del massetto" crlf
;					"rispetto al pavimento già esistente..." crlf)
;	(format t "%nControlliamo che il massetto sia a %d mm sotto la superficie del pavimento con cui deve essere raccordato%n" ?spessore_pavimento)
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Il massetto è all'altezza giusta, considerando lo spessore complessivo (piastrella + colla) del pavimento che si %npresta ad essere posato?");)
;	(assert (massetto_raccordo_livello ?risposta)))
;
;(defrule massetto_raccordo_livello_giusto
;	(massetto_raccordo_livello TRUE)
;	(pavimento_da_raccordare TRUE)
;	=>
;	(assert (ok_inizio_pavimento)))
;
;(defrule domanda_massetto_raccordo_troppo_alto_o_basso
;	?f <- (massetto_raccordo_livello FALSE)
;	=>
;	(printout t crlf "Il massetto non è idoneo alla posa del pavimento poiché con la posa del pavimento risulta non essere a livello del pavimento già presente!" crlf)
;	(retract ?f)
;	(bind ?*help* "")
;	(bind ?risposta (ask_question "Il massetto risulta troppo alto o troppo basso, considerando la posa del pavimento? (alto/basso" alto basso))
;	(assert (massetto_alto_basso ?risposta)))
;
;(defrule massetto_alto
;	?f1 <- (massetto_alto_basso alto)
;	=>
;	(retract ?f1)
;	(printout t crlf "Il massetto è troppo alto, quindi occorre smantellarlo e procedere al rifacimento!" crlf
;					"Bisogna fare in modo di togliere qualche centimetro in più oltre a quelli per cui il pavimento il pavimento risulterebbe più alto." crlf)
;	(assert (rimozione_massetto)))
;
;(defrule massetto_basso
;	?f1 <- (massetto_alto_basso basso)
;	=>
;	(retract ?f1)
;	(printout t crlf "Il massetto è troppo basso, quindi occorre aumentare lo spessore del massetto!" crlf)
;	(assert (presenza_massetto FALSE))) ;si collega a fai massetto
;
;(defrule domanda_controllo_massetto_rivestimento_presente ;controllo massetto in modo tale che sia raccordato al rivestimento presente (cioè vada a combaciare con il ;rivestimento senza lasciare intravedere spazi bianchi)
;	(massetto_livello TRUE)
;	(not (massetto_raccordo_rivestimento_livello ?))
;	(pavimento TRUE)
;	(rivestimento FALSE)			;il rivestimento è presente
;	(presenza_rivestimento TRUE)	;ma non è da fare, cioè rimane il rivestimento vecchio e il pavimento deve essere raccordato
;	(spessore_piastrella_pavimento ?spessore_piastrella)
;	=>
;	(printout t crlf "Il massetto è a livello ma bisogna controllare che sia all'altezza giusta per il raccordo con il rivestimento esistente, cioè, si deve" crlf
;					"controllare che una volta posato, esso vada a combaciare perfettamente con la parte bassa del rivestimento." crlf)
;
;	(bind ?spessore_pavimento (+ 3 ?spessore_piastrella))
;	(format t "%nBisogna controllare che lo spessore del pavimento che si deve posare (che si ottiene aggiungendo %d mm dal massetto) vada a combaciare, %nsenza lasciare ;spazi in basso, con il rivestimento già presente!%n" ?spessore_pavimento)
;
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Considerando lo spessore che si otterrebbe dalla posa del pavimento sul massetto presente, esso va a combaciare %nprecisamente con il ;rivestimento presente?"))
;	(assert (massetto_raccordo_rivestimento_livello ?risposta)))
;
;(defrule domanda_massetto_rivestimento_alto_basso
;	?f <- (massetto_raccordo_rivestimento_livello FALSE)
;	=>
;	(printout t crlf "Il massetto non è idoneo alla posa del pavimento poiché con la posa del pavimento risulta non essere a livello con il rivestimento già presente!" crlf)
;	(retract ?f)
;	(bind ?*help* "")
;	(bind ?risposta (ask_question "Il massetto risulta troppo alto o troppo basso, considerando la posa del pavimento? (alto/basso" alto basso))
;	(assert (massetto_alto_basso ?risposta)))
;
;
;;-----------------INIZIO POSA-------------------
;(defrule inizio_rivestimento_e_pavimento ;se c'è da fare pavimento e rivestimento spiega perchè prima inizio rivestimento
;	(ok_inizio_rivestimento)
;	(ok_inizio_pavimento)
;	=>
;	(printout t crlf "Si devono realizzare sia il pavimento che il rivestimento. Conviene partire sempre dal rivestimento in quanto ci sono diversi" crlf
;					"vantaggi dati dal fatto che non bisogna aspettare che il pavimento asciughi prima di poter lavorare al rivestimento e si evita di" crlf 
;					"creare danni al pavimento nuovo poiché lavorando al rivestimento potrebbe accadere di scheggiarlo." crlf)
;	(assert (inizio_rivestimento))) ;inizia rivestimento
;
;(defrule inizio_rivestimento ;c'è da fare solo rivestimento
;	(ok_inizio_rivestimento)
;	(not (ok_inizio_pavimento))
;	=>
;	(assert (inizio_rivestimento)))
;
;(defrule inizio_pavimento ;c'è da fare solo pavimento
;	(ok_inizio_pavimento)
;	(not (ok_inizio_rivestimento))
;	=>
;	(assert (inizio_pavimento)))
;
;
;
;
;
;
;
;
;
;
;