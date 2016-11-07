(defglobal ?*highest_priority* = 1000)
(defglobal ?*high_priority* = 100)
(defglobal ?*low_priority* = -100)
(defglobal ?*lowest_priority* = -1000)
(defglobal ?*help* = "")
(defglobal ?*spiegazione* = "")
(defglobal ?*soluzione* = "")

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
  	(format t (str-cat "%n" ?question " (si/no/help): "))
  	(bind ?answer (read))

  	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

  	(while (not (member ?answer ?allowed_values)) do
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "Non è presente alcun help!" crlf)
		  				else (format t (str-cat ?*help* "%n"))))
		(format t (str-cat "%n" ?question " (si/no/help): "))
	    (bind ?answer (read))
	    (if (lexemep ?answer) 
			then (bind ?answer (lowcase ?answer))))

  	(if (or (eq ?answer si) (eq ?answer s))
         then TRUE 
         else FALSE))

(deffunction ask_number (?question)
	(format t (str-cat "%n" ?question " (help): "))
	(bind ?answer (read))

	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

	(while (not (numberp ?answer)) do  ;check if answer is a NUMBER
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "Non è presente alcun help!" crlf)
		  				else (format t (str-cat ?*help* "%n"))))
		(format t (str-cat "%n" ?question " (help): "))
	    (bind ?answer (read)))
	 ?answer)



;----------------------FUNZIONI PER RITRATTAZIONE-------------------------

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



;  /----------------------------------------/
; /-----------------LAVORI-----------------/
;/----------------------------------------/
(defrule impianti_umidita_esperto
	(preparazione_utente alta)
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome umidita) (valore si))
	(car (nome impianti_umidita) (valore si))
	=>
	(bind ?*soluzione* "Controlla lo stato delle tubazioni.")
	(bind ?*spiegazione* "Avendo dedotto che ci sono tubazioni che trasportano acqua sotto il pavimento, l'umidità potrebbe essere causata da esse.") 
	(bind ?*help* "Rimuovi (con attenzione a non rompere nulla) il pavimento e il massetto (se presenti) e controlla se ci sono perdite, poi chiama uno specialista.")
	(assert (lavoro)))

(defrule umidita_principiante
	(preparazione_utente bassa)
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome umidita) (valore si))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere causata da diversi fattori, chiama uno specialista.")
	(bind ?*spiegazione* "")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule umidita_esperto
	(preparazione_utente alta)
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome umidita) (valore si))
	(car (nome impianti_umidita) (valore no))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere quella di risalita dai muri, consulta uno specialista.")
	(bind ?*spiegazione* "Non essendoci tubazioni che trasportano acqua, l'umidità potrebbe essere causata da quella di risalita dei muri.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule rifacimento_impianti_esperto
	(preparazione_utente alta)
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome rifacimento_impianti) (valore si))
	=>
	(bind ?*soluzione* "Rimuovi tutto il pavimento e lo strato di cemento sottostante (massetto), poi chiama uno specialista per realizzare l'impianto.")
	(bind ?*spiegazione* "Avendo dedotto che bisogna rifare gli impianti, il consiglio è quello di rimuovere tutto il pavimento e il massetto.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule rifacimento_impianti_principiante
	(preparazione_utente bassa)
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome rifacimento_impianti) (valore si))
	=>
	(bind ?*soluzione* "Consulta uno specialista.")
	(bind ?*spiegazione* "")
	(bind ?*help* "")
	(assert (lavoro)))

;---------------------------------------------------------------------------------------------------------------------------------------

(defrule fai_massetto_raccordo
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore no))
	(car (nome presenza_pavimento) (valore no))
	(car (nome umidita) (valore no))
	(car (nome rifacimento_impianti) (valore no))
	(car (nome pavimento_da_raccordare) (valore si))
	=>
	(bind ?*soluzione* "Realizza il massetto tenendo conto del fatto che bisogna raccordarsi con un pavimento già esistente.")
	(bind ?*spiegazione* "Avendo dedotto che non è presente nè un pavimento nè un rivestimento, che non c'è umidità e non si devono fare impianti, ma c'è un pavimento con cui raccordarsi, allora il lavoro da fare è il massetto.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule fai_massetto_no_raccordo
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore no))
	(car (nome presenza_pavimento) (valore no))
	(car (nome umidita) (valore no))
	(car (nome rifacimento_impianti) (valore no))
	(car (nome pavimento_da_raccordare) (valore no))
	=>
	(bind ?*soluzione* "Realizza il massetto.")
	(bind ?*spiegazione* "Avendo dedotto che non è presente nè un pavimento nè un rivestimento, che non c'è umidità e non si devono fare impianti e no c'è un pavimento con cui raccordarsi, allora il lavoro da fare è il massetto.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule ok_pavimento_interno1  ;pavimento non da raccordare
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_pavimento) (valore no))
	(car (nome presenza_massetto) (valore si))
	(car (nome pavimento_da_raccordare) (valore no))
	(car (nome massetto_livello) (valore si))
	(car (nome massetto_friabile) (valore no))
	=>
	(bind ?*soluzione* "Il massetto su cui porre il pavimento è a livello, puoi effettuare la posa.")
	(bind ?*spiegazione* "Avendo dedotto che non è presente un pavimento, che è presente un massetto,che è a livello e in buone condizioni e che non ci si deve preoccupare di raccordarsi con un altro tipo di pavimento presente, si può iniziare la posa sopra di una qualsiasi tipo di pavimento.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule ok_pavimento_interno2  ;pavimento da raccordare
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_pavimento) (valore no))
	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_livello) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome massetto_altezza) (valore giusto))
	(car (nome massetto_friabile) (valore no))
	=>
	(bind ?*soluzione* "Il massetto su cui porre il pavimento è a livello e alla giusta altezza, puoi iniziare la posa.")
	(bind ?*spiegazione* "Avendo dedotto che non è presente un pavimento il pavimento è da raccordare, ma è all'altezza giusta in base al tipo di pavimento da posare, e che è a livello e in buone condizioni, si può allora iniziare la posa.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule ok_pavimento_esterno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore no))
	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_pendenza) (valore si))
	(car (nome massetto_friabile) (valore no))
	=>
	(bind ?*soluzione* "Il massetto ha la giusta pendenza, puoi iniziare la posa sopra.")
	(bind ?*spiegazione* "Avendo dedotto che si parla di lavoro esterno, che è presente un massetto che è in buone condizioni e che ha la giusta pendenza, si può allora iniziare la posa.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule massetto_friabile
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_friabile) (valore si))
	=>
	(bind ?*soluzione* "Procedi alla rimozione e al rifacimento del massetto.")
	(bind ?*spiegazione* "Avendo dedotto che è presente un massetto che non è compatto, conviene procedere alla sua rimozione e al rifacimento.")
	(bind ?*help* "Rimuovi il massetto esistente e rifallo.")
	(assert (lavoro)))

(defrule massetto_non_a_livello
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_livello) (valore no))
	=>
	(bind ?*soluzione* "Elimina il massetto presente e rifallo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di lavoro interno, che è presente un massetto e che questo non è a livello, il consiglio è quello di rimuoverlo e rifarlo.")
	(bind ?*help* "Rimuovi il massetto esistente e rifallo.")
	(assert (lavoro)))

(defrule massetto_alto
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome massetto_livello) (valore si))
	(car (nome massetto_altezza) (valore alto))
	=>
	(bind ?*soluzione* "Rimuovi rifai daccapo il massetto.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di lavoro interno, che il pavimento da posare è da raccordare, che è presente un massetto, che è a livello ma che è troppo alto per il tipo di pavimento che si dovrà porre, allora il consiglio è di rimuoverlo e rifarlo.")
	(bind ?*help* "Rimuovi il massetto esistente e rifallo.")
	(assert (lavoro)))

(defrule massetto_basso
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome massetto_livello) (valore si))
	(car (nome massetto_altezza) (valore basso))
	=>
	(bind ?*soluzione* "Costruisci sopra al presente massetto uno nuovo in modo che sia alla altezza esatta.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di lavoro interno, che il pavimento da posare è da raccordare, che è presente un massetto, che è a livello ma che è troppo basso per il tipo di pavimento che si dovrà porre, allora il consiglio è di costruire sopra il presente massetto uno nuovo per fare in modo che si trovi alla altezza giusta.")
	(bind ?*help* "Costruisci un massetto su quello esistente.")
	(assert (lavoro)))

(defrule massetto_non_pendenza
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_pendenza) (valore no))
	=>
	(bind ?*soluzione* "Rimuovere il massetto e rifarlo daccapo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro esterno, che è presente un massetto e che la pendenza per favorire lo scolo dell'acqua non è stata rispettata, il consiglio è quello di rimuoverlo e rifarlo daccapo.")
	(bind ?*help* "Rimuovi il massetto facendo attenzione ad eventuali tubi di impianti e rifallo.")
	(assert (lavoro)))

;----------------------------------------------------------------------------------------------------------------------------------------

(defrule rattoppo_pavimento_interno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pavimento_livello) (valore si))
	(or (car (nome piastrelle_sollevate_pavimento) (valore si))
		(car (nome piastrelle_scheggiate_pavimento) (valore si)))
	=>
	(bind ?*soluzione* "Sostituisci le piastrelle difettate se sono poche (rattoppo), facendo attenzione al fatto che non vi sia differenza di tonalità tra le piastrelle nuove e quelle vecchie; altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro interno, che un pavimento è presente ed è a livello ma vi sono piastrelle danneggiate, il consiglio è di sostituirle se sono poche, altrimenti rifare tutto il pavimento.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule rattoppo_pavimento_esterno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pendenza_pavimento) (valore si))
	(or (car (nome piastrelle_sollevate_pavimento) (valore si))
		(car (nome piastrelle_scheggiate_pavimento) (valore si)))
	=>
	(bind ?*soluzione* "Sostituisci le piastrelle difettate se sono poche (rattoppo), facendo attenzione al fatto che non vi sia differenza di tonalità tra le piastrelle nuove e quelle vecchie; altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro esterno, che un pavimento è presente ed ha la pendenza giusta ma vi sono piastrelle danneggiate, il consiglio è di sostituirle se sono poche, altrimenti rifare tutto il pavimento.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule posa_sopra_interno ;TODO da rivedere
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pavimento_livello) (valore si))
	(and (not (car (nome piastrelle_sollevate_pavimento) (valore no)))
		 (not (car (nome piastrelle_scheggiate_pavimento) (valore no))))
	=>
	(bind ?*soluzione* "Si può optare per la posa sopra al pavimento esistente. Bisogna considerare però che vi sarà un innalzamento del pavimento che dovrà portare a diverse modifiche (è il caso delle porte), altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro interno, il pavimento è presente ed è a livello e non vi sono piastrelle scheggiate o sollevate, si può optare per la posa sopra.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule posa_sopra_esterno ;TODO da rivedere
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pendenza_pavimento) (valore si))
	(and (not (car (nome piastrelle_sollevate_pavimento) (valore no)))
		 (not (car (nome piastrelle_scheggiate_pavimento) (valore no))))
	=>
	(bind ?*soluzione* "Se si deve rinnovare il pavimento si può decidere anche per la posa sopra al pavimento esistente. Bisogna considerare però che vi sarà %nun innalzamento del pavimento che dovrà portare a diverse modifiche (è il caso delle porte), altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro esterno, il pavimento è presente ed è alla giusta pendenza e non vi sono piastrelle scheggiate o sollevate, si può optare per la posa sopra.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule pavimento_non_a_livello
	(declare (salience ?*high_priority*))
	(not (lavoro))
	
	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pavimento_livello) (valore no))
	=>
	(bind ?*soluzione* "Rimuovi il pavimento assieme allo strato di cemento sottostante (massetto) e rifai entrambi.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un luogo esterno, che il pavimento è presente ma non è a livello, quindi occorre toglierlo e rifarlo.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule pavimento_non_pendenza
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pendenza_pavimento) (valore no))
	=>
	(bind ?*soluzione* "Rimuovi il pavimento insieme allo strato di cemento sottostante (massetto) e rifallo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un luogo esterno, che il pavimento è presente ma non ha la pendenza giusta per favorire lo scolo dell'acqua, quindi occorre toglierlo e rifarlo.")
	(bind ?*help* "")
	(assert (lavoro)))

;------------------------------------------------------------------------------------------------------------------------------------------------

(defrule spostamento_sanitari
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome tipo_stanza) (valore bagno))
	(car (nome spostamento_sanitari) (valore si))
	=>
	(bind ?*soluzione* "Rimuovi il pavimento esistente e lo strato di cemento sottostante (massetto), poi chiama uno specialista.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro interno, in particolare il bagno e che si deve fare uno spostamento di sanitari, il consiglio è quello di rimuovere il pavimento e il massetto se presenti e poi procedere chiamando uno specialista per il posizionamento degli impianti")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule muri_non_a_piombo
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_rivestimento) (valore si))
	(car (nome muri_a_piombo) (valore no))
	=>
	(bind ?*soluzione* "Raddrizzare i muri in modo che siano a piombo, poi procedere con la posa o con l'intonaco.")
	(bind ?*spiegazione* "Avendo dedotto che il lavoro riguarda l'interno, che è presente un rivestimento e che i muri non sono a piombo, il consiglio è di rifare i muri e poi procedere alla posa del rivestimento")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule rattoppo_rivestimento
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome piastrelle_scheggiate_rivestimento) (valore si))
		(car (nome piastrelle_sollevate_rivestimento) (valore si)))
	=>
	(bind ?*soluzione* "Sostituisci le piastrelle difettate se sono poche (rattoppo), facendo attenzione al fatto che non vi sia differenza di tonalità tra le piastrelle nuove e quelle vecchie; altrimenti procedere a sostituire il rivestimento con uno nuovo..")
	(bind ?*spiegazione* "Avendo dedotto che il lavoro riguarda l'interno, che è presente un pavimento e che ci sono piastrelle scheggiate o sollevate, il consiglio è di effettuare un rattoppo se il numero delle piastrelle è limitato, altrimenti rifare tutto il rivestimento.")
	(bind ?*help* "")
	(assert (lavoro)))


;-----------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------

(defrule lavoro_trovato
	(declare (salience ?*high_priority*))
	(lavoro)
	=>
	(format t (str-cat "%n>>>>> Soluzione: %n%s%n%n") ?*soluzione*)

	(if (yes_or_no_p "Vuoi rivedere qualcosa?")
		then (assert (rivedi_scelte_lavoro))
		else (halt)))

(defrule lavoro_non_trovato
	(declare (salience ?*lowest_priority*))
	(not (lavoro))
	=>
	(printout t crlf "Lavoro non trovato!" crlf)
	(if (yes_or_no_p "Vuoi rivedere le scelte fatte o cambiare qualcosa?")
		then (assert (rivedi_scelte_lavoro))
		else (printout t crlf "Prova a riavviare il sistema") 
			 (halt)))

(defrule rivedi_scelte_lavoro
	(declare (salience ?*high_priority*))
	?f2 <- (lavoro)
	?f1 <- (rivedi_scelte_lavoro)
	=>
	(retract ?f1 ?f2)
	(chiedi_cambio_scelte_lavoro "Inserisci il numero della scelta che vuoi modificare o 't' per terminare o 'c' per continuare: "))

(defrule rivedi_scelte_no_lavoro
	(declare (salience ?*high_priority*))
	(not (lavoro))
	?f <- (rivedi_scelte_lavoro)
	=>
	(retract ?f)
	(chiedi_cambio_scelte_lavoro "Inserisci il numero della scelta che vuoi modificare o 't' per terminare o 'c' per continuare: "))

;da aggiungere: rumori, squadro
