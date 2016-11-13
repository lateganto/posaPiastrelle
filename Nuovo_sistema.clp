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
	(printout t crlf "*****************************************************************************************************" crlf) 
	(format t (str-cat "%n" ?question))

	(bind ?i 1)
	(progn$ (?o ?allowed_values)
		(format t (str-cat "%n (%d) %s" ) ?i (nth$ ?i ?allowed_values))
		(bind ?i (+ 1 ?i)))
	(format t "%n (%d) help" ?i)

	(if (neq (length$ ?*spiegazione*) 0)
		then (format t "%n (%d) perche" (+ ?i 1)))
	(printout t crlf) 
	

	(format t "Inserire scelta: ")
	(bind ?answer (read))

	(while (or (not (numberp ?answer)) (< ?answer 1) (> ?answer (length$ ?allowed_values)))
		(if (eq ?answer (+ 1 (length$ ?allowed_values)))
			then (if (eq (length$ ?*help*) 0)
				  	then (printout t "Non è presente alcun aiuto!" crlf)
				  	else (format t (str-cat ?*help* "%n"))))

		(if (neq (length$ ?*spiegazione*) 0)
			then 
				(if (eq ?answer (+ 2 (length$ ?allowed_values)))
					then (format t (str-cat ?*spiegazione* "%n"))))

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
	(printout t crlf "*****************************************************************************************************" crlf)
	(bind ?allowed_values (create$ si no s n))

	(if (neq (length$ ?*spiegazione*) 0)
		then (format t (str-cat "%n" ?question " (si/no/help/perche): "))
		else (format t (str-cat "%n" ?question " (si/no/help): ")))
  	(bind ?answer (read))

  	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

  	(while (not (member ?answer ?allowed_values)) do
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "Non è presente alcun aiuto!" crlf)
		  				else (format t (str-cat ?*help* "%n"))))

		(if (neq (length$ ?*spiegazione*) 0)
			then 
				(if (or (eq ?answer perche) (eq ?answer p))
					then (format t (str-cat ?*spiegazione* "%n"))))

		(if (neq (length$ ?*spiegazione*) 0)
			then (format t (str-cat "%n" ?question " (si/no/help/perche): "))
			else (format t (str-cat "%n" ?question " (si/no/help): ")))
	    (bind ?answer (read))
	    (if (lexemep ?answer) 
			then (bind ?answer (lowcase ?answer))))

  	(if (or (eq ?answer si) (eq ?answer s))
         then TRUE 
         else FALSE))

(deffunction ask_number (?question)
	(printout t crlf "*****************************************************************************************************" crlf)
	(if (neq (length$ ?*spiegazione*) 0)
		then (format t (str-cat "%n" ?question " (help/perche): "))
		else (format t (str-cat "%n" ?question " (help): ")))
	(bind ?answer (read))

	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

	(while (not (numberp ?answer)) do  ;check if answer is a NUMBER
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "Non è presente alcun help!" crlf)
		  				else (format t (str-cat ?*help* "%n"))))

		(if (neq (length$ ?*spiegazione*) 0)
			then 
				(if (or (eq ?answer perche) (eq ?answer p))
					then (format t (str-cat ?*spiegazione* "%n"))))

		(if (neq (length$ ?*spiegazione*) 0)
			then (format t (str-cat "%n" ?question " (help/perche): "))
			else (format t (str-cat "%n" ?question " (help): ")))
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
		(if (numberp (fact-slot-value ?f valore)) ;controllo che il valore sia un numero o una stringa
			then (format t "(%d) %s: %d%n" ?i (fact-slot-value ?f nome) (fact-slot-value ?f valore))
			else (format t "(%d) %s: %s%n" ?i (fact-slot-value ?f nome) (fact-slot-value ?f valore)))
		(bind ?i (+ 1 ?i))))

(deffunction cambia_scelta_da_indice
	(?indice)
	(bind ?f (nth$ ?indice (get-all-facts-by-names car)))

	(bind ?fact-name (fact-slot-value ?f nome))
	(switch ?fact-name
		;----------fatti iniziali----------
		(case luogo
			then (do-for-all-facts ((?f1 car)) TRUE ;elimina tutti i fatti di tipo caratteristica
					(retract ?f1)))
		(case tipo_stanza
			then (do-for-all-facts ((?f1 car)) (neq ?f1:valore interno) ;elimina tutti i fatti tranne interno
					(retract ?f1)))
		(case presenza_pavimento
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza))) ;elimina tutti i fatti tranne interno/esterno e tipo_stanza
					(retract ?f1)))
		(case presenza_rivestimento
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza))) ;elimina tutti i fatti tranne interno/esterno e tipo_stanza
					(retract ?f1)))
		(case presenza_massetto
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza))) ;elimina tutti i fatti tranne interno/esterno e tipo_stanza
					(retract ?f1)))

		;------------massetto----------
		(case impianti_fatti
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento)))
					(retract ?f1)))
		(case massetto_fresco
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento)))
					(retract ?f1)))
		(case massetto_fragile
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome massetto_fresco)))
					(retract ?f1)))

		(case umidita_massetto
			then (do-for-all-facts ((?f1 car)) (or (eq ?f1:nome impianti_umidita) (eq ?f1:nome piano_terra))
				(retract ?f1)))

		(case massetto_a_livello
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile))) 
					(retract ?f1)))
		(case pavimento_da_raccordare
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello)))
					(retract ?f1)))
		(case altezza_massetto
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello) (eq ?f1:nome pavimento_da_raccordare)))
					(retract ?f1)))
		(case spessore_pavimento
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello) (eq ?f1:nome pavimento_da_raccordare)))
					(retract ?f1)))
		(case pendenza_massetto
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello)))
					(retract ?f1)))
		(case tipo_pavimento_da_porre
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello) (eq ?f1:nome pavimento_da_raccordare)
				(eq ?f1:nome spessore_pavimento) (eq ?f1:nome altezza_massetto) (eq ?f1:nome pendenza_massetto)))
					(retract ?f1)))
		(case muri_a_squadra
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello) (eq ?f1:nome pavimento_da_raccordare)
				(eq ?f1:nome spessore_pavimento) (eq ?f1:nome altezza_massetto) (eq ?f1:nome pendenza_massetto) (eq ?f1:nome tipo_pavimento_da_porre)))
					(retract ?f1)))

		;------------PAVIMENTO------------
		(case tipo_pavimento_presente
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome presenza_massetto) (eq ?f1:nome pavimento_livello) (eq ?f1:nome pendenza_pavimento) (eq ?f1:nome umidita_pavimento) (eq ?f1:nome impianti_umidita) (eq ?f1:nome piano_terra)))
				(retract ?f1)))
		(case umidita_pavimento
			then (do-for-all-facts ((?f1 car)) (or (eq ?f1:nome impianti_umidita) (eq ?f1:nome piano_terra))
				(retract ?f1)))

		;-----------RIVESTIMENTO-----------
		(case muri_a_piombo
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_rivestimento)))
				(retract ?f1)))

	 	(default (retract ?f))))

(deffunction gen-int-list
  (?max-n)
  (bind ?int-list (create$))
  (loop-for-count (?i 1 ?max-n)
		  (bind ?int-list (create$ ?int-list ?i)))
  ?int-list)

(deffunction chiedi_cambio_scelte_lavoro
	(?question)
	(printout t "Vuoi modificare qualche scelta?" crlf)
	(stampa_scelte_lavoro)

	(bind ?num_scelte (length$ (get-all-facts-by-names car)))
	(bind ?response (ask_question1 ?question (create$ (gen-int-list ?num_scelte) c t)))
	(printout t crlf crlf)
	(switch ?response 
		(case t 
			then (printout t crlf "Bye bye!" crlf)
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
	(printout t crlf "*****************************************************************************************************" crlf
					 "******************************************** EXPERT TILER *******************************************" crlf 
					 "*****************************************************************************************************" crlf crlf))

(defrule domanda_anni
	(not (domanda (valore one)))
	?f1 <- (esperienza (esperto ?val_esp) (principiante ?val_princ))
	?f2 <- (domande_poste (numero ?x))
	=>
	(bind ?*help* "Indica la tua età.")
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
	(bind ?*help* "Rispondi affermativamente se hai realizzato qualche volta un piccolo lavoro in casa o qualche tipo riparazione.")
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
	(bind ?*help* "Rispondi affermativamente se nella tua vita hai mai lavorato come piastrellista professionista.")
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
	(bind ?*help* "Rispondi affermativamente nel caso in cui hai già effettuato la posa di un pavimento o di un rivestimento.")
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
	(bind ?*help* "Rispondi affermativamente nel caso in cui tu abbia mai svolto nella tua vita un lavoro di tipo manuale (l'operaio ad esempio).")
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
	(preparazione_utente alta | bassa)
	?f1 <- (domande_poste (numero ?))
	?f2 <- (esperienza (esperto ?) (principiante ?))
	=>
	(do-for-all-facts ((?domanda domanda)) TRUE (retract ?domanda))  ;elimina tutti i fatti di tipo "domanda"
	(retract ?f1 ?f2)
	(printout t crlf)
	(set-strategy depth))


;  /---------------------------------------------------------------------------/
; /----------------------------------DOMANDE----------------------------------/
;/---------------------------------------------------------------------------/
(defrule domanda_interno_esterno
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome luogo) (valore ?)))
	=>
	(bind ?*help* "Interno indica una qualsiasi stanza che non sarà soggetta alle intemperie (bagno, cucina, stanza da letto, etc), %nesterno in caso contrario (balcone, terrazzo,etc).")
	(bind ?*spiegazione* "La situazione cambia a seconda che il luogo del lavoro sia interno o esterno.")
	(bind ?risposta (ask_question "La stanza riguarda l'interno o l'esterno?" interno esterno))
	(if (eq ?risposta interno)
		then (assert (car (nome luogo) (valore interno)))
		else (assert (car (nome luogo) (valore esterno)))))

(defrule domanda_tipo_stanza
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome tipo_stanza) (valore ?)))

	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "Indica a quale tipo tra quelli elencati corrisponde la stanza.")
	(bind ?*spiegazione* "Potrebbero esserci dei lavori specifici relativi alla particolare stanza.")
	(bind ?risposta (ask_question "Quale è il tipo di stanza?" bagno cucina altro))
	(assert (car (nome tipo_stanza) (valore ?risposta))))

(defrule domanda_inquadramento_situazione_rivestimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome presenza_pavimento) (valore ?)))
	(not (car (nome presenza_massetto) (valore ?)))
	(not (car (nome presenza_rivestimento) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore cucina))
		(car (nome tipo_stanza) (valore bagno)))
	=>
	(bind ?*help* "Indica se è presente già un pavimento (parquet, piastrelle, marmo, etc.)")
	(bind ?*spiegazione* "A seconda che il pavimento sia o meno presente potrebbero esserci dei lavori specifici da fare.")
	(bind ?risposta1 (yes_or_no_p "È già presente un pavimento?"))
	(if ?risposta1
		then (assert (car (nome presenza_pavimento) (valore si)))
		else (assert (car (nome presenza_pavimento) (valore no)))
			 (bind ?*help* "Indicare se è presente un fondo si sabbia e cemento al posto di piastrelle o altra pavimentazione.")
			 (bind ?*spiegazione* "Se il massetto non è presente occorrerà rifarlo.")
			 (bind ?risposta2 (yes_or_no_p "È presente un massetto?"))
			 (if ?risposta2
				 then (assert (car (nome presenza_massetto) (valore si)))
				 else (assert (car (nome presenza_massetto) (valore no)))))

	(bind ?*help* "Indica se è già presente un rivestimento (piastrelle o altro) sul muro.")
	(bind ?*spiegazione* "Se è presente e non è in buone condizioni si dovrà rimuoverlo.")
	(bind ?risposta3 (yes_or_no_p "È già presente un rivestimento?"))
	(if ?risposta3
		then (assert (car (nome presenza_rivestimento) (valore si)))
		else (assert (car (nome presenza_rivestimento) (valore no)))))

(defrule domanda_inquadramento_situazione_solo_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome presenza_pavimento) (valore ?)))
	(not (car (nome presenza_massetto) (valore ?)))

	(or (car (nome luogo) (valore esterno))
		(and (car (nome luogo) (valore interno))
			 (car (nome tipo_stanza) (valore altro))))
	=>
	(bind ?*help* "Indica se è presente già un pavimento (parquet, piastrelle, marmo, etc.)")
	(bind ?*spiegazione* "A seconda che il pavimento sia o meno presente potrebbero esserci dei lavori specifici da fare.")
	(bind ?risposta1 (yes_or_no_p "È già presente un pavimento?"))
	(if ?risposta1
		then (assert (car (nome presenza_pavimento) (valore si)))
		else (assert (car (nome presenza_pavimento) (valore no)))
			 (bind ?*help* "Indica se è presente un fondo si sabbia e cemento al posto di piastrelle o altra pavimentazione.")
			 (bind ?*spiegazione* "Se il massetto non è presente occorrerà rifarlo.")
			 (bind ?risposta2 (yes_or_no_p "È presente un massetto?"))
			 (if ?risposta2
				 then (assert (car (nome presenza_massetto) (valore si)))
				 else (assert (car (nome presenza_massetto) (valore no))))))



; /-----------------MASSETTO-----------------/

(defrule domanda_massetto_non_presente_impianti
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome impianti_fatti) (valore ?)))

	(car (nome presenza_massetto) (valore no))
	=>
	(bind ?*help* "Gli impianti riguardano quello idrico, elettrico, delle fogna, del riscaldamento, etc")
	(bind ?*spiegazione* "Se gli impianti non sono stati fatti, non si può continuare con la realizzazione del massetto.")
	(bind ?risposta (yes_or_no_p "Sono stati fatti tutti gli impianti sottotraccia?"))
	(if ?risposta
		then (assert (car (nome impianti_fatti) (valore si)))
		else (assert (car (nome impianti_fatti) (valore no)))))

(defrule domanda_massetto_fresco
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome massetto_fresco) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "Il massetto è fresco se non sono trascorsi ancora 10 giorni dalla sua realizzazione.")
	(bind ?*spiegazione* "Se il massetto è ancora fresco, non si può partire con la posa.")
	(bind ?risposta (yes_or_no_p "Il massetto è fresco (realizzato da poco o umido)?"))
	(if ?risposta
		then (assert (car (nome massetto_fresco) (valore si)))
		else (assert (car (nome massetto_fresco) (valore no)))))

(defrule umidita_massetto
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome umidita_massetto) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "Verificare se si notano elementi che farebbero pensare ad umidità.")
	(bind ?*spiegazione* "Se c'è umidità potrebbe esserci qualche impianto guasto o umidità che viene dal terreno.")
	(bind ?risposta (yes_or_no_p "C'è evidente umidità (il massetto ha un colore scuro?"))
	(if ?risposta 
		then (assert (car (nome umidita_massetto) (valore si)))
		else (assert (car (nome umidita_massetto) (valore no)))))

(defrule domanda_impianti_umidita_massetto
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome impianti_umidita) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome umidita_massetto) (valore si))
	=>
	(bind ?*help* "Controllare nei progetti della casa se nella stanza dove si sta lavorando passano degli impianti sotto il massetto o nei muri.")
	(bind ?*spiegazione* "Se passano dei tubi, potrebbe significare che qualcuno di essi è rotto e causa l'umidità.")
	(bind ?risposta (ask_question "Passano dei tubi di acqua, fognatura o riscaldamento sotto il massetto o nei muri?" si no non_so))
	(if (eq ?risposta no)
		then (assert (car (nome impianti_umidita) (valore no)))
		else (assert (car (nome impianti_umidita) (valore si)))))

(defrule domanda_piano_terra_massetto
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome piano_terra) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome umidita_massetto) (valore si))
	(car (nome impianti_umidita) (valore no))
	=>
	(bind ?*help* "Piano terra è quel piano posto direttamente sul terreno.")
	(bind ?*spiegazione* "Se ci si trova a piano terra l'umidità potrebbe venire direttamente dal terreno.")
	(bind ?risposta (yes_or_no_p "Ci si trova a piano terra?"))
	(if ?risposta
		then (assert (car (nome piano_terra) (valore si)))
		else (assert (car (nome piano_terra) (valore no)))))
;---

(defrule domanda_massetto_fragile
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome massetto_fragile) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	=>
	(bind ?*help* "Il massetto si sfarina al semplice calpestio o presenta zone in cui è sgretolato.")
	(bind ?*spiegazione* "Se il massetto è fragile occorre rifarlo.")
	(bind ?risposta (yes_or_no_p "Il massetto presenta punti in cui è polveroso o friabile?"))
	(if ?risposta
		then (assert (car (nome massetto_fragile) (valore si)))
		else (assert (car (nome massetto_fragile) (valore no)))))

(defrule domanda_massetto_a_livello_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome massetto_a_livello) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "Controlla con una stadia in alluminio e una livella poggiata sopra in vari punti della stanza in modo da coprire tutta l'area %ne verifica che la bolla nella livella sia sempre nella posizione centrale e che la stadia poggi perfettamente.")
	(bind ?*spiegazione* "Se il massetto non è a livello o è irregolare occorre rifarlo altrimenti non lo sarà nemmeno il pavimento che verrà posto sopra.")
	(bind ?risposta (yes_or_no_p "Il massetto è a livello e non è irregolare?"))
	(if ?risposta
		then (assert (car (nome massetto_a_livello) (valore si)))
		else (assert (car (nome massetto_a_livello) (valore no)))))

(defrule domanda_massetto_a_livello_non_esperto
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome massetto_a_livello) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "La stadia è un'asta in alluminio di diverse dimensioni (da 50 a più di 200 cm), usata per verificare i dislivelli, la livella è lo %nstrumento che consente di verificare se un piano è perfettamente orizzontale o verticale.")
	(bind ?*spiegazione* "Se il massetto non è a livello o è irregolare occorre rifarlo altrimenti non lo sarà nemmeno il pavimento che verrà posto sopra.")
	(bind ?risposta (yes_or_no_p "Prendi una stadia in alluminio e poggiala sul massetto, poni sopra essa una livella e verifica in vari punti della stanza, in modo %nda coprire tutta l'area, che la posizione della bolla sulla livella sia sempre centrale e che la stadia poggi perfettamente. %n%nIn tutte le misurazioni, la posizione della bolla è sempre centrale e la stadia poggia perfettamente?"))
	(if ?risposta
		then (assert (car (nome massetto_a_livello) (valore si)))
		else (assert (car (nome massetto_a_livello) (valore no)))))

(defrule domanda_pavimento_da_raccordare_massetto
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome pavimento_da_raccordare) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "Controlla se esiste nello stesso piano un'altra stanza in cui è già presente un qualsiasi tipo di pavimento.")
	(bind ?*spiegazione* "Se vi è un pavimento in un'altra stanza occorre raccordarsi ad esso, cioè il nuovo pavimento dovrà essere alla medesima altezza.")
	(bind ?risposta (yes_or_no_p "È presente una stanza adiacente a questa in cui è presente già un pavimento?"))
	(if ?risposta
		then (assert (car (nome pavimento_da_raccordare) (valore si)))
		else (assert (car (nome pavimento_da_raccordare) (valore no)))))

(defrule domanda_massetto_altezza
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome altezza_massetto) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore interno))
	(car (nome massetto_a_livello) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	=>
	(bind ?*help* "Indica se sai già il tipo di pavimento che si dovrà porre.")
	(bind ?*spiegazione* "In base allo spessore del pavimento da porre si può capire a che altezza deve essere il massetto per far in modo che il %npavimento, a fine opera, sia allo stesso livello del pavimento già presente in un'altra stanza.")
	(bind ?risposta1 (yes_or_no_p "Conosci già il tipo di pavimento che dovrai posare?"))
	(if ?risposta1
		then (bind ?*help* "Indica lo spessore del pavimento da porre in millimetri.")
			 (bind ?*spiegazione* "In base allo spessore del pavimento da porre si può capire a che altezza deve essere il massetto per far in modo che %nil pavimento, a fine opera, sia allo stesso livello del pavimento già presente in un'altra stanza.")
			 (bind ?risposta2 (ask_number "Indica il suo spessore in millimetri"))

			 (format t "%n*****************************************************************************************************%n")
			 (format t "%nIl massetto si dovrà trovare esattamente %d mm sotto il pavimento con cui raccordarsi.%n" (+ ?risposta2 3))
			 (bind ?*help* "Indica se il massetto presente, considerata l'aggiunta dello spessore del pavimento da porre, è più alto, più basso, o alla %naltezza giusta rispetto al pavimento già presente in un'altra stanza.")
			 (bind ?*spiegazione* "Se il massetto risultasse troppo alto o basso, dovrà essere rifatto, altrimenti si procederà con la posa.")
			 (bind ?altezza (ask_question "Com'è allora il pavimento presente?" alto basso giusto))
			 (assert (car (nome altezza_massetto) (valore ?altezza)))
		else (assert (car (nome spessore_pavimento) (valore no)))))

(defrule domanda_massetto_pendenza_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome pendenza_massetto) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore esterno))
	=>
	(bind ?*help* "Prendi una stadia in alluminio e ponila nella stessa direzione in cui dovrà essere lo scolo dell'acqua. All'estremità più bassa della %nstadia inserisci tra essa e il pavimento uno spessore di 1,5 cm e poni sopra di essa una livella verificando che la bolla sia in posizione %ncentrale e che la stadia poggi perfettamente. Ripeti l'operazione più volte per coprire tutta l'area.")
	(bind ?*spiegazione* "Se il massetto non ha la giusta pendenza, non agevola lo scolo dell'acqua causando anche problemi di umidità.")
	(bind ?risposta (yes_or_no_p "Il massetto ha la giusta pendenza di 1,5 cm ogni due metri lineari e non è irregolare?"))
	(if ?risposta
		then (assert (car (nome pendenza_massetto) (valore si)))
		else (assert (car (nome pendenza_massetto) (valore no)))))

(defrule domanda_massetto_pendenza_non_esperto
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome pendenza_massetto) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore esterno))
	=>
	(bind ?*help* "La stadia è un'asta in alluminio di diverse dimensioni (da 50 a più di 200 cm), usata per verificare i dislivelli, la livella è lo %nstrumento che consente di verificare che se un piano è perfettamente orizzontale o verticale.")
	(bind ?*spiegazione* "Se il massetto non ha la giusta pendenza o è irregolare, non agevola lo scolo dell'acqua causando anche problemi di umidità.")
	(bind ?risposta (yes_or_no_p "Prendi una stadia in alluminio e poggiala sul massetto nella direzione in cui dovrà fuoriuscire l'acqua. Verifica che %npoggi perfettamente. All'estremità più bassa della stadia, inserisci tra essa e il massetto uno spessore di 1,5 cm. %nPoni sopra la stadia una livella e verifica in vari punti della stanza, in modo da coprire tutta l'area, che la posizione %ndella bolla sulla livella sia sempre centrale e che la stadia poggi perfettamente. %n%nIn tutte le misurazioni, la posizione della bolla è sempre centrale e la stadia poggia perfettamente?"))
	(if ?risposta
		then (assert (car (nome pendenza_massetto) (valore si)))
		else (assert (car (nome pendenza_massetto) (valore no)))))

(defrule domanda_pavimento_interno
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome tipo_pavimento_da_porre) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_a_livello) (valore si))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore interno))
	(or (car (nome pavimento_da_raccordare) (valore no))
		(car (nome altezza_massetto) (valore giusto)))
	=>
	(bind ?*help* "Indica tra quelli disponibili il tipo di pavimento da porre sul massetto. Con piastrelle si intendono quelle di qualunque tipo (ceramica, %ngres, cotto, etc)")
	(bind ?*spiegazione* "In base al tipo di pavimento ci sono tipi di pose da consigliare.")
	(bind ?risposta (ask_question "Che tipo di pavimento intendi porre?" piastrella parquet marmo))
	(assert (car (nome tipo_pavimento_da_porre) (valore ?risposta))))

(defrule domanda_pavimento_esterno
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome tipo_pavimento_da_porre) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore esterno))
	(car (nome pendenza_massetto) (valore si))
	=>
	(bind ?*help* "Indica tra quelli disponibili il tipo di pavimento da porre sul massetto. Con piastrelle si intendono quelle di qualunque tipo (ceramica, %ngres, cotto, etc)")
	(bind ?*spiegazione* "In base al tipo di pavimento ci sono tipi di pose da consigliare.")
	(bind ?risposta (ask_question "Che tipo di pavimento intendi porre?" piastrella marmo))
	(assert (car (nome tipo_pavimento_da_porre) (valore ?risposta))))

(defrule domanda_muri_a_squadra
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome muri_a_squadra) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(or (car (nome altezza_massetto) (valore giusto))
		(car (nome pendenza_massetto) (valore si)))
	=>
	(bind ?*help* "Due muri sono a squadra se, ponendo la squadra (strumento ad angolo di 90°) negli angoli tra due muri, i due elementi di cui essa è %ncomposta poggiano perfettamente senza fare movimenti.")
	(bind ?*spiegazione* "Se i muri non sono a squadra ci sono alcuni tipi di pose da consigliare.")
	(bind ?risposta (yes_or_no_p "I muri sono a squadra?"))
	(if ?risposta
		then (assert (car (nome muri_a_squadra) (valore si)))
		else (assert (car (nome muri_a_squadra) (valore no)))))



; /-----------------PAVIMENTO-----------------/

(defrule domanda_tipo_pavimento_presente_interno
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome tipo_pavimento_presente) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Indicare quale tra le scelte presenti corrisponde al pavimento presente. Con piastrelle si intendono quelle di qualunque tipo (ceramica, %ngres, cotto, etc)")
	(bind ?*spiegazione* "In base al tipo di pavimento presente si può fare un lavoro piuttosto che un altro.")
	(bind ?risposta (ask_question "Quale tipo di pavimento è presente?" piastrelle marmo parquet))
	(assert (car (nome tipo_pavimento_presente) (valore ?risposta))))

(defrule domanda_tipo_pavimento_presente_esterno
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome tipo_pavimento_presente) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Indica quale tra le scelte presenti corrisponde al pavimento presente. Con piastrelle si intendono quelle di qualunque tipo (ceramica, %ngres, cotto, etc)")
	(bind ?*spiegazione* "In base al tipo di pavimento presente si può fare un lavoro piuttosto che un altro.")
	(bind ?risposta (ask_question "Quale tipo di pavimento è presente?" piastrelle marmo))
	(assert (car (nome tipo_pavimento_presente) (valore ?risposta))))

(defrule domanda_livello_pavimento_esistente_interno_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome pavimento_livello) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Controlla con una stadia in alluminio e una livella poggiata sopra in vari punti della stanza in modo da coprire tutta l'area %ne verifica che la bolla nella livella sia sempre nella posizione centrale e che la stadia poggi perfettamente.")
	(bind ?*spiegazione* "Se il pavimento non è a livello o è irregolare occorre rimuoverlo.")
	(bind ?risposta (yes_or_no_p "Il pavimento presente è a livello e non è irregolare?"))
	(if ?risposta
		then (assert (car (nome pavimento_livello) (valore si)))
		else (assert (car (nome pavimento_livello) (valore no)))))

(defrule domanda_livello_pavimento_esistente_interno_non_esperto
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome pavimento_livello) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "La stadia è un'asta in alluminio di diverse dimensioni (da 50 a più di 200 cm), usata per verificare i dislivelli, la livella è lo %nstrumento che consente di verificare se un piano è perfettamente orizzontale o verticale.")
	(bind ?*spiegazione* "Se il pavimento non è a livello o è irregolare occorre rimuoverlo.")
	(bind ?risposta (yes_or_no_p "Prendi una stadia in alluminio e poggiala sul pavimento, poni sopra essa una livella e verifica in vari punti della stanza, in modo %nda coprire tutta l'area, che la posizione della bolla sulla livella sia sempre centrale e che la stadia poggi perfettamente. %n%nIn tutte le misurazioni, la posizione della bolla è sempre centrale e la stadia poggia perfettamente?"))
	(if ?risposta
		then (assert (car (nome pavimento_livello) (valore si)))
		else (assert (car (nome pavimento_livello) (valore no)))))

(defrule domanda_pendenza_pavimento_esistente_esterno_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome pendenza_pavimento) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Prendi una stadia in alluminio e ponila nella stessa direzione dello scolo dell'acqua. All'estremità più bassa della stadia inserisci %ntra essa e il pavimento uno spessore di 1,5 cm e poni sopra di essa una livella verificando che la bolla sia in posizione %ncentrale e che la stadia poggi perfettamente. Ripeti l'operazione più volte per coprire tutta l'area.")
	(bind ?*spiegazione* "Se il pavimento non ha la giusta pendenza, non agevola lo scolo dell'acqua causando anche problemi di umidità.")
	(bind ?risposta (yes_or_no_p "Il pavimento ha la giusta pendenza di 1,5 cm ogni due metri lineari e non è irregolare?"))
	(if ?risposta
		then (assert (car (nome pendenza_pavimento) (valore si)))
		else (assert (car (nome pendenza_pavimento) (valore no)))))

(defrule domanda_pendenza_pavimento_esistente_esterno_non_esperto
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome pendenza_pavimento) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "La stadia è un'asta in alluminio di diverse dimensioni (da 50 a più di 200 cm), usata per verificare i dislivelli, la livella è lo %nstrumento che consente di verificare che se un piano è perfettamente orizzontale o verticale.")
	(bind ?*spiegazione* "Se il pavimento non ha la giusta pendenza o è irregolare, non agevola lo scolo dell'acqua causando anche problemi di umidità.")
	(bind ?risposta (yes_or_no_p "Prendi una stadia in alluminio e poggiala sul pavimento nella direzione in cui dovrà fuoriuscire l'acqua. Verifica che %npoggi perfettamente. All'estremità più bassa della stadia, inserisci tra essa e il pavimento uno spessore di 1,5 cm. %nPoni sopra la stadia una livella e verifica in vari punti della stanza, in modo da coprire tutta l'area, che la posizione %ndella bolla sulla livella sia sempre centrale e che la stadia poggi perfettamente. %n%nIn tutte le misurazioni, la posizione della bolla è sempre centrale e la stadia poggia perfettamente?"))
	(if ?risposta
		then (assert (car (nome pendenza_pavimento) (valore si)))
		else (assert (car (nome pendenza_pavimento) (valore no)))))

(defrule domanda_pezzi_sollevati_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome pezzi_sollevati_pavimento) (valore ?)))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Per pezzi sollevati si intendono pezzi di pavimento che sono alzati o che non aderiscono più.")
	(bind ?*spiegazione* "Se ci sono pezzi sollevati occorre rimuovere il pavimento.")
	(bind ?risposta (yes_or_no_p "Ci sono pezzi sollevati nel pavimento?"))
	(if ?risposta
		then (assert (car (nome pezzi_sollevati_pavimento) (valore si)))
		else (assert (car (nome pezzi_sollevati_pavimento) (valore no)))))

(defrule domanda_pezzi_rovinati_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome pezzi_rovinati_pavimento) (valore ?)))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Verificare che ci siano pezzi con scheggiature (ammaccature) o lesionati.")
	(bind ?*spiegazione* "Se ci sono pezzi scheggiati o rotti occorre valutare se sostituirli o rimuovere tutto il pavimento.")
	(bind ?risposta1 (yes_or_no_p "Ci sono pezzi scheggiati o lesionati nel pavimento?"))
	(if ?risposta1
		then (bind ?*help* "Indicare se ci sono più di due o tre pezzi rovinati")
			 (bind ?*spiegazione* "Se ci sono molti pezzi rovinati occorre rimuovere tutto il pavimento.")
			 (bind ?risposta2 (yes_or_no_p "Nel pavimento ci sono più di un paio di pezzi scheggiati o lesionati?"))
			 (if ?risposta2
				 then (assert (car (nome pezzi_rovinati_pavimento) (valore molti)))
				 else (assert (car (nome pezzi_rovinati_pavimento) (valore pochi))))
		else (assert (car (nome pezzi_rovinati_pavimento) (valore no)))))

(defrule umidita_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome umidita_pavimento) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Verificare se si notano elementi che farebbero pensare ad umidità.")
	(bind ?*spiegazione* "Se c'è umidità potrebbe esserci qualche impianto guasto o umidità che viene dal terreno.")
	(bind ?risposta (yes_or_no_p "C'è evidente umidità? (Fughe di colore più scuro, le piastrelle non aderiscono, moquette bagnata)"))
	(if ?risposta 
		then (assert (car (nome umidita_pavimento) (valore si)))
		else (assert (car (nome umidita_pavimento) (valore no)))))

(defrule domanda_impianti_umidita
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome impianti_umidita) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome umidita_pavimento) (valore si))
	=>
	(bind ?*help* "Controllare nei progetti della casa se nella stanza dove si sta lavorando passano degli impianti sotto il pavimento o nei muri.")
	(bind ?*spiegazione* "Se passano dei tubi, potrebbe significare che qualcuno di essi è rotto e causa l'umidità.")
	(bind ?risposta (ask_question "Passano dei tubi di acqua, fognatura o riscaldamento sotto il pavimento o nei muri?" si no non_so))
	(if (eq ?risposta no)
		then (assert (car (nome impianti_umidita) (valore no)))
		else (assert (car (nome impianti_umidita) (valore si)))))

(defrule domanda_piano_terra
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome piano_terra) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome umidita_pavimento) (valore si))
	(car (nome impianti_umidita) (valore no))
	=>
	(bind ?*help* "Piano terra è quel piano posto direttamente sul terreno.")
	(bind ?*spiegazione* "Se ci si trova a piano terra l'umidità potrebbe venire direttamente dal terreno.")
	(bind ?risposta (yes_or_no_p "Ci si trova a piano terra?"))
	(if ?risposta
		then (assert (car (nome piano_terra) (valore si)))
		else (assert (car (nome piano_terra) (valore no)))))

(defrule domanda_rumore_calpestio_piastrelle_marmo
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome rumore_calpestio) (valore ?)))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	=>
	(bind ?*help* "Il rumore deve essere sentito nel momento in cui si cammina sul pavimento.")
	(bind ?*spiegazione* "Se si sente rumore il pavimento non è stato fatto bene e occorre fare una insonorizzazione.")
	(bind ?risposta (yes_or_no_p "Si sente rumore che si propaga alle pareti al calpestio?"))
	(if ?risposta
		then (assert (car (nome rumore_al_calpestio) (valore si)))
		else (assert (car (nome rumore_al_calpestio) (valore no)))))

(defrule domanda_fughe_polvere
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome polvere_sulle_fughe) (valore ?)))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	=>
	(bind ?*help* "")
	(bind ?*spiegazione* "Se c'è polvere vuol dire che le fughe sono consumate e occorre rinforzarle.")
	(bind ?risposta (yes_or_no_p "C'è polvere sulle fughe del pavimento?"))
	(if ?risposta
		then (assert (car (nome polvere_sulle_fughe) (valore si)))
		else (assert (car (nome polvere_sulle_fughe) (valore no)))))



; /-----------------RIVESTIMENTO-----------------/

(defrule domanda_muri_a_piombo_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome muri_a_piombo) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore no))
	=>
	(bind ?*help* "Prendere una livella e controllare poggiandola al muro che la posizione della bolla sia centrale. Ripetere l'operazione più volte")
	(bind ?*spiegazione* "Se i muri non sono a piombo occorre rifarli per garantire un risultato migliore con la posa del rivestimento.")
	(bind ?risposta (yes_or_no_p "I muri sono a piombo?"))
	(if ?risposta
		then (assert (car (nome muri_a_piombo) (valore si)))
		else (assert (car (nome muri_a_piombo) (valore no)))))

(defrule domanda_muri_a_piombo_non_esperto
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome muri_a_piombo) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore no))
	=>
	(bind ?*help* "La livella è lo strumento che consente di verificare che se un piano è perfettamente orizzontale o verticale.")
	(bind ?*spiegazione* "Se i muri non sono a piombo occorre rifarli per garantire un risultato migliore con la posa del rivestimento.")
	(bind ?risposta (yes_or_no_p "Prendere una livella e controllare poggiandola al muro che la posizione della bolla sia centrale. Ripetere l'operazione più volte %nper coprire diversi punti in tutti i muri. %n%nLa posizione della bolla nella livella è sempre centrale?"))
	(if ?risposta
		then (assert (car (nome muri_a_piombo) (valore si)))
		else (assert (car (nome muri_a_piombo) (valore no)))))

(defrule domanda_fondo_muri
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome sottofondo_muri) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore no))
	(car (nome muri_a_piombo) (valore si))
	=>
	(bind ?*help* "Indica il tipo di fondo tra quelli presenti.")
	(bind ?*spiegazione* "In base al fondo presente si potrebbero dover fare dei lavori prima di porre il rivestimento.")
	(bind ?risposta (ask_question "Come è realizzato il fondo?" gesso_rasato muro_pitturato sabbia_e_cemento))
	(assert (car (nome sottofondo_muri) (valore ?risposta))))

(defrule domanda_rivestimento_a_piombo_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome rivestimento_a_piombo) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	=>
	(bind ?*help* "Prendi una livella e controlla poggiandola al muro che la posizione della bolla sia centrale. Ripeti l'operazione più volte")
	(bind ?*spiegazione* "Se i muri sono a piombo e le piastrelle sono in buone condizioni si potrebbe anche optare solo per un restyling del rivestimento.")
	(bind ?risposta (yes_or_no_p "Il rivestimento presente è a piombo?"))
	(if ?risposta
		then (assert (car (nome rivestimento_a_piombo) (valore si)))
		else (assert (car (nome rivestimento_a_piombo) (valore no)))))

(defrule domanda_rivestimento_a_piombo_non_esperto
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome rivestimento_a_piombo) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	=>
	(bind ?*help* "La livella è lo strumento che consente di verificare che se un piano è perfettamente orizzontale o verticale.")
	(bind ?*spiegazione* "Se i muri sono a piombo e le piastrelle sono in buone condizioni si potrebbe anche optare solo per un restyling del rivestimento.")
	(bind ?risposta (yes_or_no_p "Prendere una livella e controllare poggiandola al muro che la posizione della bolla sia centrale. Ripetere l'operazione più volte %nper coprire diversi punti in tutti i muri. %n%nLa posizione della bolla nella livella è sempre centrale?"))
	(if ?risposta
		then (assert (car (nome rivestimento_a_piombo) (valore si)))
		else (assert (car (nome rivestimento_a_piombo) (valore no)))))

(defrule domanda_pezzi_sollevati_rivestimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome pezzi_sollevati_rivestimento) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	=>
	(bind ?*help* "Per pezzo sollevato si intende un pezzo non aderente o alzato.")
	(bind ?*spiegazione* "Se ci sono pezzi sollevati devi rimuovere il rivestimento.")
	(bind ?risposta (yes_or_no_p "Ci sono pezzi sollevati nel rivestimento?"))
	(if ?risposta
		then (assert (car (nome pezzi_sollevati_rivestimento) (valore si)))
		else (assert (car (nome pezzi_sollevati_rivestimento) (valore no)))))

(defrule domanda_pezzi_rovinati_rivestimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome pezzi_rovinati_rivestimento) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	=>
	(bind ?*help* "Per pezzo rovinato si intende un pezzo lesionato o scheggiato.")
	(bind ?*spiegazione* "Se ci sono pezzi rovinati si deve decidere se sostituirli o rimuovere tutto il rivestimento.")
	(bind ?risposta1 (yes_or_no_p "Ci sono pezzi scheggiati nel rivestimento?"))
	(if ?risposta1
		then (bind ?*help* "Verifica se ci sono più di due o tre pezzi rovinati.")
			 (bind ?*spiegazione* "Se ci sono troppi pezzi rovinati occorre rimuovere il rivestimento.")
			 (bind ?risposta2 (yes_or_no_p "Ci sono più di un paio di pezzi scheggiati o lesionati?"))
			 (if ?risposta2
				 then (assert (car (nome pezzi_rovinati_rivestimento) (valore molti)))
				 else (assert (car (nome pezzi_rovinati_rivestimento) (valore pochi))))
		else (assert (car (nome pezzi_rovinati_rivestimento) (valore no)))))



;  /----------------------------------------/
; /-----------------LAVORI-----------------/
;/----------------------------------------/



;------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------

(defrule lavoro_trovato
	(declare (salience ?*high_priority*))
	(lavoro)
	(not (rivestimento_parte_due))
	=>
	(printout t crlf "*****************************************************************************************************" crlf)
	(format t (str-cat "%n>>>SOLUZIONE:%n" ?*soluzione* "%n"))
	(if (neq (length$ ?*spiegazione*) 0)
		then (format t (str-cat "%n>>>SPIEGAZIONE:%n" ?*spiegazione* "%n")))


	(printout t crlf "Digita il numero corrispondente alla scelta:" crlf
				"(1) Rivedi scelte o modifica" crlf
				"(2) Termina" crlf crlf)

	(printout t "Scelta: ")
	(bind ?risposta (read))
	(while (or (< ?risposta 1) (> ?risposta 2))
		(printout t "Risposta: ")
		(bind ?risposta (read)))

	(switch ?risposta
		(case 1 then (assert (rivedi_scelte_lavoro)))
		(case 2 then (halt))))

(defrule lavoro_trovato_rivestimento
	(declare (salience ?*high_priority*))
	?f1 <- (lavoro)
	?f2 <- (rivestimento_parte_due)
	=>
	(printout t crlf "*****************************************************************************************************" crlf)
	(format t (str-cat "%n>>>SOLUZIONE:%n" ?*soluzione* "%n"))
	(if (neq (length$ ?*spiegazione*) 0)
		then (format t (str-cat "%n>>>SPIEGAZIONE:%n" ?*spiegazione* "%n")))


	(printout t crlf "Digita il numero corrispondente alla scelta:" crlf
				"(1) Continua con seconda parte (pavimento)" crlf
				"(2) Rivedi scelte o modifica" crlf 
				"(3) Termina"crlf)

	(printout t "Scelta: ")
	(bind ?risposta (read))
	(while (or (< ?risposta 1) (> ?risposta 3))
		(printout t "Risposta: ")
		(bind ?risposta (read)))

	(switch ?risposta
		(case 1 then 
			(do-for-all-facts ((?f car)) (not (or (eq ?f:nome presenza_pavimento) (eq ?f:nome presenza_massetto) (eq ?f:nome luogo) (eq ?f:nome tipo_stanza))) 
				(retract ?f))
			(retract ?f1 ?f2))
		(case 2 then (assert (rivedi_scelte_lavoro)))
		(case 3 then (halt))))

(defrule lavoro_non_trovato
	(declare (salience ?*lowest_priority*))
	(not (lavoro))
	=>
	(printout t crlf "Lavoro non trovato!" crlf)
	(if (yes_or_no_p "Vuoi rivedere le scelte fatte o cambiare qualcosa?")
		then (assert (rivedi_scelte_lavoro))
		else (printout t crlf "Prova a riavviare il sistema" crlf) 
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

