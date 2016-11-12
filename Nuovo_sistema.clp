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
	(printout t crlf "------------------------------------------------------------------------------------------------------" crlf) 
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
	(printout t crlf "------------------------------------------------------------------------------------------------------" crlf)
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
	(printout t crlf "------------------------------------------------------------------------------------------------------" crlf)
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
		(case luogo
			then (progn$ (?f1 (get-all-facts-by-names car)) ;elimina tutti i fatti di tipo caratteristica
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
		(case umidita
			then (do-for-all-facts ((?f1 car)) (neq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) ;elimina tutti i fatti tranne interno
					(retract ?f1)))
		(case impianti_umidita
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome umidita))) ;elimina tutti i fatti tranne interno e umidità e tipo_stanza
					(retract ?f1)))
		(case rifacimento_impianti
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_pavimento))) ;elimina tutti i fatti tranne interno/esterno e tipo_stanza
					(retract ?f)))

		(case massetto_friabile
			then (retract ?f))
		(case pavimento_da_raccordare
			then (do-for-all-facts ((?f1 car)) (eq ?f1:nome massetto_altezza)
					(retract ?f1)))
		(case massetto_a_livello
			then (do-for-all-facts ((?f1 car)) (eq ?f1:nome massetto_altezza)
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
	(bind ?*help* "Interno riguarda una qualsiasi stanza che non sarà soggetta alle intemperie (bagno, cucina, stanza da letto, etc), Esterno %nin caso contrario (balcone, terrazzo,etc).")
	(bind ?*spiegazione* "")
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
	(bind ?*help* "Indicare a quale tipo tra quelli elencati corrisponde la stanza in cui deve essere fatto il lavoro.")
	(bind ?*spiegazione* "Potrebbero esserci dei problemi specifici relativi al diverso luogo in cui si trova il pavimento.")
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
	(bind ?risposta1 (yes_or_no_p "È già presente un pavimento?"))
	(if ?risposta1
		then (assert (car (nome presenza_pavimento) (valore si)))
		else (assert (car (nome presenza_pavimento) (valore no)))
			 (bind ?risposta2 (yes_or_no_p "È presente un massetto?"))
			 (if ?risposta2
				 then (assert (car (nome presenza_massetto) (valore si)))
				 else (assert (car (nome presenza_massetto) (valore no)))))

	(bind ?risposta3 (yes_or_no_p "È già presente un rivestimento?"))
	(if ?risposta3
		then (assert (car (nome presenza_rivestimento) (valore si)))
		else (assert (car (nome presenza_rivestimento) (valore no)))))

(defrule domanda_inquadramento_situazione_solo_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome presenza_pavimento) (valore ?)))
	(not (car (nome presenza_massetto) (valore ?)))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(not (or (car (nome tipo_stanza) (valore cucina))
		 	 (car (nome tipo_stanza) (valore bagno))))
	=>
	(bind ?risposta1 (yes_or_no_p "È già presente un pavimento?"))
	(if ?risposta1
		then (assert (car (nome presenza_pavimento) (valore si)))
		else (assert (car (nome presenza_pavimento) (valore no)))
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
	(bind ?risposta (yes_or_no_p "Il massetto è fresco (realizzato da poco)?"))
	(if ?risposta
		then (assert (car (nome massetto_fresco) (valore si)))
		else (assert (car (nome massetto_fresco) (valore no)))))

(defrule domanda_massetto_fragile
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome massetto_fragile) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	=>
	(bind ?risposta (yes_or_no_p "Il massetto presenta punti in cui è polveroso o friabile?"))
	(if ?risposta
		then (assert (car (nome massetto_fragile) (valore si)))
		else (assert (car (nome massetto_fragile) (valore no)))))

(defrule domanda_massetto_a_livello
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome massetto_a_livello) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore interno))
	=>
	(bind ?risposta (yes_or_no_p "Il massetto è a livello?"))
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
	(bind ?risposta (yes_or_no_p "È presente una stanza adiacente a questa in si cui intende lavorare in cui è presente già un pavimento?"))
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
	(bind ?risposta1 (yes_or_no_p "Conosci già il tipo di pavimento che dovrai posare?"))
	(if ?risposta1
		then (bind ?risposta2 (ask_number "Indica il suo spessore in millimetri"))
			 (format t "%n------------------------------------------------------------------------------------------------------")
			 (format t "%nIl massetto si dovrà trovare esattamente %d mm sotto il pavimento con cui raccordarsi.%n" (+ ?risposta2 3))
			 (bind ?altezza (ask_question "Com'è allora il pavimento presente?" alto basso giusto))
			 (assert (car (nome altezza_massetto) (valore ?altezza)))
		else (assert (car (nome spessore_pavimento) (valore no)))))

(defrule domanda_massetto_pendenza
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome pendenza_massetto) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore esterno))
	=>
	(bind ?risposta (yes_or_no_p "Il massetto ha la giusta pendenza di 1,5 cm ogni due metri lineari?"))
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
	(bind ?risposta (ask_question "Che tipo di pavimento intendi porre?" piastrella parquet))
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
	(bind ?risposta (ask_question "Quale tipo di pavimento è presente?" piastrelle marmo))
	(assert (car (nome tipo_pavimento_presente) (valore ?risposta))))

(defrule domanda_tipo_pavimento_presente_esterno
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome tipo_pavimento_presente) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?risposta (ask_question "Quale tipo di pavimento è presente?" piastrelle marmo))
	(assert (car (nome tipo_pavimento_presente) (valore ?risposta))))

(defrule domanda_livello_pavimento_esistente_interno
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome pavimento_livello) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?risposta (yes_or_no_p "Il pavimento presente è a livello?"))
	(if ?risposta
		then (assert (car (nome pavimento_livello) (valore si)))
		else (assert (car (nome pavimento_livello) (valore no)))))

(defrule domanda_pendenza_pavimento_esistente_esterno
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome pendenza_pavimento) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?risposta (yes_or_no_p "Il pavimento ha la giusta pendenza di 1,5 cm ogni due metri lineari?"))
	(if ?risposta
		then (assert (car (nome pendenza_pavimento) (valore si)))
		else (assert (car (nome pendenza_pavimento) (valore no)))))

(defrule domanda_piastrelle_sollevate_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome piastrelle_sollevate_pavimento) (valore ?)))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	=>
	(bind ?risposta (yes_or_no_p "Ci sono piastrelle sollevate nel pavimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_sollevate_pavimento) (valore si)))
		else (assert (car (nome piastrelle_sollevate_pavimento) (valore no)))))

(defrule domanda_piastrelle_scheggiate_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome piastrelle_scheggiate_rotte_pavimento) (valore ?)))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	=>
	(bind ?risposta1 (yes_or_no_p "Ci sono piastrelle scheggiate o rotte nel pavimento?"))
	(if ?risposta1
		then (bind ?risposta2 (yes_or_no_p "Ci sono più di un paio di piastrelle scheggiate o rotte?"))
			 (if ?risposta2
				 then (assert (car (nome piastrelle_scheggiate_rotte_pavimento) (valore molte)))
				 else (assert (car (nome piastrelle_scheggiate_rotte_pavimento) (valore poche))))
		else (assert (car (nome piastrelle_scheggiate_rotte_pavimento) (valore no)))))

(defrule umidita_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome umidita_pavimento) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	=>
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
	(bind ?risposta (yes_or_no_p "C'è polvere sulle fughe del pavimento?"))
	(if ?risposta
		then (assert (car (nome polvere_sulle_fughe) (valore si)))
		else (assert (car (nome polvere_sulle_fughe) (valore no)))))



; /-----------------RIVESTIMENTO-----------------/

(defrule domanda_muri_a_piombo
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome muri_a_piombo) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore no))
	=>
	(bind ?risposta (yes_or_no_p "I muri sono a piombo?"))
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
	(bind ?risposta (ask_question "Come è realizzato il fondo" gesso_rasato muro_pitturato sabbia_e_cemento))
	(assert (car (nome sottofondo_muri) (valore ?risposta))))

(defrule domanda_piastrelle_sollevate_rivestimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome piastrelle_sollevate_rivestimento) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	=>
	(bind ?risposta (yes_or_no_p "Ci sono piastrelle sollevate nel rivestimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_sollevate_rivestimento) (valore si)))
		else (assert (car (nome piastrelle_sollevate_rivestimento) (valore no)))))

(defrule domanda_piastrelle_scheggiate_rivestimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome piastrelle_scheggiate_rivestimento) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	=>
	(bind ?risposta1 (yes_or_no_p "Ci sono piastrelle scheggiate nel rivestimento?"))
	(if ?risposta1
		then (bind ?risposta2 (yes_or_no_p "Ci sono più di un paio di piastrelle scheggiate?"))
			 (if ?risposta2
				 then (assert (car (nome piastrelle_scheggiate_rivestimento) (valore molte)))
				 else (assert (car (nome piastrelle_scheggiate_rivestimento) (valore poche))))
		else (assert (car (nome piastrelle_scheggiate_rivestimento) (valore no)))))



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
	(printout t crlf "------------------------------------------------------------------------------------------------------" crlf)
	(format t (str-cat "%n>>>SOLUZIONE:%n" ?*soluzione* "%n"))
	(if (neq (length$ ?*spiegazione*) 0)
		then (format t (str-cat "%n>>>SPIEGAZIONE:%n" ?*spiegazione* "%n")))
	(if (neq (length$ ?*help*) 0) 
		then (format t (str-cat "%n>>>AIUTO:%n" ?*help* "%n")))


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
	?l <- (lavoro)
	(rivestimento_parte_due)
	=>
	(printout t crlf "------------------------------------------------------------------------------------------------------" crlf)
	(format t (str-cat "%n>>>SOLUZIONE:%n" ?*soluzione* "%n"))
	(if (neq (length$ ?*spiegazione*) 0)
		then (format t (str-cat "%n>>>SPIEGAZIONE:%n" ?*spiegazione* "%n")))
	(if (neq (length$ ?*help*) 0) 
		then (format t (str-cat "%n>>>AIUTO:%n" ?*help* "%n")))


	(printout t crlf "Digita il numero corrispondente alla scelta:" crlf
				"(1) Continua con parte del pavimento" crlf
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
			(retract ?l))
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

