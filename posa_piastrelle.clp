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
		(case umidita
			then (do-for-all-facts ((?f1 car)) (neq ?f1:nome luogo) ;elimina tutti i fatti tranne interno
					(retract ?f1)))
		(case impianti_umidita
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome umidita))) ;elimina tutti i fatti tranne interno/esterno e umidità
					(retract ?f1)))
		(case rifacimento_impianti
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_pavimento))) ;elimina tutti i fatti tranne interno/esterno e tipo_stanza
					(retract ?f)))
))

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
	(printout t crlf crlf)
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

(defrule domanda_presenza_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome presenza_pavimento) (valore ?)))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	=>
	(bind ?*help* "Rispondere 'si' se è già presente un pavimento nella stanza in cui si intende lavorare, 'no' altrimenti.")
	(bind ?*spiegazione* "In base al fatto che sia presente o no il pavimento si dovranno realizzare lavori diversi.")
	(bind ?risposta (yes_or_no_p "È già presente un pavimento?"))
	(if ?risposta
		then (assert (car (nome presenza_pavimento) (valore si)))
		else (assert (car (nome presenza_pavimento) (valore no)))))

(defrule domanda_presenza_rivestimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome presenza_rivestimento) (valore ?)))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	=>
	(bind ?*help* "Rispondere 'si' se è già presente un rivestimento, cioè le pareti della stanza sono ricoperte con piastrelle, 'no' altrimenti.")
	(bind ?*spiegazione* "In base al fatto che sia presente o no il rivestimento si dovranno realizzare lavori diversi.")
	(bind ?risposta (yes_or_no_p "È già presente un rivestimento?"))
	(if ?risposta
		then (assert (car (nome presenza_rivestimento) (valore si)))
		else (assert (car (nome presenza_rivestimento) (valore no)))))

(defrule domanda_presenza_massetto
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome presenza_massetto) (valore ?)))

	(car (nome presenza_pavimento) (valore no))
	=>
	(bind ?*help* "Il massetto è quello strato di cemento e sabbia che deve essere perfettamente a livello e la cui presenza è fondamentale perché %nsopra di esso verrà posto il pavimento vero e proprio.")
	(bind ?*spiegazione* "In base al fatto che sia presente o no il massetto si dovranno realizzare lavori diversi.")
	(bind ?risposta (yes_or_no_p "È presente un massetto?"))
	(if ?risposta
		then (assert (car (nome presenza_massetto) (valore si)))
		else (assert (car (nome presenza_massetto) (valore no)))))

(defrule domanda_rifacimento_impianti
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome rifacimento_impianti) (valore ?)))

	=>
	(bind ?*help* "Rispondi 'si' se si deve rifare qualche impianto che ha a che fare con il pavimento o o i muri.")
	(bind ?*spiegazione* "Nel caso in cui si dovrà rifare l'impianto, si dovrà rimuovere il pavimento o il rivestimento esistente.")
	(bind ?risposta (yes_or_no_p "Devi rifare qualche impianto che passa per il pavimento o i muri (idrico, termosifoni, fognatura, etc.)?"))
	(if ?risposta
		then (assert (car (nome rifacimento_impianti) (valore si)))
		else (assert (car (nome rifacimento_impianti) (valore no)))))

(defrule domanda_umidita
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome umidita) (valore ?)))

	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "Rispondere 'si' se sono presenti macchie di umidità sui muri o sulle fughe tra le piastrelle.")
	(bind ?*spiegazione* "Se c'è umidità allora bisogna eliminare tale problema, altrimenti la posa delle piastrelle potrebbe non essere duratura.")
	(bind ?risposta (yes_or_no_p "Hai problemi di umidità?"))
	(if ?risposta
		then (assert (car (nome umidita) (valore si)))
		else (assert (car (nome umidita) (valore no)))))

(defrule domanda_impianti_umidita
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome impianti_umidita) (valore ?)))

	(car (nome umidita) (valore si))
	=>
	(bind ?*help* "Controllare nei progetti della casa se nella stanza dove si sta lavorando passano degli impianti sotto il pavimento o nei muri.")
	(bind ?*spiegazione* "Se passano dei tubi, potrebbe significare che qualcuno di essi è rotto e causa l'umidità.")
	(bind ?risposta (ask_question "Passano dei tubi di acqua, fognatura o riscaldamento sotto il pavimento o nei muri?" si no non_so))
	(if (eq ?risposta no)
		then (assert (car (nome impianti_umidita) (valore no)))
		else (assert (car (nome impianti_umidita) (valore si)))))

; /-----------------MASSETTO-----------------/
(defrule domanda_massetto_friabile
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome massetto_friabile) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "Il massetto non è consistente se presenta una superficie polverosa e si sgretola facilmente.")
	(bind ?*spiegazione* "Se non è consistente occorre rimuoverlo prima di andare avanti.")
	(bind ?risposta (yes_or_no_p "Il massetto non è molto consistente?"))
	(if ?risposta 
		then (assert (car (nome massetto_friabile) (valore si)))
		else (assert (car (nome massetto_friabile) (valore no)))))

(defrule domanda_pavimento_raccordo
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome pavimento_da_raccordare) (valore ?)))

	(or (car (nome presenza_massetto) (valore si))
		(car (nome presenza_massetto) (valore no)))
	=>
	(bind ?*help* "Rispondere 'si' se ci sono dei pavimenti già posati.")
	(bind ?*spiegazione* "Se ci sono pavimenti già posati si dovrà raccordare il pavimento esistente con questi.")
	(bind ?raccordo_pavimento (yes_or_no_p "Esistono dei pavimenti nello stesso piano?"))

	(bind ?*help* "Rispondere 'si' se ci sono porte o balconi nella stanza in cui si deve lavorare.")
	(bind ?*spiegazione* "Se sono presenti vuol dire che il pavimento dovrà essere raccordato con essi.")
	(bind ?raccordo_porte (yes_or_no_p "Esistono delle porte già montate o balconi nella stanza?"))
	(if (or ?raccordo_pavimento ?raccordo_porte)
		then (assert (car (nome pavimento_da_raccordare) (valore si)))
		else (assert (car (nome pavimento_da_raccordare) (valore no)))))

(defrule domanda_livello_massetto_interno_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome massetto_livello) (valore ?)))
	
	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "Poni sul massetto una stadia della dimensione adeguata alla dimensione della stanza e adagia su di essa una livella per controllare che la bolla %nsia in posizione centrale. Ripeti poi l'operazione diverse volte per controllare tutta l'area.")
	(bind ?*spiegazione* "Se il massetto non fosse a livello occorre rifarlo.")
	(bind ?risposta (yes_or_no_p "Il massetto è a livello?"))
	(if ?risposta
		then (assert (car (nome massetto_livello) (valore si)))
		else (assert (car (nome massetto_livello) (valore no)))))

(defrule domanda_livello_massetto_interno_principiante
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome massetto_livello) (valore ?)))
	
	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "La livella è lo strumento che permette di verificare che una superficie sia a livello, la stadia è un'asta di alluminio con dimensione da 50 a 200 cm.")
	(bind ?*spiegazione* "Se il massetto non fosse a livello occorre rifarlo.")
	(bind ?risposta (yes_or_no_p "Prendi una stadia di dimensione adeguata a quella della stanza e ponila sul massetto, adagia sopra essa una livella %ne verifica che la bolla presente sia esattamente in posizione centrale. Ripeti l'operazione diverse volte in diversi punti della stanza. %nLa bolla nella livella è sempre in posizione centrale?"))
	(if ?risposta
		then (assert (car (nome massetto_livello) (valore si)))
		else (assert (car (nome massetto_livello) (valore no)))))

(defrule domanda_altezza_massetto
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome massetto_altezza) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome massetto_livello) (valore si))
	=>
	(bind ?*help* "Misura lo spessore della piastrella o del pavimento che verrà posto.")
	(bind ?*spiegazione* "Serve per capire quale dovrà essere l'altezza del massetto.")
	(bind ?risposta (ask_number "Indica lo spessore della piastrella o del pavimento da porre in millimetri (-1 se non lo conosci)"))
	(if (= ?risposta -1)
		then (printout t crlf "Se non si conosce lo spessore della piastrella, allora non si può controllare se il massetto è al giusto livello!" crlf)
			 (halt)
		else (format t "%nConsidera che lo spessore complessivo del pavimento sarà di %d mm%n" (+ 3 ?risposta))
			 (bind ?*help* "Misura la differenza di altezza tra il massetto presente e le porte o un altro pavimento presente.")
			 (bind ?*spiegazione* "Serve per capire se il massetto va bene o deve essere sostituito.")
			 (bind ?risposta (ask_question "Considerando lo spessore indicato, il massetto è alto, basso o alla dimensione giusta?" alto basso giusto))
			 (assert (car (nome massetto_altezza) (valore ?risposta)))))

(defrule domanda_pendenza_massetto_esterno_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome massetto_pendenza) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "Poni una stadia di 2 metri sul massetto verso il punto in cui dovrà uscire l'acqua e poni uno spessore di 1,5 cm all'estremità più %nbassa della stadia. Adagia sulla stadia una livella controllando che la bolla sia precisamente nella posizione centrale.")
	(bind ?*spiegazione* "Questo abbassamento del massetto garantisce la pendenza giusta per lo scolo dell'acqua.")
	(bind ?risposta (yes_or_no_p "Controlla che il massetto sia più basso di 1 - 1,5 cm ogni due metri lineari...%nÈ così?"))
	(if ?risposta
		then (assert (car (nome massetto_pendenza) (valore si)))
		else (assert (car (nome massetto_pendenza) (valore no)))))

(defrule domanda_pendenza_massetto_esterno_principiante
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome massetto_pendenza) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "La livella è lo strumento che permette di verificare che una superficie sia a livello, la stadia è un'asta di alluminio con dimensione da 50 a 200 cm.")
	(bind ?*spiegazione* "Questo abbassamento del massetto garantisce la pendenza giusta per lo scolo dell'acqua.")
	(bind ?risposta (yes_or_no_p "Poni una stadia di 2 metri (di dimensione adeguata a quella dell'area in cui si sta lavorando) sul massetto verso il punto in %ncui dovrà uscire l'acqua e poni uno spessore di 1,5 cm all'estremità più bassa della stadia. Adagia su di essa una livella controllando %nche la bolla sia precisamente nella posizione centrale. %nÈ così?"))
	(if ?risposta
		then (assert (car (nome massetto_pendenza) (valore si)))
		else (assert (car (nome massetto_pendenza) (valore no)))))

; /-----------------PAVIMENTO----------------/
(defrule domanda_piatrelle_scheggiate_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome piastrelle_scheggiate_pavimento) (valore ?)))

	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Una piastrella è scheggiata se la superficie presenta delle irregolarità causate generalmente da un urto con qualche oggetto.")
	(bind ?*spiegazione* "Se sono presenti diverse piastrelle scheggiate conviene rifare il pavimento.")
	(bind ?risposta (yes_or_no_p "È presente qualche piastrella scheggiata nel pavimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_scheggiate_pavimento) (valore si)))
		else (assert (car (nome piastrelle_scheggiate_pavimento) (valore no)))))

(defrule domanda_piastrelle_sollevate_pavimento
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome piastrelle_sollevate_pavimento) (valore ?)))

	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Una piastrella è non aderente se è sollevata o se battendola si sente un rumore vuoto.")
	(bind ?*spiegazione* "Se sono presenti diverse piastrelle sollevate conviene rifare il pavimento.")
	(bind ?risposta (yes_or_no_p "È presente qualche piastrella non aderente o sollevata nel pavimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_sollevate_pavimento) (valore si)))
		else (assert (car (nome piastrelle_sollevate_pavimento) (valore no)))))

;TODO valutare
(defrule domanda_livello_pavimento_interno_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome pavimento_livello) (valore ?)))
	
	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Poni sul pavimento una stadia della dimensione adeguata alla dimensione della stanza e adagia su di essa una livella per controllare che la bolla %nsia in posizione centrale. Ripeti poi l'operazione diverse volte per coprire tutta l'area.")
	(bind ?*spiegazione* "Se il pavimento non è a livello conviene rifarlo.")
	(bind ?risposta (yes_or_no_p "Il pavimento è a livello?"))
	(if ?risposta
		then (assert (car (nome pavimento_livello) (valore si)))
		else (assert (car (nome pavimento_livello) (valore no)))))

(defrule domanda_livello_pavimento_interno_principiante
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome pavimento_livello) (valore ?)))
	
	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "La livella è lo strumento che permette di verificare che una superficie sia a livello, la stadia è un'asta di alluminio con dimensione da 50 a 200 cm.")
	(bind ?*spiegazione* "Se il pavimento non è a livello conviene rifarlo.")
	(bind ?risposta (yes_or_no_p "Prendi una stadia di dimensione adeguata a quella della stanza e ponila sul pavimento, adagia sopra essa una livella %ne verifica che la bolla presente sia esattamente in posizione centrale. Ripeti l'operazione diverse volte in diversi punti della stanza. %nLa bolla nella livella è sempre in posizione centrale?"))
	(if ?risposta
		then (assert (car (nome pavimento_livello) (valore si)))
		else (assert (car (nome pavimento_livello) (valore no)))))

(defrule domanda_pendenza_pavimento_esterno_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome pendenza_pavimento) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Poni una stadia di 2 metri sul pavimento verso il punto in cui dovrà uscire l'acqua e poni uno spessore di 1,5 cm all'estremità più %nbassa della stadia. Adagia sulla stadia una livella controllando che la bolla sia precisamente nella posizione centrale.")
	(bind ?*spiegazione* "Se il pavimento non ha la pendenza giusta non permette all'acqua di fuoriuscire.")
	(bind ?risposta (yes_or_no_p "Controlla che il pavimento sia più basso di 1 - 1,5 cm ogni due metri lineari...%nÈ così?"))
	(if ?risposta
		then (assert (car (nome pendenza_pavimento) (valore si)))
		else (assert (car (nome pendenza_pavimento) (valore no)))))

(defrule domanda_pendenza_pavimento_esterno_principiante
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome pendenza_pavimento) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "La livella è lo strumento che permette di verificare che una superficie sia a livello, la stadia è un'asta di alluminio con dimensione da 50 a 200 cm.")
	(bind ?*spiegazione* "Se il pavimento non ha la pendenza giusta non permette all'acqua di fuoriuscire.")
	(bind ?risposta (yes_or_no_p "Poni una stadia di 2 metri (di dimensione adeguata a quella dell'area in cui si sta lavorando) sul pavimento verso il punto in %ncui dovrà uscire l'acqua e poni uno spessore di 1,5 cm all'estremità più bassa della stadia. Adagia su di essa una livella controllando %nche la bolla sia precisamente nella posizione centrale. %nÈ così?"))
	(if ?risposta
		then (assert (car (nome pendenza_pavimento) (valore si)))
		else (assert (car (nome pendenza_pavimento) (valore no)))))

; /------------------RIVESTIMENTO----------------/
(defrule domanda_spostamento_sanitari
	(preparazione_utente alta | bassa)
	(not (lavoro))
	(not (car (nome spostamento_sanitari) (valore ?)))

	(car (nome luogo) (valore interno))
	(car (nome tipo_stanza) (valore bagno))
	=>
	(bind ?*help* "Rispondere 'si' se si deve cambiare la disposizione di lavabo, bidet, tazza o doccia o bagno.")
	(bind ?*spiegazione* "Se si deve cambiare la disposizione dei sanitari, si deve obbligatoriamente rimuovere il pavimento e il massetto.")
	(bind ?risposta (yes_or_no_p "Devi cambiare la disposizione dei sanitari?"))
	(if ?risposta
		then (assert (nome spostamento_sanitari) (valore si))
		else (assert (nome spostamento_sanitari) (valore no))))

(defrule domanda_piatrelle_scheggiate_rivestimento
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome presenza_rivestimento) (valore si))
	(not (car (nome piastrelle_scheggiate_rivestimento) (valore ?)))
	=>
	(bind ?*help* "Una piastrella è scheggiata se la superficie presenta delle irregolarità causate generalmente da un urto con qualche oggetto.")
	(bind ?*spiegazione* "Se è presente qualche piastrella scheggiata potrebbe essere necessario sostituire il rivestimento.")
	(bind ?risposta (yes_or_no_p "È presente qualche piastrella scheggiata nel rivestimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_scheggiate_rivestimento) (valore si)))
		else (assert (car (nome piastrelle_scheggiate_rivestimento) (valore no)))))

(defrule domanda_piastrelle_sollevate_rivestimento
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome presenza_rivestimento) (valore si))
	(not (car (nome piastrelle_sollevate_rivestimento) (valore ?)))
	=>
	(bind ?*help* "Una piastrella è non aderente se è sollevata o se battendola si sente un rumore vuoto.")
	(bind ?*spiegazione* "Se è presente qualche piastrella sollevata potrebbe essere necessario sostituire il rivestimento.")
	(bind ?risposta (yes_or_no_p "È presente qualche piastrella non aderente o sollevata nel rivestimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_sollevate_rivestimento) (valore si)))
		else (assert (car (nome piastrelle_sollevate_rivestimento) (valore no)))))

(defrule domanda_piombo_muri_esperto
	(preparazione_utente alta)
	(not (lavoro))

	(car (nome presenza_rivestimento) (valore si))
	(not (car (nome muri_a_piombo) (valore ?)))
	=>
	(bind ?*help* "Prendi la livella e ponila sulla parete in posizione perpendicolare al pavimento e verifica se la bolla è in posizione centrale. %nRipeti l'operazione per ogni muro e in punti diversi.")
	(bind ?*spiegazione* "Se il muro non è a piombo, occorre raddrizzarlo.")
	(bind ?risposta (yes_or_no_p "I muri sono a piombo?"))
	(if ?risposta
		then (assert (car (nome muri_a_piombo) (valore si)))
		else (assert (car (nome muri_a_piombo) (valore no)))))

(defrule domanda_piombo_muri_principiante
	(preparazione_utente bassa)
	(not (lavoro))

	(car (nome presenza_rivestimento) (valore si))
	(not (car (nome muri_a_piombo) (valore ?)))
	=>
	(bind ?*help* "La livella è lo strumento che permette di verificare che una superficie sia a livello.")
	(bind ?*spiegazione* "Se il muro non è a piombo, occorre raddrizzarlo.")
	(bind ?risposta (yes_or_no_p "Prendi una livella e ponila sulla parete in posizione perpendicolare al pavimento e verifica se la bolla è in posizione centrale. %nIn tutte le misurazioni la bolla è sempre in posizione centrale?"))
	(if ?risposta
		then (assert (car (nome muri_a_piombo) (valore si)))
		else (assert (car (nome muri_a_piombo) (valore no)))))



;  /----------------------------------------/
; /-----------------LAVORI-----------------/
;/----------------------------------------/

(defrule umidita_principiante
	(declare (salience ?*high_priority*))
	(preparazione_utente bassa)
	(not (lavoro))

	(car (nome umidita) (valore si))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere causata da diversi fattori, chiama uno specialista.")
	(bind ?*spiegazione* "")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule umidita_esperto
	(declare (salience ?*high_priority*))
	(preparazione_utente alta)
	(not (lavoro))

	(car (nome umidita) (valore si))
	(car (nome impianti_umidita) (valore no))
	=>
	(bind ?*soluzione* "L'umidità è quella di risalita dai muri, impermeabilizza i muri da cui proviene l'umidità.")
	(bind ?*spiegazione* "Non essendoci tubazioni che trasportano acqua, l'umidità è essere causata da quella di risalita dei muri.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule impianti_umidita_esperto
	(declare (salience ?*high_priority*))
	(preparazione_utente alta)
	(not (lavoro))

	(car (nome umidita) (valore si))
	(car (nome impianti_umidita) (valore si))
	=>
	(bind ?*soluzione* "Controlla lo stato delle tubazioni.")
	(bind ?*spiegazione* "Avendo dedotto che ci sono tubazioni che trasportano acqua sotto il pavimento, l'umidità potrebbe essere causata da esse.") 
	(bind ?*help* "Rimuovi (con attenzione a non rompere nulla) il pavimento e il massetto (se presenti) e controlla se ci sono perdite, poi chiama uno specialista.")
	(assert (lavoro)))

(defrule rifacimento_impianti_principiante
	(declare (salience ?*high_priority*))
	(preparazione_utente bassa)
	(not (lavoro))

	(car (nome rifacimento_impianti) (valore si))
	=>
	(bind ?*soluzione* "Consulta uno specialista.")
	(bind ?*spiegazione* "")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule rifacimento_impianti_esperto
	(declare (salience ?*high_priority*))
	(preparazione_utente alta)
	(not (lavoro))

	(car (nome rifacimento_impianti) (valore si))
	=>
	(bind ?*soluzione* "Rimuovi tutto il pavimento e lo strato di cemento sottostante (massetto).")
	(bind ?*spiegazione* "Avendo dedotto che bisogna rifare gli impianti, il consiglio è quello di rimuovere tutto il pavimento e il massetto in modo da %npoter rifare gli impianti.")
	(bind ?*help* "")
	(assert (lavoro)))

;---------------------------------------------------------------------------------------------------------------------------------------

(defrule fai_massetto_raccordo
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome presenza_massetto) (valore no))
	(car (nome presenza_pavimento) (valore no))
	(car (nome umidita) (valore no))
	(car (nome rifacimento_impianti) (valore no))
	(car (nome pavimento_da_raccordare) (valore si))
	=>
	(bind ?*soluzione* "Realizza il massetto tenendo conto del fatto che bisogna raccordarsi con un pavimento già esistente.")
	(bind ?*spiegazione* "Avendo dedotto che non è presente né un pavimento né un rivestimento, che non c'è umidità e non si devono fare impianti, ma c'è %nun pavimento con cui raccordarsi, allora il lavoro da fare è il massetto.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule fai_massetto_no_raccordo
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome presenza_massetto) (valore no))
	(car (nome presenza_pavimento) (valore no))
	(car (nome umidita) (valore no))
	(car (nome rifacimento_impianti) (valore no))
	(car (nome pavimento_da_raccordare) (valore no))
	=>
	(bind ?*soluzione* "Realizza il massetto.")
	(bind ?*spiegazione* "Avendo dedotto che non è presente né un pavimento né un rivestimento, che non c'è umidità e non si devono fare impianti e no c'è %nun pavimento con cui raccordarsi, allora il lavoro da fare è il massetto.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule ok_pavimento_interno1  ;pavimento non da raccordare
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome presenza_pavimento) (valore no))
	(car (nome presenza_massetto) (valore si))
	(car (nome pavimento_da_raccordare) (valore no))
	(car (nome massetto_livello) (valore si))
	(car (nome massetto_friabile) (valore no))
	=>
	(bind ?*soluzione* "Il massetto su cui porre il pavimento è a livello, puoi effettuare la posa.")
	(bind ?*spiegazione* "Avendo dedotto che non è presente un pavimento, che è presente un massetto,che è a livello e in buone condizioni e che non ci si %ndeve preoccupare di raccordarsi con un altro tipo di pavimento presente, si può iniziare la posa sopra di una qualsiasi tipo di pavimento.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule ok_pavimento_interno2  ;pavimento da raccordare
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome presenza_pavimento) (valore no))
	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_livello) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome massetto_altezza) (valore giusto))
	(car (nome massetto_friabile) (valore no))
	=>
	(bind ?*soluzione* "Il massetto su cui porre il pavimento è a livello e alla giusta altezza, puoi iniziare la posa.")
	(bind ?*spiegazione* "Avendo dedotto che non è presente un pavimento il pavimento è da raccordare, ma è all'altezza giusta in base al tipo di pavimento %nda posare, e che è a livello e in buone condizioni, si può allora iniziare la posa.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule ok_pavimento_esterno
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore no))
	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_pendenza) (valore si))
	(car (nome massetto_friabile) (valore no))
	=>
	(bind ?*soluzione* "Il massetto ha la giusta pendenza, puoi iniziare la posa sopra.")
	(bind ?*spiegazione* "Avendo dedotto che si parla di lavoro esterno, che è presente un massetto che è in buone condizioni e che ha la giusta pendenza, %nsi può allora iniziare la posa.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule massetto_friabile
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
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
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_livello) (valore no))
	=>
	(bind ?*soluzione* "Elimina il massetto presente e rifallo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di lavoro interno, che è presente un massetto e che questo non è a livello, il consiglio è quello %ndi rimuoverlo e rifarlo.")
	(bind ?*help* "Rimuovi il massetto esistente e rifallo.")
	(assert (lavoro)))

(defrule massetto_alto
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome massetto_livello) (valore si))
	(car (nome massetto_altezza) (valore alto))
	=>
	(bind ?*soluzione* "Rimuovi rifai daccapo il massetto.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di lavoro interno, che il pavimento da posare è da raccordare, che è presente un massetto, che è %na livello ma che è troppo alto per il tipo di pavimento che si dovrà porre, allora il consiglio è di rimuoverlo e rifarlo.")
	(bind ?*help* "Rimuovi il massetto esistente e rifallo.")
	(assert (lavoro)))

(defrule massetto_basso
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome massetto_livello) (valore si))
	(car (nome massetto_altezza) (valore basso))
	=>
	(bind ?*soluzione* "Costruisci sopra al presente massetto uno nuovo in modo che sia alla altezza esatta.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di lavoro interno, che il pavimento da posare è da raccordare, che è presente un massetto, che è %na livello ma che è troppo basso per il tipo di pavimento che si dovrà porre, allora il consiglio è di costruire sopra il presente %nmassetto uno nuovo per fare in modo che si trovi alla altezza giusta.")
	(bind ?*help* "Costruisci un massetto su quello esistente.")
	(assert (lavoro)))

(defrule massetto_non_pendenza
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_pendenza) (valore no))
	=>
	(bind ?*soluzione* "Rimuovere il massetto e rifarlo daccapo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro esterno, che è presente un massetto e che la pendenza per favorire lo scolo dell'acqua %nnon è stata rispettata, il consiglio è quello di rimuoverlo e rifarlo daccapo.")
	(bind ?*help* "Rimuovi il massetto facendo attenzione ad eventuali tubi di impianti e rifallo.")
	(assert (lavoro)))

;----------------------------------------------------------------------------------------------------------------------------------------

(defrule rattoppo_pavimento_interno
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pavimento_livello) (valore si))
	(or (car (nome piastrelle_sollevate_pavimento) (valore si))
		(car (nome piastrelle_scheggiate_pavimento) (valore si)))
	=>
	(bind ?*soluzione* "Sostituisci le piastrelle difettate se sono poche (rattoppo), facendo attenzione al fatto che non vi sia differenza di tonalità %ntra le piastrelle nuove e quelle vecchie; altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro interno, che un pavimento è presente ed è a livello ma vi sono piastrelle danneggiate, %nil consiglio è di sostituirle se sono poche, altrimenti rifare tutto il pavimento.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule rattoppo_pavimento_esterno
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pendenza_pavimento) (valore si))
	(or (car (nome piastrelle_sollevate_pavimento) (valore si))
		(car (nome piastrelle_scheggiate_pavimento) (valore si)))
	=>
	(bind ?*soluzione* "Sostituisci le piastrelle difettate se sono poche (rattoppo), facendo attenzione al fatto che non vi sia differenza di tonalità %ntra le piastrelle nuove e quelle vecchie; altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro esterno, che un pavimento è presente ed ha la pendenza giusta ma vi sono piastrelle %ndanneggiate, il consiglio è di sostituirle se sono poche, altrimenti rifare tutto il pavimento.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule posa_sopra_interno ;TODO da rivedere
	(declare (salience ?*high_priority*))
	(preparazione_utente alta)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pavimento_livello) (valore si))
	(and (not (car (nome piastrelle_sollevate_pavimento) (valore no)))
		 (not (car (nome piastrelle_scheggiate_pavimento) (valore no))))
	=>
	(bind ?*soluzione* "Si può optare per la posa sopra al pavimento esistente. Bisogna considerare però che vi sarà un innalzamento del pavimento che %ndovrà portare a diverse modifiche (è il caso delle porte), altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro interno, il pavimento è presente ed è a livello e non vi sono piastrelle scheggiate %no sollevate, si può optare per la posa sopra.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule posa_sopra_esterno ;TODO da rivedere
	(declare (salience ?*high_priority*))
	(preparazione_utente alta)
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pendenza_pavimento) (valore si))
	(and (not (car (nome piastrelle_sollevate_pavimento) (valore no)))
		 (not (car (nome piastrelle_scheggiate_pavimento) (valore no))))
	=>
	(bind ?*soluzione* "Se si deve rinnovare il pavimento si può decidere anche per la posa sopra al pavimento esistente. Bisogna considerare però %nche vi sarà %nun innalzamento del pavimento che dovrà portare a diverse modifiche (è il caso delle porte), altrimenti %nprocedere a sostituire il pavimento con uno nuovo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro esterno, il pavimento è presente ed è alla giusta pendenza e non vi sono piastrelle scheggiate o sollevate, si può optare per la posa sopra.")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule pavimento_non_a_livello
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
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
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pendenza_pavimento) (valore no))
	=>
	(bind ?*soluzione* "Rimuovi il pavimento insieme allo strato di cemento sottostante (massetto) e rifallo.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un luogo esterno, che il pavimento è presente ma non ha la pendenza giusta per favorire lo %nscolo dell'acqua, quindi occorre toglierlo e rifarlo.")
	(bind ?*help* "")
	(assert (lavoro)))

;------------------------------------------------------------------------------------------------------------------------------------------------

(defrule spostamento_sanitari
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome tipo_stanza) (valore bagno))
	(car (nome spostamento_sanitari) (valore si))
	=>
	(bind ?*soluzione* "Rimuovi il pavimento esistente e lo strato di cemento sottostante (massetto), poi chiama uno specialista.")
	(bind ?*spiegazione* "Avendo dedotto che si tratta di un lavoro interno, in particolare il bagno e che si deve fare uno spostamento di sanitari, %nil consiglio è quello di rimuovere il pavimento e il massetto se presenti e poi procedere chiamando uno specialista per il posizionamento degli impianti")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule muri_non_a_piombo
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_rivestimento) (valore si))
	(car (nome muri_a_piombo) (valore no))
	=>
	(bind ?*soluzione* "Raddrizzare i muri in modo che siano a piombo, poi procedere con la posa o con l'intonaco.")
	(bind ?*spiegazione* "Avendo dedotto che il lavoro riguarda l'interno, che è presente un rivestimento e che i muri non sono a piombo, il consiglio %nè di rifare i muri e poi procedere alla posa del rivestimento")
	(bind ?*help* "")
	(assert (lavoro)))

(defrule rattoppo_rivestimento
	(declare (salience ?*high_priority*))
	(preparazione_utente alta | bassa)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_rivestimento) (valore si))
	(or (car (nome piastrelle_scheggiate_rivestimento) (valore si))
		(car (nome piastrelle_sollevate_rivestimento) (valore si)))
	=>
	(bind ?*soluzione* "Sostituisci le piastrelle difettate se sono poche (rattoppo), facendo attenzione al fatto che non vi sia differenza di tonalità %n tra le piastrelle nuove e quelle vecchie; altrimenti procedere a sostituire il rivestimento con uno nuovo..")
	(bind ?*spiegazione* "Avendo dedotto che il lavoro riguarda l'interno, che è presente un pavimento e che ci sono piastrelle scheggiate o sollevate, %n il consiglio è di effettuare un rattoppo se il numero delle piastrelle è limitato, altrimenti rifare tutto il rivestimento.")
	(bind ?*help* "")
	(assert (lavoro)))


;-----------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------

(defrule lavoro_trovato
	(declare (salience ?*high_priority*))
	(lavoro)
	=>
	(format t "%n>>SOLUZIONE: %n%s%n" ?*soluzione*)
	(if (neq (length$ ?*spiegazione*) 0)
		then (format t "%n>>SPIEGAZIONE: %n%s%n" ?*spiegazione*))
	(if (neq (length$ ?*help*) 0) 
		then (format t "%n>>AIUTO: %n%s%n" ?*soluzione*))


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


