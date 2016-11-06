(defglobal ?*highest_priority* = 1000)
(defglobal ?*high_priority* = 100)
(defglobal ?*low_priority* = -100)
(defglobal ?*lowest_priority* = -1000)
(defglobal ?*help* = "")
(defglobal ?*spiegazione* = "")
(defglobal ?*lavoro* = "")

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
; /---------------------------------DIAGNOSI----------------------------------/
;/---------------------------------------------------------------------------/
(defrule domanda_interno_esterno
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome luogo) (valore ?)))
	=>
	(bind ?*help* "'Interno' riguarda una qualsiasi stanza che non sarà soggetta alle intemperie (bagno, cucina, stanza da letto, etc), 'Esterno' in caso %ncontrario (balcone, terrazzo).")
	(bind ?*spiegazione* "Potrebbero esserci dei problemi specifici relativi al luogo in cui si trova il pavimento.")
	(bind ?risposta (ask_question "La stanza riguarda l'interno o l'esterno?" interno esterno))
	(if (eq ?risposta interno)
		then (assert (car (nome luogo) (valore interno)))
		else (assert (car (nome luogo) (valore esterno)))))

(defrule domanda_tipo_stanza
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome tipo_stanza) (valore ?)))

	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "Indicare a quale tipo tra quelli elencati corrisponde la stanza in cui deve essere fatto il lavoro.")
	(bind ?*spiegazione* "Potrebbero esserci dei problemi specifici relativi al diverso luogo in cui si trova il pavimento.")
	(bind ?risposta (ask_question "Quale è il tipo di stanza?" bagno cucina altro))
	(assert (car (nome tipo_stanza) (valore ?risposta))))

(defrule domanda_rifacimento_impianti
	(preparazione_utente ?)
	(not (lavoro))
	
	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi rifare qualche impianto (idrico, metano, etc.)?"))
	(if ?risposta
		then (assert (car (nome rifacimento_impianti) (valore si)))
		else (assert (car (nome rifacimento_impianti) (valore no)))))

(defrule domanda_umidita
	(preparazione_utente ?)
	(not (lavoro))

	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Hai problemi di umidità?"))
	(if ?risposta
		then (assert (car (nome umidita) (valore si)))
		else (assert (car (nome umidita) (valore no)))))


;/--------------------------------------------/
;/-----------------PAVIMENTO-----------------/
;/------------------------------------------/
(defrule domanda_presenza_pavimento
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome presenza_pavimento) (valore ?)))

	(not (car (nome presenza_massetto) (valore si)))
	=>
	(bind ?*help* "Rispondere 'si' se è già presente un pavimento nella stanza in cui si intende lavorare, 'no' altrimenti.")
	(bind ?risposta (yes_or_no_p "È già presente un pavimento?"))
	(if ?risposta
		then (assert (car (nome presenza_pavimento) (valore si)))
		else (assert (car (nome presenza_pavimento) (valore no)))))

(defrule domanda_anni_pavimento
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome anni_pavimento) (valore ?)))

	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Rispondere indicando (se si conoscono) gli anni che ha il pavimento presente.")
	(bind ?risposta (ask_number "Quanti anni sono che il pavimento presente non viene sostituito (-1 nel caso non si sappia)?"))
	(while (< ?risposta -1)
		(printout t crlf "Inserisci un numero da 0 in poi, o -1 nel caso tu non conosca gli anni.")
		(bind ?risposta (ask_number "Quanti anni sono che il pavimento presente non viene sostituito (-1 nel caso non si sappia)?")))
	(assert (car (nome anni_pavimento) (valore ?risposta))))

(defrule domanda_dimensione_area_pavimento_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome area_pavimento) (valore ?)))

	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (ask_number "Indicare la dimensione del pavimento in metri al quadrato"))
	(assert (car (nome area_pavimento) (valore ?risposta))))

(defrule domanda_dimensione_area_pavimento_principiante
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome area_pavimento) (valore ?)))

	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "La misura dell'area da pavimentare non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un pavimento si%n" 
				"effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella%n"
				"dell'area da pavimentare, ma deve essere maggiore. Procedere individuando la forma di tale superficie, se questa può essere ricondotta%n" 
				"ad una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, allora, per ottenere l'area, bisogna ricordare che:%n"
				"	* se la superficie ha la forma di un quadrato, allora si calcola la lunghezza di un muro (che rappresenta il lato) e la si moltiplica%n"
				"	  per se stesso%n"
				"	* se la superficie ha la forma di un rettangolo, allora si moltiplica la dimensione del muro più lungo (che rappresenta la lunghezza)%n"
				"	  per la dimensione del muro più piccolo (che rappresenta la larghezza)%n"
				"	* se la superficie ha la forma di un triangolo, allora si trova la lunghezza del muro che rappresenta la base e quella del muro che%n"
				"	  rappresenta l'altezza  del triangolo, si moltiplicando tra di loro le due misure e si divide per due il risultato%n"
				"	* se la superficie ha la forma di un cerchio, allora si deve trovare la misura del raggio e si usa la formula 2πr, dove π = 3.14 e%n" 
				"	  r = raggio calcolato (cioè si calcola il raggio, che è la metà del diametro e lo si moltiplica prima per due e poi per 3.14)%n"
				"	* se la superficie ha la forma di una semicirconferenza, allora si procede come nel caso precedente (cerchio) a trovare la misura del%n"
				"	  raggio della circonferenza e si usa la formula 2πr (dove π = 3.14 e r = raggio trovato) e si divide il risultato per due.%n"
				"Nel caso in cui la forma della superficie da pavimentare non fosse simile ad una delle precedenti, allora si suddivide l'area in parti più%n"
				"piccole dalla forma riconducibile ad una di quelle precedenti, si calcola l'area di ogni parte e si sommano i vari risultati ottenuti%n"
				"Le misure vanno espresse in metri al quadrato")
	(bind ?risposta (ask_number "Indicare la dimensione del pavimento in metri al quadrato"))
	(assert (car (nome area_pavimento) (valore ?risposta))))

(defrule domanda_condizioni_pavimento_esperto
	(preparazione_utente alta)
	(not (lavoro))

	(car (nome presenza_pavimento) (valore si))
	(not (car (nome condizioni_pavimento) (valore ?)))
	=>
	(bind ?*help* "Rispondere 'si' se il pavimento in questione presenta segni di usura come piastrelle scheggiate, consumate o non aderenti.")
	(bind ?risposta (yes_or_no_p "Il pavimento esistente presenta diverse piastrelle consumate o non perfettamente aderenti?"))
	(if ?risposta
		then (assert (car (nome condizioni_pavimento) (valore non_buone)))
		else (assert (car (nome condizioni_pavimento) (valore buone)))))

(defrule domanda_condizioni_pavimento_principiante
	(preparazione_utente bassa)
	(not (lavoro))

	(car (nome presenza_pavimento) (valore si))
	(not (car (nome condizioni_pavimento) (valore ?)))
	=>
	(bind ?*help* "")
	(bind ?usura (yes_or_no_p "Guarda attentamente il pavimento presente... %nLe piastrelle sembrano opache o sono graffiate (usurate)?"))

	(bind ?*help* "")
	(bind ?rumore (yes_or_no_p "Batti con il manico di un martello o con il piede la superficie dove sono posate le piastrelle... %nSenti un rumore vuoto?"))
	(if (or ?usura ?rumore)
		then (assert (car (nome condizioni_pavimento) (valore buone)))
		else (assert (car (nome condizioni_pavimento) (valore non_buone)))))

(defrule domanda_livello_pavimento_esterno
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome pavimento_livello) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Controlla che il pavimento sia più basso di 1-1,5 cm ogni due metri lineari...%nÈ così?"))
	(if ?risposta
		then (assert (car (nome pavimento_livello) (valore si)))
		else (assert (car (nome pavimento_livello) (valore si)))))

(defrule domanda_piastrella_presente
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome tipo_piastrella) (valore ?)))

	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (ask_question "Che tipo di pavimento è presente?" ))
	)

;/--------------------------------------------/
;/-----------------MASSETTO------------------/
;/------------------------------------------/
(defrule domanda_presenza_massetto
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome presenza_massetto) (valore ?)))

	(car (nome presenza_pavimento) (valore no))
	=>
	(bind ?*help* "Il massetto è quello strato di cemento e sabbia che deve essere perfettamente a livello e la cui presenza è fondamentale perché %nsopra di esso verrà posto il pavimento vero e proprio.")
	(bind ?risposta (yes_or_no_p "È presente un massetto?"))
	(if ?risposta
		then (assert (car (nome presenza_massetto) (valore si)))
		else (assert (car (nome presenza_massetto) (valore no)))))

(defrule domanda_massetto_friabile
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome massetto_friabile) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "Il massetto non è consistente se presenta una superficie polverosa e si sgretola facilmente.")
	(bind ?risposta (yes_or_no_p "Il massetto non è molto consistente?"))
	(if ?risposta 
		then (assert (car (nome massetto_friabile) (valore si)))
		else (assert (car (nome massetto_friabile) (valore no)))))

(defrule domanda_pavimento_raccordo
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome pavimento_da_raccordare) (valore ?)))

	(or (car (nome presenza_massetto) (valore si))
		(car (nome presenza_massetto) (valore no)))
	=>
	(bind ?*help* "")
	(bind ?raccordo_pavimento (yes_or_no_p "Esistono dei pavimenti nello stesso piano?"))
	(bind ?*help* "")
	(bind ?raccordo_porte (yes_or_no_p "Esistono delle porte già montate o balconi nella stanza?"))
	(if (or ?raccordo_pavimento ?raccordo_porte)
		then (assert (car (nome pavimento_da_raccordare) (valore si)))
		else (assert (car (nome pavimento_da_raccordare) (valore no)))))

(defrule domanda_livello_massetto_interno
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome massetto_livello) (valore ?)))
	
	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Il massetto è a livello?"))
	(if ?risposta
		then (assert (car (nome massetto_livello) (valore si)))
		else (assert (car (nome massetto_livello) (valore no)))))

(defrule domanda_altezza_massetto
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome altezza_massetto) (valore ?)))

	(car (nome presenza_massetto) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome massetto_livello) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (ask_number "Indica lo spessore della piastrella da porre in millimetri (-1 se non lo conosci)"))
	(if (= ?risposta -1)
		then (printout t crlf "Se non si conosce lo spessore della piastrella, allora non si può controllare se il massetto è al giusto livello!" crlf)
			 (halt)
		else (format t "%nConsidera che lo spessore complessivo del pavimento sarà di %d mm%n" (+ 3 ?risposta))
			 (bind ?*help* "")
			 (bind ?risposta (ask_question "Considerando lo spessore indicato, il massetto è alto, basso o alla dimensione giusta?" alto basso giusto))
			 (assert (car (nome altezza_massetto) (valore ?risposta)))))

(defrule domanda_livello_massetto_esterno
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome massetto_livello) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Controlla che il massetto sia più basso di 1-1,5 cm ogni due metri lineari...%nÈ così?"))
	(if ?risposta
		then (assert (car (nome massetto_livello) (valore si)))
		else (assert (car (nome massetto_livello) (valore si)))))

;/------------------------------------------------/
;/------------------RIVESTIMENTO-----------------/
;/----------------------------------------------/
(defrule domanda_presenza_rivestimento
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome presenza_rivestimento) (valore ?)))

	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	=>
	(bind ?*help* "Rispondere 'si' se è già presente un rivestimento, cioè le pareti della stanza sono ricoperte con piastrelle, 'no' altrimenti.")
	(bind ?risposta (yes_or_no_p "È già presente un rivestimento?"))
	(if ?risposta
		then (assert (car (nome presenza_rivestimento) (valore si)))
		else (assert (car (nome presenza_rivestimento) (valore no)))))

(defrule domanda_spostamento_sanitari
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome spostamento_sanitari) (valore ?)))

	(car (nome tipo_stanza) (valore bagno))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi spostare i sanitari?"))
	(if ?risposta
		then (assert (nome spostamento_sanitari) (valore si))
		else (assert (nome spostamento_sanitari) (valore no))))

(defrule domanda_spostamento_cucina
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome spostamento_cucina) (valore ?)))

	(car (nome tipo_stanza) (valore cucina))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi effettuare qualche spostamento di lavello o lavastoviglie?"))
	(if ?risposta
		then (assert (nome spostamento_cucina) (valore si))
		else (assert (nome spostamento_cucina) (valore no))))

(defrule domanda_dimensione_area_rivestimento_esperto
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome area_rivestimento) (valore ?)))

	(car (nome presenza_rivestimento) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (ask_number "Indicare la dimensione del rivestimento in metri al quadrato"))
	(assert (car (nome area_rivestimento) (valore ?risposta))))

(defrule domanda_dimensione_area_rivestimento_principiante
	(preparazione_utente bassa)
	(not (lavoro))
	(not (car (nome area_rivestimento) (valore ?)))

	(car (nome presenza_rivestimento) (valore si))
	=>
	(bind ?*help* "La misura dell'area da rivestire non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un pavimento si%n" 
				"effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella%n"
				"dell'area, ma deve essere maggiore. Procedere individuando la forma di tale superficie, se questa può essere ricondotta%n" 
				"ad una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, allora, per ottenere l'area, bisogna ricordare che:%n"
				"	* se la superficie ha la forma di un quadrato, allora si calcola la lunghezza di un muro (che rappresenta il lato) e la si moltiplica%n"
				"	  per se stesso%n"
				"	* se la superficie ha la forma di un rettangolo, allora si moltiplica la dimensione del muro più lungo (che rappresenta la lunghezza)%n"
				"	  per la dimensione del muro più piccolo (che rappresenta la larghezza)%n"
				"	* se la superficie ha la forma di un triangolo, allora si trova la lunghezza del muro che rappresenta la base e quella del muro che%n"
				"	  rappresenta l'altezza  del triangolo, si moltiplicando tra di loro le due misure e si divide per due il risultato%n"
				"	* se la superficie ha la forma di un cerchio, allora si deve trovare la misura del raggio e si usa la formula 2πr, dove π = 3.14 e%n" 
				"	  r = raggio calcolato (cioè si calcola il raggio, che è la metà del diametro e lo si moltiplica prima per due e poi per 3.14)%n"
				"	* se la superficie ha la forma di una semicirconferenza, allora si procede come nel caso precedente (cerchio) a trovare la misura del%n"
				"	  raggio della circonferenza e si usa la formula 2πr (dove π = 3.14 e r = raggio trovato) e si divide il risultato per due.%n"
				"Nel caso in cui la forma della superficie da rivestire non fosse simile ad una delle precedenti, allora si suddivide l'area in parti più%n"
				"piccole dalla forma riconducibile ad una di quelle precedenti, si calcola l'area di ogni parte e si sommano i vari risultati ottenuti%n"
				"Le misure vanno espresse in metri al quadrato")
	(bind ?risposta (ask_number "Indicare la dimensione dell'area del rivestimento in metri al quadrato"))
	(assert (car (nome area_rivestimento) (valore ?risposta))))

(defrule domanda_condizioni_rivestimento_esperto
	(preparazione_utente alta)
	(not (lavoro))

	(car (nome presenza_rivestimento) (valore si))
	(not (car (nome condizioni_rivestimento) (valore ?)))
	=>
	(bind ?*help* "Rispondere 'si' se il rivestimento in questione presenta segni di usura come piastrelle scheggiate, consumate o non aderenti.")
	(bind ?risposta (yes_or_no_p "Il rivestimento esistente presenta diverse piastrelle consumate o non perfettamente aderenti?"))
	(if ?risposta
		then (assert (car (nome condizioni_rivestimento) (valore non_buone)))
		else (assert (car (nome condizioni_rivestimento) (valore buone)))))

(defrule domanda_condizioni_rivestimento_principiante
	(preparazione_utente bassa)
	(not (lavoro))

	(car (nome presenza_rivestimento) (valore si))
	(not (car (nome condizioni_rivestimento) (valore ?)))
	=>
	(bind ?*help* "")
	(bind ?usura (yes_or_no_p "Guarda attentamente il rivestimento presente... %nLe piastrelle sembrano opache o sono graffiate (usurate)?"))

	(bind ?*help* "")
	(bind ?rumore (yes_or_no_p "Batti con il manico di un martello la superficie dove sono posate le piastrelle... %nSenti un rumore vuoto?"))
	(if (or ?usura ?rumore)
		then (assert (car (nome condizioni_rivestimento) (valore buone)))
		else (assert (car (nome condizioni_rivestimento) (valore non_buone)))))


;/------------------------------------------/
;/-----------------LAVORI------------------/
;/----------------------------------------/

(defrule umidita_massetto
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome umidita) (valore si))
	=>
	(bind ?*lavoro* "Devi rompere il massetto e verificare se qualche tubo perde, poi aggiungi uno strato impermeabilizzante, il massetto e il pavimento.")
	(assert (lavoro)))

(defrule umidita_pavimento
	(declare (salience ?*high_priority*)))

(defrule fughe_pavimento ;domanda fughe in caso di pavimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(car (nome condizioni_pavimento) (valore buone))
	(car (nome ristrutturazione_pavimento) (valore no))
	=>
	(assert (lavoro fughe)))

(defrule fughe_rivestimento ;domanda fughe in caso di rivestimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_rivestimento) (valore si))
	(car (nome condizioni_rivestimento) (valore buone))
	(car (nome ristrutturazione_rivestimento) (valore no))
	=>
	(assert (lavoro fughe)))

(defrule battiscopa1
	(declare (salience ?*high_priority*))
	(not (continua))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome condizioni_pavimento) (valore buone))
	(car (nome ristrutturazione_pavimento) (valore no))
	=>
	(assert (lavoro battiscopa)))

(defrule battiscopa2
	(declare (salience ?*high_priority*))
	(not (continua))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore altro))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_pavimento) (valore si))
	(car (nome condizioni_pavimento) (valore buone))
	(car (nome ristrutturazione_pavimento) (valore no))
	=>
	(assert (lavoro battiscopa)))

(defrule rattoppo
	(declare (salience ?*high_priority*))
	(not (continua))

	(car (nome presenza_pavimento) (valore si))
	(car (nome condizioni_pavimento) (valore buone))
	(car (nome anni_pavimento) (valore ?x))
	(test (<= ?x 6))  ;se il pavimento è troppo vecchio non si fa il rattoppo perché ci sarà una differenza di colore tra la piastrella nuova e quella vecchia
	=>
	(assert (rattoppo)))

(defrule pavimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(or (car (nome condizioni_pavimento) (valore cattive))
		(car (nome ristrutturazione_pavimento) (valore si))
		(car (nome presenza_pavimento) (valore no))
		(car (nome presenza_massetto) (valore si)))
	=>
	(assert (lavoro pavimento)))

(defrule rivestimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome condizioni_rivestimento) (valore cattive))
		(car (nome ristrutturazione_rivestimento) (valore si))
		(car (nome presenza_rivestimento) (valore no)))
	=>
	(assert (lavoro rivestimento)))

(defrule pavimento_rivestimento
	(declare (salience ?*high_priority*))
	(not (continua))

	(or (car (nome condizioni_pavimento) (valore cattive))
		(car (nome ristrutturazione_pavimento) (valore si))
		(car (nome presenza_pavimento) (valore no))
		(car (nome presenza_massetto) (valore si)))
	(or (car (nome condizioni_rivestimento) (valore cattive))
		(car (nome ristrutturazione_rivestimento) (valore si))
		(car (nome presenza_rivestimento) (valore no)))
	=>
	(assert (lavoro pavimento_rivestimento)))


;-----------------------------------------------------------------------------------------------------------------

(defrule lavoro_trovato
	(declare (salience ?*high_priority*))
	(lavoro)
	=>
	(printout t crlf ">>>>> Il lavoro diagnosticato da fare è: " ?lavoro crlf crlf)

	(if (yes_or_no_p "Vuoi rivedere qualcosa?")
		then (halt)
		else (assert (rivedi_scelte_lavoro))))

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


