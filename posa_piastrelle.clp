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
; /----------------------------------DOMANDE----------------------------------/
;/---------------------------------------------------------------------------/
(defrule domanda_interno_esterno
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome luogo) (valore ?)))
	=>
	(bind ?*help* "'Interno' riguarda una qualsiasi stanza che non sarà soggetta alle intemperie (bagno, cucina, stanza da letto, etc), 'Esterno' in caso %ncontrario (balcone, terrazzo,etc).")
	(bind ?*spiegazione* "")
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

(defrule domanda_rifacimento_impianti
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome rifacimento_impianti) (valore ?)))

	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi rifare qualche impianto che passa per il pavimento o i muri (idrico, termosifoni, fognatura, etc.)?"))
	(if ?risposta
		then (assert (car (nome rifacimento_impianti) (valore si)))
		else (assert (car (nome rifacimento_impianti) (valore no)))))

(defrule domanda_umidita
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome umidita) (valore ?)))

	(car (nome luogo) (valore interno))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Hai problemi di umidità?"))
	(if ?risposta
		then (assert (car (nome umidita) (valore si)))
		else (assert (car (nome umidita) (valore no)))))

(defrule domanda_impianti_umidita
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome impianti_umidita) (valore ?)))

	(car (nome umidita) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (ask_question "Passano dei tubi di acqua, fognatura o riscaldamento sotto il pavimento o nei muri?" si no non_so))
	(if (eq ?risposta no)
		then (assert (car (nome impianti_umidita) (valore no)))
		else (assert (car (nome impianti_umidita) (valore si)))))




;/--------------------------------------------/
;/-----------------MASSETTO------------------/
;/------------------------------------------/

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
	(not (car (nome massetto_altezza) (valore ?)))

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
			 (assert (car (nome massetto_altezza) (valore ?risposta)))))

(defrule domanda_pendenza_massetto_esterno
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome massetto_pendenza) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_massetto) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Controlla che il massetto sia più basso di 1-1,5 cm ogni due metri lineari...%nÈ così?"))
	(if ?risposta
		then (assert (car (nome massetto_pendenza) (valore si)))
		else (assert (car (nome massetto_pendenza) (valore no)))))



;/--------------------------------------------/
;/-----------------PAVIMENTO-----------------/
;/------------------------------------------/
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

(defrule domanda_piatrelle_scheggiate_pavimento
	(preparazione_utente ?)
	(not (lavoro))

	(car (nome presenza_pavimento) (valore si))
	(not (car (nome piastrelle_scheggiate_pavimento) (valore ?)))
	=>
	(bind ?*help* "Una piastrella è scheggiata se la superficie presenta delle irregolarità causate generalmente da un urto con qualche oggetto.")
	(bind ?risposta (yes_or_no_p "È presente qualche piastrella scheggiata nel pavimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_scheggiate_pavimento) (valore si)))
		else (assert (car (nome piastrelle_scheggiate_pavimento) (valore no)))))

(defrule domanda_piastrelle_sollevate_pavimento
	(preparazione_utente ?)
	(not (lavoro))

	(car (nome presenza_pavimento) (valore si))
	(not (car (nome piastrelle_sollevate_pavimento) (valore ?)))
	=>
	(bind ?*help* "Una piastrella è non aderente se è sollevata o se battendola si sente un rumore vuoto.")
	(bind ?risposta (yes_or_no_p "È presente qualche piastrella non aderente o sollevata nel pavimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_sollevate_pavimento) (valore si)))
		else (assert (car (nome piastrelle_sollevate_pavimento) (valore no)))))

;TODO valutare
(defrule domanda_livello_pavimento_interno
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome pavimento_livello) (valore ?)))
	
	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Il pavimento è a livello?"))
	(if ?risposta
		then (assert (car (nome pavimento_livello) (valore si)))
		else (assert (car (nome pavimento_livello) (valore no)))))

(defrule domanda_pendenza_pavimento_esterno
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome pendenza_pavimento) (valore ?)))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Controlla che il pavimento sia più basso di 1-1,5 cm ogni due metri lineari...%nÈ così?"))
	(if ?risposta
		then (assert (car (nome pendenza_pavimento) (valore si)))
		else (assert (car (nome pendenza_pavimento) (valore no)))))


;/------------------------------------------------/
;/------------------RIVESTIMENTO-----------------/
;/----------------------------------------------/
(defrule domanda_spostamento_sanitari
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome spostamento_sanitari) (valore ?)))

	(car (nome tipo_stanza) (valore bagno))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi cambiare la disposizione dei sanitari?"))
	(if ?risposta
		then (assert (nome spostamento_sanitari) (valore si))
		else (assert (nome spostamento_sanitari) (valore no))))

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
	(bind ?*help* "La misura dell'area da rivestire non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un rivestimento si%n" 
				"effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella%n"
				"dell'area da rivestire, ma deve essere maggiore. Procedere individuando la forma di tale superficie, se questa può essere ricondotta%n" 
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
	(bind ?risposta (ask_number "Indicare la dimensione del rivestimento in metri al quadrato"))
	(assert (car (nome area_rivestimento) (valore ?risposta))))

(defrule domanda_piatrelle_scheggiate_rivestimento
	(preparazione_utente ?)
	(not (lavoro))

	(car (nome presenza_rivestimento) (valore si))
	(not (car (nome piastrelle_scheggiate_rivestimento) (valore ?)))
	=>
	(bind ?*help* "Una piastrella è scheggiata se la superficie presenta delle irregolarità causate generalmente da un urto con qualche oggetto.")
	(bind ?risposta (yes_or_no_p "È presente qualche piastrella scheggiata nel rivestimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_scheggiate_rivestimento) (valore si)))
		else (assert (car (nome piastrelle_scheggiate_rivestimento) (valore no)))))

(defrule domanda_piastrelle_sollevate_rivestimento
	(preparazione_utente ?)
	(not (lavoro))

	(car (nome presenza_rivestimento) (valore si))
	(not (car (nome piastrelle_sollevate_rivestimento) (valore ?)))
	=>
	(bind ?*help* "Una piastrella è non aderente se è sollevata o se battendola si sente un rumore vuoto.")
	(bind ?risposta (yes_or_no_p "È presente qualche piastrella non aderente o sollevata nel rivestimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_sollevate_rivestimento) (valore si)))
		else (assert (car (nome piastrelle_sollevate_rivestimento) (valore no)))))

(defrule domanda_piombo_muri
	(preparazione_utente ?)
	(not (lavoro))

	(car (nome presenza_rivestimento) (valore si))
	(not (car (nome muri_a_piombo) (valore ?)))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "I muri sono a piombo?"))
	(if ?risposta
		then (assert (car (nome muri_a_piombo) (valore si)))
		else (assert (car (nome muri_a_piombo) (valore no)))))



;/------------------------------------------/
;/-----------------LAVORI------------------/
;/----------------------------------------/
(defrule impianti_umidita
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome umidita) (valore si))
	(car (nome impianti_umidita) (valore si))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere causata dalle tubazioni, rimuovi pavimento e massetto (se presenti) con attenzione (per non provocare %ndanni alle tubature) e chiama uno specialista.")
	(assert (lavoro)))

(defrule umidita
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome umidita) (valore si))
	(car (nome impianti_umidita) (valore no))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere quella di risalita dai muri, consulta uno specialista.")
	(assert (lavoro)))

(defrule rifacimento_impianti
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome rifacimento_impianti) (valore si))

	=>
	(bind ?*soluzione* "Rimuovi il massetto e il pavimento (se presenti), poi consulta uno specialista per il rifacimento degli impianti.")
	(assert (lavoro)))

;---------------------------------------------------------------------------------------------------------------------------------------

(defrule ok_pavimento_interno1  ;pavimento non da raccordare
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome pavimento_da_raccordare) (valore no))
	(car (nome massetto_livello) (valore si))
	(car (nome massetto_friabile) (valore no))
	=>
	(bind ?*soluzione* "Il massetto su cui porre il pavimento è a livello, puoi effettuare la posa.")
	(assert (lavoro)))

(defrule ok_pavimento_interno2  ;pavimento da raccordare
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome massetto_altezza) (valore giusto))
	(car (nome massetto_friabile) (valore no))
	=>
	(bind ?*soluzione* "Il massetto su cui porre il pavimento ha la giusta altezza per la posa del pavimento selezionato, puoi iniziare la posa.")
	(assert (lavoro)))

(defrule ok_pavimento_esterno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome massetto_pendenza) (valore si))
	(car (nome presenza_pavimento) (valore no))
	(car (nome massetto_friabile) (valore no))
	=>
	(bind ?*soluzione* "Il massetto ha la giusta pendenza, puoi iniziare la posa sopra.")
	(assert (lavoro)))

(defrule massetto_friabile
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome massetto_friabile) (valore si))
	=>
	(bind ?*soluzione* "Il massetto non può stare in queste condizioni, procedi alla rimozione e al rifacimento.")
	(assert (lavoro)))

(defrule massetto_non_a_livello
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome massetto_livello) (valore no))
	=>
	(bind ?*soluzione* "Il massetto deve essere a livello per potervi effettuare la posa sopra, eliminare quello esistente e rifarlo.")
	(assert (lavoro)))

(defrule massetto_alto_basso
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(or (car (nome massetto_altezza) (valore alto))
		(car (nome massetto_altezza) (valore basso)))
	=>
	(bind ?*soluzione* "Il massetto deve essere all'altezza giusta, se troppo basso creare sopra un nuovo massetto; se troppo alto, rimuoverlo e rifarlo daccapo.")
	(assert (lavoro)))

(defrule massetto_non_pendenza
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome massetto_pendenza) (valore no))
	=>
	(bind ?*soluzione* "Il massetto non ha la giusta pendenza per favorire lo scolo dell'acqua, rimuoverlo e rifarlo daccapo.")
	(assert (lavoro)))

(defrule fai_massetto
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore no))
	(not (nome rifacimento_impianti) (valore no))
	(not (umid))

	)


;----------------------------------------------------------------------------------------------------------------------------------------
(defrule rattoppo_pavimento ;TODO da rivedere
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(or (car (nome piastrelle_sollevate_pavimento) (valore si))
		(car (nome piastrelle_scheggiate_pavimento) (valore si)))
	(or (car (nome pavimento_livello) (valore si))
		(car (nome pendenza_pavimento) (valore si)))
	=>
	(bind ?*soluzione* "Se il numero di piastrelle difettate non è elevato, si può optare per la loro sostituzione (rattoppo), facendo attenzione al fatto che %nnon vi sia differenza di tonalità tra le piastrelle nuove e quelle vecchie; altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(assert (lavoro)))

(defrule posa_sopra_interno ;TODO da rivedere
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(and (not (car (nome piastrelle_sollevate_pavimento) (valore no)))
		 (not (car (nome piastrelle_scheggiate_pavimento) (valore no))))
	(car (nome pavimento_livello) (valore si))
	=>
	(bind ?*soluzione* "Se si deve rinnovare il pavimento si può decidere anche per la posa sopra al pavimento esistente. Bisogna considerare però che vi sarà %nun innalzamento del pavimento che dovrà portare a diverse modifiche (è il caso delle porte), altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(assert (lavoro)))

(defrule posa_sopra_esterno ;TODO da rivedere
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(and (not (car (nome piastrelle_sollevate_pavimento) (valore no)))
		 (not (car (nome piastrelle_scheggiate_pavimento) (valore no))))
	(car (nome pendenza_pavimento) (valore si))
	=>
	(bind ?*soluzione* "Se si deve rinnovare il pavimento si può decidere anche per la posa sopra al pavimento esistente. Bisogna considerare però che vi sarà %nun innalzamento del pavimento che dovrà portare a diverse modifiche (è il caso delle porte), altrimenti procedere a sostituire il pavimento con uno nuovo.")
	(assert (lavoro)))

(defrule pavimento_non_a_livello
	(declare (salience ?*high_priority*))
	(not (lavoro))
	
	(car (nome pavimento_livello) (valore no))
	=>
	(bind ?*soluzione* "Il pavimento non è a livello, toglilo insieme allo strato di cemento sottostante (massetto) e rifallo.")
	(assert (lavoro)))

(defrule pavimento_non_pendenza
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome pendenza_pavimento) (valore no))
	=>
	(bind ?*soluzione* "Il pavimento non ha la pendenza giusta per garantire lo scolo dell'acqua, eliminalo insieme allo strato di cemento sottostante (massetto) e rifallo.")
	(assert (lavoro)))

;------------------------------------------------------------------------------------------------------------------------------------------------

(defrule spostamento_sanitari
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome spostamento_sanitari) (valore si))
	=>
	(bind ?*soluzione* "Bisogna rimuovere il pavimento esistente e lo strato di cemento sottostante (massetto), poi chiamare un idraulico per l'aggiustamento.")
	(assert (lavoro)))

(defrule muri_non_a_piombo
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome muri_a_piombo) (valore no))
	=>
	(bind ?*soluzione* "Raddrizzare i muri in modo che siano a piombo, poi procedere con la posa o con l'intonaco.")
	(assert (lavoro)))

(defrule rattoppo_rivestimento
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(or (car (nome piastrelle_scheggiate_rivestimento) (valore si))
		(car (nome piastrelle_sollevate_rivestimento) (valore si)))
	=>
	(bind ?*soluzione* "Se il numero delle piastrelle difettate non è elevato si può decidere per il rattoppo, altrimenti eliminare il rivestimento esistente e rifarlo.")
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
