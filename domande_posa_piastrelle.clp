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
	(preparazione_utente alta)
	(not (lavoro))
	(not (car (nome impianti_umidita) (valore ?)))

	(car (nome umidita) (valore si))
	=>
	(bind ?*help* "")
	(bind ?risposta (ask_question "Passano dei tubi di acqua, fognatura o riscaldamento sotto il pavimento o nei muri?" si no non_so))
	(if (eq ?risposta no)
		then (assert (car (nome impianti_umidita) (valore no)))
		else (assert (car (nome impianti_umidita) (valore si)))))




;  /------------------------------------------/
; /-----------------MASSETTO-----------------/
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



;  /------------------------------------------/
; /-----------------PAVIMENTO----------------/
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


;  /----------------------------------------------/
; /------------------RIVESTIMENTO----------------/
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