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

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
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

	(car (nome luogo) (valore interno))
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
	(bind ?risposta (yes_or_no_p "Controlla che il massetto sia più basso di 1 - 1,5 cm ogni due metri lineari...%nÈ così?"))
	(if ?risposta
		then (assert (car (nome massetto_pendenza) (valore si)))
		else (assert (car (nome massetto_pendenza) (valore no)))))



;  /------------------------------------------/
; /-----------------PAVIMENTO----------------/
;/------------------------------------------/
(defrule domanda_piatrelle_scheggiate_pavimento
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome piastrelle_scheggiate_pavimento) (valore ?)))

	(car (nome presenza_pavimento) (valore si))
	=>
	(bind ?*help* "Una piastrella è scheggiata se la superficie presenta delle irregolarità causate generalmente da un urto con qualche oggetto.")
	(bind ?risposta (yes_or_no_p "È presente qualche piastrella scheggiata nel pavimento?"))
	(if ?risposta
		then (assert (car (nome piastrelle_scheggiate_pavimento) (valore si)))
		else (assert (car (nome piastrelle_scheggiate_pavimento) (valore no)))))

(defrule domanda_piastrelle_sollevate_pavimento
	(preparazione_utente ?)
	(not (lavoro))
	(not (car (nome piastrelle_sollevate_pavimento) (valore ?)))

	(car (nome presenza_pavimento) (valore si))
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

	(car (nome luogo) (valore interno))
	(car (nome tipo_stanza) (valore bagno))
	=>
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Devi cambiare la disposizione dei sanitari?"))
	(if ?risposta
		then (assert (nome spostamento_sanitari) (valore si))
		else (assert (nome spostamento_sanitari) (valore no))))

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