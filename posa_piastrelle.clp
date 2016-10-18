(defglobal ?*highest_priority* = 1000)
(defglobal ?*high_priority* = 100)
(defglobal ?*low_priority* = -100)
(defglobal ?*lowest_priority* = -1000)
(defglobal ?*help* = "")

;-----------TEMPLATE----------
(deftemplate esperienza
	(slot valore))

(deftemplate domande_poste
	(slot numero))

(deftemplate domanda
	(slot numero))

;--------------FUNCTIONS------------
(deffunction ask_question (?question $?allowed_values)
	(insert$ ?allowed_values 1 help h)
	(printout t ?question "/help/h): ")
	(bind ?answer (read))

	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

	(while (not (member ?answer ?allowed_values)) do
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "No help found!" crlf)
		  				else (printout t ?*help* crlf)))
		(printout t ?question "/help/h): ")
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
	(printout t ?question " (help/h): ")
	(bind ?answer (read))

	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

	(while (not (numberp ?answer)) do  ;check if answer is a NUMBER
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "No help found!" crlf)
		  				else (printout t ?*help* crlf)))
		(printout t ?question " (help/h): ")
	    (bind ?answer (read)))
	 ?answer)

(deffunction domanda_random ()
	(bind ?n (random 1 8))
	(while (any-factp ((?domanda domanda)) (= ?domanda:numero ?n)) do
		(bind ?n (random 1 8)))
	(return ?n))

(deffunction esperienza_binario
	(?risposta)
	(if ?risposta
		then (return 1)
		else (return 0)))

(deffunction calcola_esperienza_utente
	(?num)
	(if (> ?num 3)
		then (return intermedio)
		else (return principiante)))


;--------------START---------------

;--------------Domande per capire il tipo di utente-------------
(defrule inizio
	(declare (salience ?*highest_priority*))
	=>
	(printout t crlf "*** Un sistema per la posa di pavimenti e rivestimenti in gres porcellanato ***" crlf crlf))

(defrule chiedi_esperto
	=>
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Hai mai realizzato prima d'ora la posa di un pavimento?"))
	(assert (utente_esperto ?risposta)))

(defrule chiedi_princ_interm
	?u <- (utente_esperto FALSE)
	(not (inizia_domande))
	=>
	(retract ?u)
	(assert (inizia_domande))
	(assert (esperienza (valore 0)))
	(assert (domande_poste (numero 0))))

(defrule chiedi_domanda_princ_interm
	(inizia_domande)
	(not (domande_poste (numero 5)))
	=>
	(bind ?n (domanda_random))
	(assert (question ?n)))

(defrule domanda_random1
	?d <- (question 1)
	?i <- (inizia_domande)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?i)
	(retract ?d)
	(assert (domanda (numero 1)))
	(assert (inizia_domande))   ;;;controllare se corretto!!
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Sai cos'è un frattazzo dentellato?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random2
	?d <- (question 2)
	?i <- (inizia_domande)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?i)
	(retract ?d)
	(assert (domanda (numero 2)))
	(assert (inizia_domande))
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Hai mai usato la livella?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random3
	?d <- (question 3)
	?i <- (inizia_domande)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?i)
	(retract ?d)
	(assert (domanda (numero 3)))
	(assert (inizia_domande))
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Sai calcolare se un muro è a squadro?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random4
	?d <- (question 4)
	?i <- (inizia_domande)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?i)
	(retract ?d)
	(assert (domanda (numero 4)))
	(assert (inizia_domande))
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Sai cos'è la mazza in gomma?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random5
	?d <- (question 5)
	?i <- (inizia_domande)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?i)
	(retract ?d)
	(assert (domanda (numero 5)))
	(assert (inizia_domande))
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Sai cosa è un distanziatore e a che serve?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random6
	?d <- (question 6)
	?i <- (inizia_domande)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?i)
	(retract ?d)
	(assert (domanda (numero 6)))
	(assert (inizia_domande))
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Sai a cosa serve la tenaglia da piastrellista?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random7
	?d <- (question 7)
	?i <- (inizia_domande)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?i)
	(retract ?d)
	(assert (domanda (numero 7)))
	(assert (inizia_domande))
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Sai usare una tagliapiastrelle?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random8
	?d <- (question 8)
	?i <- (inizia_domande)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?i)
	(retract ?d)
	(assert (domanda (numero 8)))
	(assert (inizia_domande))
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Hai mai usato una smerigliatrice angolare?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule definisci_utente
	?a <- (domande_poste (numero 5))
	?b <- (esperienza (valore ?x))
	?c <- (inizia_domande)
	=>
	(do-for-all-facts ((?domanda domanda)) TRUE (retract ?domanda))
	(retract ?a ?b ?c)
	(assert (utente (calcola_esperienza_utente ?x))))


;----------DOMANDE PER INQUADRARE LA SITUAZIONE----------

;-------------1° step-------------
(defrule domanda_interno_esterno
	(declare (salience ?*low_priority*))
	(not (interno ?))
	=>
	(bind ?*help* "Dipende dal fatto che il pavimento potrebbe essere esposto agli agenti atmosferici oppure no e quindi richiede alcune accortezze, come l'uso di piastrelle apposite e colle antigelive.")
	(bind ?risposta (yes_or_no_p "Il lavoro è per interno?"))
	(assert (interno ?risposta)))

(defrule domanda_tipo_stanza
	(declare (salience ?*low_priority*))
	(not (tipo_stanza ?))
	(interno TRUE)
	=>
	(bind ?*help* "A seconda del tipo di stanza potrebbe essere richiesto di effettuare un lavoro diverso.")
	(bind ?risposta (ask_question "Indicare in quale stanza si deve effettuare la posa? (bagno/cucina/altro" bagno cucina altro))
	(assert (tipo_stanza ?risposta)))

(defrule domanda_formato_piastrella
	(declare (salience ?*low_priority*))
	(not (formato_piastrella ?))
	=>
	(bind ?*help* "In base al formato della piastrella alcuni tipi di posa non sono realizzabili.")
	(bind ?risposta (ask_question "Qual è il formato della piastrella? (quadrata/rettangolare" quadrata rettangolare))
	(assert (formato_piastrella ?risposta)))

(defrule domanda_disposizione_piastrella_quadrata
	(declare (salience ?*low_priority*))
	(formato_piastrella quadrata)
	(not (disposizione ?))
	=>
	(bind ?*help* "") ;TODO far vedere immagini
	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle? (dritta/sfalsata/diagonale" dritta sfalsata diagonale))
	(assert (disposizione ?risposta)))

(defrule domanda_disposizione_piastrella_rettangolare
	(declare (salience ?*low_priority*))
	(formato_piastrella rettangolare)
	(not (disposizione ?))
	=>
	(bind ?*help* "") ;TODO far vedere immagini
	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle? (dritta/sfalsata/spina_di_pesce_dritta/spina_di_pesce_obliqua" dritta sfalsata spina_di_pesce_dritta spina_di_pesce_obliqua))
	(assert (disposizione ?risposta)))

;TODO decidere se inserire, a causa delle disposizioni oblique che fanno cambiare come si deve porre la decorazione all'inizio!!
(defrule domanda_decorazioni
	(declare (salience ?*low_priority*))
	(not (decorazioni ?))
	=>
	(bind ?*help* "In caso di presenza di realizzazione di un disegno con le piastrelle bisognerà partire proprio dalla loro posa e poi continuare con il resto del pavimento, che verrà raccordato ad esso.")
	(bind ?risposta (yes_or_no_p "C'è da porre un rosone o un disegno in particolare?"))
	(assert (decorazioni ?risposta)))

(defrule domanda_presenza_pavimento
	(declare (salience ?*low_priority*))
	(not (presenza_pavimento ?))
	=>
	(bind ?*help* "Si può decidere di posare anche su un pavimento già esistente.")
	(bind ?risposta (yes_or_no_p "E' già presente un pavimento?"))
	(assert (presenza_pavimento ?risposta)))

(defrule domanda_presenza_rivestimento
	(declare (salience ?*low_priority*))
	(not (presenza_rivestimento ?))
	(or (tipo_stanza bagno)
		(tipo_stanza cucina))
	=>
	(bind ?*help* "Il rivestimento andrà rimosso per far spazio a quello nuovo")
	(bind ?risposta (yes_or_no_p "E' già presente un rivestimento?"))
	(assert (presenza_rivestimento ?risposta)))

(defrule domanda_distanziatori
	(declare (salience ?*lowest_priority*))
	(not (dim_distanziatori ?))
	=>
	(bind ?*help* "I distanziatori sono quei piccoli pezzi di plastica con forma a T o a croce che si pongono tra due piastrelle in modo da mantenere sempre la stessa distanza.")
	(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori in millimetri?"))
	(while (or (< ?risposta 1) (> ?risposta 10)) do 
		(printout t crlf "La dimensione deve essere compresa tra 1 e 10" crlf)
		(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori in millimetri?")))
	(assert (dim_distanziatori ?risposta))
	(assert (step2)))  ;prosegui alla successiva fase


;----------------2° step-------------
(defrule attrezzi_necessari_rivestimento
	(declare (salience ?*high_priority*))
	(rivestimento TRUE)
	=>
	(printout t crlf crlf "Assicurati di procurarti tutti questi attrezzi: " crlf
					"	* tagliapiastrelle (manuale o elettrica)" crlf
					"	* smerigliatrie angolare (grande e piccola)" crlf
					"	* tenaglia per piastrelle" crlf
					"	* 2-3 cazzuole (almeno una piccola a punta)" crlf
					"	* colla" crlf
					"	* fugante" crlf
					"	* spatola liscia" crlf
					"	* frattazzo dentellato" crlf
					"	* 2-3 secchi da muratore" crlf
					"	* stadie di alluminio (varie dimensioni da 1 fino a 3 metri)" crlf
					"	* mazza in gomma" crlf
					"	* frattazzo in pugna" crlf
					"	* secchio lavaggio per piastrellisti" crlf
					"	* distanziatori" crlf
					"	* squadra in acciaio per carpentieri" crlf
					"	* livella" crlf
					"	* matite in legno da muratori" crlf
					"	* profili angolari (in numero pari agli angoli presenti nella stanza)" crlf
					"	* filo a piombo" crlf crlf))

(defrule attrezzi_necessari
	(declare (salience ?*high_priority*))
	(rivestimento FALSE)
	=>
	(printout t crlf crlf "Assicurati di procurarti tutti questi attrezzi: " crlf
					"	* tagliapiastrelle (manuale o elettrica)" crlf
					"	* smerigliatrie angolare (grande e piccola)" crlf
					"	* tenaglia per piastrelle" crlf
					"	* 2-3 cazzuole (almeno una piccola a punta)" crlf
					"	* colla" crlf
					"	* fugante" crlf
					"	* spatola liscia" crlf
					"	* frattazzo dentellato" crlf
					"	* 2-3 secchi da muratore" crlf
					"	* stadie di alluminio (varie dimensioni da 1 fino a 3 metri)" crlf
					"	* mazza in gomma" crlf
					"	* frattazzo in pugna" crlf
					"	* secchio lavaggio per piastrellisti" crlf
					"	* distanziatori" crlf
					"	* squadra in acciaio per carpentieri" crlf
					"	* livella" crlf
					"	* matite in legno da muratori" crlf crlf))

(defrule esterno_no_rivestimento ;se esterno allora nessun rivestimento
	?f <- (step2)
	(interno FALSE)
	=>
	(retract ?f)
	(assert (pavimento TRUE))
	(assert (rivestimento FALSE)))

(defrule stanze_no_rivestimento ;se stanza diversa da cucina o bagno allora niente rivestimento
	?f <- (step2)
	(tipo_stanza altro)
	=>
	(retract ?f)
	(assert (pavimento TRUE))
	(assert (rivestimento FALSE)))

(defrule domanda_rivestimento_pavimento ;domanda riguardo a cosa effettuare (pavimento, rivestimento o entrambi) nel caso di cucina o bagno
	?f <- (step2)
	(or (tipo_stanza bagno)
		(tipo_stanza cucina))
	=>
	(retract ?f)
	(bind ?*help* "Scegliere 'rivestimento' se il pavimento è in buono stato e non si desidera modificarlo, scegliere 'pavimento', se si vuole realizzare solo il pavimento, scegliere 'entrambi', se si vuole realizzare sia il pavimento che il rivestimento.")
	(bind ?risposta (ask_question "Cosa devi realizzare? (rivestimento/pavimento/entrambi" rivestimento pavimento entrambi))
	(switch ?risposta
		(case rivestimento then (assert (rivestimento TRUE) (pavimento FALSE)))
		(case pavimento then (assert (pavimento TRUE) (rivestimento FALSE)))
		(case entrambi then (assert (pavimento TRUE) (rivestimento TRUE)))))

(defrule rimozione_rivestimento  ;se è presente un rivestimento e quello che voglio fare è il rivestimento, allora bisogna toglierlo
	?f <- (presenza_rivestimento TRUE)
	(rivestimento TRUE)
	=>
	(printout t crlf "Procedi alla rimozione del rivestimento" crlf)
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Hai rimosso il rivestimento?"))
	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rimosso il rivestimento?")))
	(retract ?f)                                   ;il rivestimento
	(assert (presenza_rivestimento FALSE)))        ;è stato tolto

(defrule domanda_rivestimento_cucina  ;chiedere se fare il rivestimento di tutta la stanza o solo la parete dove sta la cucina o solo la fascia di parete visibile dietro la cucina
	(tipo_stanza cucina)
	(rivestimento TRUE)
	=>
	(printout t crlf "La stanza in cui fare il lavoro è una cucina ed è stato scelto di effettuare il rivestimento" crlf)
	(bind ?*help* = "Si può decidere di rivestire solo la parete (o le pareti) dove andrà posta la cucina in modo da poter pitturare come si vuole le pareti restanti, oppure si potrebbe decidere di rivestire le pareti in un altro modo.")
	(bind ?risposta (ask_question "Vuoi rivestire tutta le pareti della cucina o solo la parete (o le pareti) dove andrà posta la cucina? (tutta/solo_parete" tutta solo_parete))
	(assert (rivestimento_cucina ?risposta)))

(defrule domanda_posa_sopra_pavimento  ;se il pavimento è presente e si è scelto di porre un nuovo pavimento, chiedere se fare la posa sopra il pavimento esistente
	(pavimento TRUE)
	(presenza_pavimento TRUE)
	=>
	(bind ?*help* "Effettuare la posa del nuovo pavimento sopra uno già esistente. Tuttavia occorre valutare bene la scelta poichè si dovrebbero fare alcune modifiche alle porte, in quanto il piano verrà rialzato. Inoltre ci potrebbe essere un dislivello nel caso in cui il pavimento da porre nella stanza è collegato con un altro.")
	(bind ?risposta (yes_or_no_p "Vuoi effettuare la posa sopra il pavimento esistente?"))
	(assert (posa_sopra_pavimento ?risposta)))

(defrule domanda_condizioni_pavimento  ;se si è scelti la posa sopra, verificare le condizioni del pavimento presente
	(posa_sopra_pavimento TRUE)
	=>
	(bind ?*help* "")
	(printout t crlf "Guarda con attenzione in ogni punto il pavimento già presente..." crlf)
	(bind ?risposta (yes_or_no_p "Ci sono piastrelle rialzate o non perfettamente aderenti?"))
	(assert (piastrelle_sollevate ?risposta))

        (bind ?*help* "")
        (printout t crlf "Posa una stadia sul pavimento da un angolo all'opposto facendo in modo che poggi bene. Poni su di essa un livello..." crlf
                "Controlla se ci sono punti in cui la stadia si allontana dal pavimento di diversi centimetri..." crlf
                "Controlla se la bolla d'aria sul livello si trova nella posizione centrale..." crlf
                "Ripeti l'operazione diverse volte in modo da coprire tutta l'area da pavimentare e poi rispondi alle seguenti domande..." crlf)
        (bind ?risposta1 (yes_or_no_p "Nelle varie misurazioni fatte ci sono stati casi in cui la stadia era lontana dal pavimento di diversi centimetri?"))
        (bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. Non interessa il verso.")
        (bind ?risposta2 (yes_or_no_p "Nelle varie misurazioni fatte la bolla d'aria sulla livella era sempre nella posizione centrale?"))
	(if (and (not ?risposta1) ?risposta2) then (assert (pavimento_livello TRUE)) else (assert (pavimento_livello FALSE))))

;TODO ampliamento: come si fa la malta cementizia e come riempire i buchi
(defrule piastrelle_sollevate ;se ci sono piastrelle sollevate falle aggiustare e poi inizia la posa
	?f <- (piastrelle_sollevate TRUE)
	?f1 <- (pavimento_livello TRUE)  ;il pavimento deve essere a livello
	=>
	(printout t crlf "Procedere alla rimozione di tali piastrelle rialzate o non aderenti e riempire i buchi creati con malta cementizia." crlf)
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Hai rattoppato il vuoto creato dalle piastrelle eliminate?"))
	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rattoppato il vuoto creato dalle piastrelle eliminate?")))
	(retract ?f ?f1) ;piastrelle non più sollevate
	(assert (ok_inizio_pavimento)))

(defrule piastrelle_non_sollevate ;se le piastrelle non sono sollevate e il pavimento è a livello procedi con la posa del pavimento
	?f <- (piastrelle_sollevate FALSE)
	?f1 <- (pavimento_livello TRUE)
	=>
	(retract ?f ?f1)
	(assert (ok_inizio_pavimento)))

;TODO spiegazione: spiegare come rimuovere pavimento 
(defrule pavimento_non_livello ;non si può fare la posa sopra il pavimento perchè non a livello
	?f1 <- (pavimento_livello FALSE)
	?f <- (presenza_pavimento TRUE)
	=>
	(printout t crlf "Il pavimento non è in condizioni tali da potervi effettuare una posa sopra. Procedere alla rimozione e proseguire." crlf)
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Hai rimosso il pavimento?"))
	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rimosso il pavimento?")))
	(retract ?f ?f1)  ;rimuovo il pavimento
	(assert (presenza_pavimento FALSE)))

;TODO spiegazione: spiegare come rimuovere pavimento 
(defrule posa_sopra_pavimento_false ;se non si opta per la posa sopra si elimina il pavimento
	?f1 <- (posa_sopra_pavimento FALSE)
	?f <- (presenza_pavimento TRUE)
	=>
	(printout t crlf "Occorre procedere alla rimozione del pavimento..." crlf)
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Hai rimosso il pavimento?"))
	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rimosso il pavimento?")))
	(retract ?f ?f1)  ;rimuovo il pavimento
	(assert (presenza_pavimento FALSE)))

(defrule domanda_controllo_massetto ;se il pavimento non c'è e si è scelti di effettuare la posa del pavimento, allora controllare il massetto
	(presenza_pavimento FALSE)
	(pavimento TRUE)
	=>
	(bind ?*help* "")
	(printout t crlf "Posa una stadia sul pavimento da un angolo all'opposto facendo in modo che poggi bene. Poni su di essa un livello..." crlf
                "Controlla se ci sono punti in cui la stadia si allontana dal pavimento di diversi centimetri..." crlf
                "Controlla se la bolla d'aria sul livello si trova nella posizione centrale..." crlf
                "Ripeti l'operazione diverse volte in modo da coprire tutta l'area da pavimentare e poi rispondi alle seguenti domande..." crlf)
	(bind ?risposta1 (yes_or_no_p "Nelle varie misurazioni fatte ci sono stati casi in cui la stadia era lontana dal pavimento di diversi centimetri?"))
	(bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. Non interessa il verso.")
	(bind ?risposta2 (yes_or_no_p "Nelle varie misurazioni fatte la bolla d'aria sulla livella era sempre nella posizione centrale?"))
	(if (and (not ?risposta1) ?risposta2) then (assert (massetto_livello TRUE)) else (assert (massetto_livello FALSE))))

(defrule massetto_a_livello  ;il massetto è a livello, quindi inizio
	?f <- (massetto_livello TRUE)
	=>
        (retract ?f)
	(assert (ok_inizio_pavimento)))

;TODO ampliamento: aggiungere come si aggiusta il massetto?
(defrule massetto_non_livello  ;il massetto non è a livello, aggiustarlo prima
        (massetto_livello FALSE)
	=>
	(printout t crlf "Il massetto non è a livello, quindi occorre prima aggiustarlo e poi proseguire." crlf)
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Hai aggiustato il massetto?"))
	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai aggiustato il massetto?")))
	(assert (ok_inizio_pavimento)))

;TODO ampliamento: aggiungere come si aggiusta un muro?
(defrule domanda_controllo_muri_rivestimento  ;il rivestimento non c'è e si è deciso di farlo, allora si controllano se i muri sono a piombo
	(presenza_rivestimento FALSE)
	(rivestimento TRUE)
	=>
	(printout t crlf "Controllo se i muri sono a piombo..." crlf
			"Prendi il filo a piombo. Prendi la rocchetta di cui è dotato e poggiala sulla parete da misurare..." crlf
			"Vedi se il piombo (il peso) alla fine del filo è lontano dal muro, troppo vicino oppure si muove liberamente..." crlf
			"Ripeti la procedura per ogni parete della stanza su cui stai lavorando...")
        (bind ?risposta (yes_or_no_p "Tutti i muri sono a piombo?"))
        (while (not ?risposta) do 
                (printout t crlf "Non puoi proseguire nella posa del rivestimento. Devi prima riparare i muri!" crlf)
                (bind ?risposta (yes_or_no_p "Tutti i muri sono a piombo?")))
        (assert (ok_inizio_rivestimento)))


;(defrule pavimento_e_rivestimento  ;chiedere all'utente cosa fare prima, nel caso in cui ci sia da fare sia pavimento che rivestimento
;	(rivestimento TRUE)
;	(pavimento TRUE)
;	(pavimento)
;	) 
;-----------------INIZIO-------------------
;(defrule attrezzi_necessari
;	?i <- (inizio_posa)
;	?r <- (rivestimento ?v)
;	=>
;	(retract ?i)
;	(printout t crlf crlf "Assicurati di procurarti tutti questi attrezzi: " crlf
;					"	* tagliapiastrelle (manuale o elettrica)" crlf
;					"	* smerigliatrie angolare (grande e piccola)" crlf
;					"	* tenaglia per piastrelle" crlf
;					"	* 2-3 cazzuole (almeno una piccola a punta)" crlf
;					"	* colla" crlf
;					"	* fugante" crlf
;					"	* spatola liscia" crlf
;					"	* frattazzo dentellato" crlf
;					"	* 2-3 secchi da muratore" crlf
;					"	* stadie di alluminio (varie dimensioni da 1 fino a 3 metri)" crlf
;					"	* mazza in gomma" crlf
;					"	* frattazzo in pugna" crlf
;					"	* secchio lavaggio per piastrellisti" crlf
;					"	* distanziatori" crlf
;					"	* squadra in acciaio per carpentieri" crlf
;					"	* livella" crlf
;					"	* matite legno da muratori" crlf)
;	(if ?v then (printout t 
;					"	* profili angolari (in numero pari agli angoli presenti nella stanza)" crlf
;					"	* piombo" crlf))
;	(printout t crlf)
;	(assert (calcolo_metri_quadri)))
					

;(defrule calcolo_metri_quadri_solo_pavimento
;	(declare (salience ?*low_priority*))
;	?f <- (calcolo_metri_quadri)
;	=>
;	(retract ?f)
;	(printout t crlf "Adesso si può procedere al calcolo della dimensione dell'area da pavimentare." crlf)
;	(printout t	"A tale proposito, bisogna analizzare l'area. Le misure non devono essere precise, " crlf
;		"ad esempio non si considerano eventuali elementi, in corrispondenza dei quali non " crlf
;		"si poserà il pavimento (come pilastri, caminetti o altro);" crlf
;		"mentre invece si considerano eventuali rientranze da pavimentare (come ripostigli e disimpegni)." crlf crlf
;		"Dopo aver individuato lo spazio da pavimentare, analizziamone la forma." crlf 
;		"Se questa si può ricondurre a una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, " crlf
;		"allora è semplice calcolarla, ricordando che: " crlf
;		"	* l'area di un quadrato si calcola elevando al quadrato il lato" crlf
;		"	* l'area di un rettangolo si calcola moltiplicando la larghezza per la lunghezza" crlf
;		"	* l'area di un triangolo si calcola moltiplicando la base per l'altezza e dividendo tutto per due" crlf
;		"	* l'area di una cerchio si calcola con la formula 2πr, dove π = 3.14 e r = al raggio della circonferenza" crlf
;		"	* l'area di una semicirconferenza si calcola con la formula (2πr)/2." crlf
;		"Altrimenti si suddivide l'area in parti più piccole dalla forma riconducibile ad una di quelle precedenti 
;		e si sommano i vari valori ottentuti." crlf 
;		"Le misure ottenute sono espresse in metri quadri." crlf crlf)
;
;	(bind ?*help* "")
;	(bind ?risposta (ask_number "Qual è quindi la dimensione in metri quadri dell'area da pavimentare?"))
;	(assert (dimensioni_pavimento ?risposta))
;	(assert (controllo_superficie)))
;
;(defrule calcolo_metri_quadri_rivestimento
;	(declare (salience ?*low_priority*))
;	?f <- (calcolo_metri_quadri)
;	(rivestimento TRUE)
;	=>
;	(retract ?f)
;	(printout t crlf "Adesso si può procedere al calcolo della dimensione dell'area da pavimentare per sapere quante 
;		piastrelle prendere." crlf)
;	(printout t	"A tale proposito, bisogna analizzare l'area. Le misure non devono essere precise, " crlf
;		"ad esempio non si considerano eventuali elementi, in corrispondenza dei quali non " crlf
;		"si poserà il pavimento (come pilastri, caminetti o altro);" crlf
;		"mentre invece si considerano eventuali rientranze da pavimentare (come ripostigli e disimpegni)." crlf crlf
;		"Dopo aver individuato lo spazio da pavimentare, analizziamone la forma." crlf 
;		"Se questa si può ricondurre a una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, " crlf
;		"allora è semplice calcolarla, ricordando che: " crlf
;		"	* l'area di un quadrato si calcola elevando al quadrato il lato" crlf
;		"	* l'area di un rettangolo si calcola moltiplicando la larghezza per la lunghezza" crlf
;		"	* l'area di un triangolo si calcola moltiplicando la base per l'altezza e dividendo tutto per due" crlf
;		"	* l'area di una cerchio si calcola con la formula 2πr, dove π = 3.14 e r = al raggio della circonferenza" crlf
;		"	* l'area di una semicirconferenza si calcola con la formula (2πr)/2." crlf
;		"Altrimenti si suddivide l'area in parti più piccole dalla forma riconducibile ad una di quelle precedenti 
;		e si sommano i vari valori ottentuti." crlf 
;		"Le misure ottenute sono espresse in metri quadri." crlf crlf)
;
;	(bind ?*help* "")
;	(bind ?risposta (ask_number "Qual è quindi la dimensione in metri quadri dell'area da pavimentare?"))
;	(assert (dimensioni_pavimento ?risposta))
;
;	(printout t crlf "In modo analogo si deve calcolare anche l'area complessiva del rivestimento." crlf)
;	(bind ?risposta (ask_number "Qual è quindi la dimensione in metri quadri dell'area da rivestire?"))
;	(assert (dimensioni_rivestimento ?risposta))
;	(assert (controllo_superficie)))
;
;
;;REGOLA QUANTITA' PIASTRELLE DA PRENDERE
;(defrule quantità_piastrelle_pavimento
;	(declare (salience ?*low_priority*))
;	(disposizione ?disp)
;	(dimensioni_pavimento ?dim)
;	=>
;	(switch ?disp
;		(case diagonale then (format t "Devi prendere %d metri quadri%n!" (* ?dim 1.15)))
;		(case sfalsata then (format t "Devi prendere %d metri quadri%n!" (* ?dim 1.10)))
;		(case spina_di_pesce_dritta then (format t "Devi prendere %d metri quadri%n!" (* ?dim 1.10)))
;		(case spina_di_pesce_obliqua then (format t "Devi prendere %d metri quadri%n!" (* ?dim 1.10)))
;		(case dritta then (format t "Devi prendere %d metri quadri%n!" (* ?dim 1.10)))))
;
;;(defrule stanza_piccola
;;	(declare (salience ?*low_priority*))
;;	(dimensioni_pavimento ?dim)
;;	(< ?dim 25)
;;	=>
;;	(printout t crlf "Non conviene usare una piastrella troppo grande poichè farebbe sembrare la stanza troppo piccola" crlf))
;;
;;(defrule stanza_grande
;;	(declare (salience ?*low_priority*))
;;	(dimensioni_pavimento ?dim)
;;	=>
;;	(if (> ?dim 100) then (printout t crlf "Non conviene usare una piastrella troppo piccola poichè farebbe sembrare la stanza troppo grande" crlf)))
;
;(defrule rivestimento_da_togliere
;	(declare (salience ?*lowest_priority*))
;	(presenza_rivestimento TRUE)
;	(rivestimento TRUE)
;	=>
;	(printout t crlf "Procedere alla rimozione del rivestimento." crlf ))
;
;(defrule domanda_posa_sopra_pavimento
;	(declare (salience ?*low_priority*))
;	(presenza_pavimento TRUE)
;	(controllo_superficie)
;	=>
;	(printout t crlf "E' stato rilevato che un altro pavimento è presente." crlf)
;	(bind ?*help* "La posa sopra il pavimento si realizza posando il nuovo pavimento sopra quello già esistente. 
;		Bisognerà valutare bene questa scelta poichè bisognerà fare alcune modifiche come un accorciamento delle 
;		porte, inoltre bisogna controllare che non vi siano piastrelle rialzate e che il pavimento sia a livello")
;	(bind ?risposta (yes_or_no_p "Si vuole procedere alla posa sopra il pavimento esistente?")) 
;	(assert (posa_sopra_pavimento ?risposta)))
;
;(defrule controllo_condizioni_pavimento
;	(declare (salience ?*low_priority*))
;	(posa_sopra_pavimento TRUE)
;	=>
;	(bind ?*help* "")
;	(printout t crlf "Guarda con attenzione in ogni punto il pavimento già presente..." crlf)
;	(bind ?risposta (yes_or_no_p "Ci sono piastrelle rialzate o non perfettamente aderenti?"))
;	(assert (piastrelle_sollevate ?risposta))
;
;	(bind ?*help* "")
;	(printout t crlf "Posa una stadia da un angolo all'opposto e poni su di essa un livello. Ripeti l'operazione in 
;		diversi punti della stanza..." crlf)
;	(bind ?risposta1 (yes_or_no_p "La stadia poggia perfettamente sul pavimento in tutte le misurazioni?"))
;	(printout t crlf "Controlla la bolla d'aria presente sulla livella..." crlf)
;	(bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. 
;		Non interessa il verso.")
;	(bind ?risposta2 (yes_or_no_p "La bolla d'aria sulla livella è in posizione centrale?"))
;	(if (and ?risposta1 ?risposta2) then (assert (pavimento_non_livello FALSE)) else (assert (pavimento_non_livello TRUE))))
;
;(defrule pavimento_da_togliere_piastrelle_sollevate
;	(declare (salience ?*low_priority*))
;	?f <- (piastrelle_sollevate TRUE)
;	(pavimento_non_livello FALSE)
;	=>
;	(retract ?f)
;	(printout t crlf "Il pavimento versa in condizioni non idonee per effettuarci una posa sopra. Prima di procedere 
;		nella posa " crlf 
;				"occorre togliere le piastrelle non perfettamente aderenti e procedere ad un riempimento 
;				con malta cementizia dei buchi creati." crlf)
;	(assert (pavimento_rattoppo)))
;
;(defrule domanda_pavimento_riparato
;	?f1 <- (pavimento_rattoppo)
;	?f2 <- (controllo_superficie)
;	=>
;	(bind ?*help* "")
;	(bind ?risposta (yes_or_no_p "Hai provveduto a rattoppare il pavimento?"))
;	(while (neq ?risposta TRUE) do
;		(printout t "Togli le piastrelle non aderenti o sollevate e procedi al riempimento con malta cementizia 
;			dei buchi" crlf)
;		(bind ?risposta (yes_or_no_p "Hai finito con i rattoppi?")))
;	(retract ?f1 ?f2)
;	(assert (partenza)))
;
;;inserire retract se non vuole più porre sopra il pavimento vecchio
;
;(defrule pavimento_da_togliere_non_livello
;	(declare (salience ?*low_priority*))
;	?f1 <- (posa_sopra_pavimento TRUE)
;	;?f2 <- (presenza_pavimento TRUE)
; 	?f3 <- (pavimento_non_livello TRUE)
;	=>
;	(retract ?f1 ?f3)
;	(printout t crlf "Il pavimento versa in condizioni non idonee per effettuarci una posa sopra. Procedere nella " crlf
;					"rimozione e proseguire al controllo del massetto sottostante" crlf))
;
;(defrule pavimento_buone_condizioni
;	?f1 <- (piastrelle_sollevate FALSE)
;	?f2 <- (pavimento_non_livello FALSE)
;	=>
;	(retract ?f1 ?f2)
;	(assert (partenza)))
;
;(defrule domanda_condizioni_massetto
;	(declare (salience ?*lowest_priority*))
;	?f <- (controllo_superficie)
;	=>
;	(retract ?f)
;
;	(bind ?*help* "")
;	(printout t crlf "Posa una stadia da un angolo all'opposto e poni su di essa un livello. Ripeti l'operazione in 
;		diversi punti della stanza..." crlf)
;	(bind ?risposta1 (yes_or_no_p "La stadia poggia perfettamente sul pavimento in tutte le misurazioni?"))
;	(printout t "Controlla la bolla d'aria presente sulla livella..." crlf)
;	(bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. Non 
;		interessa il verso.")
;	(bind ?risposta2 (yes_or_no_p "La bolla d'aria sulla livella è in posizione centrale?"))
;	(if (and ?risposta1 ?risposta2) then (assert (rifare_massetto FALSE)) else (assert (rifare_massetto TRUE))))
;
;(defrule rifare_massetto
;	(declare (salience ?*low_priority*))
;	?f <- (rifare_massetto TRUE)
;	=>
;	(retract ?f)
;	(printout t crlf "Il massetto non è in condizione di potervi effettuare la posa di un pavimento sopra! Procedi 
;		prima a rifare il massetto e poi riprendi.")
;	(assert (partenza)))
;
;(defrule massetto_buone_condizioni
;	(declare (salience ?*low_priority*))
;	?f <- (rifare_massetto FALSE)
;	=>
;	(retract ?f)
;	(assert (partenza)))
;
;(defrule partenza
;	(declare (salience ?*low_priority*))
;	(partenza)
;	=>
;	(printout t "partenzaaaaaa"))