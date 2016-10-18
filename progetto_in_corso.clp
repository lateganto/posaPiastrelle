(defglobal ?*highest_priority* = 1000)
(defglobal ?*high_priority* = 100)
(defglobal ?*low_priority* = -100)
(defglobal ?*lowest_priority* = -1000)
(defglobal ?*help* = "")

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


;-----------------------------------------------------------------------------------------------------------------------------
;(deffacts fatti_iniziali
;	(utente_esperto TRUE)
;        (presenza_pavimento TRUE)
;        (decorazioni FALSE)
;        (formato_piastrella quadrata)
;        (disposizione dritta)
;        (interno TRUE)
;        (tipo_stanza bagno)
;        (presenza_rivestimento TRUE)
;        (dim_distanziatori 3)
;        (step2)
;        )
(deffacts fatti_iniziali
        (utente_esperto TRUE)
        (decorazioni FALSE)
        (formato_piastrella quadrata)
        (disposizione dritta)
        (dim_distanziatori 3)

        (presenza_pavimento TRUE)
        (interno TRUE)
        (tipo_stanza bagno)
        (presenza_rivestimento TRUE)
        
        (step2))


;-----------------------------------------------------------------------------------------------------------------------------
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

(defrule massetto_non_livello  ;il massetto non è a livello, aggiustarlo prima
        (massetto_livello FALSE)
	=>
	(printout t crlf "Il massetto non è a livello, quindi occorre prima aggiustarlo e poi proseguire." crlf)
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Hai aggiustato il massetto?"))
	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai aggiustato il massetto?")))
	(assert (ok_inizio_pavimento)))

;TODO chiedere meglio a papà come si controllano i muri 
(defrule domanda_controllo_muri_rivestimento  ;il rivestimento non c'è e si è deciso di farlo, allora si controllano se i muri sono a piombo
	(presenza_rivestimento FALSE)
	(rivestimento TRUE)
	=>
	(printout t crlf "Controllo se i muri sono a piombo..." crlf)
        (bind ?risposta (yes_or_no_p "I muri sono a piombo?"))

        (while (not ?risposta) do 
                (printout t crlf "Riparare i muri!" crlf)
                (bind ?risposta (yes_or_no_p "I muri sono a piombo?")))
        (assert (ok_inizio_rivestimento)))


;-----------------Domande per calcolo piastrelle da prendere---------------



;-----------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------------------------------------------