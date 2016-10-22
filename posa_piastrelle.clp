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
	(printout t crlf ?question "/help/h): ")
	(bind ?answer (read))

	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

	(while (not (member ?answer ?allowed_values)) do
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "Non è presente alcun help!" crlf)
		  				else (printout t ?*help* crlf)))
		(printout t crlf ?question "/help/h): ")
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
	(printout t crlf ?question " (help/h): ")
	(bind ?answer (read))

	(if (lexemep ?answer)
		then (bind ?answer (lowcase ?answer)))

	(while (not (numberp ?answer)) do  ;check if answer is a NUMBER
		(if (or (eq ?answer help) (eq ?answer h))
	  			then (if (eq (length$ ?*help*) 0)
		  				then (printout t "Non è presente alcun help!" crlf)
		  				else (printout t ?*help* crlf)))
		(printout crlf t ?question " (help/h): ")
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

(defrule domanda_presenza_pavimento
	(declare (salience ?*low_priority*))
	(not (presenza_pavimento ?))
	=>
	(bind ?*help* "Si può decidere di posare anche su un pavimento già esistente.")
	(bind ?risposta (yes_or_no_p "E' già presente un pavimento?"))
	(assert (presenza_pavimento ?risposta)))

;TODO remember: la domanda se un rivestimento è presente viene fatta solo se il tipo di stanza è un bagno o una cucina
(defrule domanda_presenza_rivestimento
	(declare (salience ?*lowest_priority*))
	(not (presenza_rivestimento ?))
	(or (tipo_stanza bagno)
		(tipo_stanza cucina))
	=>
	(bind ?*help* "Il rivestimento andrà rimosso per far spazio a quello nuovo")
	(bind ?risposta (yes_or_no_p "E' già presente un rivestimento?"))
	(assert (presenza_rivestimento ?risposta))
	(assert (step2))) ;prosegui alla successiva fase 

;----------------2° step-------------
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

(defrule pavimento_non_presente  ;regola relativa a quando il pavimento non è presente, quindi bisognerebbe farlo mentre l'utente ha deciso di non farlo.
	?f <- (pavimento FALSE)
	(presenza_pavimento FALSE)
	=>
	(printout t crlf "Il pavimento non è presente però hai scelto di non realizzarlo, forse dovresti fare anche il pavimento!" crlf)
	(bind ?risposta (yes_or_no_p "Vuoi quindi realizzare anche il pavimento?"))
	(if ?risposta then (retract ?f) (assert (pavimento TRUE))))

(defrule domanda_condizioni_muri  ;regola per chiedere le condizioni dei muri quando il rivestimento è stato tolto o non è presente e non si intende farlo
	(presenza_rivestimento FALSE)
	(rivestimento FALSE)
	=>
	(bind ?risposta (yes_or_no_p "I muri necessitano di lavori per essere rimessi a nuovo?"))
	(while ?risposta do
		(printout t crlf "Aggiustare i muri prima di continuare!")
		(bind ?risposta (yes_or_no_p "I muri necessitano di lavori per essere rimessi a nuovo?"))))

(defrule domanda_rivestimento_pavimento ;domanda riguardo a cosa effettuare (pavimento, rivestimento o entrambi) nel caso di cucina o bagno
	?f <- (step2)
	;(or (not pavimento ?) 
	;	(not rivestimento ?))
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

(defrule domanda_metri_quadri_pavimento
	(pavimento TRUE)
	(not (dimensioni_pavimento ?))
	=>
	(printout t crlf "Adesso si può procedere al calcolo della dimensione dell'area da pavimentare." crlf)
	(printout t "La misura dell'area da pavimentare non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un pavimento si effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella dell'area da pavimentare, ma deve essere maggiore. Procedere individuando la forma di tale superficie, se questa può essere ricondotta ad una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, allora, per ottenere l'area, bisogna ricordare che:" crlf
		"	* se la superficie ha la forma di un quadrato, allora si calcola la lunghezza di un muro (che rappresenta il lato) e la si moltiplica per se stesso" crlf
		"	* se la superficie ha la forma di un rettangolo, allora si moltiplica la lunghezza dei due muri (che rappresentano i lati) di dimensione diversa" crlf
		"	* se la superficie ha la forma di un triangolo, allora si trova la lunghezza del muro che rappresenta la base e quella del muro che rappresenta l'altezza  del triangolo, si moltiplicando tra di loro le due misure e si divide per due il risultato" crlf
		"	* se la superficie ha la forma di un cerchio, allora si deve trovare la misura del raggio e si usa la formula 2πr, dove π = 3.14 e r = al raggio trovato" crlf
		"	* se la superficie ha la forma di una semicirconferenza, allora si procede come nel caso precedente (cerchio) a trovare la misura del raggio della circonferenza e si usa la formula 2πr (dove π = 3.14 e r = al raggio trovato) e si divide il risultato per due." crlf
		"Nel caso in cui la forma della superficie da pavimentare non fosse simile ad una delle precedenti, allora si suddivide l'area in parti più piccole dalla forma riconducibile ad una di quelle precedenti, si calcola l'area di ogni parte e si sommano i vari risultati ottenuti" crlf
		"Le misure vanno espresse in metri al quadrato" crlf crlf)
	(bind ?*help* "")
	(bind ?risposta (ask_number "Qual è quindi la dimensione in metri quadri dell'area da pavimentare?"))
	(assert (dimensioni_pavimento ?risposta)))

(defrule domanda_metri_quadri_rivestimento
	(not (dimensioni_rivestimento ?))
	(rivestimento TRUE)
	=>
	(printout t crlf "Adesso si può procedere al calcolo della dimensione dell'area da rivestire." crlf)
	(printout t "La misura dell'area da rivestire non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un rivestimento si effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella dell'area da rivestire, ma deve essere maggiore. Bisogna calcolare la dimensione dell'area delle pareti che andranno rivestite. Generalmente la forma della parete sarà rettangolare o al massimo quadrata. Bisogna comunque eliminare dal calcolo dell'area eventuali elementi che non saranno interessati dalla posa del rivestimento, come le finestre." crlf
		"Procedere individuando la forma di tale superficie, se questa può essere ricondotta ad una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, allora, per ottenere l'area, bisogna ricordare che:" crlf
		"	* se la superficie ha la forma di un quadrato, allora si calcola la lunghezza di un muro (che rappresenta il lato) e la si moltiplica per se stesso" crlf
		"	* se la superficie ha la forma di un rettangolo, allora si moltiplica la lunghezza dei due muri (che rappresentano i lati) di dimensione diversa" crlf
		"	* se la superficie ha la forma di un triangolo, allora si trova la lunghezza del muro che rappresenta la base e quella del muro che rappresenta l'altezza  del triangolo, si moltiplicando tra di loro le due misure e si divide per due il risultato" crlf
		"	* se la superficie ha la forma di un cerchio, allora si deve trovare la misura del raggio e si usa la formula 2πr, dove π = 3.14 e r = al raggio trovato" crlf
		"	* se la superficie ha la forma di una semicirconferenza, allora si procede come nel caso precedente (cerchio) a trovare la misura del raggio della circonferenza e si usa la formula 2πr (dove π = 3.14 e r = al raggio trovato) e si divide il risultato per due." crlf
		"Nel caso in cui la forma della superficie non fosse simile ad una delle precedenti, allora si suddivide l'area in parti più piccole dalla forma riconducibile ad una di quelle precedenti, si calcola l'area di ogni parte e si sommano i vari risultati ottenuti" crlf
		"Lo stesso procedimento si ripete per ogni parete e si sommano i risultati ottenuti." crlf
		"Le misure vanno espresse in metri al quadrato" crlf crlf)
	(bind ?*help* "")
	(bind ?risposta (ask_number "Qual è quindi la dimensione in metri quadri dell'area da rivestire?"))
	(assert (dimensioni_rivestimento ?risposta)))

(defrule quantità_piastrelle_pavimento
	(declare (salience ?*high_priority*))
	(disposizione_pavimento ?disp)
	(dimensioni_pavimento ?dim)
	=>
	(printout t crlf "Per il pavimento ")
	(switch ?disp
		(case diagonale then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.15)))
		(case sfalsata then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
		(case spina_di_pesce_dritta then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
		(case spina_di_pesce_obliqua then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
		(case dritta then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))))

(defrule quantità_piastrelle_rivestimento
	(declare (salience ?*high_priority*))
	(disposizione_rivestimento ?disp)
	(dimensioni_rivestimento ?dim)
	=>
	(printout t crlf "Per il rivestimento ")
	(switch ?disp
		(case diagonale then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.15)))
		(case sfalsata then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
		(case spina_di_pesce_dritta then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))
		(case spina_di_pesce_obliqua then (format t "devi prendere %d metri quadri di piastrelle di piastrelle!%n" (* ?dim 1.10)))
		(case dritta then (format t "devi prendere %d metri quadri di piastrelle!%n" (* ?dim 1.10)))))

(defrule attrezzi_necessari_rivestimento
	(declare (salience ?*high_priority*))
	(rivestimento TRUE)
	(interno ?valore)
	=>
	(printout t crlf crlf "Assicurati di procurarti tutti questi attrezzi: " crlf
					"	* tagliapiastrelle (manuale o elettrica)" crlf
					"	* smerigliatrice angolare (grande e piccola)" crlf
					"	* tenaglia per piastrelle" crlf
					"	* 2-3 cazzuole (almeno una piccola a punta)" crlf
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
					"	* filo a piombo" crlf )
	(if (not ?valore) then (printout t   			;nel caso di lavoro esterno
					"	* colla da esterno" crlf
					"	* fugante da esterno (stucco per fughe)" crlf)
			  else (printout t 
					"	* colla" crlf
					"	* fugante (stucco per fughe)" crlf))
	printout t crlf)

(defrule attrezzi_necessari
	(declare (salience ?*high_priority*))
	(rivestimento FALSE)
	(interno ?valore)
	=>
	(printout t crlf crlf "Assicurati di procurarti tutti questi attrezzi: " crlf
					"	* tagliapiastrelle (manuale o elettrica)" crlf
					"	* smerigliatrice angolare (grande e piccola)" crlf
					"	* tenaglia per piastrelle" crlf
					"	* 2-3 cazzuole (almeno una piccola a punta)" crlf
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
					"	* matite in legno da muratori" crlf)
	(if (not ?valore) then (printout t 			;nel caso di esterno
					"	* colla da esterno" crlf
					"	* fugante da esterno (stucco per fughe)" crlf)
			  else (printout t 
					"	* colla" crlf
					"	* fugante (stucco per fughe)" crlf))
	printout t crlf)

(defrule domanda_formato_piastrella_rivestimento
	(declare (salience ?*high_priority*))
	(not (formato_piastrella_rivestimento ?))
	(rivestimento TRUE)
	=>
	(bind ?*help* "In base al formato della piastrella alcuni tipi di posa non sono realizzabili.")
	(bind ?risposta (ask_question "Qual è il formato della piastrella per il rivestimento? (quadrata/rettangolare" quadrata rettangolare))
	(assert (formato_piastrella_rivestimento ?risposta)))

(defrule domanda_formato_piastrella_pavimento
	(declare (salience ?*high_priority*))
	(not (formato_piastrella_pavimento ?))
	(pavimento TRUE)
	=>
	(bind ?*help* "In base al formato della piastrella alcuni tipi di posa non sono realizzabili.")
	(bind ?risposta (ask_question "Qual è il formato della piastrella per il pavimento? (quadrata/rettangolare" quadrata rettangolare))
	(assert (formato_piastrella_pavimento ?risposta)))

(defrule domanda_spessore_piastrella_pavimento
	(pavimento TRUE)
	(not (spessore_piastrella_pavimento ?))
	=>
	(bind ?*help* "")
	(bind ?risposta (ask_number "Qual'è la dimensione in millimetri dello spessore della piastrella scelta per il pavimento?"))
	(while (or (< ?risposta 1) (> ?risposta 10)) do 
		(if (< ?risposta 1) then (printout t crlf "La dimensione è troppo piccola!" crlf))
		(if (> ?risposta 10) then (printout t crlf "La dimensione è troppo grande!" crlf))
		(bind ?risposta (ask_number "Qual'è la dimensione in millimetri dello spessore della piastrella scelta per il pavimento?")))
	(assert (spessore_piastrella_pavimento ?risposta)))

(defrule domanda_distanziatori_pavimento
	(not (dim_distanziatori_pavimento ?))
	(pavimento TRUE)
	=>
	(bind ?*help* "I distanziatori sono quei piccoli pezzi di plastica con forma a T o a croce che si pongono tra due piastrelle in modo da mantenere sempre la stessa distanza.")
	(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori per il pavimento in millimetri?"))
	(while (or (< ?risposta 1) (> ?risposta 10)) do 
		(printout t crlf "La dimensione deve essere compresa tra 1 e 10!" crlf)
		(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori per il pavimento in millimetri?")))
	(assert (dim_distanziatori_pavimento ?risposta)))  ;prosegui alla successiva fase

(defrule domanda_distanziatori_rivestimento
	(not (dim_distanziatori_rivestimento ?))
	(rivestimento TRUE)
	=>
	(bind ?*help* "I distanziatori sono quei piccoli pezzi di plastica con forma a T o a croce che si pongono tra due piastrelle in modo da mantenere sempre la stessa distanza.")
	(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori per il rivestimento in millimetri?"))
	(while (or (< ?risposta 1) (> ?risposta 10)) do 
		(printout t crlf "La dimensione deve essere compresa tra 1 e 10!" crlf)
		(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori per il rivestimento in millimetri?")))
	(assert (dim_distanziatori_rivestimento ?risposta)))

(defrule domanda_disposizione_piastrella_quadrata_pavimento
	(formato_piastrella_pavimento quadrata)
	(not (disposizione_pavimento ?))
	=>
	(bind ?*help* "") ;TODO far vedere immagini
	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle scelta per il pavimento? (dritta/sfalsata/diagonale" dritta sfalsata diagonale))
	(assert (disposizione_pavimento ?risposta)))

(defrule domanda_disposizione_piastrella_rettangolare_pavimento
	(formato_piastrella_pavimento rettangolare)
	(not (disposizione_pavimento ?))
	=>
	(bind ?*help* "") ;TODO far vedere immagini
	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle scelta per il pavimento? (dritta/sfalsata/spina_di_pesce_dritta/spina_di_pesce_obliqua" dritta sfalsata spina_di_pesce_dritta spina_di_pesce_obliqua))
	(assert (disposizione_pavimento ?risposta)))

(defrule domanda_disposizione_piastrella_quadrata_rivestimento
	(formato_piastrella_rivestimento quadrata)
	(not (disposizione_rivestimento ?))
	=>
	(bind ?*help* "") ;TODO far vedere immagini
	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle scelta per il rivestimento? (dritta/sfalsata/diagonale" dritta sfalsata diagonale))
	(assert (disposizione_rivestimento ?risposta)))

(defrule domanda_disposizione_piastrella_rettangolare_rivestimento
	(formato_piastrella_rivestimento rettangolare)
	(not (disposizione_rivestimento ?))
	=>
	(bind ?*help* "") ;TODO far vedere immagini
	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle scelta per il rivestimento? (dritta/sfalsata/spina_di_pesce_dritta/spina_di_pesce_obliqua" dritta sfalsata spina_di_pesce_dritta spina_di_pesce_obliqua))
	(assert (disposizione_rivestimento ?risposta)))

;TODO decidere se inserire, a causa delle disposizioni oblique che fanno cambiare come si deve porre la decorazione all'inizio!!
(defrule domanda_decorazioni_pavimento
	(not (decorazioni_pavimento ?))
	(pavimento TRUE)
	=>
	(bind ?*help* "In caso di presenza di un disegno o di un rosone da realizzare con le piastrelle, bisognerà partire proprio dalla posa di tali piastrelle e poi continuare con il resto del pavimento, che verrà raccordato alle piastrelle appena poste.")
	(bind ?risposta (yes_or_no_p "C'è da porre un rosone o un disegno in particolare nel pavimento?"))
	(assert (decorazioni_pavimento ?risposta)))

(defrule domanda_decorazioni_rivestimento
	(not (decorazioni_rivestimento ?))
	(rivestimento TRUE)
	=>
	(bind ?*help* "In caso di presenza di un disegno o di un rosone da realizzare con le piastrelle, bisognerà partire proprio dalla posa di tali piastrelle e poi continuare con il resto del pavimento, che verrà raccordato alle piastrelle appena poste.")
	(bind ?risposta (yes_or_no_p "C'è da porre un rosone o un disegno in particolare nel rivestimento?"))
	(assert (decorazioni_rivestimento ?risposta)))

;------------3 step--------------
(defrule rimozione_rivestimento  ;se è presente un rivestimento e quello che voglio fare è il rivestimento, allora bisogna toglierlo
	(declare (salience ?*low_priority*))
	?f <- (presenza_rivestimento TRUE)
	(rivestimento TRUE)
	=>
	(printout t crlf "Procedi alla rimozione del rivestimento" crlf)
	(bind ?*help* = "")
	(bind ?risposta (yes_or_no_p "Hai rimosso il rivestimento?"))
	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai rimosso il rivestimento?")))
	(retract ?f)                                   ;il rivestimento
	(assert (presenza_rivestimento FALSE)))        ;è stato tolto

;TODO ampliamento: indicare anche la possibilità di rivestire solo la fascia di muro non coperta dalla cucina
(defrule domanda_rivestimento_cucina  ;chiedere se fare il rivestimento di tutta la stanza o solo la parete dove sta la cucina o solo la fascia di parete visibile dietro la cucina
	(declare (salience ?*low_priority*))
	(not (rivestimento_cucina ?))
	(tipo_stanza cucina)
	(rivestimento TRUE)
	=>
	(printout t crlf "La stanza in cui fare il lavoro è una cucina ed è stato scelto di effettuare il rivestimento" crlf)
	(bind ?*help* = "Si può decidere di rivestire solo la parete (o le pareti) dove andrà posta la cucina in modo da poter pitturare come si vuole le pareti restanti, oppure si potrebbe decidere di rivestire le pareti in un altro modo.")
	(bind ?risposta (ask_question "Vuoi rivestire tutta le pareti della cucina o solo la parete (o le pareti) dove andrà posta la cucina? (tutta/solo_parete" tutta solo_parete))
	(assert (rivestimento_cucina ?risposta)))

(defrule domanda_pavimento_da_raccordare  ;regola per capire se il pavimento che si deve realizzare è indipendente o deve essere conciliato con altri già presenti
	(declare (salience ?*low_priority*))
	(not (pavimento_da_raccordare ?))
	(pavimento TRUE)
	=>
	(bind ?*help* "Se nello stesso piano in cui si deve effettuare la posa del pavimento è presente già un altro pavimento già posato che non si intende eliminare e con cui ci si deve raccordare (cioè il pavimento che si sta realizzando non dovrà essere né più alto e né più basso), allora tale pavimento deve essere realizzato in modo che, una volta completato, sia all'altezza giusta.")
	(bind ?risposta (yes_or_no_p "E' presente in una stanza adiacente a quella in cui si sta lavorando un pavimento con cui ci si dovrà raccordare? (cioè, dopo la posa del pavimento, esso dovrà essere alla stessa altezza del pavimento già presente)"))
	(assert (pavimento_da_raccordare ?risposta)))

(defrule no_posa_sopra ;non è applicabile la posa sopra se è presente un rivestimento ed è stato stabilito di fare solo il pavimento
	(presenza_rivestimento TRUE)
	(rivestimento FALSE)
	(pavimento TRUE)
	(presenza_pavimento TRUE)
	=>
	(assert (posa_sopra_pavimento FALSE)))

(defrule domanda_posa_sopra_pavimento  ;se il pavimento è presente e si è scelto di porre un nuovo pavimento, chiedere se fare la posa sopra il pavimento esistente
	(declare (salience ?*low_priority*))
	(not (posa_sopra_pavimento ?))
	?f <- (pavimento_da_raccordare FALSE)  ;Il pavimento non dovrà essere raccordato 
	(pavimento TRUE)
	(presenza_pavimento TRUE)
	(spessore_piastrella_pavimento ?spessore_piastrella)
	=>
	(printout t crlf "Considera che la posa sopra un pavimento già esistente rialzerà il piano, quindi non è da scegliere nel caso in cui ci si deve raccordare con un " crlf
					"altro pavimento già esistente. Dalle scelte effettuate sembra che il pavimento non debba essere raccordato con nessun altro pavimento." crlf)
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "E' cosi, cioè il pavimento non deve essere raccordato ad alcun altro pavimento?"))
	(if (not ?risposta) 
		then
			(retract ?f)
			(assert (pavimento_da_raccordare TRUE))
		else
			(format t "Il nuovo pavimento da porre sopra a quello già esistente avrà uno spessore di %d mm. %nOccorrerà effettuare delle modifiche anche alle porte (che dovranno essere ridotte).%n" (+ 3 ?spessore_piastrella))
			(bind ?*help* "Scegliendo tale tipo di posa, il nuovo pavimento verrà realizzato posandolo sopra quello già esistente. Tuttavia occorre valutare bene la scelta poiché si devono fare alcune modifiche alle porte, in quanto il piano verrà rialzato. Inoltre ci potrebbe essere un dislivello nel caso in cui il pavimento da porre nella stanza è collegato con un altro già presente.")
			(bind ?risposta (yes_or_no_p "Vuoi effettuare la posa sopra il pavimento esistente?"))
			(assert (posa_sopra_pavimento ?risposta))))

(defrule domanda_posa_sopra_pavimento_da_raccordare  ;se il pavimento è presente e si è scelto di porre un nuovo pavimento, chiedere se fare la posa sopra il pavimento esistente
	(declare (salience ?*low_priority*))
	(not (posa_sopra_pavimento ?))
	(pavimento_da_raccordare TRUE)  ;Il pavimento dovrà essere raccordato 
	(pavimento TRUE)
	(presenza_pavimento TRUE)
	(spessore_piastrella_pavimento ?spessore_piastrella)
	=>
	(format t "Il nuovo pavimento, da porre sopra a quello già esistente, avrà uno spessore di %d mm. %nOccorrerà effettuare delle modifiche anche alle porte (che dovranno essere ridotte).%n" (+ 3 ?spessore_piastrella))
	(format t "%nIl pavimento dovrà essere raccordato con un altro già presente, quindi a meno che il pavimento già presente si trovi esattamente a %d mm%n sotto il pavimento con cui fare il raccordo, il pavimento presente dovrà essere rimosso!%n" (+ ?spessore_piastrella 3))
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Il pavimento si trova alla dimensione giusta sotto il pavimento già presente?"))

	(if ?risposta 
		then 
			(bind ?*help* "Scegliendo tale tipo di posa, il nuovo pavimento verrà realizzato posandolo sopra quello già esistente. Tuttavia occorre valutare bene la scelta poiché si devono fare alcune modifiche alle porte, in quanto il piano verrà rialzato. Inoltre ci potrebbe essere un dislivello nel caso in cui il pavimento da porre nella stanza è collegato con un altro già presente.")
			(bind ?risposta (yes_or_no_p "Vuoi effettuare la posa sopra il pavimento esistente?"))
			(assert (posa_sopra_pavimento ?risposta))
		else
			(assert (posa_sopra_pavimento FALSE))))

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
	(assert (piastrelle_sollevate FALSE))
	(assert (ok_inizio_pavimento)))

(defrule piastrelle_non_sollevate ;se le piastrelle non sono sollevate e il pavimento è a livello procedi con la posa del pavimento
	?f <- (piastrelle_sollevate FALSE)
	?f1 <- (pavimento_livello TRUE)
	=>
	(retract ?f ?f1)
	(assert (ok_inizio_pavimento)))

;TODO spiegazione: spiegare come rimuovere pavimento 
(defrule pavimento_non_livello ;non si può fare la posa sopra il pavimento perché non a livello
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

(defrule domanda_presenza_massetto  ;regola per verificare la presenza del massetto
	(presenza_pavimento FALSE)
	(not (presenza_massetto ?))
	=>
	(bind ?*help* "Il massetto è uno strato di cemento la cui presenza è essenziale perché sopra esso verranno posate le piastrelle.")
	(bind ?risposta (yes_or_no_p "E' presente un massetto?"))
	(assert (presenza_massetto ?risposta)))

;TODO ampliamento: spiegare come si fa il massetto
;TODO remember: qui si collegano anche i rami riguardanti il massetto troppo alto o troppo basso nel caso di raccordo con pavimenti presenti e rivestimenti presenti
(defrule massetto_non_presente
	?f <- (presenza_massetto FALSE)
	=>
	(printout t crlf "Devi fare il massetto tenendo conto anche di eventuali raccordi!" crlf)
	(bind ?risposta (yes_or_no_p "Hai fatto il massetto?"))
	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai fatto il massetto?")))
	(retract ?f)
	(assert (ok_inizio_pavimento))
	(assert (presenza_massetto TRUE)))

(defrule domanda_controllo_massetto_a_livello ;controllo se il massetto presente è a livello nel caso di pavimento da non raccordare
	(declare (salience ?*low_priority*))
	(presenza_massetto TRUE)
	=>
	(printout t crlf "Controlliamo se il massetto è a livello..." crlf crlf
					"Posa una stadia sul massetto da un angolo all'opposto facendo in modo che poggi bene. Poni su di essa un livello..." crlf
	                "Controlla se ci sono punti in cui la stadia si allontana dal massetto di diversi centimetri..." crlf
	                "Controlla se la bolla d'aria sul livello si trova nella posizione centrale..." crlf
	                "Ripeti l'operazione diverse volte in modo da coprire da un alto all'altro tutta l'area da pavimentare e poi rispondi alle seguenti domande..." crlf)
	(bind ?*help* "")
	(bind ?risposta1 (yes_or_no_p "Nelle varie misurazioni fatte ci sono stati casi in cui la stadia era lontana dal massetto di diversi centimetri?"))
	(bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. Non interessa il verso.")
	(bind ?risposta2 (yes_or_no_p "Nelle varie misurazioni fatte la bolla d'aria sulla livella era sempre nella posizione centrale?"))

	(bind ?massettoalivello (and (not ?risposta1) ?risposta2))
	(if ?massettoalivello then (assert (massetto_livello TRUE)) else (assert (massetto_livello FALSE))))

(defrule domanda_controllo_massetto_raccordo ;controllo se il massetto presente è a livello nel caso di pavimento da raccordare
	(declare (salience ?*low_priority*))
	(pavimento_da_raccordare TRUE)
	(massetto_livello TRUE)
	(spessore_piastrella_pavimento ?spessore_piastrella)
	=>
	(printout t crlf "Il massetto è a livello, ma bisogna controllare che sia realizzato in modo tale che con la posa del pavimento esso si trovi allo stesso livello del pavimento in un'altra stanza con cui si andrà a raccordare" crlf)
	(format t "%nLo spessore della piastrella è di %d mm%n" ?spessore_piastrella)
	(printout t "Lo spessore della colla sarà di 3mm" crlf)
	(bind ?spessore_pavimento (+ ?spessore_piastrella 3))
	(format t "Il pavimento avrà uno spessore totale di %d mm%n" ?spessore_pavimento)

	(printout t crlf "Il pavimento verrà raccordato con quello di un'altra stanza, quindi occorre procedere alla verifica dell'altezza del massetto rispetto al pavimento già esistente..." crlf)
	(format t "%nControlliamo che il massetto è a %d mm sotto la superficie del pavimento con cui deve essere raccordato%n" ?spessore_pavimento)
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Il massetto è all'altezza giusta, considerando lo spessore complessivo (piastrella + colla) del pavimento che si presta ad essere posato?"))
	(assert (massetto_raccordo_livello ?risposta)))

(defrule domanda_massetto_raccordo_troppo_alto_o_basso
	?f <- (massetto_raccordo_livello FALSE)
	=>
	(printout t crlf "Il massetto non è idoneo alla posa del pavimento poiché con la posa del pavimento risulta non essere a livello del pavimento già presente!" crlf)
	(retract ?f)
	(bind ?*help* "")
	(bind ?risposta (ask_question "Il massetto risulta troppo alto o troppo basso, considerando la posa del pavimento? (alto/basso" alto basso))
	(assert (massetto_alto_basso ?risposta)))

(defrule massetto_alto
	?f1 <- (massetto_alto_basso alto)
	?f2 <- (presenza_massetto TRUE)
	=>
	(retract ?f1 ?f2)
	(printout t crlf "Il massetto è troppo alto, quindi occorre smantellarlo e procedere al rifacimento!" crlf)
	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Hai smantellato il massetto?"))
	(while (not ?risposta) do (bind ?risposta (yes_or_no_p "Hai smantellato il massetto?")))
	(assert (presenza_massetto FALSE)))

(defrule massetto_basso
	?f1 <- (massetto_alto_basso basso)
	?f2 <- (presenza_massetto TRUE)
	=>
	(retract ?f1 ?f2)
	(printout t crlf "Il massetto è troppo basso, quindi occorre aumentare lo spessore del massetto!" crlf)
	(assert (presenza_massetto FALSE)))

(defrule domanda_controllo_massetto_rivestimento_presente ;controllo massetto in modo tale che sia raccordato al rivestimento presente (cioè vada a combaciare con il rivestimento senza lasciare intravedere spazi bianchi)
	(massetto_livello TRUE)
	(rivestimento FALSE)			;il rivestimento è presente
	(presenza_rivestimento TRUE)	;ma non è da fare, cioè rimane il rivestimento vecchio e il pavimento deve essere raccordato
	(spessore_piastrella_pavimento ?spessore_piastrella)
	=>
	(printout t crlf "Il pavimento presente deve essere raccordato al rivestimento esistente, cioè una volta posato esso deve andare a coprire il rivestimento senza far in modo che si vedano spazi, cioè non copra per bene il rivestimento." crlf)

	(bind ?spessore_pavimento (+ 3 ?spessore_piastrella))
	(format t "Bisogna controllare che lo spessore del pavimento che si deve posare (che si ottiene aggiungendo %d mm dal massetto) vada a coprire senza lasciare spazi in basso il rivestimento già presente!%n" ?spessore_pavimento)

	(bind ?*help* "")
	(bind ?risposta (yes_or_no_p "Considerando lo spessore che si otterrebbe dalla posa del pavimento sul massetto presente, si va a raccordare con il rivestimento già presente senza presentare spazi?"))
	(assert (massetto_raccordo_rivestimento_livello ?risposta)))

(defrule domanda_massetto_rivestimento_alto_basso
	?f <- (massetto_raccordo_rivestimento_livello FALSE)
	=>
	(printout t crlf "Il massetto non è idoneo alla posa del pavimento poiché con la posa del pavimento risulta non essere a livello con il rivestimento già presente!" crlf)
	(retract ?f)
	(bind ?*help* "")
	(bind ?risposta (ask_question "Il massetto risulta troppo alto o troppo basso, considerando la posa del pavimento? (alto/basso" alto basso))
	(assert (massetto_alto_basso ?risposta)))

;TODO ampliamento: aggiungere come si aggiusta un muro?
(defrule domanda_controllo_muri_rivestimento  ;il rivestimento non c'è e si è deciso di farlo, allora si controllano se i muri sono a piombo
	(declare (salience ?*low_priority*))
	(presenza_rivestimento FALSE)
	(rivestimento TRUE)
	=>
	(printout t crlf "Controllo se i muri sono a piombo..." crlf
			"Prendi il filo a piombo. Prendi la rocchetta di cui è dotato e poggiala sulla parete da misurare..." crlf
			"Vedi se il piombo (il peso) alla fine del filo è lontano dal muro, troppo vicino oppure si muove liberamente..." crlf
			"Ripeti la procedura per ogni parete della stanza su cui stai lavorando..." crlf)
        (bind ?risposta (yes_or_no_p "Tutti i muri sono a piombo?"))
        (while (not ?risposta) do 
                (printout t crlf "Non puoi proseguire nella posa del rivestimento. Devi prima riparare i muri!" crlf)
                (bind ?risposta (yes_or_no_p "Tutti i muri sono a piombo?")))
        (assert (ok_inizio_rivestimento)))


;-----------------INIZIO-------------------
(defrule inizio_rivestimento_e_pavimento ;se c'è da fare pavimento e rivestimento spiega perchè prima inizio rivestimento
	(ok_inizio_rivestimento)
	(ok_inizio_pavimento)
	=>
	(printout t crlf "Si devono realizzare sia il pavimento che il rivestimento. Conviene partire sempre dal rivestimento in quanto ci sono diversi vantaggi dati dal fatto che non bisogna aspettare che il pavimento asciughi prima di poter lavorare al rivestimento e si evita di creare qualche danno in quanto lavorando al rivestimento sopra il pavimento appena realizzato potrebbe accadere di scheggiarlo." crlf)
	(assert (inizio_rivestimento))) ;inizia rivestimento

(defrule inizio_rivestimento ;c'è da fare solo rivestimento
	(ok_inizio_rivestimento)
	=>
	(assert (inizio_rivestimento)))

(defrule inizio_pavimento ;c'è da fare solo pavimento
	(ok_inizio_pavimento)
	=>
	(assert (inizio_pavimento)))



;ALTRO
;;(defr le stanza_piccola
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













