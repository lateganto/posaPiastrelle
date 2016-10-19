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

(defrule condizioni_muri  ;regola per chiedere le condizioni dei muri quando il rivestimento è stato tolto o non è presente e non si intende farlo
	(presenza_rivestimento FALSE)
	(rivestimento FALSE)
	=>
	(bind ?risposta (yes_or_no_p "I muri necessitano di lavori per essere rimessi a nuovo?"))
	(assert (rifare_muri ?risposta)))

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

(defrule calcolo_metri_quadri_pavimento
	(pavimento TRUE)
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

(defrule calcolo_metri_quadri_rivestimento
	(rivestimento TRUE)
	=>
	(printout t crlf "Adesso si può procedere al calcolo della dimensione dell'area da rivestire." crlf)
	(printout t "La misura dell'area da rivestire non deve essere estremamente precisa. Tuttavia bisogna sapere che nel realizzare un rivestimento si effettuano diversi tagli di piastrelle. Quindi la quantità di piastrelle da avere a disposizione non deve essere precisamente quella dell'area da rivestire, ma deve essere maggiore. Bisogna calcolare la dimensione dell'area delle pareti che andranno rivestite. Generalmente la forma della parete sarà rettangolare o al massimo quadrata. Bisogna comunque eliminare dal calcolo dell'area eventuali elementi che non saranno interessati dalla posa del rivestimento, come le finestre.
		Procedere individuando la forma di tale superficie, se questa può essere ricondotta ad una forma semplice come quadrato, rettangolo, triangolo, cerchio o semicerchio, allora, per ottenere l'area, bisogna ricordare che:" crlf
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
	(if (not ?valore) then (printout t 
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
	(if (not ?valore) then (printout t 
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
	(tipo_stanza cucina)
	(rivestimento TRUE)
	=>
	(printout t crlf "La stanza in cui fare il lavoro è una cucina ed è stato scelto di effettuare il rivestimento" crlf)
	(bind ?*help* = "Si può decidere di rivestire solo la parete (o le pareti) dove andrà posta la cucina in modo da poter pitturare come si vuole le pareti restanti, oppure si potrebbe decidere di rivestire le pareti in un altro modo.")
	(bind ?risposta (ask_question "Vuoi rivestire tutta le pareti della cucina o solo la parete (o le pareti) dove andrà posta la cucina? (tutta/solo_parete" tutta solo_parete))
	(assert (rivestimento_cucina ?risposta)))

(defrule domanda_posa_sopra_pavimento  ;se il pavimento è presente e si è scelto di porre un nuovo pavimento, chiedere se fare la posa sopra il pavimento esistente
	(declare (salience ?*low_priority*))
	(pavimento TRUE)
	(presenza_pavimento TRUE)
	(presenza_rivestimento TRUE) 	;non chiedere la posa sopra se è presente un rivestimento
	(rivestimento FALSE)			;ed è stato stabilito di fare solo il pavimento
	=>
	(bind ?*help* "Effettuare la posa del nuovo pavimento sopra uno già esistente. Tuttavia occorre valutare bene la scelta poiché si dovrebbero fare alcune modifiche alle porte, in quanto il piano verrà rialzato. Inoltre ci potrebbe essere un dislivello nel caso in cui il pavimento da porre nella stanza è collegato con un altro.")
	(bind ?risposta (yes_or_no_p "Vuoi effettuare la posa sopra il pavimento esistente?"))
	(assert (posa_sopra_pavimento ?risposta)))

;In una cucina (a meno che non ci sia il rivestimento solo di una piccola fascia) o in un bagno, se si decide di fare solo il pavimento e un rivestimento è già presente, allora bisogna fare in modo che il pavimento vada a combaciare precisamente con il rivestimento presente.
(defrule stanza_rivestita_da_fare_solo_pavimento
	(pavimento TRUE)
	(presenza_rivestimento TRUE)
	(rivestimento FALSE)
	;(not (rivestimento solo_fascia))  ;per il caso della cucina
	=>
	(printout t crlf "E' presente un rivestimento ma si è deciso di fare solo il pavimento" crlf
					"Bisogna procedere alla rimozione del pavimento e al controllo del massetto" crlf)
	(assert (posa_sopra_pavimento FALSE)))  ;non è possibile quindi fare la posa sopra il pavimento

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

(defrule domanda_controllo_massetto_pavimento_rivestimento_presente
	(declare (salience ?*high_priority*))
	(presenza_pavimento FALSE)
	(pavimento TRUE)
	(rivestimento FALSE)			;il rivestimento è presente 
	(presenza_rivestimento TRUE)	;ma non lo si deve fare
	(spessore_piastrella_pavimento ?spessore)
	=>
	(printout t crlf "Bisogna controllare il massetto che deve essere a livello e fatto in modo tale che con la posa del pavimento esso sia a livello con il pavimento che si dovrà realizzare o che è già presente in un'altra stanza..." crlf)
	(format t "%nLo spessore della piastrella è di %dmm%n" ?spessore)
	(printout t "Lo spessore della colla sarà di 3mm" crlf)
	(bind ?dim_pavimento (+ spessore 3))
	(format t "Il pavimento avrà uno spessore totale di %d mm%n" ?dim_pavimento)

	(printout t crlf "Posa una stadia sul pavimento da un angolo all'opposto facendo in modo che poggi bene. Poni su di essa un livello..." crlf
                "Controlla se ci sono punti in cui la stadia si allontana dal pavimento di diversi centimetri..." crlf
                "Controlla se la bolla d'aria sul livello si trova nella posizione centrale..." crlf
                "Ripeti l'operazione diverse volte in modo da coprire tutta l'area da pavimentare e poi rispondi alle seguenti domande..." crlf)
	(bind ?*help* "")
	(bind ?risposta1 (yes_or_no_p "Nelle varie misurazioni fatte ci sono stati casi in cui la stadia era lontana dal pavimento di diversi centimetri?"))
	(bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. Non interessa il verso.")
	(bind ?risposta2 (yes_or_no_p "Nelle varie misurazioni fatte la bolla d'aria sulla livella era sempre nella posizione centrale?"))

	(bind ?massettolivello (and (not ?risposta1) ?risposta2))
	(if ?massettolivello then (assert (massetto_livello TRUE)) else (assert (massetto_livello FALSE)))

	(bind ?risposta (yes_or_no_p "La stanza è collegata ad un'altra con pavimento già presente oppure nello stesso piano ci sono pavimenti già posati con cui ci si deve raccordare? (Cioè il pavimento da realizzare sarà collegato anche se non direttamente a quello di un'altra stanza)"))
	(if (and ?risposta ?massettolivello)
		then 	(format t "%nIl massetto è a livello ma bisogna controllare che sia %dmm sotto la superficie del pavimento collegato%n" ?dim_pavimento)
			(format t "Inoltre, se il massetto è preciso, bisogna controllare che lo spessore del pavimento che si deve posare (che si ottiene aggiungendo %dmm dal massetto) vada a coprire senza lasciare spazi in basso il rivestimento già presente!")

			(assert (massetto_rivestimento_livello (yes_or_no_p "Il massetto è della dimensione giusta sotto il pavimento e considerando la posa del pavimento sopra va a coprire il rivestimento già presente?")))
		else 	(if (massettolivello)
				then (format t "%nIl massetto è a livello ma bisogna controllare che sia %dmm sotto la superficie del pavimento collegato%n" ?dim_pavimento)))


	)

(defrule domanda_controllo_livello_massetto ;se il pavimento non c'è, il rivestimento non si deve fare e si è scelti di effettuare la posa del pavimento, allora controllare il massetto
	(declare (salience ?*low_priority*))
	(presenza_pavimento FALSE)
	(pavimento TRUE)
	(rivestimento FALSE)
	=>
	(printout t crlf "Posa una stadia sul pavimento da un angolo all'opposto facendo in modo che poggi bene. Poni su di essa un livello..." crlf
                "Controlla se ci sono punti in cui la stadia si allontana dal pavimento di diversi centimetri..." crlf
                "Controlla se la bolla d'aria sul livello si trova nella posizione centrale..." crlf
                "Ripeti l'operazione diverse volte in modo da coprire tutta l'area da pavimentare e poi rispondi alle seguenti domande..." crlf)
	(bind ?*help* "")
	(bind ?risposta1 (yes_or_no_p "Nelle varie misurazioni fatte ci sono stati casi in cui la stadia era lontana dal pavimento di diversi centimetri?"))
	(bind ?*help* "Il livello deve essere posto precisamente sopra la stadia, nello stesso senso della stadia. Non interessa il verso.")
	(bind ?risposta2 (yes_or_no_p "Nelle varie misurazioni fatte la bolla d'aria sulla livella era sempre nella posizione centrale?"))
	(if (and (not ?risposta1) ?risposta2) then (assert (massetto_livello TRUE)) else (assert (massetto_livello FALSE))))

(defrule massetto_a_livello_pavimento_rivestimento_presente  ;massetto a livello per il pavimento da mettere con rivestimento presente ma da non fare
	(massetto_rivestimento_livello TRUE)
	=>
	(assert (ok_inizio_pavimento))) ;inizia il pavimento

(defrule massetto_non_a_livello_pavimento_rivestimento_presente ;massetto non a livello per il pavimento da mettere con rivestimento presente ma da non fare
	?f <- (massetto_rivestimento_livello FALSE)
	=>
	()
	(whi)
	)

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

(defrule domanda_spessore_)



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













