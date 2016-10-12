(defglobal ?*highest_priority* = 1000)
(defglobal ?*high_priority* = 100)
(defglobal ?*low_priority* = -100)
(defglobal ?*lowest_priority* = -1000)

;; Routines for question-driven interaction
;; Modified from Riley's& Giarratano's
(deffunction ask_question (?question $?allowed_values)
  (printout t ?question " ")
  (bind ?answer (read))
  (if (lexemep ?answer) ;; TRUE is ?answer is a STRING or SYMBOL
      then (bind ?answer (lowcase ?answer)))

  (while (not (member ?answer ?allowed_values)) do
	    (printout t ?question " ")
	    (bind ?answer (read))
	    (if (lexemep ?answer) 
		      then (bind ?answer (lowcase ?answer))))
     ?answer)

(deffunction yes_or_no_p (?question)
  (bind ?question (sym-cat ?question " (si/s/no/n): "))
     (bind ?response (ask_question ?question si no s n))
     (if (or (eq ?response si) (eq ?response s))
         then TRUE 
         else FALSE))

(deffunction ask_number (?question)
  (printout t ?question " ")
  (bind ?answer (read))
  (while (not (numberp ?answer)) do  ;check if answer is a NUMBER
	    (printout t ?question)
	    (bind ?answer (read)))
     ?answer)

(deffunction domanda_random ()
	(random 1 8))

(deffunction esperienza_binario
	(?risposta)
	(if ?risposta
		then (return 1)
		else (return 0)))

(deffunction esperienza_utente
	(?num)
	(if (> ?num 3)
		then (return intermedio)
		else (return principiante)))

;-----------TEMPLATE----------
(deftemplate esperienza
	(slot valore))

(deftemplate domande_poste
	(slot numero))

;--------------START---------------

;--------------Domande per capire il tipo di utente-------------
(defrule inizio
	(declare (salience ?*highest_priority*))
	=>
	(printout t crlf "*** Un sistema per la posa di pavimenti e rivestimenti in gres porcellanato ***" crlf crlf))

(defrule chiedi_esperto
	;(declare (salience ?*low_priority*))
	=>
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
	;(bind ?n (domanda_random))
	;(not (domanda ?n))
	=>
	(bind ?n (domanda_random))
	(assert (domanda ?n)))

(defrule domanda_random1
	?i <- (inizia_domande)
	?d <- (domanda 1)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?d)
	(retract ?i)
	(assert (inizia_domande))   ;;;controllare se corretto!!
	(bind ?risposta (yes_or_no_p "Sai cos'è una spatola dentellata?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random2
	?i <- (inizia_domande)
	?d <- (domanda 2)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?d)
	(retract ?i)
	(assert (inizia_domande))
	(bind ?risposta (yes_or_no_p "Hai mai usato la livella?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random3
	?i <- (inizia_domande)
	?d <- (domanda 3)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?d)
	(retract ?i)
	(assert (inizia_domande))
	(bind ?risposta (yes_or_no_p "Sai calcolare se un muro è a squadro?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random4
	?i <- (inizia_domande)
	?d <- (domanda 4)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?d)
	(retract ?i)
	(assert (inizia_domande))
	(bind ?risposta (yes_or_no_p "Sai cos'è la mazza in gomma?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random5
	?i <- (inizia_domande)
	?d <- (domanda 5)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?d)
	(retract ?i)
	(assert (inizia_domande))
	(bind ?risposta (yes_or_no_p "Sai cosa è un distanziatore e a che serve?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random6
	?i <- (inizia_domande)
	?d <- (domanda 6)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?d)
	(retract ?i)
	(assert (inizia_domande))
	(bind ?risposta (yes_or_no_p "Sai a cosa serve la tenaglia da piastrellista?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random7
	?i <- (inizia_domande)
	?d <- (domanda 7)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?d)
	(retract ?i)
	(assert (inizia_domande))
	(bind ?risposta (yes_or_no_p "Sai usare una tagliapiastrelle?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule domanda_random8
	?i <- (inizia_domande)
	?d <- (domanda 8)
	?f1 <- (esperienza (valore ?x1))
	?f2 <- (domande_poste (numero ?x2))
	=>
	(retract ?d)
	(retract ?i)
	(assert (inizia_domande))
	(bind ?risposta (yes_or_no_p "Hai mai usato una smerigliatrice angolare?"))
	(modify ?f2 (numero (+ ?x2 1)))
	(modify ?f1 (valore (+ ?x1 (esperienza_binario ?risposta)))))

(defrule definisci_utente
	?a <- (domande_poste (numero 5))
	?b <- (esperienza (valore ?x))
	?c <- (inizia_domande)
	=>
	(retract ?a ?b ?c)
	(assert (utente (esperienza_utente ?x))))

;----------Domande per inquadrare la situazione-----------

(defrule domanda_interno_esterno
	(declare (salience ?*low_priority*))
	(not (interno ?))
	=>
	(bind ?risposta (yes_or_no_p "Il lavoro è per interno?"))
	(assert (interno ?risposta)))

(defrule domanda_tipo_stanza
	(declare (salience ?*low_priority*))
	(not (tipo_stanza))
	(interno TRUE)
	=>
	(bind ?risposta (ask_question "Indicare in quale stanza si deve effettuare la posa? (bagno, cucina, altro):" bagno cucina altro))
	(assert (tipo_stanza ?risposta)))

(defrule domanda_formato_piastrella
	(declare (salience ?*low_priority*))
	(not (formato_piastrella ?))
	=>
	(bind ?risposta (ask_question "Qual è il formato della piastrella? (quadrata, rettangolare):" quadrata rettangolare))
	(assert (formato_piastrella ?risposta)))

(defrule domanda_disposizione_piastrella_quadrata
	(declare (salience ?*low_priority*))
	(formato_piastrella quadrata)
	(not (disposizione ?))
	=>
	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle? (dritta, sfalsata, diagonale):" dritta sfalsata diagonale))
	(assert (disposizione ?risposta)))

(defrule domanda_disposizione_piastrella_rettangolare
	(declare (salience ?*low_priority*))
	(formato_piastrella rettangolare)
	(not (disposizione ?))
	=>
	(bind ?risposta (ask_question "Qual è la disposizione delle piastrelle? (dritta, sfalsata, spina_di_pesce_dritta, spina_di_pesce_obliqua):" dritta sfalsata spina_di_pesce_dritta spina_di_pesce_obliqua))
	(assert (disposizione ?risposta)))

(defrule domanda_dimensioni_stanza
	(declare (salience ?*low_priority*))
	(not (dimensioni-stanza ?))
	=>
	(bind ?risposta (ask_number "Inserire la dimensione dell'area da pavimentare in metri quadri:"))
	(assert (dimensione_area ?risposta)))

(defrule domanda_presenza_pavimento
	(declare (salience ?*low_priority*))
	(not (presenza_pavimento ?))
	=>
	(bind ?risposta (yes_or_no_p "E' già presente un pavimento?"))
	(assert (presenza_pavimento ?risposta)))

;decidere se inserire, a causa delle disposizioni oblique che fanno cambiare come si deve porre la decorazione all'inizio!!
(defrule domanda_decorazioni
	(declare (salience ?*low_priority*))
	(not (decorazioni ?))
	=>
	(bind ?risposta (yes_or_no_p "Ci sono decorazioni nel pavimento da posare?"))
	(assert (decorazioni ?risposta)))

(defrule domanda_distanziatori
	(declare (salience ?*low_priority*))
	(not (dim_distanziatori ?))
	=>
	(bind ?risposta (ask_number "Qual è la dimensione dei distanziatori in millimetri?"))
	(assert (dim_distanziatori ?risposta)))
























