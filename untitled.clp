(defglobal ?*highest_priority* = 1000)
(defglobal ?*high_priority* = 100)
(defglobal ?*low_priority* = -100)
(defglobal ?*lowest_priority* = -1000)

;; Routines for question-driven interaction
;; Modified from Riley's& Giarratano's
(deffunction ask_question (?question $?allowed_values)
  (printout t ?question)
  (bind ?answer (read))
  (if (lexemep ?answer) ;; TRUE is ?answer is a STRING or SYMBOL
      then (bind ?answer (lowcase ?answer)))

  (while (not (member ?answer ?allowed_values)) do
	    (printout t ?question)
	    (bind ?answer (read))
	    (if (lexemep ?answer) 
		      then (bind ?answer (lowcase ?answer))))
     ?answer)

(deffunction yes_or_no_p (?question)
  (bind ?question (sym-cat ?question " (yes/y/no/n): "))
     (bind ?response (ask_question ?question yes no y n))
     (if (or (eq ?response yes) (eq ?response y))
         then TRUE 
         else FALSE))

(deffunction ask_number (?question)
  (printout t ?question)
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
	(if (> ?num 4)
		then (return intermedio)
		else (return principiante)))

;-----------TEMPLATE----------
(deftemplate esperienza
	(slot valore))

(deftemplate domande_poste
	(slot numero))

;--------------START---------------
(defrule inizio
	(declare (salience ?*highest_priority*))
	=>
	(printout t crlf "*** Un sistema per la posa di pavimenti e rivestimenti in gres porcellanato ***" crlf crlf))

(defrule chiedi_esperto
	(declare (salience ?*low_priority*))
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
	(assert (inizia_domande))
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



;(defrule ask-interno-esterno
;	(declare (salience ?*low-priority*))
;	(not (interno (value ?)))
;	=>
;	(bind ?answer (yes-or-no-p "E' un pavimento per interni? "))
;	(assert (interno (value ?answer))))
;
;(defrule ask-tipo-stanza
;	(declare (salience ?*low-priority*))
;	(not (tipo_stanza (value ?)))
;	=>
;	(bind ?answer (ask-question "Indicare in quale stanza si deve effettuare la posa? (cucina, bagno, altro): " cucina bagno altro))
;	(assert (tipo_stanza (value ?answer))))
;
;(defrule ask-dimensioni-stanza
;	(declare (salience ?*low-priority*))
;	(not (dimensioni_stanza (value ?)))
;	=>
;	(bind ?length (ask-number "Indicare la lunghezza della stanza in metri: "))
;	(bind ?width (ask-number "Indicare la larghezza della stanza in metri: "))
;	(assert (dimensioni_stanza (value (* ?length ?width)))))
;
;(defrule ask-piastrella-quadrata
;	(declare (salience ?*low-priority*))
;	(not (piastrella_quadrata (value ?)))
;	=>
;	(bind ?answer (yes-or-no-p "La piastrella è quadrata?"))
;	(assert (piastrella_quadrata (value ?answer))))
;
;(defrule ask-disposizione
;	(declare (salience ?*low-priority*))
;	(not (disposizione (value ?)))
;	=>
;	(bind ?answer (ask-question "Indicare in che modo si intende disporre le piastrelle. (diagonale, normale): " diagonale normale))
;	(assert (disposizione (value ?answer))))
;
;(defrule ask-pav-presente
;	(declare (salience ?*low-priority*))
;	(not (pavimento_presente (value ?)))
;	=>
;	(bind ?answer (yes-or-no-p "E' già presente un pavimento?"))
;	(assert (pavimento_presente (value ?answer))))
;
;(defrule ask-decorazioni
;	(declare (salience ?*low-priority*))
;	(not (decorazioni (value ?)))
;	=>
;	(bind ?answer (yes-or-no-p "Il pavimento prevede greche o decorazioni?"))
;	(assert (decorazioni (value ?answer))))
;	
;	
;	asd























