(defglobal ?*highest-priority* = 1000)
(defglobal ?*high-priority* = 100)
(defglobal ?*low-priority* = -100)
(defglobal ?*lowest-priority* = -1000)


;--------TEMPLATES----------
(deftemplate interno
	(slot value
		(allowed-symbols TRUE FALSE)))

(deftemplate dimensioni_stanza
	(slot value))

(deftemplate tipo_stanza
	(slot value
		(allowed-symbols cucina bagno altro)))

(deftemplate disposizione
	(slot value
		(allowed-symbols diagonale normale sfalsato spina_dritta spina_obliqua)))

(deftemplate piastrella_quadrata
	(slot value
		(allowed-symbols TRUE FALSE)))

(deftemplate pavimento_presente
	(slot value
		(allowed-symbols TRUE FALSE)))

(deftemplate decorazioni
	(slot value
		(allowed-symbols TRUE FALSE)))

;(deftemplate pavimento
;	(slot interno
;		(allowed-symbols TRUE FALSE)
;		(default ?DERIVE))
;	(slot dimensioni_stanza
;		(type SYMBOL)
;		(default ?DERIVE))
;	(slot tipo_stanza
;		(allowed-symbols cucina bagno altro)
;		(default ?DERIVE))
;	(slot disposizione
;		(allowed-symbols diagonale normale)
;		(default ?DERIVE))
;	(slot piastrella_quadrata
;		(allowed-symbols TRUE FALSE)
;		(default ?DERIVE))
;	(slot pavimento_presente
;		(allowed-symbols TRUE FALSE)
;		(default ?DERIVE))
;	(slot decorazioni
;		(allowed-symbols TRUE FALSE)
;		(default ?DERIVE)))

;; Routines for question-driven interaction
;; Modified from Riley's& Giarratano's
(deffunction ask-question (?question $?allowed-values)
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

(deffunction yes-or-no-p (?question)
  (bind ?question (sym-cat ?question " (yes/y/no/n): "))
     (bind ?response (ask-question ?question yes no y n))
     (if (or (eq ?response yes) (eq ?response y))
         then TRUE 
         else FALSE))

(deffunction ask-number (?question)
  (printout t ?question)
  (bind ?answer (read))
  (while (not (numberp ?answer)) do  ;check if answer is a NUMBER
	    (printout t ?question)
	    (bind ?answer (read)))
     ?answer)


;--------------START---------------
(defrule init
	(declare (salience ?*highest-priority*))
	=>
	(printout t "*** Un sistema per la posa di pavimenti in gres porcellanato ***" crlf crlf))

(defrule ask-esperto
	(declare (salience ?*low-priority*))
	=>
	(bind ?answer (yes-or-no-p "Hai mai realizzato prima la posa di un pavimento? "))
	(assert (utente_esperto ?answer)))

(defrule ask-interno-esterno
	(declare (salience ?*low-priority*))
	(not (interno (value ?)))
	=>
	(bind ?answer (yes-or-no-p "E' un pavimento per interni? "))
	(assert (interno (value ?answer))))

(defrule ask-tipo-stanza
	(declare (salience ?*low-priority*))
	(not (tipo_stanza (value ?)))
	=>
	(bind ?answer (ask-question "Indicare in quale stanza si deve effettuare la posa? (cucina, bagno, altro): " cucina bagno altro))
	(assert (tipo_stanza (value ?answer))))

(defrule ask-dimensioni-stanza
	(declare (salience ?*low-priority*))
	(not (dimensioni_stanza (value ?)))
	=>
	(bind ?length (ask-number "Indicare la lunghezza della stanza in metri: "))
	(bind ?width (ask-number "Indicare la larghezza della stanza in metri: "))
	(assert (dimensioni_stanza (value (* ?length ?width)))))

(defrule ask-piastrella-quadrata
	(declare (salience ?*low-priority*))
	(not (piastrella_quadrata (value ?)))
	=>
	(bind ?answer (yes-or-no-p "La piastrella è quadrata?"))
	(assert (piastrella_quadrata (value ?answer))))

(defrule ask-disposizione
	(declare (salience ?*low-priority*))
	(not (disposizione (value ?)))
	=>
	(bind ?answer (ask-question "Indicare in che modo si intende disporre le piastrelle. (diagonale, normale): " diagonale normale))
	(assert (disposizione (value ?answer))))

(defrule ask-pav-presente
	(declare (salience ?*low-priority*))
	(not (pavimento_presente (value ?)))
	=>
	(bind ?answer (yes-or-no-p "E' già presente un pavimento?"))
	(assert (pavimento_presente (value ?answer))))

(defrule ask-decorazioni
	(declare (salience ?*low-priority*))
	(not (decorazioni (value ?)))
	=>
	(bind ?answer (yes-or-no-p "Il pavimento prevede greche o decorazioni?"))
	(assert (decorazioni (value ?answer))))
	
	
	asd























