(deffunction cambia_scelta_da_indice
	(?indice)
	(bind ?f (nth$ ?indice (get-all-facts-by-names car)))

	(bind ?fact-name (fact-slot-value ?f nome))
	(switch ?fact-name
		(case luogo
			then (progn$ (?f1 (get-all-facts-by-names car)) ;elimina tutti i fatti di tipo caratteristica
					(retract ?f1)))
		(case tipo_stanza
			then (do-for-all-facts ((?f1 car)) (neq ?f1:valore interno) ;elimina tutti i fatti tranne interno
					(retract ?f1)))
		(case presenza_pavimento
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza))) ;elimina tutti i fatti tranne interno/esterno e tipo_stanza
					(retract ?f1)))
		(case presenza_rivestimento
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza))) ;elimina tutti i fatti tranne interno/esterno e tipo_stanza
					(retract ?f1)))
		(case presenza_massetto
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza))) ;elimina tutti i fatti tranne interno/esterno e tipo_stanza
					(retract ?f1)))
		(case umidita
			then (do-for-all-facts ((?f1 car)) (neq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) ;elimina tutti i fatti tranne interno
					(retract ?f1)))
		(case impianti_umidita
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome umidita))) ;elimina tutti i fatti tranne interno e umidità e tipo_stanza
					(retract ?f1)))
		(case rifacimento_impianti
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_pavimento))) ;elimina tutti i fatti tranne interno/esterno e tipo_stanza
					(retract ?f)))

		(case massetto_friabile
			then (retract ?f))
		(case pavimento_da_raccordare
			then (do-for-all-facts ((?f1 car)) (eq ?f1:nome massetto_altezza)
					(retract ?f1)))
		(case massetto_a_livello
			then (do-for-all-facts ((?f1 car)) (eq ?f1:nome massetto_altezza)
					(retract ?f1)))

	 	(default (retract ?f))))