(deffunction cambia_scelta_da_indice
	(?indice)
	(bind ?f (nth$ ?indice (get-all-facts-by-names car)))

	(bind ?fact-name (fact-slot-value ?f nome))
	(switch ?fact-name
		;----------fatti iniziali----------
		(case luogo
			then (do-for-all-facts ((?f1 car)) TRUE ;elimina tutti i fatti di tipo caratteristica
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

		;------------massetto----------
		(case impianti_fatti
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto)))
					(retract ?f1)))
		(case massetto_fresco
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto)))
					(retract ?f1)))
		(case massetto_fragile
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome massetto_fresco)))
					(retract ?f1)))
		(case massetto_a_livello
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile))) 
					(retract ?f1)))
		(case pavimento_da_raccordare
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello)))
					(retract ?f1)))
		(case altezza_massetto
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello) (eq ?f1:nome pavimento_da_raccordare)))
					(retract ?f1)))
		(case spessore_pavimento
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello) (eq ?f1:nome pavimento_da_raccordare)))
					(retract ?f1)))
		(case pendenza_massetto
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello)))
					(retract ?f1)))
		(case tipo_pavimento_da_porre
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello) (eq ?f1:nome pavimento_da_raccordare)
				(eq ?f1:nome spessore_pavimento) (eq ?f1:nome altezza_massetto)))
					(retract ?f1)))
		(case muri_a_squadra
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_massetto) (eq ?f1:nome massetto_fresco) (eq ?f1:nome massetto_fragile) (eq ?f1:nome massetto_a_livello) (eq ?f1:nome pavimento_da_raccordare)
				(eq ?f1:nome spessore_pavimento) (eq ?f1:nome altezza_massetto) (eq ?f1:nome pendenza_massetto) (eq ?f1:nome tipo_pavimento_da_porre)))
					(retract ?f1)))

		;------------PAVIMENTO------------
		(case tipo_pavimento_presente
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_pavimento) (eq ?f1:nome pavimento_livello) (eq ?f1:nome pendenza_pavimento) (eq ?f1:nome umidita_pavimento) (eq ?f1:nome impianti_umidita) (eq ?f1:nome piano_terra)))
				(retract ?f1)))
		(case umidita_pavimento
			then (do-for-all-facts ((?f1 car)) (or (eq ?f1:nome impianti_umidita) (eq ?f1:nome piano_terra))
				(retract ?f1)))

		;-----------RIVESTIMENTO-----------
		(case muri_a_piombo
			then (do-for-all-facts ((?f1 car)) (not (or (eq ?f1:nome luogo) (eq ?f1:nome tipo_stanza) (eq ?f1:nome presenza_rivestimento)))
				(retract ?f1)))

	 	(default (retract ?f))))






