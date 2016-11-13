;----------------------------MASSETTO---------------------------------

(defrule no_impianti_fatti
	(declare (salience ?*high_priority*))
	(not (lavoro))
	
	(car (nome presenza_massetto) (valore no))
	(car (nome impianti_fatti) (valore no))
	=>
	(bind ?*soluzione* "Devi vedere prima se passeranno degli impianti (acqua, fognatura, elettricità, etc.) sotto il massetto.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente e gli impianti non sono stati fatti. Il consiglio è dunque quello di vedere se ci sono %nimpianti che passeranno sotto il massetto.")
	(assert (lavoro)))

(defrule si_impianti_fatti
	(declare (salience ?*high_priority*))
	(not (lavoro))
	
	(car (nome presenza_massetto) (valore no))
	(car (nome impianti_fatti) (valore si))
	=>
	(bind ?*soluzione* "Procedi alla realizzazione del massetto")
	(bind ?*spiegazione* "È stato dedotto che il massetto non è presente e che gli impianti sono stati fatti. Il consiglio è quindi di procedere alla %nrealizzazione del massetto.")
	(assert (lavoro)))

(defrule massetto_fresco
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore si))
	=>
	(bind ?*soluzione* "Il massetto deve prima asciugare, non si può continuare con il lavoro.")
	(bind ?*spiegazione* "È stato dedotto che è presente un massetto ma che è fresco. Il consiglio è quindi di lasciarlo asciugare per almeno 10 giorni %ndalla sua realizzazione.")
	(assert (lavoro)))

(defrule umidita_impianti_massetto
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome umidita_massetto) (valore si))
	(car (nome impianti_umidita) (valore si))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere causata da impianti idrici guasti, rimuovi il massetto e chiama uno specialista (idraulico).")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il massetto è presente, c'è umidità e passano degli impianti sotto il %nmassetto. Il consiglio è dunque quello di rimuovere il massetto e chiamare uno specialista per verificare gli impianti.")
	(assert (lavoro)))

(defrule umidita_piano_terra_massetto
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome umidita_massetto) (valore si))
	(car (nome impianti_umidita) (valore no))
	(car (nome piano_terra) (valore si))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere causata dal sottosuolo, occorre rimuovere il massetto (se presenti) e procedere alla %nimpermeabilizzazione del sottofondo.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il massetto è presente, c'è umidità, non passano degli impianti sotto il %nmassetto ma ci si trova a piano terra. Il consiglio è dunque di rimuovere il massetto, e rifarlo con l'impermeabilizzazione.")
	(assert (lavoro)))

(defrule umidita_no_piano_terra_massetto
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_massetto) (valore si))
	(car (nome umidita_massetto) (valore si))
	(car (nome impianti_umidita) (valore no))
	(car (nome piano_terra) (valore no))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere quella di risalita dai muri, bisogna impermeabilizzare il muro in modo da non far salire l'umidità.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il massetto è presente, c'è umidità, non passano degli impianti sotto il %nmassetto e non ci si trova a piano terra. Il consiglio è dunque di impermeabilizzare il muro in quanto l'umidità potrebbe %nessere quella di risalita dai muri.")
	(assert (lavoro)))

(defrule massetto_fragile
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore si))
	=>
	(bind ?*soluzione* "Il massetto è fragile, occorre rimuoverlo e rifarlo.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco ma è fragile. Il consiglio è quindi di rimuoverlo e rifarlo poiché vuol %ndire che è vecchio o che non è stato fatto a regola.")
	(assert (lavoro)))

(defrule massetto_non_a_livello
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore interno))
	(car (nome massetto_a_livello) (valore no))
	=>
	(bind ?*soluzione* "Il massetto deve essere rimosso e rifatto poiché non a livello.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale interno, ma non è a livello. Il %nconsiglio è quindi di rimuoverlo e rifarlo poichè non è possibile farvi la posa di un qualsiasi tipo di pavimento sopra.")
	
	(assert (lavoro)))

(defrule no_spessore_piastrella
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore interno))
	(car (nome massetto_a_livello) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome spessore_pavimento) (valore no))
	=>
	(bind ?*soluzione* "Se non si conosce lo spessore della piastrella non si può cominciare la posa del pavimento poiché non si sa a che altezza %nrealizzare il massetto.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale interno, che è a livello. Inoltre %nil pavimento è da raccordare con un altro già presente ma non si conosce lo spessore del pavimento da posizionare. %nIl consiglio è quindi di scegliere prima il tipo di pavimento da posizionare e riprovare.")
	(assert (lavoro)))

(defrule massetto_alto
	(declare (salience ?*high_priority*))
	(not (lavoro))
	
	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore interno))
	(car (nome massetto_a_livello) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome altezza_massetto) (valore alto))
	=>
	(bind ?*soluzione* "Il massetto è troppo alto per la posa del pavimento selezionato, rimuoverlo e rifarlo in modo che sia alla giusta altezza.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale interno, che è a livello. Inoltre %nil pavimento è da raccordare con un altro già presente ma il massetto risulta troppo alto per il tipo di pavimento scelto %nda posare. Il consiglio è dunque di rimuoverlo e rifarlo.")
	(assert (lavoro)))

(defrule massetto_basso
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore interno))
	(car (nome massetto_a_livello) (valore si))
	(car (nome pavimento_da_raccordare) (valore si))
	(car (nome altezza_massetto) (valore basso))
	=>
	(bind ?*soluzione* "Il massetto è troppo basso per la posa del pavimento selezionato, rifarlo in modo che sia alla giusta altezza.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale interno, che è a livello. Inoltre %nil pavimento è da raccordare con un altro già presente ma il massetto risulta troppo basso per il tipo di pavimento scelto %nda posare. Il consiglio è dunque di rifarlo.")
	(assert (lavoro)))

(defrule massetto_senza_pendenza
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore esterno))
	(car (nome pendenza_massetto) (valore no))
	=>
	(bind ?*soluzione* "Il massetto non ha la giusta pendenza per lo scolo dell'acqua, rimuoverlo e rifarlo.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale esterno e che non è alla giusta %npendenza per favorire lo scolo dell'acqua. Il consiglio è dunque di rimuoverlo e rifarlo.")
	(assert (lavoro)))

(defrule muri_non_a_squadra_esterno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore esterno))
	(car (nome pendenza_massetto) (valore si))
	(car (nome muri_a_squadra) (valore no))
	=>
	(bind ?*soluzione* "I muri non sono a squadra, conviene usare un tipo di posa in diagonale e pezzi piccoli in modo da camuffare le %nimperfezioni. Inoltre parti dal punto più a vista nella stanza, in genere il lato opposto all'entrata.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale esterno e che è alla giusta %npendenza per favorire lo scolo dell'acqua, ma i muri non sono a squadra. Il consiglio è dunque di scegliere un certo tipo di %nposa (quella in diagonale o obliqua) e pezzi piccoli per camuffare le imperfezioni.")
	(assert (lavoro)))

(defrule muri_non_a_squadra_interno_piastrella
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome massetto_a_livello) (valore si))
	(car (nome luogo) (valore interno))
	(or (car (nome pavimento_da_raccordare) (valore no))
		(car (nome altezza_massetto) (valore giusto)))
	(car (nome muri_a_squadra) (valore no))
	(or (car (nome tipo_pavimento_da_porre) (valore piastrella))
		(car (nome tipo_pavimento_da_porre) (valore marmo)))
	=>
	(bind ?*soluzione* "I muri non sono a squadra, conviene usare un tipo di posa in diagonale e pezzi piccoli in modo da camuffare le %nimperfezioni. Inoltre parti dal punto più a vista nella stanza, in genere il lato opposto all'entrata.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale interno e che è a livello ma i %nmuri non sono a squadra. Il consiglio è dunque di scegliere un certo tipo di posa (quella in diagonale o obliqua) e %npezzi piccoli per camuffare le imperfezioni.")
	(assert (lavoro)))

(defrule muri_non_a_squadra_piastrella_interno_parquet
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome massetto_a_livello) (valore si))
	(car (nome luogo) (valore interno))
	(or (car (nome pavimento_da_raccordare) (valore no))
		(car (nome altezza_massetto) (valore giusto)))
	(car (nome muri_a_squadra) (valore no))
	(car (nome tipo_pavimento_da_porre) (valore parquet))
	=>
	(bind ?*soluzione* "Scegli un tipo di posa del parquet tale da permettere di camuffare le imperfezioni (spina di pesce o posa in diagonale).")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale interno, che è a livello e il tipo %ndi pavimento da porre è il parquet, ma i muri non sono a squadra. Il consiglio è dunque di scegliere un tipo di posa del parquet %ntale da permettere di camuffare le imperfezioni (come la spina di pesce o la posa in diagonale).")
	(assert (lavoro)))

(defrule muri_a_squadra_interno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome massetto_a_livello) (valore si))
	(car (nome luogo) (valore interno))
	(or (car (nome pavimento_da_raccordare) (valore no))
		(car (nome altezza_massetto) (valore giusto)))
	(car (nome muri_a_squadra) (valore si))
	=>
	(bind ?*soluzione* "Comincia con la posa del pavimento.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale interno, che è a livello, è alla %ngiusta altezza e i muri sono a squadra. Il consiglio è dunque di iniziare con la posa del pavimento.")
	(assert (lavoro)))

(defrule muri_a_squadra_esterno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_massetto) (valore si))
	(car (nome massetto_fresco) (valore no))
	(car (nome massetto_fragile) (valore no))
	(car (nome luogo) (valore esterno))
	(car (nome pendenza_massetto) (valore si))
	(car (nome muri_a_squadra) (valore si))
	=>
	(bind ?*soluzione* "Comincia con la posa del pavimento.")
	(bind ?*spiegazione* "È stato dedotto che il massetto è presente, non è fresco o fragile, che si trova in un locale esterno, che è alla giusta pendenza %nper favorire lo scolo dell'acqua e i muri sono a squadra. Il consiglio è dunque di iniziare con la posa del pavimento.")
	(assert (lavoro)))


;---------------------------------------PAVIMENTO----------------------------------------

(defrule pavimento_non_a_livello_interno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome pavimento_livello) (valore no))
	=>
	(bind ?*soluzione* "Rimuovi il pavimento esistente e lo strato di fondo sottostante e rifallo.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il pavimento è presente ma non è a livello. %nIl consiglio è dunque quello di %nrimuovere pavimento e massetto e rifarli.")
	(assert (lavoro)))

(defrule pavimento_senza_pendenza_esterno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore esterno))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	(car (nome pendenza_pavimento) (valore no))
	=>
	(bind ?*soluzione* "Rimuovi il pavimento esistente e lo strato di fondo sottostante e rifallo.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale esterno, il pavimento è presente ma non ha la giusta pendenza per favorire lo scolo %ndell'acqua. Il consiglio è dunque quello di rimuovere pavimento e massetto e rifarli per evitare problemi di umidità.")
	(assert (lavoro)))

(defrule pezzi_sollevati_marmo_piastrelle
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	(car (nome pezzi_sollevati_pavimento) (valore si))
	=>
	(bind ?*soluzione* "Rimuovi il pavimento esistente e lo strato di fondo sottostante e rifallo.")
	(bind ?*spiegazione* "È stato dedotto che il pavimento è presente ed è composto da piastrelle o marmo e vi sono pezzi sollevati o non aderenti. Il %nconsiglio è dunque quello di rimuovere pavimento e massetto e rifarli.")
	(assert (lavoro)))

(defrule pezzi_sollevati_parquet
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome tipo_pavimento_presente) (valore parquet))
	(car (nome pezzi_sollevati_pavimento) (valore si))
	=>
	(bind ?*soluzione* "Rimuovi il pavimento esistente e rifallo.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il pavimento è presente ed è costituito da parquet e vi sono pezzi sollevati o non aderenti. Il %nconsiglio è dunque quello di rimuovere pavimento e rifarlo.")
	(assert (lavoro)))

(defrule pezzi_rovinati_pochi_piastrelle_marmo
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	(car (nome pezzi_rovinati_pavimento) (valore pochi))
	=>
	(bind ?*soluzione* "Puoi optare per la sostituzione se il pavimento non è scolorito e si hanno a disposizione pezzi di ricambio, altrimenti rifai il pavimento.")
	(bind ?*spiegazione* "È stato dedotto che il pavimento è presente ed è composto da piastrelle o marmo e che vi sono pochi pezzi scheggiati o lesionati. %nIl consiglio è dunque quello di optare per il rattoppo, sostituendo solo i pezzi rovinati se sono disponibili pezzi di ricambio %ne non ci sono differenze di tonalità tra il pezzo di ricambio e il pavimento presente, altrimenti rifai il pavimento.")
	(assert (lavoro)))

(defrule pezzi_rovinati_molti_piastrelle_marmo
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	(car (nome pezzi_rovinati_pavimento) (valore molti))
	=>
	(bind ?*soluzione* "Sostituisci il pavimento poiché le piastrelle rovinate sono troppe.")
	(bind ?*spiegazione* "È stato dedotto che il pavimento è presente ed è composto da piastrelle o marmo e che vi sono molti pezzi scheggiati o lesionati. %nIl consiglio è dunque quello di rimuovere pavimento e massetto e rifarli.")
	(assert (lavoro)))

(defrule pezzi_rovinati_pochi_parquet
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome tipo_pavimento_presente) (valore parquet))
	(car (nome pezzi_rovinati_pavimento) (valore pochi))
	=>
	(bind ?*soluzione* "Puoi optare per la sostituzione se il pavimento non è scolorito e si hanno a disposizione pezzi di ricambio, altrimenti rifai il pavimento.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il pavimento è presente ed è composto da parquet e che vi sono pochi pezzi scheggiati o lesionati. %nIl consiglio è dunque quello di optare per il rattoppo, sostituendo solo i pezzi rovinati se sono disponibili pezzi di ricambio %ne non ci sono differenze di tonalità tra il pezzo di ricambio e il pavimento presente, altrimenti procedere alla sostituzione del pavimento.")
	(assert (lavoro)))

(defrule pezzi_rovinati_molti_parquet
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome tipo_pavimento_presente) (valore parquet))
	(car (nome pezzi_rovinati_pavimento) (valore molti))
	=>
	(bind ?*soluzione* "Sostituisci il pavimento poiché le piastrelle rovinate sono troppe.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il pavimento è presente ed è composto da parquet e che vi sono molti pezzi scheggiati o lesionati. %nIl consiglio è dunque quello di rimuovere il pavimento e rifarlo.")
	(assert (lavoro)))

(defrule umidita_impianti
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome umidita_pavimento) (valore si))
	(car (nome impianti_umidita) (valore si))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere causata da impianti idrici guasti, rimuovi il massetto e il pavimento e chiama uno specialista (idraulico).")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il pavimento è presente, c'è umidità e passano degli impianti sotto il %npavimento. Il consiglio è dunque quello di rimuovere pavimento e massetto e chiamare uno specialista per verificare gli impianti.")
	(assert (lavoro)))

(defrule umidita_piano_terra
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome umidita_pavimento) (valore si))
	(car (nome impianti_umidita) (valore no))
	(car (nome piano_terra) (valore si))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere causata dal sottosuolo, occorre rimuovere il pavimento e il massetto (se presenti) e procedere alla %nimpermeabilizzazione del sottofondo.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il pavimento è presente, c'è umidità, non passano degli impianti sotto il %npavimento ma ci si trova a piano terra. Il consiglio è dunque di rimuovere pavimento e massetto, impermeabilizzare il sottofondo e rifarli.")
	(assert (lavoro)))

(defrule umidita_no_piano_terra
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome umidita_pavimento) (valore si))
	(car (nome impianti_umidita) (valore no))
	(car (nome piano_terra) (valore no))
	=>
	(bind ?*soluzione* "L'umidità potrebbe essere quella di risalita dai muri, bisogna impermeabilizzare il muro in modo da non far salire l'umidità.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il pavimento è presente, c'è umidità, non passano degli impianti sotto il %npavimento e non ci si trova a piano terra. Il consiglio è dunque di impermeabilizzare il muro in quanto l'umidità potrebbe %nessere quella di risalita dai muri.")
	(assert (lavoro)))

(defrule rumore_calpestio
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	(car (nome rumore_al_calpestio) (valore si))
	=>
	(bind ?*soluzione* "Il pavimento è stato posto senza un opportuno spazio di assestamento lungo il perimetro dei muri, per risolverlo occorre rimuovere %nil pavimento.")
	(bind ?*spiegazione* "È stato dedotto che il pavimento è presente ed è costituito da piastrelle o marmi e si sente un rumore al calpestio. Il consiglio %nè dunque di rimuovere pavimento e massetto e procedere a porre uno spazio adeguato di assestamento al pavimento.")
	(assert (lavoro)))

(defrule fughe_polvere
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	(car (nome polvere_sulle_fughe) (valore si))
	=>
	(bind ?*soluzione* "Usa un impermeabilizzante per ricoprire le fughe.")
	(bind ?*spiegazione* "È stato dedotto che il pavimento è presente e si vede della polvere sulle fughe. Il consiglio è dunque di procedere ad %nimpermeabilizzarle con appositi prodotti.")
	(assert (lavoro)))

(defrule posa_sopra_si_piastrelle
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(or (car (nome luogo) (valore interno))
		(car (nome luogo) (valore esterno)))
	(car (nome presenza_pavimento) (valore si))
	(or (car (nome tipo_pavimento_presente) (valore piastrelle))
		(car (nome tipo_pavimento_presente) (valore marmo)))
	(or (car (nome pavimento_livello) (valore si))
		(car (nome pendenza_pavimento) (valore si)))
	(car (nome pezzi_sollevati_pavimento) (valore no))
	(car (nome umidita_pavimento) (valore no))
	=>
	(bind ?*soluzione* "Puoi optare per la posa sopra usando anche pavimenti ultra sottili come il gres o gli adesivi in PVC.")
	(bind ?*spiegazione* "È stato dedotto che il pavimento è presente ed è composto da piastrelle o marmi, è a livello e non presenta né problemi di %numidità e né pezzi sollevati. Il consiglio è dunque quello di procedere alla posa sopra il pavimento esistente o se si vuole rinnovarlo si possono porre gli adesivi in PVC.")
	(assert (lavoro)))

(defrule posa_sopra_parquet
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome presenza_pavimento) (valore si))
	(car (nome tipo_pavimento_presente) (valore parquet))
	(car (nome pavimento_livello) (valore si))
	(car (nome pezzi_sollevati_pavimento) (valore no))
	(car (nome umidita_pavimento) (valore no))
	=>
	(bind ?*soluzione* "Se si desidera sostituire il pavimento occorre rimuoverlo altrimenti si può lasciarlo com'è.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, il pavimento è presente ed è composto da parquet, è a livello e non presenta né problemi di %numidità e né pezzi sollevati. Il consiglio è dunque quello di rimuovere il pavimento se si desidera rinnovarlo altrimenti lasciarlo com'è poiché è in buone condizioni.")
	(assert (lavoro)))


;---------------------------------RIVESTIMENTO------------------------------------

(defrule muri_non_a_piombo
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore no))
	(car (nome muri_a_piombo) (valore no))
	=>
	(bind ?*soluzione* "Aggiustare i muri per garantire una buona posa.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, in particolare di un bagno o cucina, non è presente un rivestimento e i muri %nnon sono a piombo. Il consiglio è dunque quello di rifare i muri per garantire un corretto risultato nella posa del rivestimento.")
	(assert (lavoro))
	(assert (rivestimento_parte_due)))

(defrule fondo_gesso_rasato_bagno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome tipo_stanza) (valore bagno))
	(car (nome presenza_rivestimento) (valore no))
	(car (nome muri_a_piombo) (valore si))
	(car (nome sottofondo_muri) (valore gesso_rasato))
	=>
	(bind ?*soluzione* "Devi passare prima una mano di idropittura, farla asciugare e poi iniziare con la posa.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, in particolare di un bagno, non è presente un rivestimento, i muri sono a %npiombo e il sottofondo del muro è il gesso rasato. Il consiglio è dunque quello di passare una mano di idropittura per %ngarantire che il rivestimento aggrappi nella maniera giusta.")
	(assert (lavoro))
	(assert (rivestimento_parte_due)))

(defrule fondo_gesso_rasato_cucina
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome tipo_stanza) (valore cucina))
	(car (nome presenza_rivestimento) (valore no))
	(car (nome muri_a_piombo) (valore si))
	(car (nome sottofondo_muri) (valore gesso_rasato))
	=>
	(bind ?*soluzione* "Devi passare prima una mano di idropittura, farla asciugare e poi iniziare con la posa. Considera che si può anche effettuare la %nposa delle piastrelle nella fascia dietro la cucina.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, in particolare di una cucina, non è presente un rivestimento, i muri sono a %npiombo e il sottofondo del muro è il gesso rasato. Il consiglio è dunque quello di passare una mano di idropittura per garantire %nche il rivestimento aggrappi nella maniera giusta.")
	(assert (lavoro))
	(assert (rivestimento_parte_due)))

(defrule fondo_ok_cucina
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome tipo_stanza) (valore cucina))
	(car (nome presenza_rivestimento) (valore no))
	(car (nome muri_a_piombo) (valore si))
	(or (car (nome sottofondo_muri) (valore muro_pitturato))
		(car (nome sottofondo_muri) (valore sabbia_e_cemento)))
	=>
	(bind ?*soluzione* "Puoi iniziare la posa ricordando che puoi anche posare le piastrelle nella fascia dietro la cucina invece di posarle per tutto il muro.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, in particolare di una cucina, non è presente un rivestimento, i muri sono a %npiombo e il sottofondo è o un muro pitturato o sabbia e cemento. Il consiglio è dunque quello di passare alla posa del rivestimento.")
	(assert (lavoro))
	(assert (rivestimento_parte_due)))

(defrule fondo_ok_bagno
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(car (nome tipo_stanza) (valore bagno))
	(car (nome presenza_rivestimento) (valore no))
	(car (nome muri_a_piombo) (valore si))
	(or (car (nome sottofondo_muri) (valore muro_pitturato))
		(car (nome sottofondo_muri) (valore sabbia_e_cemento)))
	=>
	(bind ?*soluzione* "Puoi iniziare la posa.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un locale interno, in particolare di una cucina, non è presente un rivestimento, i muri sono a %npiombo e il sottofondo è o un muro pitturato o sabbia e cemento. Il consiglio è dunque quello di passare alla posa del rivestimento.")
	(assert (lavoro))
	(assert (rivestimento_parte_due)))

(defrule pezzi_sollevati_rivestimento
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	(car (nome pezzi_sollevati_rivestimento) (valore si))
	=>
	(bind ?*soluzione* "Rimuovi il rivestimento esistente e rifallo.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un luogo interno, di una cucina o di un bagno, è presente un rivestimento e ci sono pezzi %nsollevati. Il consiglio è dunque quello di rimuovere il rivestimento esistente e rifarlo.")
	(assert (lavoro))
	(assert (rivestimento_parte_due)))

(defrule pezzi_rovinati_rivestimento_pochi
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	(car (nome pezzi_rovinati_rivestimento) (valore pochi))
	=>
	(bind ?*soluzione* "Puoi sostituire i pezzi scheggiati con dei nuove se li possiedi (fai attenzione alla diversa tonalità che potrebbe esserci %ntra i pezzi presenti e quelli nuovi con cui fare il rattoppo). Altrimenti sostituisci tutto il rivestimento.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un luogo interno, in particolare di una cucina o di un bagno, è presente un rivestimento e ci %nsono pochi pezzi rovinati. Il consiglio è dunque di effettuare un rattoppo se si hanno a disposizione dei pezzi di %nricambio e se non c'è differenza di tonalità tra i pezzi presenti e quelli di ricambio.")
	(assert (lavoro))
	(assert (rivestimento_parte_due)))

(defrule pezzi_rovinati_rivestimento_molti
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	(car (nome pezzi_rovinati_rivestimento) (valore molti))
	=>
	(bind ?*soluzione* "Sostituisci tutto il rivestimento.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un luogo interno, in particolare di una cucina o di un bagno, è presente un rivestimento e ci %nsono molti pezzi rovinati. Il consiglio è dunque di sostituire il rivestimento.")
	(assert (lavoro))
	(assert (rivestimento_parte_due)))

(defrule rivestimento_buone_condizioni
	(declare (salience ?*high_priority*))
	(not (lavoro))

	(car (nome luogo) (valore interno))
	(or (car (nome tipo_stanza) (valore bagno))
		(car (nome tipo_stanza) (valore cucina)))
	(car (nome presenza_rivestimento) (valore si))
	(car (nome rivestimento_a_piombo) (valore si))
	(car (nome pezzi_rovinati_rivestimento) (valore no))
	(car (nome pezzi_sollevati_rivestimento) (valore no))
	=>
	(bind ?*soluzione* "Il pavimento è in buone condizioni potresti anche rinnovarlo usando gli adesivi in PVC.")
	(bind ?*spiegazione* "È stato dedotto che si tratta di un luogo interno, in particolare di un bagno o una cucina, è presente un rivestimento, il %nrivestimento è a piombo e non ci sono pezzi rovinati o sollevati. Il consiglio è dunque di lasciare il pavimento così %ncom'è o rinnovarlo usando adesivi in PVC.")
	(assert (lavoro))
	(assert (rivestimento_parte_due)))

