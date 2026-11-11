# Va eseguito dal progetto stpc-analisi-dati (la working directory deve essere
# la radice del repository)

# Le frequenze di ogni variabile sono fissate: il seed cambia solo l'ordine
# delle righe, non i risultati dei test

set.seed(2032)

n = 120

# reparto e benefit preferito (associati)
reparto = rep(c("amministrazione", "produzione", "commerciale"),
              times = c(35, 45, 40))

benefit_preferito = c(
  sample(rep(c("buoniPpasto", "welfare", "formazione"), times = c(10, 17, 8))),
  sample(rep(c("buoniPpasto", "welfare", "formazione"), times = c(24, 13, 8))),
  sample(rep(c("buoniPasto", "welfare", "formazione"), times = c(9, 12, 19))))

# rischio di burnout: 52 su 120 in totale, stessa quota nei tre reparti
rischio_burnout = c(sample(rep(c("si", "no"), times = c(15, 20))),
                    sample(rep(c("si", "no"), times = c(20, 25))),
                    sample(rep(c("si", "no"), times = c(17, 23))))

# modalità di lavoro e intenzione di lasciare (36 su 120 in totale)
modalita = sample(rep(c("presenza", "ibrido"), times = c(50, 70)))

intenzione_lasciare = rep(NA, n)
intenzione_lasciare[modalita == "presenza"] = sample(rep(c("si", "no"),
                                                         times = c(22, 28)))
intenzione_lasciare[modalita == "ibrido"] = sample(rep(c("si", "no"),
                                                       times = c(14, 56)))

# canale di comunicazione preferito
canale_preferito = sample(rep(c("email", "chat", "riunione", "intranet"),
                              times = c(25, 45, 30, 20)))

dati = data.frame(modalita, reparto, rischio_burnout, intenzione_lasciare,
                  canale_preferito, benefit_preferito)

# mescola l'ordine delle righe
dati = dati[sample(n), ]
rownames(dati) = NULL

write.csv2(dati, "slides/modulo6/esempi-lezione/dipendenti-categoriali.csv",
           row.names = FALSE, fileEncoding = "UTF-8")
