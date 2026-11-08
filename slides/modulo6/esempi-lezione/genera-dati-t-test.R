# Va eseguito dal progetto stpc-analisi-dati (la working directory deve essere
# la radice del repository)

set.seed(2031)

modalita = rep(c("presenza", "ibrido"), times = c(35, 45))

soddisfazione = round(c(rnorm(35, mean = 60, sd = 10),
                        rnorm(45, mean = 68, sd = 10)), 1)

stress = round(c(rnorm(35, mean = 54, sd = 12),
                 rnorm(45, mean = 49, sd = 7)), 1)

prima = rnorm(30, mean = 62, sd = 9)
dopo = prima + rnorm(30, mean = 3, sd = 6)

partecipazione_corso = sample(rep(c("sì", "no"), times = c(30, 50)))

autoefficacia_prima = rep(NA, 80)
autoefficacia_prima[partecipazione_corso == "sì"] = round(prima, 1)

autoefficacia_dopo = rep(NA, 80)
autoefficacia_dopo[partecipazione_corso == "sì"] = round(dopo, 1)

dati = data.frame(modalita, soddisfazione, stress, partecipazione_corso,
                  autoefficacia_prima, autoefficacia_dopo)

write.csv2(dati, "slides/modulo6/esempi-lezione/dipendenti.csv",
           row.names = FALSE, fileEncoding = "UTF-8")
