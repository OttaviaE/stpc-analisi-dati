# Va eseguito dal progetto stpc-analisi-dati (la working directory deve essere
# la radice del repository)
#
# Stesso dataset di slides/modulo6/esempi-lezione/genera-dati-t-test.R (stesso
# seed e stesse istruzioni, quindi le prime sei variabili sono identiche) con
# due variabili in più, generate DOPO tutte le altre per non cambiare i valori
# gia' usati a lezione:
#   - carico_lavoro: carico di lavoro percepito (0-100), associato allo stress
#     in modo diverso nelle due modalita' (interazione)
#   - reparto: amministrazione / commerciale / produzione (predittore a 3
#     categorie dello stress, per l'ANOVA)

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

# ---- nuove variabili -------------------------------------------------------

# carico di lavoro: in presenza e' legato allo stress in modo forte, in
# modalita' ibrida in modo debole (pendenza attesa di stress ~ carico circa
# 0.9 in presenza e 0.2 in ibrido)
presenza = modalita == "presenza"
carico_lavoro = rep(NA, 80)
carico_lavoro[presenza] = 15 + 0.8 * stress[presenza] +
  rnorm(sum(presenza), mean = 0, sd = 6)
carico_lavoro[!presenza] = 25 + 0.6 * stress[!presenza] +
  rnorm(sum(!presenza), mean = 0, sd = 10)
carico_lavoro = round(carico_lavoro, 1)

# reparto: si ordina lo stress (con un po' di rumore) e lo si divide in terzili,
# cosi' lo stress medio cresce da amministrazione a commerciale a produzione
z = stress + rnorm(80, mean = 0, sd = 6)
reparto = cut(z, breaks = quantile(z, probs = c(0, 1/3, 2/3, 1)),
              labels = c("amministrazione", "commerciale", "produzione"),
              include.lowest = TRUE)
reparto = as.character(reparto)

dati = data.frame(modalita, soddisfazione, stress, partecipazione_corso,
                  autoefficacia_prima, autoefficacia_dopo,
                  carico_lavoro, reparto)

# controllo: le prime sei colonne devono coincidere con il dataset del modulo 6
vecchio = read.csv2("slides/modulo6/esempi-lezione/dipendenti.csv",
                    fileEncoding = "UTF-8")
if (!isTRUE(all.equal(vecchio, dati[, 1:6], check.attributes = FALSE))) {
  warning("Le prime sei colonne non coincidono con il dataset del modulo 6")
}

dir.create("slides/modulo7/esempi-lezione", showWarnings = FALSE)
write.csv2(dati, "slides/modulo7/esempi-lezione/dipendenti.csv",
           row.names = FALSE, fileEncoding = "UTF-8")

# controlli veloci
summary(lm(stress ~ carico_lavoro, data = dati))
summary(lm(stress ~ carico_lavoro * modalita, data = dati))
summary(aov(stress ~ reparto, data = dati))

