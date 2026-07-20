# Lezione 7/10/2025 ----- 
# modulo 2-----
dati <- read.csv("~/GitHub/stpc-analisi-dati/slides/modulo2/Laboratorio/dati-es-prima-parte.csv", sep=";")
# esertcizi Rmodulo 2, Prima serie -----
# 1 Costruire la tabella inserendo i dati della stessa in un foglio 
# di calcolo (es. Excel) secondo la procedura di inserimento, 
# controllo e salvataggio descritta a lezione. 
# Caricare infine il file di dati esterno così ottenuto in R ed 
# assegnarlo ad un data frame di nome dati

dati <- read.csv("~/GitHub/stpc-analisi-dati/slides/modulo2/Laboratorio/dati-es-classe.csv", 
                 sep=";")
dat <- read.csv("~/GitHub/stpc-analisi-dati/slides/modulo2/Laboratorio/dati-es-classe.csv")
da <- read.csv2("~/GitHub/stpc-analisi-dati/slides/modulo2/Laboratorio/dati-es-classe.csv")

str(dati)

dati$SC = factor(dati$SC, ordered = TRUE)
dati$SC
# 2 Fare la somma tra la media dei RT del gruppo delle donne e 
# la media dei TP sempre nel
# gruppo delle donne.
sum(dati$GEN == "f")
table(dati$GEN)
barplot(table(dati$GEN))
mean(dati$RT[dati$GEN == "f"])
mean(dati$TP[dati$GEN == "f"])

mean(dati$RT[dati$GEN == "f"]) + mean(dati$TP[dati$GEN == "f"])

# 3 Controllare (semplicemente visualizzando i rispettivi risultati) se la 
# deviazione standard dei
# TP del gruppo delle donne appartenenti alla condizione sperimentale (CS) 
# di tipo A sia superiore
# alla deviazione standard dei TP del gruppo dei maschi appartenenti alla condizione
# sperimentale (CS) di tipo B.

tp_donne_condA = dati$TP[dati$GEN == "f" & dati$CS == "A"]
tp_donne_condA

tp_uomini_condB = dati$TP[dati$GEN == "m" & dati$CS == "B"]
sd(tp_donne_condA) > sd(tp_uomini_condB)

# 4 Costruire due nuove strutture di dati, datiSub1 e datiSub2 contenenti rispettivamente:
#   • datiSub1 la porzione di dati limitata esclusivamente alle variabili quantitative RT e TP;
#  • datiSub2 la porzione di dati limitata esclusivamente alle variabili qualitative o ordinali
# GEN, CS e SC. Cancellare dalla memoria del sistema SOLO la struttura di dati dati

datiSub1 = dati[, c("GEN", "CS", "SC")]
dati[, c(1,3, 5)]
dati[,seq(1, 5, by = 2) ]

datiSub2 = dati[, c("RT", "TP")]
ls()
rm(dati)

# 5 Visualizzare la tabella di frequenza relativa alla variabile GEN di datiSub2.
table(datiSub1$GEN)/nrow(datiSub1)


# Ripetere la procedura questa volta visualizzando la percentuale di donne e quella 
# degli uomini.
# per ottenere la percentuale: 
(table(datiSub1$GEN)/nrow(datiSub1))*100


# 6 Visualizzare la tabella di frequenza relativa alla variabile SC di datiSub2.
# Ripetere la procedura questa volta visualizzando la tabella con le frequenze relative dei tre
# livelli di scolarizzazione.

levels(datiSub1$SC)
(table(datiSub1$SC)/nrow(datiSub1))*100

# Esercizi advanced

# DOPO MODULO 3----- 

# 7 Costruire una tabella di contingenza TC di dimensione 2 × 3 tra la variabile GEN e SC. Visualizzare
# i valori percentuali di TC.
# La sintassi per ottenere una tabella di contingenza 𝑟 × 𝑐 è: table(var1, va2) dove var1 è la
# variabile che si vuole mettere sulle righe e var2 quella sulle colonne

mytab = table(dati$GEN, dati$SC)
mosaicplot(mytab)
margin.table(mytab, 1)
margin.table(mytab, 2)

addmargins(mytab)
chisq.test(mytab)$statistic

# 8 A partire dai in dati costruire una tabella di contingenza
# tra GEN e CS che sia limitata a soli quegli individui che abbiano ottenuto un punteggio di TP
# strettamente maggiore (>) di 55.
newd = dati[dati$TP > 55, ]
table(newd$GEN, newd$CS)
gen55 = dati$GEN[dati$TP > 55]
cs55 = dati$CS[dati$TP > 55]
table(gen55, cs55)
table(dati$GEN[dati$TP > 55], dati$CS[dati$TP > 55])


lavTit = matrix(c(40, 25, 20, 10, 15, 30), nrow = 2, ncol = 3, 
                byrow = TRUE)

rownames(lavTit) = c("Casalinghe", "Lavoratrici")
lavTit
colnames(lavTit) = c("Basso", "medio", "alto")
lavTit
chisq.test(lavTit)$statistic

chiMax = sum(lavTit)*min(c(nrow(lavTit)-1, ncol(lavTit)-1))
chisq.test(lavTit)$statistic/chiMax


chisq.test(lavTit*100)$statistic
chisq.test(lavTit*100)$statistic/ (sum(lavTit*100)*min(c(nrow(lavTit)-1, ncol(lavTit)-1)))
