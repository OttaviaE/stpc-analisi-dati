# Laboratorio modulo 3 - PRIMA PARTE ---- 
# fasce d'età: 
# 36-43 mesi
# 44-51 mesi 
# Condizioni sperimentali: 
# A: tipologia di giocattoli: a1 macchinine, a2 bamboline 
# B: presenza di repliche: b1 con repliche, b2 senza repliche

# Esercizi prelimanari:
# importare il dataset DataAS.csv, controllare che l'importazione sia avvenuta 
# correttamente 
# esplorare la struttura del dataset
dati = read.csv2((paste0(getwd(), 
                         "/slides/modulo3/Laboratorio/DataAS.csv")))
head(dati)
# Per ognuno degli esercizi, disporre le variabili relative alle fasce d'età e 
# e al genere sulle righe e quelle relative ai punteggi sulle colonne 
# calcolare le opportune frequenze percentuali di riga o di colonna P(X|Y) 
# oppure P(Y|A)

# 1. Studiare se vi sia una qualche associazione tra le due fasce di età ed 
# il fatto che la persona (il bambino o la bambina) ottenga un numero di 
# risposte corrette superiori o uguali alla soglia minima 10 per quanto 
# riguarda gli stimoli nello scenario macchinine. 
# Verificare tale associazione utilizzando il test descrittivo appropriato.

ecat = dati$E
ecat[ecat >= 36 & ecat <=43] = 0
ecat[ecat >= 44 & ecat <=51] = 1
ecat[ecat == 0] = "[36,43]"
ecat[ecat == "1"] = "[44, 51]"
scoreM = numeric(nrow(dati))
scoreM[dati$a1b1 + dati$a1b2 >= 10] = 1
# metodo alternativo sconsigliato 
scoreM = dati$a1b1 + dati$a1b2
scoreM[scoreM >= 10] = 1
scoreM[scoreM > 1 & scoreM < 10] = 0
# etichette 
scoreM[scoreM == 1] = "P > 10"
scoreM[scoreM == "0"] = "P < 10"
myT = table(ecat, scoreM)
myT
prop.table(myT, 1)
Chi2 = chisq.test(myT)$statistic 
Chi2
chiMax = nrow(dati)*min(c(nrow(myT)-1, ncol(myT)-1)) 
Chi2/chiMax


# esercizio 2 
# Ripetere l’analisi questa volta utilizzando come riferimento gli stimoli 
# associati allo scenario casa delle bambole 
# (si consideri sempre la stessa soglia minima pari a 10).
scoreB = numeric(nrow(dati))
scoreB[dati$a2b1 + dati$a2b2 >= 10] = 1
scoreB[scoreB == 1] = "P > 10"
scoreB[scoreB == "0"] = "P < 10"
myT1 = table(ecat, scoreB)
myT1
prop.table(myT1, 1)
Chi21 = chisq.test(myT1)$statistic 
Chi21
chiMax1 = nrow(dati)*min(c(nrow(myT1)-1, ncol(myT1)-1)) 
Chi21/chiMax1


# esercizio 3 
# Ripetere le analisi precedenti questa volta considerando la variabile genere 
#  al posto della variabile fasce di età.

myTgenM= table(dati$S, scoreM)
myTgenM
prop.table(myTgenM)
Chi2genM = chisq.test(myTgenM)$statistic 
Chi2genM
chiMaxgenM = nrow(dati)*min(c(nrow(myTgenM)-1, ncol(myTgenM)-1)) 
Chi2genM/chiMaxgenM

myTgenB= table(dati$S, scoreB)
myTgenB
prop.table(myTgenB)
Chi2genB = chisq.test(myTgenB)$statistic 
Chi2genB
chiMaxgenB = nrow(dati)*min(c(nrow(myTgenB)-1, ncol(myTgenB)-1)) 
Chi2genB/chiMaxgenB

# esercizio 4 
# Verificare se vi sia una associazione positiva tra il numero di risposte 
# corrette ottenute dai partecipanti nello scenario macchinine e il numero di 
# risposte corrette ottenute dai partecipanti nello scenario casa delle bambole 
# (si faccia uso sia di metodi di rappresentazione grafica che di idonee analisi 
# statistiche descrittive).
scoreM = dati$a1b1 + dati$a1b2
scoreB = rowSums(dati[, c("a2b1", "a2b2")])

plot(scoreM, scoreB, pch = 19)
cor(scoreM, scoreB)

# esercizio 5. 
# Stimare i parametri del modello di regressione lineare tra il numero di 
# risposte corrette nello scenario macchinine (variabile indipendente) e il 
# numero di risposte corrette nello scenario casa delle bambole 
# (variabile dipendente). Si tracci la retta di regressione utilizzando la 
# funzione grafica abline().

m = lm(scoreB ~ scoreM)
abline(m, lwd = 2, col = "red")

# esercizio 6. 
# Calcolare l’indice di bontà di adattamento R2 del modello lineare 
# utilizzato nell’esercizio 5 e studiare il comportamento dei residui del 
# modello di regressione utilizzando un’opportuna rappresentazione grafica.

cor(scoreB, scoreM)^2
residui = residuals(m)
# oppure 
# residui = m$residuals
plot(scoreM, residui)
abline(0,0)

# esercizio 7. 
# Verificare se vi sia una associazione positiva tra il numero di risposte 
# corrette ottenute dai partecipanti nello scenario senza repliche e il 
# numero di risposte corrette ottenute dai partecipanti nello scenario con 
# repliche (si faccia uso sia di metodi di rappresentazione grafica che di 
# idonee analisi statistiche descrittive).
scenR = dati$a1b1 + dati$a2b1 
scenNR = dati$a1b2 + dati$a2b2 

plot(scenR, scenNR)
cor(scenR, scenNR)

# esercizio 8. 
# Stimare i parametri del modello di regressione lineare tra il numero di 
# risposte corrette nello scenario senza repliche (variabile indipendente) e 
# il numero di risposte corrette nello scenario con repliche (variabile dipendente). Si tracci la retta di regressione sul grafico precedentemente costruito.
m1 = lm(scenR ~ scenNR)
m1
plot(scenR ~ scenNR)
abline(m1)

# esrcizio 9. 
# Calcolare l’indice di bontà di adattamento R2 del modello lineare utilizzato 
# nell’esercizio 8 e studiare il comportamento dei residui del modello di 
# regressione utilizzando un’opportuna rappresentazione grafica.
cor(scenR, scenNR)^2
res1 = m1$residuals

plot(scenNR, res1)
abline(0,0)
# Esercizio 10 (Pro advanced)
# scrivere una funzione per il calcolo del chi quadro che restituisca: 
# - la tavola delle frequenze osservate congiunte
# - la tvaola delle frequenze percentuali o di riga o di colonna (argomento 
# specificato dall'utente)
# - la statistica chi-quadro
# - il chi-quadro massimo 
# - il chi-quadro normalizzato 
# Suggerimento: per restiuire i risultati attraverso la funzione return(myresults)
# myresults ,deve essere un oggetto lista

myChisq = function(mytab, margin = 1) {
  perctab = prop.table(mytab, margin)
  chisq = chisq.test(mytab)$statistic
  chimax = sum(mytab) * min(c(nrow(mytab)-1, ncol(mytab)-1))
  chinorm = chisq/chimax
  theresults = list(frequenze = mytab, 
                    percentuali = perctab, 
                    chiquadro = chisq, 
                    chimax = chimax, 
                    chinorm = chinorm)
  return(theresults)
}
myChisq(myT)
myChisq(table(mtcars$cyl, mtcars$vs), margin = 2)


myChisq = function(x, y, margin = 1) {
  mytab = table(x, y)
  perctab = prop.table(mytab, margin)
  chisq = chisq.test(mytab)$statistic
  chimax = sum(mytab) * min(c(nrow(mytab)-1, ncol(mytab)-1))
  chinorm = chisq/chimax
  theresults = list(frequenze = mytab, 
                    percentuali = perctab, 
                    chiquadro = chisq, 
                    chimax = chimax, 
                    chinorm = chinorm)
  return(theresults)
}
myChisq(mtcars$cyl, mtcars$vs)
