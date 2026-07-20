# Esercitazione Modulo 3 -----
# File: EsercitazioneRmod3-b_Esercizi.pdf sul moodle del corso
# Solo ESERCIZIO 2: 
# Si consideri la seguente coppia di variabili categoriali A e B con rispettivamente 3 e 2 livelli:
#   A=(a1,a2,a2,a1,a3,a3,a3,a1,a2,a1,a2,a1,a3,a2,a2,a1,a3,a2,a1)
#   B=(b1,b1,b1,b1,b1,b1,b1,b1,b1,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2)
a1 = "a1"
a2 = "a2"
a3 = "a3"
A=c(a1,a2,a2,a1,a3,a3,a3,a1,a2,a1,a2,a1,a3,a2,a2,a1,a3,a2,a1)
A=c("a1","a2","a2","a1","a3","a3","a3","a1","a2","a1","a2","a1","a3","a2","a2","a1","a3","a2","a1")
B = rep(c("b1", "b2"), c(9, 10))
b1 = "b1"
b2 = "b2"
B=c(b1,b1,b1,b1,b1,b1,b1,b1,b1,b2,b2,b2,b2,b2,b2,b2,b2,b2,b2)

# 1. Si costruisca la tabella di contingenza f_o associata alle due variabili A e B.
f_o <- table(A, B)   
# 2. Costruire la tabella di contingenza, f_t, che contenga le frequenze teoriche associate al 
# modello di indipendenza tra A e B.
f_row = margin.table(f_o, 1)
f_col = margin.table(f_o, 2)

# f_t = (f_i. * f_.j)/n

f_t = matrix(nrow = nrow(f_o), ncol = ncol(f_o))
rownames(f_t) = rownames(f_o)
colnames(f_t) = colnames(f_o)
n = sum(f_o)
f_t[1,1] = (f_row[1]*f_col[1])/sum(f_o)
f_t[1,2] = (f_row[1]*f_col[2])/sum(f_o)
f_t[2,1] = (f_row[2]*f_col[1])/sum(f_o)
f_t[2,2] = (f_row[2]*f_col[2])/sum(f_o)
f_t[3,1] = (f_row[3]*f_col[1])/sum(f_o)
f_t[3,2] = (f_row[3]*f_col[2])/sum(f_o)


f_t = matrix(nrow = nrow(f_o), ncol = ncol(f_o))
rownames(f_t) = rownames(f_o)
colnames(f_t) = colnames(f_o)

for (i in 1:length(f_row)) {
  for (j in 1:length(f_col)) {
    f_t[i, j] = (f_row[i]*f_col[j])/sum(f_o)
  }
}


# 3. Calcola e visualizza il valore del Chi-quadrato osservato ottenuto sulla tabella 
# di contingenza f_o.
chisq.test(f_o*100)$statistic

sum((f_o-f_t)^2/f_t)

# 4 Sulla base dei risultati del punto precedente, calcoalre e commentare il chi quadro normalizzato


chimax = sum(f_o)*min(c(nrow(f_o)-1, ncol(f_o)-1))
chisq.test(f_o)$statistic/chimax
# Laboratorio modulo 3 - PRIMA PARTE ---- 
# fasce d'età: 
# 36-43 mesi
# 44-51 mesi 
# Condizioni sperimentali: 
# A: tipologia di giocattoli: a1 macchinine, a2 bamboline 
# B: presenza di repliche: b1 con repliche, b2 senza repliche

# Esercizi prelimanari:
# importare il dataset DataAS.csv, controllare che l'importazione sia avvenuta correttamente 
# esplorare la struttura del dataset
dir(paste0(getwd(), "/slides/modulo3/Laboratorio"))
data = read.csv2((paste0(getwd(), "/slides/modulo3/Laboratorio/DataAS.csv")))
data = read.csv((paste0(getwd(), "/slides/modulo3/Laboratorio/DataAS.csv")), sep =";")
head(data)
str(data)
# Per ognuno degli esercizi, disporre le varibaili relative alle fasce d'età e 
# e al genere sulle righe e quelle relative ai punteggi sulle colonne 
# calcolare le opportune frequenze percentuali di riga o di colonna P(X|Y) oppure P(Y|A)

# 1. Studiare se vi sia una qualche associazione tra le due fasce di età ed 
# il fatto che la persona (il bambino o la bambina) ottenga un numero di 
# risposte corrette superiori o uguali alla soglia minima 10 per quanto 
# riguarda gli stimoli nello scenario macchinine. 
# Verificare tale associazione utilizzando il test descrittivo appropriato.
hist(data$E)
summary(data$E)
data[data$E >= 36 & data$E <= 43, ]
data[data$E >= 44 & data$E <= 51, ]

sM = rowSums(data[, c("a1b1", "a1b2")])
sM[sM > 10]
sM >10
mytab = table((data$E >= 36 & data$E <= 43), sM >10)
rownames(mytab) = c("44.-51", "36-43")
colnames(mytab) = c("<10", ">10")

# 2. Ripetere l’analisi questa volta utilizzando come riferimento gli stimoli 
# associati allo scenario casa delle bambole 
# (si consideri sempre la stessa soglia minima pari a 10).

# 3. Ripetere le analisi precedenti questa volta considerando la variabile 
# sesso al posto della variabile fasce di età.