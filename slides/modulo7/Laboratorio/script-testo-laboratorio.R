# Laboratorio Modulo 6 ---- 
# dataset: `DATIBASE.CSV`
## Esercizio 1 
# Importare il dataset ed assegnarlo alla variabile `dati`
dati = read.csv2("slides/modulo6/Laboratorio/DATIBASE.csv")
head(dati)

# Esercizio 2
## Esercizio 2
# Studiare la relazione tra la variabile dipendente `MC` e la variabile 
# indipendente `TEST` tenendo conto eventualmente della modulazione della 
# variabile di classificazione `ANNOcat`. Nello specifico:
# Produrre una adeguata rappresentazione grafica della relazione tra le variabili in gioco.
plot(dati$MC ~ dati$TEST)
plot(dati$MC ~ dati$TEST, pch = 19,
     col = ifelse(dati$ANNOcat == 0, "salmon", "royalblue"))
legend("topleft",
       fill = c("salmon", "royalblue"), 
       legend = unique(dati$ANNOcat))
# Identificare i diversi modelli di regressione applicabili ai dati e specificarne la loro interpretazione.
# 0. y = beta0 +betaTest
plot(dati$MC ~ dati$TEST)
m0 = lm(MC ~ TEST, data = dati)
summary(m0)
abline(m0)
# 1. y = beta0 + beta1Test + beta2Annocat
# 2. y = beta0 + beta1Test + beta2Annocat + beta3 Test*Annocat

# Stimare il modello di regressione parallelo: 
m1 = lm(MC ~ TEST + ANNOcat, data = dati)
summary(m1)
# Ricostruire le equazioni di riferimento del modello. 
# Annocat = 0: Y = beta0 + beta1Test + beta2*0 
# Annocat = 1: Y = beta0 + beta1Test + beta2*1
# Annocat = 1:Y = beta0^* + beta1test
# beta0^* = beta0+beta2Annocat
plot(dati$MC ~ dati$TEST, pch = 19,
     col = ifelse(dati$ANNOcat == 0, "salmon", "royalblue"))
abline(17.56378, 0.29646, lwd = 2, col= "salmon")
abline(17.56378 + 15.0040, 
       0.29646, lwd = 2, col= "royalblue")
# Rappresentare graficamente le rette di regressioni associate. 
# Calcolare la misura di correlazione lineare tra la variabile dipendente osservata e la variabile dipendente ricostruita dal modello.
y = dati$MC
yhat = m1$fitted.values
plot(y, yhat)
cor(y, yhat)
cor(yhat, y)

### 2d	
# Ripetere l’analisi descritta nel punto c questa volta utilizzando il modello di regressione con componente di interazione.
m2 = lm(MC ~ TEST + ANNOcat + TEST * ANNOcat, data = dati)
summary(m2)
# Ricostruire le equazioni di riferimento del modello. 
plot(dati$MC ~ dati$TEST, pch = 19,
     col = ifelse(dati$ANNOcat == 0, "salmon", "royalblue"))
# anncat = 0 
# y = beta0 + beta1Test
abline(22.4897848, 0.2137530, lwd = 2, col = "salmon")
# y = (beta0 + beta2Annocat) + (beta1Test + beta3Annocat*Test)
abline(22.4897848 + 7.0049461, 
       0.2137530 + 0.1342034, lwd = 2, col = "royalblue")
# Rappresentare graficamente le rette di regressioni associate. 
# Calcolare la misura di correlazione lineare tra la variabile dipendente osservata e la variabile dipendente ricostruita dal modello.
cor(m2$fitted.values, dati$MC)
# Utilizzando i risultati ottenuti nel punto d verificare se la stima del coefficiente angolare associato alla condizione anno 2016 risulti significativamente differente da 0.5 (assunto una probabilità di Errore di I tipo pari a 0.05).
# sistema di ipotesi dei coefficenti dell'output: 
# H0: beta = 0 
# h1: beta != 0
# toss = (betahat - betapop)/se_beta
# H0: beta2 = .50 
# H1: beta2 != .50
# alpha = .05
# toss = (beta -betapop)/se --> betapop = 0
# betapop = 0.50
toss = (7.00495-0.50)/4.23892
toss
2*(1-pt(toss, 95))
# Stimare gli intervalli di confidenza al 95% per i parametri stimati nel punto d.
q95 = qt(.975, 95)


