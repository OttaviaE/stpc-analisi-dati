# modello di regressione lineare semplice ----
X<-c(9,8,12,9,10,14,5,4,13,7,5,3.4,15.3,6.8)
Y<-c(9,6,8,9,7,10,8,4,11,5,4,2,12.6,8.6)
LM<-lm(Y~X)
plot(X,Y,pch =8)
abline(LM,lwd =2,col ="darkorange")
summary(LM)$coefficients
summary(LM)$coefficients[,1]/summary(LM)$coefficients[,2]
confint(LM, level = .90)
summary(LM)

2*(1-pt(1.514,12))
2*(1-pt(5.495,12))

qt(.975, 12)
abs(qt(.025, 12))
qt(1-(.05/2), 12)


mycoef = data.frame(summary(LM)$coefficients)
q95 = qt(.975, 12)
deltaB0 = mycoef$Std..Error[1] * q95
deltaB1 = mycoef$Std..Error[2] * q95
mycoef$Estimate
CIB0 = c(mycoef$Estimate[1] -deltaB0, 
         mycoef$Estimate[1] +deltaB0) 
CIB0
CIB1 = c(mycoef$Estimate[2] -deltaB1, 
         mycoef$Estimate[2] +deltaB1) 
CIB1
confint(LM)

1.7131434  - 1.1316157 * q95
1.7131434  + 1.1316157 * q95

set.seed(999)
beta1 = 2


X1 <- c(9,8,12,9,10,14,5,4,13,7,5,3.4,15.3,6.8)
X2 <- c(0,1,15,8,2,6,5,1,2,7,15,2.1,10.3,1.3)
Y <- c(9,6,8,9,7,10,8,4,11,5,4,2,12.6,8.6)
LM <- lm(Y~X1+X2)
summary(LM)
2*(1-pt(1.635, 11))

confint(LM)
qt(.975, 11)

# coefficiente angolare
beta0 = 0   # intercetta
n = 100
X1=rnorm(n,100,10)  
e=rnorm(n,0,25)  
Y=beta0 + beta1*X1 + e  
dati = data.frame(X1,Y)    
cor(dati)


plot(X1,Y)
abline(lm(Y ~ X1))

# Regressione con predittore dicotomico ---- 
data = read.csv("slides/modulo3/Laboratorio/DataAS.csv", sep = ";")
# codice equivalente
# data = read.csv2("slides/modulo3/Laboratorio/DataAS.csv")
# calcolo punteggio totale scenario a1: 
data$totA = data$a1b1 + data$a1b2

m = lm(totA ~ S + E, data = data)
summary(m)
plot(data$totA ~ data$E, 
     col = ifelse(data$S == "f", "orchid", 
                  "royalblue"), 
     pch = 19)
abline(-9.6267185, 0.4556003, col = "orchid", 
       lwd = 2)
abline(-9.6267185 + 0.4418265   , 
       0.4556003, col = "royalblue", 
       lwd = 2)
cbind(model.matrix(m), data[, c("S")])

t.test(data$totA ~ data$S, var.equal = TRUE)


X1 <- c(9,8,12,9,10,14,5,4,13,7,5,3.4,15.3,6.8)
A <- c("a1","a2","a1","a1","a2","a1","a2","a2","a2",
       "a1","a2","a2","a1","a1")
A <- as.factor(A)
Y <- c(9,4,8,9,4.5,10,6,3,9.8,5,4,2,11.6,8.6)
LM <- lm(Y~X1+A)
summary(LM)
plot(Y ~ X1, col = ifelse(A == "a1", "orange", 
                         "darkgreen"), 
     pch = 19)

LM$coefficients
# quando x2 = 0 (ovvero per a1): y = beta0 + b1X1
abline(3.4576552, 0.5061069 , col = "orange", lwd =2)
# quando x2 = 1 (a2): y = beta0* + b1X1
# dove beta0* = beta0 + beta2x2
abline(3.4576552 + (-2.1998800), 
       0.5061069 , 
       col = "darkgreen", lwd =2)


linter = lm(Y ~ X1*A)
linter1 = lm(Y ~ X1 + A  + X1*A)
plot(Y ~ X1, col = ifelse(A == "a1", "orange", 
                          "darkgreen"), 
     pch = 19)
summary(linter1)
linter$coefficients
# se x2 = 0
abline(4.1547614, 
       0.4393525  , lwd = 2, col ="orange")
# se x2 = 1
abline(4.1547614 + (-3.3101965), 
       0.4393525 + 0.1265162  , lwd = 2, col ="darkgreen")



summary(LM)

LM1 = lm(Y~A)
summary(LM1)

t.test(Y~A, var.equal = T)


# dati esrecitazione 2 regressione ---- 
dati = read.csv("slides/modulo6/Laboratorio/regressione-completa.csv")
head(dati)
# y = bet0 + beta1Flex + b2X2
# x2 = 0: mindfulness = no 
# X2 = 1 mindufllnes = si
mparallelo = lm(ansia ~ flessibilita+mindfulness, data = dati) 
summary(mparallelo)


plot(dati$ansia ~dati$flessibilita, 
     col = ifelse(dati$mindfulness == "sì", "darkgreen", 
                                                 "firebrick"), 
     pch = 19)
abline(70.65484    , 
       -0.58912 , col = "firebrick", lwd = 2)
abline(70.65484 + (-15.05605 ) , 
       -0.58912 , col = "darkgreen", lwd = 2)

m = lm(ansia ~ flessibilita+mindfulness+ 
         flessibilita*mindfulness, 
       data = dati) 
summary(m)
plot(dati$ansia ~dati$flessibilita, 
     col = ifelse(dati$mindfulness == "sì", "darkgreen", 
                  "firebrick"), 
     pch = 19)
m$coefficients
# gruppo "no"
abline( 64.9351311 , -0.4768907 , 
        lwd = 2, col = "firebrick")
abline( 64.9351311 + (-2.6246182) , 
        -0.4768907 + ( -0.2471864 ), 
        lwd = 2, col = "darkgreen")


# esercizi basati sulla secodnari aserie di esrecizi del moudlo 6 
dati = read.csv("slides/modulo6/Laboratorio/regressione-completa.csv")
head(dati)

# rapporto ansia flessibilità
plot(dati$ansia ~ dati$flessibilita, pch = 6)

msemplice = lm(ansia ~ flessibilita, data = dati)
summary(msemplice)
abline(msemplice$coefficients[1], 
       msemplice$coefficients[2], lwd = 3, col ="blue")

# ic al 90% 

q95 = qt(1-.05, 298)
q95 = qt(.95, 298)

deltaB0 = 3.11204  * q95
deltaB1 = 0.06076  * q95

c(60.45049-deltaB0, 60.45049+deltaB0)
c(-0.53695-deltaB1, -0.53695+deltaB1)

confint(msemplice, level = .90)



