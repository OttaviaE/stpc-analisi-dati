# laboratorio modulo 2---- 
datitest = read.csv2("slides/modulo2/Laboratorio/datiTestCR2005.csv")
head(datitest)
str(datitest)

summary(datitest)
sum(datitest$SESSO == 1 | datitest$SESSO == 2)

datitest$VOTO >= 60 & datitest$VOTO <= 100
sum(datitest$VOTO >= 60 & datitest$VOTO <= 100)

which(datitest$VOTO < 60 | datitest$VOTO > 100)
which(!(datitest$VOTO >= 60 & datitest$VOTO <= 100))
index = which(datitest$VOTO < 60 | datitest$VOTO > 100)
datitest$VOTO[index]

datitest$VOTO == datitest$VOTO[index]
datitest$VOTO != datitest$VOTO[index]
datitest$VOTO[datitest$VOTO != datitest$VOTO[index]]
datitest$VOTO[-index]
mean(datitest$VOTO[-index])
datitest$VOTO[index] = mean(datitest$VOTO[-index])
sum(datitest$VOTO >= 60 & datitest$VOTO <= 100)

mediaUomini = mean(datitest$VOTO[datitest$SESSO == 1])
mediaDonne = mean(datitest$VOTO[datitest$SESSO == 2])
(mediaDonne > mediaUomini) 

sdUomini = sd(datitest$VOTO[datitest$SESSO == 1])
sdDonne = sd(datitest$VOTO[datitest$SESSO == 2])

sdDonne > sdUomini 

# esercizi sui grafici ---- 
hist(datitest$VOTO)
hist(datitest$VOTO, breaks = "FD")
hist(datitest$VOTO, breaks = "Sturges")


boxplot(datitest$VOTO[datitest$SESSO == 1], 
        main = "Voto Uomini")
boxplot(datitest$VOTO[datitest$SESSO == 2], 
        main = "Voto Donne")
par(mfrow = c(1, 2))
boxplot(datitest$VOTO[datitest$SESSO == 1], 
        main = "Voto Uomini", 
        ylim = c(60, 100))
boxplot(datitest$VOTO[datitest$SESSO == 2], 
        main = "Voto Donne", 
        ylim = c(60, 100))
par(mfrow = c(1, 1))


par(mfrow = c(2, 2))
boxplot(datitest$VOTO[datitest$SESSO == 1], 
        main = "Voto Uomini", 
        ylim = c(60, 100))
boxplot(datitest$VOTO[datitest$SESSO == 2], 
        main = "Voto Donne", 
        ylim = c(60, 100))
hist(datitest$VOTO[datitest$SESSO == 1])
hist(datitest$VOTO[datitest$SESSO == 2])

par(mfrow = c(1, 1))

boxplot(datitest$VOTO[datitest$SESSO == 1], 
        datitest$VOTO[datitest$SESSO == 2],
        names = c("Uomini", "DOnne"))
