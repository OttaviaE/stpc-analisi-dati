# modulo 3 b---- 
x = c(9,8,12,9,10,14,5,4,13,7,5)
y = c(9,6,8,9,7,10,8,4,11,5,4)


plot(x, y, cex = 3, pch = 19, 
     main = "Grafico a dispersione - Scatterplot", 
     ylim = c(0, 20), xlim = c(0,20))
abline(v = x[1])
abline(h = y[1])

for (i in 1:length(x)) {
  abline(v = x[i], col = i)
  abline(h = y[i], col = i)
}
abline(v = mean(x), col ="red", lty = 2, lwd = 3)
abline(h = mean(y), col ="red", lty = 2, lwd = 3)


d = data.frame(x, y)
d$scartiX = d$x - mean(d$x)
d$scartiY = d$y - mean(d$y)
d$prod = d$scartiX*d$scartiY
sum(d$prod)/(nrow(d)-1)
z = -y
plot(x, z, cex = 3, pch = 19, 
     main = "Grafico a dispersione - Scatterplot", xlim = c(0,20))



x = 1:100 
y = rnorm(length(x))

plot(x, y, pch = 19, cex = 2)
cor(x, y)
cov(x, y)

z = 2*x+rnorm(length(x), mean = 40, sd= 3)
plot(x, z)
cor(x, z)

z1 = -2*x+rnorm(length(x), mean = 40, sd= 65)
plot(x, z1)
cor(x, z1)

# regressione lineare ----- 
x = c(9,8,12,9,10,14,5,4,13,7,5)
y = c(9,6,8,9,7,10,8,4,11,5,4)
dati = data.frame(x, y ) # crea il dataset 
plot(dati$y ~ dati$x, pch = 19, cex = 2) # scatterplot
cor(dati$x, dati$y) # correlazione x-y


mX = mean(dati$x) # media di x
my = mean(dati$y) # media di y

dati$scartiX = dati$x - mX # calcolo scarti x e creazione nuova varibaile dentro dati
dati$scartiY = dati$y - my # calcolo scarti y e creazione nuova varibaile dentro dati
dati # visualizza dati in console
dati$prodScarti = dati$scartiX*dati$scartiY # calcolo prodotto scarti e creazione nuova variabile dentro dati
beta1 = sum(dati$prodScarti)/sum(dati$scartiX^2) # calcolo coefficiente angolare
beta = my - beta1*mX # calcolo intercetta
plot(dati$y ~ dati$x, pch = 19, cex = 2) # scatterplot
abline(a = beta, b = beta1, col = "red", lwd = 2) # aggiunge retta di regressione al grafico

y15 = beta + beta1*15 # previsione della y per X = 15

dati$y_hat = beta + beta1*dati$x # calcolo valori di y attesi dal modello 
points(dati$x, dati$y_hat, pch = 4, cex = 3) # aggiunta dei punti attesi al grafico

dati$e = dati$y - dati$y_hat # calcolo residui del modello 

sum(dati$e^2) # minimi quadrati

sum(dati$y - beta + (beta1+1.5)*dati$x)^2 # distanze al quadrato di un modello "sbagliato"

abline(a = beta, b = beta1+0.5, col = "blue", lwd =2) # retta del modello sbagliato

# plot residui in funzione del predittore
plot(dati$e ~ dati$x, pch = 19)
abline(0, 0, col ="red", lwd = 2)

# fit dello stesso modello usando la funzione lm
fit = lm(y ~ x, data = dati)
fit
# calcolo dei residui 
myres = fit$residuals
residuals(fit)
# plot dei residui 
plot(myres ~ dati$x, pch = 19)
hist(myres)
summary(myres)

# relazione tra r e r-quadro 
r = seq(-1, 1, length.out = 1000)
plot(r, r^2)
abline(v = c(-.10, .10))
abline(h = .10^2)

abline(v = c(-.20, .20), col = "red")
abline(h = .20^2, col = "red")


abline(v = c(-.30, .30), col = "royalblue")
abline(h = .30^2, col = "royalblue")

abline(v = c(-.50, .50), col = "salmon")
abline(h = .50^2, col = "salmon")