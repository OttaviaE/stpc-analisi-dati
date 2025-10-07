# Lezione 16/09/2025
2 + 2
sqrt(64)

var1 <- 3+3
var1

var2 = var1 + 22
var2

var3 = 1:5
var4 = 5:1

var5 = c("5", "2", 3)

# 18 settembre 
a = c(1,5, 8,0)
matrix(a, 2,2,byrow = T)
matrix(a, 2,2,byrow = F)

b = rnorm(400)
b[25]
b[c(1, 4,35, 399)]

peso = c(3.3, 4, 5, 6.4, 3.5, 4.2)
sesso = rep(c("M", "F"), length.out = length(peso))
sesso1 = rep(c("M", "F"), c(2,4))
matrix(c(sesso, peso), ncol=3)

data = data.frame(sesso, sesso1, peso)

myresults = t.test(peso ~ sesso, data= data) 

# Lezione 25/09/2025

sesso = rep(c("M", "F"), length.out = length(peso)*100)
sesso = rep(c("M", "F"), length.out = length(peso))

sesso
sex_default = factor(sesso)
sex_mod = factor(sesso, levels = c("M", "F"))

diagnosi = c(rep("border", 43), rep("ocd", 54), 
             rep("DepMax", 5), rep("Narc", 190))
diag_normale = factor(diagnosi)
levels(diag)
diag_mod = factor(diag, 
                  levels = c("ocd", "border", "Narc", "DepMax"))
levels(diag)
data.frame(diag_normale, as.integer(diag_normale), 
           diag_mod, as.integer(diag_mod))


ses =rep(c("basso", "medio", "alto"), c(20, 50, 10))

ses_factor = factor(ses)
as.integer(ses_factor)

ses_factor_mod = factor(ses, 
                        levels = c("basso", "medio", "alto"))
ses_factor_order = factor(ses, 
                           levels = c("basso", "medio", "alto"), 
                           ordered = TRUE)


genere = c("M","F", "M", "F", "M")
a = as.factor(genere)

table(ses_factor_order)
table(ses_factor_order)/length(ses_factor_order)

length(diag)

barplot(table(ses_factor_order)/length(ses_factor_order), 
        ylim=c(0,1))

set.seed(999)
rnorm()
hist(rnorm(100))
hist(rnorm(100, mean = -234), breaks = 12)


# Esercitazione 1b IN CLASSE -----

taba = data.frame(  X1 = c(12, 13, 11, 11, 13, 9, 5, 11, 13, 11, 6),
                    X2 = c(12.0, 15.3, 13.3, 17.2, 13.6, 12.4, 11.7, 10.0, 11.0, 18.3, 15.3),
                    X3 = c(1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1),
                    X4 = c("m", "m", "m", "f", "f", "m", "m", "f", "m", "f", "m"),
                    X5 = c(1, 1, 2, 1, 3, 4, 4, 1, 2, 1, 2))

# 1.  da X2: 10 < a <12.5
a = taba$X2[taba$X2 > 10 & taba$X2 < 12.5]
# 2. da X1: b < 8 e maggior = 12
taba$X1 <8 & taba$X1 >= 12
b = taba$X1[taba$X1 <8 & taba$X1 >= 12]
# 3. considerando x1 e x2. o x1 <= 6 oppure (eclusivamente) x2 <= 15 con somma dei valori di x5
(taba$X1 <=6 & taba$X2 <= 15)

data.frame(taba$X1 <=6, taba$X2 <= 15, (taba$X1 <=6 & taba$X2 <= 15))
data.frame(taba$X1 <=6, taba$X2 <= 15, !(taba$X1 <=6 & taba$X2 <= 15) & (taba$X1 <=6 | taba$X2 <= 15))

sum(taba$X5[!(taba$X1 <=6 & taba$X2 <= 15) & (taba$X1 <=6 | taba$X2 <= 15)])

taba
str(taba)
taba$X2
taba$X2[3]
taba$X2[3] <- 25
taba$X2[3]
taba$X2

taba$X4[5] <- "m"
taba$X4[5]

# punto 2 esercizio 1 ----
y2 <- taba$X2
y2
y2>10
y2[y2 > 10]
which(y2 > 10)
which(y2 <= 10)

sum(y2>10)
sum(taba$X1 <=8 )
taba$X1[taba$X1 <= 8]
which(taba$X1 <= 8)

# punto 3 esercizio 1 ---- 
taba[5, ]
p5 = taba[5, ]
p5=="m"
sum(p5=="m")
sum(p5=="m") >= 1

p5=="f"
sum(p5=="f") == 0


# Eserciio 2---- 
# punto 1 


A2=taba$X2
A2>13
B2=A2[A2>13]
B2

a4 = taba$X4
a4
a4 == "m"
b4 = a4[a4 == "m"]

ls()
taba
cbind(taba, taba$X4 == "f")
taba$X4 == "f"
taba[taba$X4 == "f", c("X1", "X2")]

tabaf = taba[taba$X4 == "f", ]
tabam = taba[taba$X4 == "m", ]
taba
cbind(taba, taba$X1 > 10, taba$X3 == 0)
tab2 = taba[taba$X1 > 10& taba$X3 == 0, ]

taba[taba$X1 >=9 & taba$X1 <= 12, ]

taba[(taba$X1 >=9 & taba$X1 <= 12) & taba$X3 == 0, ]

# Esercizio 3 -----
# parte 1 
mean(taba$X1[taba$X3 == 0])
mean(taba$X1[taba$X4 == "f"])
mean(taba$X1[taba$X4 == "m"])

sd(taba$X1[taba$X5 >= 1 & taba$X5 <= 2])

taba$somma = taba$X3 + taba$X5
mean(taba$X1[taba$somma >= 4])
