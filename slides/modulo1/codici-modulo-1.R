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
