# t test ----
# gli esercizi sono quelli dile esercizi Rmod5

set.seed(999)
x = rnorm(11, mean = 12, sd = 7.5)
mu0 = 10
t.test(x, mu = mu0) # test a due code
t.test(x)
t.test(x, mu = mu0,
       alternative = "greater") # test a una coda destra
t.test(x, mu = mu0,
       alternative = "less") # test a una coda sinistra
t.test(x) # test a due code assumedno mu0 = 0

mytest = t.test(x, mu = mu0)
mytest$statistic
mytest$p.value
pt(mytest$statistic, 10)
2*(1 - pt(abs(mytest$statistic), 10))

1 - pt(abs(mytest$statistic), 10) + pt(mytest$statistic, 10)
# calcolo intervallo di confidenza 
qt(.05/2, df = 10)
qt(.001/2, df = 10)

mx = mean(x)
q25 = abs(qt(.05/2, df = 10))
se = sqrt((var(x)/length(x)))

mx - q25*se
mx + q25*se

# ic al 90%, alpha = .10

q10 = abs(qt(.10/2, df = 10))

mx - q10*se
mx + q10*se

# hp mono sx 
pt(-0.72806, 10)
# hp mono dx
1 - pt(-0.72806, 10)



dati = read.csv2("slides/modulo5/Laboratorio/datiTestCR2005.csv")

head(dati)
summary(dati)

index = which(dati$VOTO < 60 | dati$VOTO > 100)
dati$VOTO[index] = mean(dati$VOTO[-index])
summary(dati$VOTO)
# mu0 = 62
# H0: mu = 62
# H1: mu \neq 62

t.test(dati$VOTO, mu= 62)
qt(.05/2, 69)
qt(.01/2, 69)

mean(dati$VOTO)+ abs(qt(.05/2, 69))* (sqrt(var(dati$VOTO)/70))
mean(dati$VOTO) -  abs(qt(.05/2, 69))* (sqrt(var(dati$VOTO)/70))


x<-c(12.2,23.2,14,20.3,9.2,3.7,8,16.8,3.7,1.9,20.5)
y<-c(8.5,3.2,4,10.5,7.7,3.1,2,3.9)
t.test(x,y,var.equal=TRUE)
t.test(x,y)
t.test(x,y,var.equal=TRUE, mu = 5)
m = t.test(x,y,var.equal=TRUE)
d = m$estimate[1] - m$estimate[2] 
m$parameter
qt(.05/2, m$parameter)
s = sqrt(((length(x)-1)*var(x) + ((length(y)-1)*var(y)))/(length(x) + length(y) -2))
vx = (length(x)-1)*var(x)
vy = (length(y)-1)*var(y)
den = (length(x) + length(y) -2)
sqrt((vx + vy)/den)

se = s*sqrt(1/length(x) + 1/length(y))
(mean(x) - mean(y))/se

d = mean(x) - mean(y)
q25 = abs(qt(.05/2, den))

d + q25*se
d - q25*se
d + abs(qt(.05/2, m$parameter))*se

(mean(x) - mean(y))/se

# campioni appaiati ---- 
x = c(5.2,3.5,3.4,7.1,6.2,3.0,1,4)
y = c(8.5,3.2,4,10.5,7.7,3.1,2,3.9)
m = t.test(x,y,paired=TRUE)
t.test(x,y,paired=TRUE,alternative="less")
t.test(x,y,paired=TRUE,alternative="greater")


values = seq(-3, 3, .01)
d = dt(values, 7)
plot(values, d, type = "l", lwd = 2)
polygon(c(2.30, values[values >= 2.30]), 
        c(0,d[values >= 2.30]), 
        col ="firebrick")
polygon(c(values[values <= -2.30], -2.30), 
        c(d[values <= -2.30], 0), 
        col ="blue")
2*(1-pt(2.303, 7))

m$estimate
m$stderr
myt = abs(qt(.01/2, m$parameter))
liminf = m$estimate + myt*m$stderr 
limsup = m$estimate - myt*m$stderr 

# chi quadro 

qchisq(1-.05, 4)


mytable <- matrix(c(22,45,18),1,3)
sum(mytable)
colnames(mytable) <- c("a1","a2","a3")
chisq.test(mytable)
chisq.test(mytable/10)
mytab1 = mytable*100
sum(mytab1)

chisq.test(mytab1)



mytable <- matrix(c(21,7,9,33,7,28,7,8,10),3,3)
rownames(mytable) <- c("a1","a2","a3")
colnames(mytable) <- c("b1","b2","b3")
mychi = chisq.test(mytable)
str(mychi)
cont = (mychi$observed - mychi$expected)^2
sum(cont/ mychi$expected)
mychi$statistic
