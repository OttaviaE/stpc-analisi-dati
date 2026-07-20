# Modulo 3 A ----- 
data = read.csv("slides/modulo2/Laboratorio/dati-es-classe.csv", 
                sep =";")
head(data)
table(data$GEN)
table(data$SC)

table(data$GEN, data$SC)
sum(table(data$GEN, data$SC))

mytab = table(data$GEN, data$SC)
mytab[1,]
mytab[2,]
mytab[,2]

sum(mytab[1,])
sum(mytab[2,])

margin.table(mytab, 1)
margin.table(mytab, 2)

margin.table(mytab)

table(data$GEN)/nrow(data)

prop.table(mytab)
mytab/nrow(data)

mytab[1,]/sum(mytab[1,])
mytab[2,]/sum(mytab[2,])
prop.table(mytab, 1)
prop.table(mytab, 2)


# n_{ij}^* = (fi, * f.J)/n

apply(mytab, 2, sum)
apply(mytab, 1, sum)

# che è come fare 
margin.table(mytab, 2)
margin.table(mytab, 1)

n11_t = (margin.table(mytab, 2)[1]* margin.table(mytab, 1)[1])/margin.table(mytab)
n12_t = (margin.table(mytab, 2)[2]* margin.table(mytab, 1)[1])/margin.table(mytab)
n13_t = (margin.table(mytab, 2)[3]* margin.table(mytab, 1)[1])/margin.table(mytab)

n21_t = (margin.table(mytab, 2)[1]* margin.table(mytab, 1)[2])/margin.table(mytab)
n22_t = (margin.table(mytab, 2)[2]* margin.table(mytab, 1)[2])/margin.table(mytab)
n23_t = (margin.table(mytab, 2)[3]* margin.table(mytab, 1)[2])/margin.table(mytab)

mytab_t = matrix(c(n11_t, n12_t, n13_t, 
         n21_t, n22_t, n23_t), byrow = TRUE, nrow=2, ncol=3)

mycont = mytab - mytab_t
margin.table(mycont, 1)
margin.table(mycont, 2)
(mycont^2)/mytab_t
sum((mycont^2)/mytab_t)
chisq.test(mytab)$statistic
chisq.test(mytab)$statistic/18
