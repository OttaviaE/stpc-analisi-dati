############################################################
############################################################
# Distribuzione di densità Normale
# Integrali su intervalli di quantili
############################################################
############################################################


# Grafico generale #########################################

# Disegna distribzione normale
#
# Restituisce il grafico della distribuzione normale (Densità)
# limIX Limite inferiore del supporto della distribuzione
# limSX Limite superiore del supporto della distribuzione
# media Media della distribuzione
# ds Deviazione standard
# quantX Quantili della distribuzione che si vogliono visualizzare
disegnaNorm <- function(limIX,limSX,media,ds,quantX) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX,limSX),c("",media,quantX,""))
  
  points(quantX,dnorm(quantX,media,ds),cex=2)
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}
Media <- 10
Ds <- 5
limInfX <- qnorm(0.001,Media,Ds)
limSupX <- qnorm(0.999,Media,Ds)
X <- c(3,8,12,19,14)
disegnaNorm(limInfX,limSupX,Media,Ds,X)
disegnaNorm(limInfX,limSupX,Media,Ds,X)



# Disegna distribuzione normale cumulata
# Resituisce il grafico della distribuzione normale (cumulata)
# limIX Limite inferiore del supporto della distribuzione
# limSX Limite superiore del supporto della distribuzione
# media Media della distribuzione
# ds Deviazione standard
# quantX Quantili della distribuzione che si vogliono visualizzare
disegnaNormC <- function(limIX,limSX,media,ds,quantX) {
  
  curve(pnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX,limSX),c("",media,quantX,""))
  
  points(quantX,pnorm(quantX,media,ds),cex=2)
  points(c(media,media),c(0,pnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}

Media <- 10
Ds <- 5
limInfX <- qnorm(0.001,Media,Ds)
limSupX <- qnorm(0.999,Media,Ds)
X <- c(3,8,12,19,14)
disegnaNormC(limInfX,limSupX,Media,Ds,X)

# Integrale parte destra
# 
# Area sottesa alla curva dal punto definito nell'argomento quantX fino a più infinito
# limIX Limite inferiore del supporto della distribuzione
# limSX Limite superiore del supporto della distribuzione
# media Media della distribuzione
# ds Deviazione standard
# quantX Quantile della distribuzione per cui si vuole calcolare l'area fino a più infinito
disegnaNormDX <- function(limIX,limSX,media,ds,quantX) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX,limSX),c("",media,quantX,""))
  
  vals <- seq(quantX,limSX,length=100)
  x <- c(quantX,vals,limSX,quantX)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}

Media <- 10
Ds <- 5
limInfX <- qnorm(0.001,Media,Ds)
limSupX <- qnorm(0.999,Media,Ds)
X <- c(3,8,12,19,14)
quantileX1 <- 2
quantileX2 <- 8
disegnaNormDX(limInfX,limSupX,Media,Ds,quantileX1)

# Integrale parte sinistra
# Area sottesa alla curva da meno infinito fino al punto definito nell'argomento quantX
# limIX Limite inferiore del supporto della distribuzione
# limSX Limite superiore del supporto della distribuzione
# media Media della distribuzione
# ds Deviazione standard
# quantX Quantile della distribuzione per cui si vuole calcolare l'area fino a più infinito
disegnaNormSX <- function(limIX,limSX,media,ds,quantX) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX,limSX),c("",media,quantX,""))
  
  vals <- seq(limIX,quantX,length=100)
  x <- c(limIX,vals,quantX,limIX)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}
Media <- 10
Ds <- 5
limInfX <- qnorm(0.001,Media,Ds)
limSupX <- qnorm(0.999,Media,Ds)
X <- c(3,8,12,19,14)
quantileX1 <- 2
quantileX2 <- 8
disegnaNormSX(limInfX,limSupX,Media,Ds,quantileX2)




# Integrale parte esterna
#
# Area sottesa alla curva tra -infinito e il punto definito in quantX1 e area sottesa alla curva tra quantX2 e +infinito
# limIX Limite inferiore del supporto della distribuzione
# limSX Limite superiore del supporto della distribuzione
# media Media della distribuzione
# ds Deviazione standard
# quantX1 Quantile della distribuzione (-infinito, quantX1]
# quantX2 Quantile della distribuzione [quantX2, +infinito)
disegnaNormEst <- function(limIX,limSX,media,ds,quantX1,quantX2) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX1,quantX2,limSX),c("",media,quantX1,quantX2,""))
  
  vals <- seq(limIX,quantX1,length=100)
  x <- c(limIX,vals,quantX1,limIX)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  
  vals <- seq(quantX2,limSX,length=100)
  x <- c(quantX2,vals,limSX,quantX2)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}

Media <- 10
Ds <- 5
limInfX <- qnorm(0.001,Media,Ds)
limSupX <- qnorm(0.999,Media,Ds)
X <- c(3,8,12,19,14)
quantileX1 <- 2
quantileX2 <- 8
disegnaNormEst(limInfX,limSupX,Media,Ds,quantileX1,quantileX2)


# Integrale parte Interna
# 
# Area sottesa alla curva tra quantX1 e quantX2
# limIX Limite inferiore del supporto della distribuzione
# limSX Limite superiore del supporto della distribuzione
# media Media della distribuzione
# ds Deviazione standard
# quantX1 Quantile della distribuzione, limite inferiore dell'intervallo
# quantX2 Quantile della distribuzione, limite inferiore dell'intervallo

disegnaNormInt <- function(limIX,limSX,media,ds,quantX1,quantX2) {
  
  curve(dnorm(x,media,ds),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Normale: dev.st = ",ds, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,media,quantX1,quantX2,limSX),c("",media,quantX1,quantX2,""))
  
  vals <- seq(quantX1,quantX2,length=100)
  x <- c(quantX1,vals,quantX2,quantX1)
  y <- c(0,dnorm(vals,media,ds),0,0)
  polygon(x,y,density=20,angle=45)
  
  points(c(media,media),c(0,dnorm(media,media,ds)),type="l",lty=4,lwd=2)
  
}

Media <- 10
Ds <- 5
limInfX <- qnorm(0.001,Media,Ds)
limSupX <- qnorm(0.999,Media,Ds)
X <- c(3,8,12,19,14)
quantileX1 <- 2
quantileX2 <- 8
disegnaNormEst(limInfX,limSupX,Media,Ds,quantileX1,quantileX2)

