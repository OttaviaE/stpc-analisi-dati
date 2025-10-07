############################################################
############################################################
# Distribuzione di densità Chi quadrato
# Integrali su intervalli di quantili
############################################################
############################################################

# Integrale parte DX #######################################

disegnaChiQDX <- function(limIX,limSX,gdl,quantX) {
  
  curve(dchisq(x,df=gdl),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Chi2: g.d.l. = ", gdl, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,quantX,limSX),c("0",quantX,""))
  
  vals <- seq(quantX,limSX,length=100)
  x <- c(quantX,vals,limSX,quantX)
  y <- c(0,dchisq(vals,df=gdl),0,0)
  polygon(x,y,density=20,angle=45)
  
}


# Integrale parte SX #######################################

disegnaChiQSX <- function(limIX,limSX,gdl,quantX) {
  
  curve(dchisq(x,df=gdl),limIX,limSX,axes=F,xlab="",ylab="",main=paste("Distribuzione Chi2: g.d.l. = ", gdl, sep = ""))
  
  abline(h=0)
  
  axis(1,c(limIX,quantX,limSX),c("0",quantX,""))
  
  vals <- seq(limIX,quantX,length=100)
  x <- c(limIX,vals,quantX,limIX)
  y <- c(0,dchisq(vals,df=gdl),0,0)
  polygon(x,y,density=20,angle=45)
  
}


###########################################################
# Esempi 

GDL <- 4
limInfX <- 0
limSupX <- qchisq(0.999,GDL)
quantileX <- 4.0901

disegnaChiQDX(limInfX,limSupX,GDL,quantileX)
disegnaChiQSX(limInfX,limSupX,GDL,quantileX)


###########################################################

