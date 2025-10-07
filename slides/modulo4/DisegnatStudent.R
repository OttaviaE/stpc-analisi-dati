
############################################################
############################################################
# Distribuzione di densità t di Student
# Integrali su intervalli di quantili
############################################################
############################################################

# Integrale parte DX #######################################

disegnatDX <- function(limIX,limSX,gdl,quantX) {
  
  curve(dt(x,df=gdl),limIX,limSX,axes=F,xlab="",ylab="",main=
          paste("Distribuzione t di Student: g.d.l. = ", gdl, sep = ""))
  
  lines(c(0,0),c(0,dt(0,df=gdl)),lty=4)
  abline(h=0)
  
  axis(1,c(limIX,quantX,0,limSX),c("",quantX,0,""))
  
  vals <- seq(quantX,limSX,length=100)
  x <- c(quantX,vals,limSX,quantX)
  y <- c(0,dt(vals,df=gdl),0,0)
  polygon(x,y,density=20,angle=45)
  
}


# Integrale parte SX #######################################

disegnatSX <- function(limIX,limSX,gdl,quantX) {
  
  curve(dt(x,df=gdl),limIX,limSX,axes=F,xlab="",ylab="",main=
          paste("Distribuzione t di Student: g.d.l. = ", gdl, sep = ""))
  
  lines(c(0,0),c(0,dt(0,df=gdl)),lty=4)
  abline(h=0)
  
  axis(1,c(limIX,quantX,0,limSX),c("",quantX,0,""))
  
  vals <- seq(limIX,quantX,length=100)
  x <- c(limIX,vals,quantX,limIX)
  y <- c(0,dt(vals,df=gdl),0,0)
  polygon(x,y,density=20,angle=45)
  
}


# Integrale parte Esterna #################################

disegnatEst <- function(limIX,limSX,gdl,quantX1,quantX2) {
  
  curve(dt(x,df=gdl),limIX,limSX,axes=F,xlab="",ylab="",main=
          paste("Distribuzione t di Student: g.d.l. = ", gdl, sep = ""))
  
  lines(c(0,0),c(0,dt(0,df=gdl)),lty=4)
  abline(h=0)
  
  axis(1,c(limIX,quantX1,0,quantX2,limSX),c("",quantX1,0,quantX2,""))
  
  vals <- seq(limIX,quantX1,length=100)
  x <- c(limIX,vals,quantX1,limIX)
  y <- c(0,dt(vals,df=gdl),0,0)
  polygon(x,y,density=20,angle=45)
  
  vals <- seq(quantX2,limSX,length=100)
  x <- c(quantX2,vals,limSX,quantX2)
  y <- c(0,dt(vals,df=gdl),0,0)
  polygon(x,y,density=20,angle=45)
  
}


# Integrale parte Interna #################################

disegnatInt <- function(limIX,limSX,gdl,quantX1,quantX2) {
  
  curve(dt(x,df=gdl),limIX,limSX,axes=F,xlab="",ylab="",main=
          paste("Distribuzione t di Student: g.d.l. = ", gdl, sep = ""))
  
  lines(c(0,0),c(0,dt(0,df=gdl)),lty=4)
  abline(h=0)
  
  axis(1,c(limIX,quantX1,0,quantX2,limSX),c("",quantX1,0,quantX2,""))
  
  vals <- seq(quantX1,quantX2,length=100)
  x <- c(quantX1,vals,quantX2,quantX1)
  y <- c(0,dt(vals,df=gdl),0,0)
  polygon(x,y,density=20,angle=45)
  
}


###########################################################
# Esempi

GDL <- 19
limInfX <- qt(0.001,GDL)
limSupX <- qt(0.999,GDL)
quantileX1 <- -0.7822
quantileX2 <- 0.7822

disegnatDX(limInfX,limSupX,GDL,quantileX1)
disegnatSX(limInfX,limSupX,GDL,quantileX1)
disegnatEst(limInfX,limSupX,GDL,quantileX1,quantileX2)
disegnatInt(limInfX,limSupX,GDL,quantileX1,quantileX2)


###########################################################

