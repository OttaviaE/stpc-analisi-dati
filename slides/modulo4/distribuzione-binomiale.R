binomcoef = function(n, x) {
  if (is.null(n) | is.null(x)) {
    stop("I need the cardinality of omega and the number of success")
  }
  thecoef = factorial(n)/(factorial(x)*factorial(n-x))
  return(thecoef)
}
x = 0:10
p = .25
p1 = .75
p2 = .50
myp = binomcoef(length(x), x)*((p^x)*((1-p)^(length(x)-x)))
myp1 = binomcoef(length(x), x)*((p1^x)*((1-p1)^(length(x)-x)))
myp2 = binomcoef(length(x), x)*((p2^x)*((1-p2)^(length(x)-x)))

plot(x, dbinom(x, length(x), prob = p), pch = 19)
pbinom(x, length(x), prob = .25)

plot(x, myp, pch = 19)
segments(x, 0, x, myp)
points(x, myp1, col = "red", pch = 19)
segments(x, 0, x, myp1, col = "red")
points(x, myp2, col = "blue", pch = 19)
segments(x, 0, x, myp2, col = "blue")

plot(x, cumsum(myp), pch = 19)
segments(x, 0, x, cumsum(myp))
points(x, cumsum(myp1), pch = 19, col ="red")
points(x, cumsum(myp2), pch = 19, col ="blue")
abline(h = .5)
# stessa cosa ma con la funzione interna a R pbinom()
plot(x, pbinom(x, size = length(x), prob = p), pch = 19)
segments(x, 0, x, pbinom(x, size = length(x), prob = p))
points(x, pbinom(x, size = length(x), prob = p1), 
       pch = 19, col ="red")
points(x, pbinom(x, size = length(x), prob = p2), pch = 19, col ="blue")
abline(h = .5, lwd = 2)
segments(-2, pbinom(x, size = length(x), prob = p), 
         x, pbinom(x, size = length(x), prob = p))
segments(-2, pbinom(x, size = length(x), prob = p1), 
         x, pbinom(x, size = length(x), prob = p1), col = "red")
segments(-2, pbinom(x, size = length(x), prob = p2), 
         x, pbinom(x, size = length(x), prob = p2), col = "blue")
pbinom(x, size = length(x), prob = p)[x <= 3]
1- 0.71330452
abline(v = 3, lwd = 2, lty = 2)

pbinom(x, size = length(x), prob = p)[x <= 2]
sum(dbinom(x, length(x), prob = p)[x <= 2])

qbinom(pbinom(x, size = length(x), prob = p), length(x), 
       prob = p)
qbinom(pbinom(x, size = length(x), prob = p1), length(x), 
       prob = p1)

x = seq(-3, 3, length.out=1000)
plot(x, dbinom(x, length(x), p = .5))


