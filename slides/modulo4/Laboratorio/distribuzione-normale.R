# distribuzione normale ---- 

x = seq(-4, 4, length.out=1000)
plot(x, dnorm(x, mean = 0, sd = 1), type = "l")

x = seq(10, 300, length.out = 1000)

m = c(14, 22, 56, 150, 100)
plot(x, dnorm(x, mean = 50, sd = 5), type = "l")
for(i in 1:length(m)) {
  lines(x, dnorm(x, mean = m[i], sd = 5), col = i)
}

s = c(5, 10, 20, 50, 70)

plot(x, dnorm(x, mean = 150, sd = 1), type = "l")
for(i in 1:length(s)) {
  lines(x, dnorm(x, mean = 150, sd = s[i]), col = i, lwd = 2)
}

x = seq(10, 30, length.out = 1000)
plot(x, dnorm(x, mean = 15, sd = 5), type = "l")

x = seq(-4, 4, length.out=1000)
plot(x, dnorm(x, mean = 0, sd = 1))
abline(v = 0)
plot(x, pnorm(x, mean = 0, sd = 1))
lines(x, pnorm(x, mean = 2, sd = 1), col ="firebrick", lwd = 2)
abline(h = .50)
abline(v = 0)
abline(v = 1)
abline(v = -1)

dx = dnorm(x, mean = 0, sd = 1)
px = pnorm(x)
par(mfrow = c(1,2))
plot(x, dx, type = "l", lwd = 2)
polygon(c(x[x <=2], 2), c(dx[x<=2],0), 
        col = rgb(.5,.5,.5, alpha = .4))
plot(x, px, type = "l", lwd = 2)
abline(v = 2)
pnorm(2)
abline(h = pnorm(2))

par(mfrow = c(1,2))
plot(x, dx, type = "l", lwd = 2)
polygon(c(-2, x[x >= -2 & x <=2], 2), 
        c(0, dx[x >= -2 & x<=2],0), 
        col = rgb(.5,.5,.5, alpha = .4))
plot(x, px, type = "l", lwd = 2)
abline(v = 2)
abline(v = -2)

par(mfrow = c(1,2))
plot(x, dx, type = "l", lwd = 2)
polygon(c(-1.5, x[x >= -1.5 & x <=1.5], 1.5), 
        c(0, dx[x >= -1.5 & x<=1.5],0), 
        col = rgb(.5,.5,.5, alpha = .4))
plot(x, px, type = "l", lwd = 2)
abline(v = 1.5)
abline(v = -1.5)
pnorm(1.5)- pnorm(-1.5)


x = seq(10, 50, length.out=1000)
dev.off()
dx = dnorm(x, mean = mean(x), sd = sd(x))
plot(x, dx, type = "l", lwd = 2)
mx = mean(x)
sx = sd(x)
polygon(c(mx -sx, x[x >= mx-sx & x <= mx+sx], mx + sx), 
        c(0, dx[x >= mx-sx & x <= mx+sx], 0), 
        col ="pink")
pnorm(mx+sx, mean = mx, sd = sx)- pnorm(mx-sx, 
                                        mean = mx, 
                                        sd = sx)


x = seq(-4, 4, length.out=1000)
plot(x, dnorm(x))

par(mfrow = c(1,2))
plot(x, pnorm(x))
plot(pnorm(x), qnorm(pnorm(x)))

qnorm(c(0.0000000001, .25, .50, .75, 0.9999999999))

pnorm(qnorm(.75)) - pnorm(qnorm(.25))
pnorm(qnorm(.75) - qnorm(.25))

plot(x, dnorm(x), type = "l", main = 2*pnorm(-1.28))
polygon(c(x[x<= qnorm(.10)], qnorm(.10)), 
        c(dnorm(x)[x<= qnorm(.10)], 0), col ="pink")
polygon(c(qnorm(1-.10), x[x>= qnorm(1-.10)]), 
        c(0, dnorm(x)[x>= qnorm(1-.10)]), col ="darkblue")

pnorm(-1.28) + (1-pnorm(1.28))
2*pnorm(-1.28)

myprobability = function(prob, distribution ="norm") {
  pdist = switch (distribution,
    norm = pnorm, 
    stud = pt
  )
  qdist = switch (distribution,
                  norm = qnorm, 
                  stud = qt
  )
}


plot(x, dt(x, df = 1), type = "l", 
     ylim = c(0,0.5), lty = 2, lwd = 2)
mydf = seq(2, 20, by = 3)

for (i in 1:length(mydf)) {
  lines(x, dt(x, df = mydf[i]), col =i, lwd = 1)
}


par(mfrow=c(1,2))
plot(x, pnorm(x, sd = 1))
mysd = seq(2, 30, by = 3)

for (i in 1:length(mysd)) {
  lines(x, pnorm(x, sd = mysd[i]), col = i)
}

plot(x, pt(x, df = 1))
for (i in 1:length(mysd)) {
  lines(x, pt(x, df = mysd[i]), col = i)
}


dev.off()

plot(x, pnorm(x, sd = 1), type = "l", lwd = 2)
lines(x, pt(x, df = 30), col ="red", lwd = 2)

x = seq(0, 50, length.out = 1000)
dchi = dchisq(x, df = 3)
max(dchi)
abline(v = x[which(dchi == max(dchi))])
plot(x, pchisq(x, df = 3), type = "l")
abline(v = x[which(dchi == max(dchi))])
abline(h = .5)
abline(v = qchisq(.50, df = 3))

plot(x, dchi, type = "l", lwd = 1)
abline(v = qchisq(.5, df = 3))
abline(h =.5)
mydf = seq(3, 39, by = 2)

for (i in 1:length(mydf)) {
  lines(x, dchisq(x, df = mydf[i]), lwd = 1, col = i)
}




plot(x, pchisq(x, df = 3), lwd = 2)

for (i in 1:length(mydf)) {
  lines(x, pchisq(x, df = mydf[i]), lwd = 1, col = i)
}
abline(h = .5)
