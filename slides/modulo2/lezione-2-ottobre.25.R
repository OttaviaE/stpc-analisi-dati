set.seed(999)
x = rnorm(1000, mean = 50, 
          sd = 10)
hist(x,ylim=c(0, 250))
(max(x)-min(x))/log(length(x)+1, 
                    base =2)
2*IQR(x)*length(x)^(-1/3)
hist(x, breaks = "FD", ylim=c(0, 250))

hist(x, breaks = "FD", freq = F)
lines(density(x), col ="salmon", lwd = 3)
hist(x, breaks = "Sturges", freq = F)
lines(density(x), col ="salmon", lwd = 3)

boxplot(x)
IQR(x)
quantile(x)
abline(h = quantile(x, probs = .25), col = "red")
abline(h = quantile(x, probs = .75), col = "royalblue")
abline(h = quantile(x, probs = .25) - 1.5*IQR(x), col ="seagreen")
abline(h = quantile(x, probs = .75) + 1.5*IQR(x), col ="salmon")
quantile(x, probs = .33)
quantile(x, probs = .20)

x[x<=quantile(x, probs = .25) - 1.5*IQR(x)]
x[x>=quantile(x, probs = .75) + 1.5*IQR(x)]
x[x<=quantile(x, probs = .25) - 1.5*IQR(x) | x>=quantile(x, probs = .75) + 1.5*IQR(x)]
boxplot(x, plot = FALSE)
boxplot(x, plot = FALSE)$out
y = x[x>quantile(x, probs = .25) - 1.5*IQR(x) & x<quantile(x, probs = .75) + 1.5*IQR(x)]
sum(x == boxplot(x, plot = FALSE)$out)
