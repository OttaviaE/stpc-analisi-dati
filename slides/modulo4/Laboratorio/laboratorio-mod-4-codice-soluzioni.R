# LABORATORIO DISTRIBUZIONI ----
# esrecizio 1
mx = 100; sdX = 35
x = seq(0, 200, length.out = 1000)
# E1 = x <= 75
dx = dnorm(x, mx, sdX)
plot(x, dx, type = "l")
polygon(c(x[x<= 75], 75), 
        c(dx[x<= 75],0), 
        col ="darkred")
pe1 = pnorm(75, mx, sdX)
# E2 = 50 <= x <= 70
plot(x, dx, type = "l")
polygon(c(50, x[x>= 50 & x<= 70], 70), 
        c(0, dx[x>= 50 & x<= 70],0), 
        col ="darkorange")
plot(x, dx, type = "l")
polygon(c(x[x<= 70], 70), 
        c(dx[x<= 70],0), 
        col = rgb(.2,.2,.2, alpha =.5))
polygon(c(x[x<= 50], 50), 
        c(dx[x<= 50],0), 
        col = rgb(.2,.2,.52, alpha =.5))

pe2 = pnorm(70, mx, sdX) - pnorm(50, mx, sdX)
# E3 = x >= 110
plot(x, dx, type = "l")
polygon(c(110, x[x>= 110]), 
        c(0, dx[x>= 110]), 
        col ="darkgreen")
pe3 = 1- pnorm(110, mx, sdX)
# E4 = x >= 125
plot(x, dx, type = "l")
polygon(c(125, x[x>= 125]), 
        c(0, dx[x>= 125]), 
        col ="darkblue")
pe4 = 1- pnorm(125, mx, sdX)
# calcolare
# 1. 𝑃 [𝐸1 ∪ 𝐸3]
plot(x, dx, type = "l")
polygon(c(x[x<= 75], 75), 
        c(dx[x<= 75],0), 
        col ="darkred")
polygon(c(110, x[x>= 110]), 
        c(0, dx[x>= 110]), 
        col ="darkgreen")
pe1+pe3
# 2. 𝑃 [𝐸1 ∪ 𝐸4]
plot(x, dx, type = "l")
polygon(c(125, x[x>= 125]), 
        c(0, dx[x>= 125]), 
        col ="darkblue")
polygon(c(x[x<= 75], 75), 
        c(dx[x<= 75],0), 
        col ="darkred")
pe1 + pe4
# 3. 𝑃 [𝐸1 ∩ 𝐸2]
plot(x, dx, type = "l")
polygon(c(x[x<= 75], 75), 
        c(dx[x<= 75],0), 
        col ="darkred")
polygon(c(50, x[x>= 50 & x<= 75], 75), 
        c(0, dx[x>= 50 & x<= 75],0), 
        col ="darkorange")
pe2
# 4. 𝑃 [𝐸1 ∩ 𝐸3].
plot(x, dx, type = "l")
polygon(c(x[x<= 75], 75), 
        c(dx[x<= 75],0), 
        col ="darkred")
polygon(c(110, x[x>= 110]), 
        c(0, dx[x>= 110]), 
        col ="darkgreen")
# non c'è
# esercizio 2
# normale standard
x = seq(-3,3, length.out=1000)
dx = dnorm(x)
# E1 = x <= .05
plot(x, dx, type ="l")
polygon(c(x[x<= .05], .05), 
        c(dx[x<= .05],0), 
        col ="darkred")
pe1 =pnorm(.05)
# E2 = x >= -0.5
plot(x, dx, type ="l")
polygon(c(-0.05, x[x>= -.05]), 
        c(0,dx[x>= -.05]), 
        col ="darkorchid")
pe2 = 1-pnorm(-0.05)
# Calcolare:
# 1. 𝑃 [𝐸1 ∪ 𝐸2].
plot(x, dx, type ="l")
polygon(c(x[x<= .05], .05), 
        c(dx[x<= .05],0), 
        col =rgb(.2,.2,.2, alpha = .2))
polygon(c(-0.05, x[x>= -.05]), 
        c(0,dx[x>= -.05]), 
        col =rgb(.2,.2,.2, alpha = .2))
pe1 + pe2
# interesezione tra E1 ed E2 è l'intervallo 
# -0.05 <= x <= .05
pinter = pnorm(.05) - pnorm(-0.05)
(pe1 + pe2) - pinter
# 2. 𝑃 [𝐸1 ∩ 𝐸2].
plot(x, dx, type ="l")
polygon(c(x[x<= .05], .05), 
        c(dx[x<= .05],0), 
        col =rgb(.2,.2,.2, alpha = .2))
polygon(c(-0.05, x[x>= -.05]), 
        c(0,dx[x>= -.05]), 
        col =rgb(.2,.2,.2, alpha = .2))
polygon(c(-0.05, x[x>= -.05 & x <= .05], 0.05), 
        c(0,dx[x>= -.05 & x <= .05],0), 
        col ="red")

# 3. 𝑃 [𝐸1△𝐸2].

plot(x, dx, type ="l")
polygon(c(x[x<= .05], .05), 
        c(dx[x<= .05],0), 
        col =rgb(.2,.2,.2, alpha = .2))
polygon(c(-0.05, x[x>= -.05]), 
        c(0,dx[x>= -.05]), 
        col =rgb(.2,.2,.2, alpha = .2))
polygon(c(-0.05, x[x>= -.05 & x <= .05], 0.05), 
        c(0,dx[x>= -.05 & x <= .05],0), 
        col ="white")
pnorm(-0.05)
1-pnorm(0.05)

pnorm(-0.05) + (1-pnorm(0.05))
1-pinter

# Esercizio 3 
mx = 10; sx = 5
my=7; sy= 8
values = seq(-4, 50, length.out=1000)
dx = dnorm(values, mx, sx)
dy = dnorm(values, my, sy)
# eventi di X
# e1 = 5 <= x <= 9
plot(values, dx, type = "l")
polygon(c(5, values[values>=5 & values<=9], 9), 
        c(0, dx[values>=5 & values<=9], 0), 
        col = "darkblue")
# e2 = 6.5 <= x <= 13
plot(values, dx, type = "l")
polygon(c(6.5, values[values>=6.5 & values<=13], 13), 
        c(0, dx[values>=6.5 & values<=13], 0), 
        col = "darkorange")

# evento di y 
# g1 = 4 <= y <= 8.5
plot(values, dy, type = "l")
polygon(c(4, values[values>=4 & values<=8.5], 13), 
        c(0, dy[values>=4 & values<=8.5], 0), 
        col = "darkgreen")

# Calcolare: 
# 1.  𝑃 [𝐸1 ∪ 𝐺1].
# 2. 𝑃 [𝐸2 ∩ 𝐺1].

# Esercizio 4
# variabile distributita secondo t 
# t di student con df = 35
# Calcolare
# 1. 𝑞𝑊 (0.33).
qt(.33, 35)
# 2. 𝑞𝑊 (0.75) + 𝑞𝑊 (0.25).
qt(.75, 35) + qt(.25, 35)
# 3. 𝑞𝑊 (0.75) − 𝑞𝑊 (0.25).
qt(.75, 35) - qt(.25, 35)
# 4. 𝑞𝑊 (0.975).
qt(.975, 35) 

# quantile di ordine 90 (.90) della distribuzione normale 
qnorm(.90)
qnorm(.10)

# Esercizio 5
# variabile distribuita secondo chiquadro con df = 4
# calcolare: 
# 1. 𝑃 [𝑈 ≥ 1.5].
1-pchisq(1.5, df = 4)
# 2. 𝑃 [1.1 ≤ 𝑈 ≤ 2.5].
pchisq(2.5, df = 4) - pchisq(1.1, df = 4)
# 3. 𝑃 [𝑈 ≥ 𝑞𝑈 (0.950)].
1-pchisq(qchisq(.95,df=4), df = 4)

values = seq(0, 20, length.out = 200)
chid = dchisq(values, 4)
plot(values, chid, type = "l")
