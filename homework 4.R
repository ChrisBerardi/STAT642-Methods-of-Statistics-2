#2a

none = c(10.2, 10.8, 10.1, 10.9, 11.1, 11.8, 11.3, 11.9, 9.3, 9.9)
tw = c(9.2,  9.8,  9.1,  9.9,  10.1, 10.8, 10.3, 10.9, 9.3, 9.8)
fr = c(9.0,  9.9,  9.2,  9.8,  10.0, 10.8, 10.2, 10.8, 9.9, 9.0)
sx = c(8.1,  8.1,  8.0,  8.9,  8.2,  8.9,  8.1,  8.8,  9.2, 9.9)
ei = c(7.2,  7.8,  7.1,  7.9,  8.1,  8.8,  8.3,  8.9,  9.3, 9.8)
total = c(none,tw,fr,sx,ei)

mean(none)
mean(tw)
mean(fr)
mean(sx)
mean(ei)

sd(total)

library(mvtnorm)
d = qmvt(p = .95, tail = "lower.tail", df = 45, corr = matrix(rep(.5,4^2),4) + diag(4) *.5)$quantile
d*sd(total)*sqrt(2/10)

#6b
11*qf(1-.01,11, 220)