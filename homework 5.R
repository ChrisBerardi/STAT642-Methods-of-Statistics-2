

data = matrix(0,30,2)
y = matrix(0,30,1)
temp40 = c(1953, 2135, 2471, 4727, 6134, 6314)
temp45 = c(1190, 1286, 1550, 2125, 2557, 2845)
temp55 = c(651, 817, 848, 1038, 1361, 1543)
temp70 = c(511, 651, 651, 652, 688, 729)
y = c(temp40, temp45, temp55, temp70)
temp=c(rep("temp40",6),rep("temp45",6),rep("temp55",6),rep("temp70",6))

#1a Runs test for correlation
data3 = matrix(y,nrow=4,byrow=T)
resid = matrix(0,4,6)
means3 = matrix(0,4)
resid1 = matrix(0,4,5)
residl1 = matrix(0,4,5)
dif1  = matrix(0,4,5)
prd1  = matrix(0,4,5)
rho = matrix(0,4,5)
DW = matrix(0,4,5)
for (i in 1:4) {
means3[i] = mean(data3[i,])
resid[i,] = data3[i,]-means3[i]
resid1[i,] = resid[i,2:6]
residl1[i,] = resid[i,1:5]
for (j in 1:5){
dif1[i,j] = (resid1[i,j]-residl1[i,j])^2
prd1[i,j] = resid1[i,j]*residl1[i,j]
}
rho[i] = sum(prd1[i,])/sum((resid[i,])^2)
DW[i] = sum(dif1[i,])/sum((resid[i,])^2)
}
n.neg =rep(0,4)
n.pos =rep(0,4)
for (i in 1:4) {
n.neg[i] =length(resid[i,][resid[i,]<0])
n.pos[i] =length(resid[i,][resid[i,]>0])
}
numb.runs =rep(1,4)
for (i in 1:4) {
for (j in 2:6) {
if (sign(resid[i,j]) != sign(resid[i,j-1])) {numb.runs[i] =numb.runs[i] + 1}
}
}
residruns.result =as.data.frame(cbind(numb.runs, n.pos, n.neg))
names(residruns.result) =c("No. runs", "N+", "N-")
residruns.result

#Problem 1b.
si=c(sd(temp40),sd(temp45),sd(temp55),sd(temp70))
transsi=log(si)
yi=c(mean(temp40),mean(temp45),mean(temp55),mean(temp70))
transyi=log(yi)
fit= lm(transsi ~ transyi)
xtrend = seq(6,9,.01)
ytrend = -6.583+1.729*xtrend
plot(transyi,transsi, xlab="Log(Sample Mean)", ylab="Log(Sample St Dev)")
lines(xtrend,ytrend)

#Problem 1c. Box-Cox Transformation
s1 = rep("T40",6)
s2 = rep("T45",6)
s3 = rep("T55",6)
s4 = rep("T70",6)
hab = c(s1,s2,s3,s4)
site = as.factor(hab)
library(MASS)
like=boxcox(y+1~site,lambda=seq(-2.5,2.1,.01))
like_max=max(like$y)
imax = which(like$y==like_max)
thmax=like$x[imax]
thmax

#1d
d = data.frame(y^-.64,temp)
anal1 = aov(y^-.64 ~ temp,data = d)
rs1 = resid(anal1,type = "response")
rstime1 = ts(rs1,start = 1,frequency = 1)
rsraw = rs1[2:24]
rsrawl1 = rs1[1:23]

dif1 = (rsraw-rsrawl1)^2
num1 = sum(dif1)
rs12 = rs1^2
den1 = sum(rs12)
DW1 = num1/den1
prd1 = rsraw*rsrawl1
prdsum1 = sum(prd1)
rho1 = prdsum1/den1
(DW1)
(rho1)

#2b. Use Hollander-Wolfe since n is small
library(pgirmess)
temp = as.factor(hab)
d=data.frame(y**-.64,temp)
y_trans=y**-.64

kruskal.test(y_trans,temp,y_trans~temp)

kruskalmc(y_trans~temp)

#3
usda=c(418,906,28,277,634,48,369,137,29,522,319,242,261,566,734)
field=c(211,276,415,787,18,118,1,151,0,253,61,0,275,0,153)
resist=c(0,9,143,1,26,127,161,294,0,348,0,14,21,0,218)

#3a
hist(usda)
hist(field)
hist(resist)

x=usda
n = length(x)
x = sort(x)
i = seq(1:n)
u = (i-.5)/n
z = qpois(u,mean(x))
plot(z,x,datax=F,plot=T,xlab="Poisson Quantile",ylab="USDA Quantiles",
       lab=c(7,8,7),
       main="Poisson Reference Distribution Plot\n USDA Strain",
       cex=.95)
abline(lm(x~z))

x=field
n = length(x)
x = sort(x)
i = seq(1:n)
u = (i-.5)/n
z = qpois(u,mean(x))
plot(z,x,datax=F,plot=T,xlab="Poisson Quantile",ylab="Field Quantiles",
       lab=c(7,8,7),
       main="Poisson Reference Distribution Plot\n Field Strain",
       cex=.95)
abline(lm(x~z))

x=resist
n = length(x)
x = sort(x)
i = seq(1:n)
u = (i-.5)/n
z = qpois(u,mean(x))
plot(z,x,datax=F,plot=T,xlab="Poisson Quantile",ylab="Resist Quantiles",
       lab=c(7,8,7),
       main="Poisson Reference Distribution Plot\n Resist Strain",
       cex=.95)
abline(lm(x~z))

#3c. Runs Test for correlation
strain = c(usda,field,resist)
data3 = matrix(strain,nrow=3,byrow=T)
resid = matrix(0,3,15)
means3 = matrix(0,3)
resid1 = matrix(0,3,14)
residl1 = matrix(0,3,14)
dif1  = matrix(0,3,14)
prd1  = matrix(0,3,14)
rho = matrix(0,3,14)
DW = matrix(0,3,14)
for (i in 1:3) {
means3[i] = mean(data3[i,])
resid[i,] = data3[i,]-means3[i]
resid1[i,] = resid[i,2:15]
residl1[i,] = resid[i,1:14]
for (j in 1:14){
dif1[i,j] = (resid1[i,j]-residl1[i,j])^2
prd1[i,j] = resid1[i,j]*residl1[i,j]
}
rho[i] = sum(prd1[i,])/sum((resid[i,])^2)
DW[i] = sum(dif1[i,])/sum((resid[i,])^2)
}
n.neg =rep(0,3)
n.pos =rep(0,3)
for (i in 1:3) {
n.neg[i] =length(resid[i,][resid[i,]<0])
n.pos[i] =length(resid[i,][resid[i,]>0])
}
numb.runs =rep(1,3)
for (i in 1:3) {
for (j in 2:15) {
if (sign(resid[i,j]) != sign(resid[i,j-1])) {numb.runs[i] =numb.runs[i] + 1}
}
}
residruns.result =as.data.frame(cbind(numb.runs, n.pos, n.neg))
names(residruns.result) =c("No. runs", "N+", "N-")
residruns.result
