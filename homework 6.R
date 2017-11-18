#Homework 6 

#4h
x <- c(25,50,75)
y1 <- c(201,237,268.75)
y2 <- c(209.5,232.5,240.5)
y3 <- c(164,188,227.25)
y4 <- c(148.5,174.5,200)
y <-cbind(y1,y2,y3,y4)

par(lab=c(3,15,4))
matplot(x,y,type="b",xlab="PROPORTION LEVELS",ylab="MEAN WEIGHT LOSS PER SPECIMEN (mg)",
        main="TRENDS IN WEIGHT LOSS FOR PROPORTION",cex=.99,col="black",
        ylim=c(0,300),lab=c(3,15,4),pch=c("#*@+"),xaxt="n")
axis(side=1,at=c(25,50,75),labels=c("25%","50%","75%"))
legend(25,300,pch=c("#*@+"),legend=c("S1,F1","S1,F2","S2,F1","S2,F2"))


Ca100 =  c(5.8,7.3,7.4,7.3)
Ca200 =  c(7.4,7.3,7.6,7.1)
Ca300 =  c(6.4,7.4,7.2,6.6)
SE  = .153
Ca = cbind(Ca100,Ca200,Ca300)
x = c(1,2,3,4)
LxCa100 = c(1,2,3,4)
UxCa100 = c(1,2,3,4)
LyCa100 = Ca100 - SE
UyCa100 = Ca100 + SE
LxCa200 = c(1,2,3,4)
UxCa200 = c(1,2,3,4)
LyCa200 = Ca200 - SE
UyCa200 = Ca200 + SE
LxCa300 = c(1,2,3,4)
UxCa300 = c(1,2,3,4)
LyCa300 = Ca300 - SE
UyCa300 = Ca300 + SE
par(lab=c(3,15,4))
matplot(x,Ca,type="b",xlab="Ca Level",ylab="Mean Diameter Change +/- SE",
        main="Profile Plot of Ca*pH Interaction With SE",cex=.99,
        ylim=c(5,8),lab=c(3,10,5),col="black",pch=c("#&@"),xaxt="n")
segments(x,LyCa100,x,UyCa100)
segments(x,LyCa200,x,UyCa200)
segments(x,LyCa300,x,UyCa300)
BxL=c(.98,1.98,2.98,3.98)
BxR=c(1.02,2.02,3.02,4.02)
segments(BxL,LyCa100,BxR,LyCa100)
segments(BxL,UyCa100,BxR,UyCa100)
segments(BxL,LyCa200,BxR,LyCa200)
segments(BxL,UyCa200,BxR,UyCa200)
segments(BxL,LyCa300,BxR,LyCa300)
segments(BxL,UyCa300,BxR,UyCa300)
axis(side=1,at=c(1,2,3,4),labels=c("pH4","pH5","pH6","pH7"))
legend(3.3,5.5,pch=c("#&@"),legend=c("Ca=Ca100","Ca=Ca200","Ca=Ca300"))
text(1.2,5.79,"Ca100")
text(1.2,6.39,"Ca300")
text(1.2,7.48,"Ca200")