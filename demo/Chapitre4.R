#' ---
#' title: "Initiation \u00e0 la statistique avec R, code et compl\u00e9ments chapitre 4"
#' author: "Fr\u00e9d\u00e9ric Bertrand et Myriam Maumy-Bertrand"
#' date: "20 mars 2023"
#' ---

#Chapitre 4
#page 208
#Exercice 4.2
#2)
dnorm(0)
pnorm(2.58)
qnorm(0.975)
rnorm(50)
rnorm(20,mean=10,sd=2)
x=seq(-5,5,0.1) ;pdf=dnorm(x) ;plot(x,pdf,type="l",main="Densit\'e d'une loi 
  normale centr\'ee et r\'eduite")
library(ggplot2)
ggplot(data.frame(x=c(-5,5)),aes(x))+stat_function(fun=dnorm)+
  ggtitle("Densit\'e d'une loi normale centr\'ee et r\'eduite")+ylab("Densit\'e")
runif(10)
rt(10,20)

#Exercice 4.1
#page 211
#1)
#couleurs <- gray(c(0,.25,.5,.75)) #En nuances de gris comme dans le livre
couleurs<-c("black","red","green","blue") #En couleurs

fd<-function(x) {dbinom(x,5,0.5)}
plot(cbind(0:5,sapply(0:5,fd)),xlim=c(0,20),ylim=c(0,.40),type="p",ylab="",xlab="",
  pch=15,cex=2,lwd=3,col=couleurs[1],cex.axis=2)
fd<-function(x) {dbinom(x,10,0.5)}
points(cbind(0:10,sapply(0:10,fd)),xlim=c(0,20),ylim=c(0,.40),type="p",ylab="",xlab="",
  pch=16,cex=2,lwd=3,col=couleurs[2])
#L'option new=TRUE n'est pas n\'ecessaire pour que la fonction points ajoute les points 
# au graphique d\'ej\`a existant
fd<-function(x) {dbinom(x,20,0.5)}
points(cbind(0:20,sapply(0:20,fd)),xlim=c(0,20),ylim=c(0,.40),type="p",ylab="",xlab="",
  pch=17,cex=2,lwd=3,col=couleurs[3])
#L'option new=TRUE n'est pas n\'ecessaire pour que la fonction points ajoute les points 
# au graphique d\'ej\`a existant
legtxt<-c(expression(paste(italic(n)," = 5",sep="")),expression(paste(italic(n)," = 10",
  sep="")),expression(paste(italic(n)," = 20",sep="")))
legend("topright",legtxt,title=expression(paste(italic(p)," = 0,5",sep="")),pch=c(15,16,
  17),col=c(couleurs[1],couleurs[2],couleurs[3]),cex=2,bg="white",inset=.075)

#page 212
#2)
dhypergeom<-function(x,N,n,p) (choose(N*p,x)*choose(N*(1-p),n-x)/choose(N,n))
fd<-function(x) {dhypergeom(x,14,10,0.5)}
plot(cbind(0:10,sapply(0:10,fd)),xlim=c(0,10),ylim=c(0,.5),type="p",ylab="",xlab="",
  pch=15,cex=2,lwd=3,col=couleurs[4],cex.axis=2)
fd<-function(x) {dhypergeom(x,20,10,0.5)}
points(cbind(0:10,sapply(0:10,fd)),xlim=c(0,10),ylim=c(0,.5),type="p",ylab="",xlab="",
  pch=16,cex=2,lwd=3,col=couleurs[3],new=T)
fd<-function(x) {dhypergeom(x,50,10,0.5)}
points(cbind(0:10,sapply(0:10,fd)),xlim=c(0,10),ylim=c(0,.5),type="p",ylab="",xlab="",
  pch=17,cex=2,lwd=3,col=couleurs[2],new=T)
fd<-function(x) {dbinom(x,10,0.5)}
points(cbind(0:10,sapply(0:10,fd)),xlim=c(0,10),ylim=c(0,.5),type="p",ylab="",xlab="",
  pch=18,cex=2,lwd=3,col=couleurs[1],new=T)
legtxt<-c(expression(paste(italic(N)," = 14",sep="")),expression(paste(italic(N)," = 20",
  sep="")),expression(paste(italic(N)," = 50",sep="")),expression(paste(italic(B),
  "(10;0,5)",sep="")))
legend("topright",legtxt,title=expression(paste(italic(n)," = 10 et ",italic(p)," = 0,5",
  sep="")),pch=c(15,16,17,18),col=c(couleurs[4],couleurs[3],couleurs[2],couleurs[1]),
  cex=1.6,bg="white",inset=.0)

#3)
fr<-function(x) {pchisq(x,1)}
curve(fr,from=-1,to=9,ylab="",xlab="",lty=1,lwd=3,col=couleurs[1],type="n",cex.axis=2)
curve(fr,from=-1,to=-0.000001,ylab="",xlab="",lty=5,lwd=3,add=TRUE,col=couleurs[1])
curve(fr,from=0.000001,to=9,ylab="",xlab="",lty=5,lwd=3,add=TRUE,col=couleurs[1])
fr<-function(x) {pchisq(x,3)}
curve(fr,from=-1,to=-0.000001,ylab="",xlab="",lty=1,lwd=3,col=couleurs[3],add=TRUE)
curve(fr,from=0.000001,to=9,ylab="",xlab="",lty=4,lwd=3,col=couleurs[3],add=TRUE)
fr<-function(x) {pchisq(x,2)}
curve(fr,from=-1,to=-0.000001,ylab="",xlab="",lty=2,lwd=3,add=TRUE,col=couleurs[2])
curve(fr,from=0.000001,to=9,ylab="",xlab="",lty=2,lwd=3,add=TRUE,col=couleurs[2])
fr<-function(x) {pchisq(x,6)}
curve(fr,from=-1,to=-0.000001,ylab="",xlab="",lty=4,lwd=3,add=TRUE,col=couleurs[1])
#la fin de cette instruction est sur la page 212
curve(fr,from=0.000001,to=9,ylab="",xlab="",lty=1,lwd=3,add=TRUE,col=couleurs[4])

#page 213
legtxt<-c(expression(paste(italic(p)," = 1",sep="")),expression(paste(italic(p)," = 2",
  sep="")),expression(paste(italic(p)," = 3",sep="")),expression(paste(italic(p)," = 6",
  sep="")))
legend("bottomright",legtxt,lty=c(5,2,4,1),lwd=3,col=c(couleurs[1],couleurs[2],
  couleurs[3],couleurs[4]),cex=2,bg="white",inset=.0375)

#4)
fd<-function(x) {dnorm(x)}
curve(fd,from=-4,to=4,ylab="",xlab="",lty=5,lwd=3,add=FALSE,col=couleurs[1],cex.axis=2)
fd<-function(x) {dt(x,1)}
curve(fd,from=-4,to=4,ylab="",xlab="",lty=1,lwd=3,add=TRUE,col=couleurs[2])
fd<-function(x) {dt(x,2)}
curve(fd,from=-4,to=4,ylab="",xlab="",lty=2,lwd=3,add=TRUE,col=couleurs[3])
fd<-function(x) {dt(x,5)}
curve(fd,from=-4,to=4,ylab="",xlab="",lty=4,lwd=3,add=TRUE,col=couleurs[4])
legtxt<-c(expression(paste(italic(n)," = 1",sep="")),expression(paste(italic(n)," = 2",
  sep="")),expression(paste(italic(n)," = 5",sep="")),expression(paste(italic(N),"(0;1)",
  sep="")))
legend("topleft",legtxt,lty=c(1,2,4,5),lwd=3,col=c(couleurs[2],couleurs[3],couleurs[4],
  couleurs[1]),cex=1.6,bg="white",inset=.0375)

#Exercice 4.2
dnorm(0)

#page 214
1/sqrt(2*pi)
pnorm(2.58)
qnorm(0.975)
rnorm(50)
rnorm(20,mean=10,sd=2)

#page 215
x=seq(-5,5,0.1) ;pdf=dnorm(x) ;plot(x,pdf,type="l",
  main="Densit\'e de la loi normale centr\'ee et r\'eduite")
runif(10)

#page 216
rt(10,20)

#Exercice 4.3
#1)
dbinom(5,150,0.02)

#page 217
pbinom(3,150,0.02)
qbinom(0.99,150,0.02)

#page 218
#Exercice 4.4
#1)
qbinom(0.95,230,0.85,lower.tail = FALSE)
qbinom(0.95,240,0.85,lower.tail = FALSE)
qbinom(0.95,246,0.85,lower.tail = FALSE)

plot(230:250,qbinom(0.95,230:250,0.85,lower.tail = FALSE))
abline(h=200)
abline(v=246)

#page 219
#2)
which.max(dbinom(0:330,330,.85))

plot(0:330,dbinom(0:330,330,.85),xlab="n",ylab="Probabilit\'e",lwd=2)
plot(270:285,dbinom(270:285,330,.85),xlab="n",ylab="Probabilit\'e",lwd=2)

#En plus : code figure 424
old.par <- par(no.readonly = TRUE)
layout(t(1:2))
plot(0:330,dbinom(0:330,330,.85),xlab="n",ylab="Probabilit\'e",lwd=2)
plot(270:285,dbinom(270:285,330,.85),xlab="n",ylab="Probabilit\'e",lwd=2)
abline(v=281)
layout(1)
par(old.par)

old.par <- par(no.readonly = TRUE)
pdf("figure424.pdf",h=6,w=9)
layout(t(1:2))
par(oma=rep(0,4));par(mar=c(4, 4, 2, 2) + 0.1)
plot(0:330,dbinom(0:330,330,.85),xlab="n",ylab="Probabilit\'e",lwd=2)
plot(270:285,dbinom(270:285,330,.85),xlab="n",ylab="Probabilit\'e",lwd=2)
abline(v=281)
layout(1)
dev.off()
par(old.par)

#page 220
#Exercice 4.5
#1)
1-pnorm(80,92,8)

#page 221
#2)
(1-pnorm(80,92,8))*6000
#3)
which.max(dbinom(0:6000,6000,.9331928))
plot(0:6000,dbinom(0:6000,6000,.9331928),xlab="n",ylab="Probabilit\'e",lwd=2)
plot(5500:5700,dbinom(5500:5700,6000,.9331928),xlab="n",ylab="Probabilit\'e",lwd=2)
plot(5590:5610,dbinom(5590:5610,6000,.9331928),xlab="n",ylab="Probabilit\'e",lwd=2)
dbinom(5599,6000,.9331928)

#page 222
dbinom(5600,6000,.9331928)
#En plus : code figure 425
old.par <- par(no.readonly = TRUE)
layout(matrix(c(1,2,1,3),nrow=2))
par(oma=rep(0,4));par(mar=c(4, 4, 2, 2) + 0.1)
plot(0:6000,dbinom(0:6000,6000,.9331928),xlab="n",ylab="Probabilit\'e",lwd=2)
plot(5500:5700,dbinom(5500:5700,6000,.9331928),xlab="n",ylab="Probabilit\'e",lwd=2)
plot(5590:5610,dbinom(5590:5610,6000,.9331928),xlab="n",ylab="Probabilit\'e",lwd=2)
layout(1)
par(old.par)

old.par <- par(no.readonly = TRUE)
pdf("figure425.pdf",h=6,w=9)
layout(matrix(c(1,2,1,3),nrow=2))
par(oma=rep(0,4));par(mar=c(4, 4, 2, 2) + 0.1)
plot(0:6000,dbinom(0:6000,6000,.9331928),xlab="n",ylab="Probabilit\'e",lwd=2)
plot(5500:5700,dbinom(5500:5700,6000,.9331928),xlab="n",ylab="Probabilit\'e",lwd=2)
plot(5590:5610,dbinom(5590:5610,6000,.9331928),xlab="n",ylab="Probabilit\'e",lwd=2)
layout(1)
dev.off()
par(old.par)


