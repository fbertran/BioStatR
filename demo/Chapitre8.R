#' ---
#' title: "Initiation \u00e0 la statistique avec R, code et compl\u00e9ments chapitre 8"
#' author: "Fr\u00e9d\u00e9ric Bertrand et Myriam Maumy-Bertrand"
#' date: "20 mars 2023"
#' ---

#Chapitre 8
require(BioStatR)
#page 348
fisher.test(matrix(c(5,1,0,14),ncol=2,byrow=TRUE))

#page 357
#Exercice 8.1
#1)
Rhesus<-matrix(c(3620,3805,934,172,631,676,165,30),nrow=2,byrow=TRUE)
rownames(Rhesus)<-c("Rh+","Rh-")
colnames(Rhesus)<-c("O","A","B","AB")
#2)
Rhesus
#3)
class(Rhesus)
Rhesus<-as.table(Rhesus)
class(Rhesus)
#4)
plot(Rhesus,main="D\'enombrements")
pdf("figexo81.pdf")
plot(Rhesus,main="D\'enombrements")
dev.off()

#page 358
#5)
margin.table(Rhesus)
margin.table(Rhesus,margin=1)
margin.table(Rhesus,margin=2)
#6)
chisq.test(Rhesus,simulate.p.value=FALSE)$expected
chisq.test(Rhesus,simulate.p.value=FALSE)
#7)
chisq.test(Rhesus,simulate.p.value=TRUE,B=50000)

#page 359
#8)
fisher.test(Rhesus)
#9)
fisher.test(Rhesus,simulate.p.value=TRUE,B=50000)

#Exercice 8.2
#1)
flor<-matrix(c(34,73,63,16,12,12),nrow=2,byrow=T)
rownames(flor)<-c("Fleuri","Pas fleuri")
colnames(flor)<-c("Engrais A","Engrais B","Engrais C")
flor<-as.table(flor)

#page 360
#2)
flor
#3)
dim(flor)
#4)
plot(flor,main="D\'enombrements")
#5)
chisq.test(flor)$expected
chisq.test(flor)
#En plus : calcul de la p-valeur par simulation
chisq.test(flor,simulate.p.value=T,B=100000)

#page 361
#6)
chisq.test(flor)$residuals

#page 362
#7)
if(!("vcd" %in% rownames(installed.packages()))){install.packages("vcd")}
library(vcd)
assoc(flor,shade=TRUE)
assoc(t(flor),shade=TRUE)
pdf("figexo82.pdf")
assoc(flor,shade=TRUE)
dev.off()
pdf("figexo82transpose.pdf")
assoc(t(flor),shade=TRUE)
dev.off()

#Exercice 8.3
res.test<-chisq.test(c(100,18,24,18),p=c(90,30,30,10),rescale.p=TRUE)
res.test$expected
res.test
chisq.test(c(100,18,24,18),p=c(90,30,30,10),rescale.p=TRUE,simulate=TRUE)

#page 363
#Exercice 8.4
#1)
radio<-matrix(c(103,12,18,35),nrow=2,byrow=T)
rownames(radio)<-c("Bras cass\'e","Bras normal")
colnames(radio)<-c("Bras cass\'e","Bras normal")
radio<-as.table(radio)
#2)
radio
#4)
mcnemar.test(radio)

#page 364
#5)
binom.test(radio[2],n=sum(radio[c(2,3)]))
