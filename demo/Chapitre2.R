#' ---
#' title: "Initiation \`a la statistique avec R, code et compl\'ements chapitre 2"
#' author: "Fr\'ed\'eric Bertrand et Myriam Maumy-Bertrand"
#' date: "04 octobre 2022"
#' ---

#Chapitre 2
#page 22
data(package="datasets")
?iris   

#page 23
help(iris)
iris

#page 24
n<-28
N<-20

#page 25
m=1973
m
n
N+n

#page 26
rm(m)
rm(n,N)
rm(list = ls())

#page 27
class(iris)
mode(iris)
names(iris)
length(iris)
dim(iris)

#page 29
serie1<-c(1.2,36,5.33,-26.5)
serie1
mode(serie1)
class(serie1)
c(1.2,36,5.33,-26.5)
(serie1<-c(1.2,36,5.33,-26.5))

#page 30
serie2<-c("bleu","vert","marron")
serie2
mode(serie2)
#serie2<-c(bleu,vert,marron)
serie3<-c(T,T,F,F,T)
serie3

#page 31
serie3<-c(TRUE,TRUE,FALSE,FALSE,TRUE)
serie3
mode(serie3)
serie1[3]
serie1[3:4]

#page 32
head(serie1,n=2)
tail(serie1,n=2)
v<-c(2.3,3.5,6,14,12)
w<-c(3.2,5,0.7,1,3.5)

#page 33
x<-c(v,w)
x
y<-c(w,v)
y
v[c(2,5)]
v[-c(2,3)]

#page 34
v[v>4]
w[v>4]
(v+w)/2
20+5*v
z<-c(2.8,3,19.73)
z

#page 35
v+z
length(v)
length(z)
s<-1:10
s

#page 36
s[3]<-35
s
s[s==1]<-25
s
s[s>=5]<-20
s
donnees<-c(1,2,3)
donnees

#page 37
rep(x=donnees,times=2)
rep(x=donnees,2)
rep(1,50)
rep("chien",4)

#page 38
notes.Guillaume<-c(Anglais=12,Informatique=19.5,Biologie=14)
notes.Guillaume
matiere<-c("Anglais","Informatique","Biologie")
matiere
note<-c(12,19.5,14)
note
names(note)<-matiere
note
names(note)<-NULL
note

#page 39
sort(note)
rev(sort(note))
rev(note)
serie4<-c(1.2,36,NA,-26.5)
serie4

#page 40
mode(serie4)
is.na
is.na(serie4)
matrice1<-matrix(1:12,ncol=3)
matrice1

#page 41
matrice2<-matrix(1:12,ncol=3,byrow=TRUE)
matrice2
class(matrice2)
length(matrice2)

#page 42
dim(matrice2)
matrice3<-matrix(1:12,nrow=4,ncol=4)
matrice3
matrice3[3,3]

#page 43
matrice3[3,]
matrice3[,3]
matrice3[,3,drop=FALSE]

#page 44
(matrice4<-matrice3[,c(2,4)])
(matrice5<-matrice3[,-1])
nrow(matrice5)

#page 45
ncol(matrice5)
dim(matrice5)
rbind(matrice5,c(13:15))
cbind(matrice5,c(13:16))

#page 46
matrice6<-matrix(1:6,ncol=3)
matrice6
matrice7<-matrix(1:12,ncol=4)
matrice7
matrice8<-matrice6 %*% matrice7
matrice8

#page 47
#matrice6 * matrice7
matrice9<-matrix(7:12,ncol=3)
matrice9
matrice10<-matrice6 * matrice9
matrice10
matrice11<-matrice9 * matrice6

#page 48
matrice11<-matrice9 * matrice6
matrice11
#matrice12<-matrice7 %*% matrice6

#page 49
mode

#page 50
args(matrix)

#page 51
aov(Sepal.Length~Species,data=iris)
#jeu1<-scan()
#1.2
#36
#5.33
#-26.5
#

#page 52
#jeu1

#matrix(scan(),nrow=2,byrow=TRUE)
#1 3 4
#5 2 1

mat<-c(19.6,17.6,18.2,16.0)
phy<-c(19.1,17.8,18.7,16.1)

#page 53
res<-data.frame(mat,phy)
res
res2<-data.frame(mat,phy,row.names=c("Guillaume","Val\'erie","Thomas","Julie"))
res2
#page 54
getwd()
#setwd("C:\\Data")
#setwd("C:/Data")
#page 55
Chemin<-"/Users/fbertran/Documents/GitHub/R3ed_complements/"
Chemin
pH<-c(1.2,3.5,11.0,7.1,8.2)

#page 56
pH
setwd(Chemin)
save(pH,file="FichierpH.RData")
#page 55
rm(pH)
#pH
load("FichierpH.RData")
pH

#page 57
read.table(paste(Chemin,"table1.txt",sep=""))
read.table("table1.txt")
#read.table(file.choose())

#page 58
read.table("https://fbertran.github.io/homepage/BioStatR/table1.txt")
table1<-read.table("table1.txt")
table1
table1$V1

#page 59
table1[1,1]
table1[c(1),c(1)]
table1[1:2,1]
table1[1:2,1:2]
masse<-table1$V1
taille<-table1$V2
masse

#page 60
taille
read.table("table2.txt",header=TRUE)
read.table("table3.txt",dec=",")
read.table("table4.txt",sep=";")

#page 61
#write.table(table1,file=file.choose())
read.csv("table6.csv")
read.csv2("table5.csv")
#write.csv(table1,file=file.choose())
#write.csv2(table1,file=file.choose())

#page 63
if(!("xlsx" %in% rownames(installed.packages()))){install.packages("xlsx")}
library(xlsx)
(data<-read.xlsx("table7.xls",1))
args(read.xlsx)

#page 65
data$BMI<-data$Masse/(data$Taille/100)^2
write.xlsx(x=data,file="table10.xlsx",sheetName="FeuilleTest",row.names=FALSE)
write.xlsx(x=data,file="table10.xlsx",sheetName="AutreFeuilleTest",row.names=FALSE,append=TRUE)

#page 66
args(write.xlsx)
wb<-loadWorkbook("table10.xlsx")
feuilles <- getSheets(wb)
feuille <- feuilles[[1]]

#page 67
feuille <- createSheet(wb, sheetName="ajout1")
addDataFrame(x=data,sheet=feuille,row.names = FALSE, startRow = 1, startColumn = 5)
feuille2 <- createSheet(wb, sheetName="graphique")
png(filename = "matplotdata.png", width=6, height=6, units= "in", pointsize=12, res=120)
plot(data)
dev.off()
addPicture("matplotdata.png", feuille2, scale=1, startRow =2, startColumn=2)

png(filename = "matplotdata2.png", width=6, height=8, units= "in", pointsize=12, res=300)
plot(data)
dev.off()
addPicture("matplotdata2.png", feuille2, scale=.4, startRow =62, startColumn=1)
addPicture("matplotdata2.png", feuille2, scale=1, startRow =62, startColumn=14)

#page 68
saveWorkbook(wb,"table8bis.xlsx")

#if(!("RODBC" %in% rownames(installed.packages()))){install.packages("RODBC")}
#library(RODBC)
#connexion<-odbcConnectExcel()
# sqlTables(connexion)
#data<-sqlFetch(connexion,"Feuil1")
#close(connexion)
#data

#page 69
#connexion<-odbcConnectExcel(,readOnly=FALSE)
#data<-sqlFetch(connexion,"Feuil1")
#data$BMI<-data$Masse/(data$Taille/100)^2
#sqlSave(connexion,data,rownames=FALSE)
#close(connexion)

#connexion<-odbcConnectExcel(,readOnly=FALSE)
#data<-sqlFetch(connexion,"Feuil2")
#data$BMI<-data$Masse/(data$Taille/100)^2
#sqlUpdate(connexion,data,"Feuil2",index="F1")
#close(connexion)

#page 70
if(!("gdata" %in% rownames(installed.packages()))){install.packages("gdata")}
library(gdata)
read.xls("table7.xls")
#Pas de donn\'ees dans la feuille 2 donc erreur lors de la lecture
#read.xls("table7.xls",sheet=2)

#page 71
read.xls("https://fbertran.github.io/homepage/BioStatR/table7.xls",sheet=1)
if(!("XLConnect" %in% rownames(installed.packages()))){install.packages("XLConnect")}
#vignette("XLConnect")
#vignette("XLConnectImpatient")

#page 77
u<-1:10
v<-1:8
outer(u,v,"*")

x<-c(NA,FALSE,TRUE)
names(x)<-as.character(x)
!x
outer(x,x,"&")

#page 78
outer(x,x,"|")
outer(x,x,"xor")

#page 79
#Exercice 2.1
v<-101:112
v
#page 80
v<-seq(101,112)
v
w<-rep(c(4,6,3),4)
w
length(w)
x<-c(rep(4,8),rep(6,7),rep(3,5))
x
length(x)
x<-rep(c(4,6,3),c(8,7,5))
x

#page 81
#Exercice 2.2
masse<-c(28,27.5,27,28,30.5,30,31,29.5,30,31,31,31.5,32,30,30.5)
masse
masse1<-c(40,39,41,37.5,43)
masse1
nouveau.masse<-c(rep(masse1,2),masse[6:15])
nouveau.masse
length(nouveau.masse)

#page 82
(nouveau.masse<-c(rep(masse1,2),tail(masse,n=10)))
nouveau.masse
library(xlsx)
write.xlsx(nouveau.masse,file="test.xlsx")
#write.xls(data.frame(masse=nouveau.masse),file=file.choose())
#massedf<-data.frame(nouveau.masse)
#library(RODBC)
#connexion<-odbcConnectExcel("Resultat.xls",readOnly = FALSE)
#sqlSave(connexion,massedf)
#close(connexion)

#page 83
#Exercice 2.3
nom<-c("Guillaume","Val\'erie","Thomas","Julie","S\'ebastien","St\'ephanie","Gr\'egory","Ambre",
       "Jean-S\'ebastien","Camille")
nom
age<-c(25,24,23,22,41,40,59,58,47,56)
names(age)<-nom
age
str(age)

#page 84
age<-data.frame(age,row.names=nom)
age
masse<-c(66.5,50.5,67.5,52,83,65,79,64,81,53)
names(masse)<-nom
masse
c("Guillaume"=66.5,"Val\'erie"=50.5,"Thomas"=67.5,"Julie"=52.0,"S\'ebastien"=83.0,
  "St\'ephanie"=65.0,"Gr\'egory"=79.0,"Ambre"=64.0,"Jean-S\'ebastien"=81.0,"Camille"=53.0)

#page 85
masse<-data.frame(masse,row.names=nom)
masse
taille<-c(1.86,1.62,1.72,1.67,1.98,1.77,1.83,1.68,1.92,1.71)
names(taille)<-nom
taille
taille<-data.frame(taille,row.names=nom)
taille

#page 86
masse.lourde<-masse[masse>80]
masse.lourde
masse<-data.frame(masse,row.names=nom)
masse.lourde<-masse[masse>80]
masse.lourde
str(masse.lourde)

#page 87
masse.lourde<-masse[masse>80,,drop=FALSE]
masse.lourde
masse.lourde<-masse[masse>80]
taille.masse.lourde<-taille[masse>=80]
taille.masse.lourde
taille.masse.lourde<-taille[masse>=80,,drop=FALSE]
taille.masse.lourde

#page 88
taille.vieux.masse.lourde<-taille[masse>=80 & age>=30]
taille.vieux.masse.lourde
taille.vieux.masse.lourde<-taille[masse>=80 & age>=30,,drop=FALSE]
taille.vieux.masse.lourde
ensemble<-cbind(age,masse,taille)
ensemble

#page 89
suite<-1:12
suite
suite>6
suite<6
!(suite>=6)
suite==6

#page 90
suite<=6 & suite>=6
suite<=8 && suite>=4
suite<=4 | suite>=8
suite<=4||suite>=8

