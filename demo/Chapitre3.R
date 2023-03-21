#' ---
#' title: "Initiation \u00e0 la statistique avec R, code et compl\u00e9ments chapitre 3"
#' author: "Fr\u00e9d\u00e9ric Bertrand et Myriam Maumy-Bertrand"
#' date: "20 mars 2023"
#' ---

#Chapitre 3
#page 95
library(BioStatR)
Mesures
head(Mesures)

#page 96
head(Mesures,10)
tail(Mesures)

#page 97
str(Mesures)
class(Mesures$espece)
names(Mesures$espece)
names(Mesures)

#page 98
levels(Mesures$espece)
?factor
str(Mesures5)
Mesures5

#page 101
table_graines<-table(Mesures5$graines)
table_graines
effcum_graines<-cumsum(table_graines)
effcum_graines

#page 102
table(Mesures5$espece)
freq_table_graines<-table_graines/sum(table_graines)
options(digits=3)
freq_table_graines
freq_table_graines<-prop.table(table(Mesures5$graines))
freq_table_graines

#page 103
freqcum_table_graines<-cumsum(table_graines/sum(table_graines))
freqcum_table_graines
freqcum_table_graines<-cumsum(prop.table((table(Mesures5$graines))))
freqcum_table_graines

#page 104
?hist

#page 105
minmax<-c(min(Mesures$masse),max(Mesures$masse))
minmax
histo<-hist(Mesures$masse)
classes<-histo$breaks
classes

#page 106
effectifs<-histo$counts
effectifs
effectifs<-histo$counts
cumsum(effectifs)
frequences<-effectifs/sum(effectifs)
print(frequences,digits=3)
sum(frequences)

#page 107
print(cumsum(frequences),digits=3)
table(Mesures$espece)
plot(taille~masse,data=Mesures)
#ggplot est une biblioth\`eque graphique \`a conna^itre
if(!("ggplot2" %in%
     rownames(installed.packages()))){install.packages("ggplot2")}
library(ggplot2)
#ggplot(Mesures, aes(x = masse)) + geom_histogram()
#Pas le m^eme calcul de la largeur des classes par d\'efaut. Dans ggplot2, la
#largeur des classes (binwidth) est \'egale \`a l'\'etendue divis\'ee par 30.
ggplot(Mesures,aes(x=masse,y=taille))+geom_point()
pdf("figure32Bggplot.pdf")
print(ggplot(Mesures, aes(x = masse,y=taille)) + geom_point())
dev.off()

#page 109
args(plot.default)
names(par())

#page 110
plot(taille~masse,pch=19,main="Taille vs. Masse",xlab="Masse",ylab="Taille",data=Mesures)
ggplot(Mesures, aes(x = masse,y=taille)) + geom_point(pch=19) + xlab("Masse") +
  ylab("Taille") + ggtitle("Taille vs. Masse")
#Autre mani\`ere de sp\'ecifier le titre et le noms des axes
ggplot(Mesures, aes(x = masse,y=taille)) + geom_point(pch=19) + labs(title =
  "Taille vs. Masse", x = "Masse", y = "Taille")

#page 111
pdf("figure33Bggplot.pdf")
print(ggplot(Mesures, aes(x = masse,y=taille)) + geom_point(pch=19) +
        xlab("Masse") + ylab("Taille") + ggtitle("Taille vs. Masse"))
dev.off()
ggplot(Mesures, aes(x = masse,y=taille)) + geom_point(pch=19) + xlab("Masse") +
  ylab("Taille") + ggtitle("Taille vs. Masse")+theme(plot.title=element_text(hjust = 0.5))
#Titre au centre
theme_update(plot.title = element_text(hjust = 0.5))
ggplot(Mesures, aes(x = masse,y=taille)) + geom_point(pch=19) + labs(title =
  "Taille vs. Masse", x = "Masse", y = "Taille")
#Titre \`a gauche
theme_update(plot.title = element_text(hjust = 0))
ggplot(Mesures, aes(x = masse,y=taille)) + geom_point(pch=19) + labs(title =
  "Taille vs. Masse", x = "Masse", y = "Taille")

#page 112
#Titre \`a droite
theme_update(plot.title = element_text(hjust = 1))
ggplot(Mesures, aes(x = masse,y=taille)) + geom_point(pch=19) + labs(title =
  "Taille vs. Masse", x = "Masse", y = "Taille")

pdf("figure33Cggplot.pdf")
theme_update(plot.title = element_text(hjust = 0.5))
print(ggplot(Mesures, aes(x = masse,y=taille)) + geom_point(pch=19) +
        xlab("Masse") + ylab("Taille") + ggtitle("Taille vs. Masse")) 
dev.off()

#page 113
pairs(Mesures5)
pdf("figure34.pdf")
pairs(Mesures5)
dev.off()
pairs(Mesures5,diag.panel=panel.hist)
pdf("figure35A.pdf")
pairs(Mesures5,diag.panel=panel.hist)
dev.off()

#page 114
if(!("GGally" %in% rownames(installed.packages()))){install.packages("GGally")}
library(GGally)
#Noir et blanc
ggpairs(Mesures5)
pdf("figure35Bggplot.pdf")
print(ggpairs(Mesures5))
dev.off()
#Si besoin, cr\'eer des abr\'eviations pour les noms des variables
Mesures5abbr <- Mesures5
Mesures5abbr$espece <- abbreviate(Mesures5$espece)
ggpairs(Mesures5abbr, axisLabels='show')
pdf("figure35abbrggplot.pdf")
print(ggpairs(Mesures5abbr, axisLabels='show'))
dev.off()
#Couleur et groupes
ggpairs(Mesures5abbr, ggplot2::aes(colour=espece, alpha=0.4), axisLabels='show')
pdf("figure35couleurggplot.pdf")
print(ggpairs(Mesures5abbr, ggplot2::aes(colour=espece, alpha=0.4),
              axisLabels='show'))
dev.off()

#En plus
#Noir et blanc
Mesuresabbr <- Mesures
Mesuresabbr$espece <- abbreviate(Mesures$espece)
ggpairs(Mesuresabbr, diag=list(continuous="bar"), axisLabels='show')
ggpairs(Mesures5abbr, diag=list(continuous="bar"), axisLabels='show')
pdf("figure35Mesuresggplot.pdf")
print(ggpairs(Mesuresabbr, diag=list(continuous="bar"), axisLabels='show'))
dev.off()
pdf("figure35Mesures5ggplot.pdf")
print(ggpairs(Mesures5abbr, diag=list(continuous="bar"), axisLabels='show'))
dev.off()
#Couleur
ggpairs(Mesuresabbr, ggplot2::aes(colour=espece, alpha=0.4),
        diag=list(continuous="bar"), axisLabels='show')
pdf("figure35MesuresCouleurggplot.pdf")
print(ggpairs(Mesuresabbr, ggplot2::aes(colour=espece, alpha=0.4),
              diag=list(continuous="bar"), axisLabels='show'))
dev.off()
ggpairs(Mesures5abbr, ggplot2::aes(colour=espece, alpha=0.4),
        diag=list(continuous="bar"), axisLabels='show')
pdf("figure35Mesures5Couleurggplot.pdf")
print(ggpairs(Mesures5abbr, ggplot2::aes(colour=espece, alpha=0.4),
              diag=list(continuous="bar"), axisLabels='show'))
dev.off()

#page 116
plot(table(Mesures5$graines),type="h",lwd=4,col="red",xlab="Nombre de graines",ylab="Effectif")
pdf("figure36Aggplot.pdf")
plot(table(Mesures5$graines),type="h",lwd=4,col="red",xlab="Nombre de graines",ylab="Effectif")
dev.off()

#page 117
table(Mesures5$graines)

#page 118
ggplot(Mesures5, aes(x = graines)) + geom_bar(fill=I("red")) + 
  xlab("Nombre de graines") + ylab("Effectif")
ggplot(Mesures5, aes(x = graines)) + geom_histogram(binwidth=.1,fill=I("red")) +
  xlab("Nombre de graines") + ylab("Effectif")
pdf("figure36Bggplot.pdf")
ggplot(Mesures5, aes(x = graines)) + geom_histogram(binwidth=.1,fill=I("red")) +
  xlab("Nombre de graines") + ylab("Effectif")
dev.off()

#page 119
ggplot(Mesures5, aes(x = graines)) + geom_histogram(binwidth=.1,fill=I("red")) +
  xlab("Nombre de graines") + ylab("Effectif") + facet_grid(.~espece)
ggplot(Mesures5, aes(x = graines)) + geom_histogram(binwidth=.1,fill=I("red")) +
  xlab("Nombre de graines") + ylab("Effectif") + facet_grid(espece~.)
ggplot(Mesures5, aes(x = graines)) + geom_histogram(binwidth=.1,fill=I("red")) +
  xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)
pdf("figure36Cggplot.pdf")
ggplot(Mesures5, aes(x = graines)) + geom_histogram(binwidth=.1,fill=I("red")) +
  xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)
dev.off()
tapply(Mesures5$graines,Mesures5$espece,table)

#En plus avec ggplot
data.graines_espece<-as.data.frame(table(Mesures5$graines,Mesures5$espece))
colnames(data.graines_espece)<-c("nbr.graines","espece","effectif")
ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines))+geom_bar(stat=
  "identity")+ facet_grid(espece~.)
ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines))+geom_bar(stat=
  "identity")+ facet_grid(~espece)
ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines))+geom_bar(stat=
  "identity")+ facet_wrap(~espece)

ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines,fill=espece))+geom_bar(
  stat="identity")+ facet_wrap(~espece)
ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines,fill=espece))+geom_bar(
  stat="identity")+ facet_wrap(~espece) + scale_fill_grey() + theme_bw()
ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))+
  geom_bar(stat="identity")+ facet_wrap(~espece)
ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))+
  geom_bar(stat="identity")+ facet_wrap(~espece) + scale_fill_grey() + theme_bw()

pdf("figure36Dggplot.pdf")
print(ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines))+geom_bar(stat=
  "identity")+ facet_grid(espece~.))
dev.off()
pdf("figure36Eggplot.pdf")
print(ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines))+geom_bar(stat=
  "identity")+ facet_grid(~espece))
dev.off()
pdf("figure36Fggplot.pdf")
print(ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines))+geom_bar(stat=
  "identity")+ facet_wrap(~espece))
dev.off()
pdf("figure36Gggplot.pdf")
print(ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines,fill=espece))+
        geom_bar(stat="identity")+ facet_wrap(~espece))
dev.off()
pdf("figure36Hbwggplot.pdf")
print(ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines,fill=espece))+
        geom_bar(stat="identity")+ facet_wrap(~espece) + scale_fill_grey() + theme_bw())
dev.off()
pdf("figure36Iggplot.pdf")
print(ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))
      +geom_bar(stat="identity")+ facet_wrap(~espece))
dev.off()
pdf("figure36Jbwggplot.pdf")
print(ggplot(data.graines_espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))
      +geom_bar(stat="identity")+ facet_wrap(~espece) + scale_fill_grey() +
        theme_bw())
dev.off()

#page 120
tapply(Mesures5$graines,Mesures5$espece,table)
if(!("lattice" %in%
     rownames(installed.packages()))){install.packages("lattice")}
library("lattice")
data.graines_espece<-as.data.frame(table(Mesures5$graines,Mesures5$espece))
colnames(data.graines_espece)<-c("nbr.graines","espece","effectif")
barchart(effectif~nbr.graines|espece,data=data.graines_espece,layout=c(1,4))

#page 121
as.data.frame(table(Mesures5$graines,Mesures5$espece))
(table.graines.espece <-
    table(Mesures5$graines,Mesures5$espece,dnn=c("nbr.graines","espece")))
print(table.graines.espece,zero.print=".")
(data.graines.espece <-
    as.data.frame(table.graines.espece,responseName="effectif"))
barchart(effectif~nbr.graines|espece,data= data.graines.espece)
pdf("figure38lattice.pdf")
barchart(effectif~nbr.graines|espece,data= data.graines.espece)
dev.off()

#En plus avec ggplot2
ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))+
  geom_bar(stat="identity")+ facet_wrap(~espece)
pdf("figure38ggplot.pdf")
print(ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))
      +geom_bar(stat="identity")+ facet_wrap(~espece))
dev.off()

#page 122
(table.graines.espece <-
    table(factor(Mesures5$graines),Mesures5$espece,dnn=c("nbr.graines","espece"),
          exclude=c("bignone","laurier rose")))
#En plus pour supprimer la modalit\'e <NA>
(table.graines.espece <-
    table(factor(Mesures5$graines),Mesures5$espece,dnn=c("nbr.graines","espece"),
          exclude=c("bignone","laurier rose"), useNA="no"))

#page 123
(data.graines.espece<-as.data.frame(table.graines.espece,responseName="effectif"
))
pdf("figure39lattice.pdf")
barchart(effectif~nbr.graines|espece,data=data.graines.espece)
dev.off()
#En plus avec ggplot
ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))+
  geom_bar(stat="identity")+ facet_grid(~espece)
pdf("figure39ggplot.pdf")
print(ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))
      +geom_bar(stat="identity")+ facet_grid(~espece))
dev.off()
print(ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))
      +geom_bar(stat="identity")+ facet_grid(~espece) + scale_fill_grey() +
        theme_bw())
pdf("figure39bwggplot.pdf")
print(ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))
      +geom_bar(stat="identity")+ facet_grid(~espece) + scale_fill_grey() +
        theme_bw())
dev.off()
barchart(effectif~nbr.graines|espece,data=data.graines.espece,layout=c(1,2))
pdf("figure310lattice.pdf")
barchart(effectif~nbr.graines|espece,data=data.graines.espece,layout=c(1,2))
dev.off()
#En plus
ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))+
  geom_bar(stat="identity")+ facet_grid(espece~.)
pdf("figure310ggplot.pdf")
print(ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))
      +geom_bar(stat="identity")+ facet_grid(espece~.))
dev.off()
print(ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))
      +geom_bar(stat="identity")+ facet_grid(espece~.) + scale_fill_grey() +
        theme_bw())
pdf("figure310bwggplot.pdf")
print(ggplot(data.graines.espece,aes(y=effectif,x=nbr.graines,fill=nbr.graines))
      +geom_bar(stat="identity")+ facet_grid(espece~.) + scale_fill_grey() +
        theme_bw())
dev.off()

#page 125
xyplot(effectif~nbr.graines|espece,data=data.graines.espece,type="h",lwd=4)
pdf("figure311lattice.pdf")
xyplot(effectif~nbr.graines|espece,data=data.graines.espece,type="h",lwd=4)
dev.off()
#En plus ggplot
ggplot(data.graines.espece, aes(x = nbr.graines)) +
  geom_linerange(aes(ymin=0,ymax=effectif,group=espece),size=1.2,color=I("blue"))+
  xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)
pdf("figure311ggplot.pdf")
print(ggplot(data.graines.espece, aes(x = nbr.graines)) +
        geom_linerange(aes(ymin=0,ymax=effectif,group=espece),size=1.2,color=I("blue"))+
        xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece))
dev.off()
xyplot(effectif~nbr.graines|espece,data=data.graines.espece,type="h",layout=c(1,2),lwd=4)
pdf("figure312lattice.pdf")
xyplot(effectif~nbr.graines|espece,data=data.graines.espece,type="h",layout=c(1,2),lwd=4)
dev.off()
ggplot(data.graines.espece, aes(x = nbr.graines)) +
  geom_linerange(aes(ymin=0,ymax=effectif,group=espece),size=1.2,color=I("blue"))+
  xlab("Nombre de graines") + ylab("Effectif") + facet_grid(espece~.)
pdf("figure312ggplot.pdf")
print(ggplot(data.graines.espece, aes(x = nbr.graines)) +
        geom_linerange(aes(ymin=0,ymax=effectif,group=espece),size=1.2,color=I("blue"))+
        xlab("Nombre de graines") + ylab("Effectif") + facet_grid(espece~.))
dev.off()

#page 126
barplot(table.graines.espece,beside=TRUE,legend=rownames(table.graines.espece))
pdf("figure313.pdf")
barplot(table.graines.espece,beside=TRUE,legend=rownames(table.graines.espece))
dev.off()
#En plus avec ggplot
ggplot(data.graines.espece, aes(x = nbr.graines, y= effectif, fill =
  nbr.graines)) + geom_bar(stat="identity") + xlab("Nombre de graines") +
  ylab("Effectif") + facet_wrap(~espece) + scale_fill_grey() + theme_bw()
pdf("figure313ggplot.pdf")
print(ggplot(data.graines.espece, aes(x = nbr.graines, y= effectif, fill =
 nbr.graines)) + geom_bar(stat="identity") + xlab("Nombre de graines") +
 ylab("Effectif") + facet_wrap(~espece) + scale_fill_grey() + theme_bw())
dev.off()

plot(table(Mesures5$graines),lwd=4,col="red",xlab="Nombre de graines",ylab="Effectif")
lines(table(Mesures5$graines),type="l",lwd=4)
pdf("figure314.pdf")
plot(table(Mesures5$graines),lwd=4,col="red",xlab="Nombre de graines",ylab="Effectif")
lines(table(Mesures5$graines),type="l",lwd=4)
dev.off()

#En plus avec ggplot
df.table_graines<-as.data.frame(table(Mesures5$graines,dnn="nbr.graines"),
                                responseName="effectif")
ggplot(df.table_graines, aes(x = nbr.graines)) +
  geom_linerange(aes(ymin=0,ymax=effectif),size=1.8,color=I("red"))+ 
  xlab("Nombre de graines") + ylab("Effectif")
pdf("figure314ggplot.pdf")
ggplot(df.table_graines, aes(x = nbr.graines)) + geom_linerange(aes(ymin=0,ymax=effectif),
  size=1.8,color=I("red"))+ xlab("Nombre de graines") + ylab("Effectif")
dev.off()
ggplot(df.table_graines, aes(x = nbr.graines)) + geom_linerange(aes(ymin=0, ymax=effectif), 
  size=1.2,color=I("red"))+ geom_line(aes(y=effectif,group=""),size=1.2,color=I("black"))+ 
  xlab("Nombre de graines") + ylab("Effectif")
pdf("figure314aggplot.pdf")
print(ggplot(df.table_graines, aes(x = nbr.graines)) +
        geom_linerange(aes(ymin=0, ymax=effectif), size=1.2,color=I("red"))+
        geom_line(aes(y=effectif,group=""), size=1.2,color=I("black"))+ 
        xlab("Nombre de graines") + ylab("Effectif"))
dev.off()
ggplot(df.table_graines, aes(x = nbr.graines))+
  geom_line(aes(y=effectif,group=""), size=1.2,color=I("black")) +
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""),
                  size=1.2,color=I("red"))+ xlab("Nombre de graines") + ylab("Effectif")
pdf("figure314bggplot.pdf")
print(ggplot(df.table_graines, aes(x = nbr.graines))+
        geom_line(aes(y=effectif,group=""), size=1.2,color=I("black")) +
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""),
                        size=1.2,color=I("red"))+ xlab("Nombre de graines") + ylab("Effectif"))
dev.off()
ggplot(df.table_graines, aes(x = nbr.graines)) +
  geom_ribbon(aes(ymin=0,ymax=effectif,group=""),fill=I("red"),alpha=.5)+
  geom_line(aes(y=effectif,group=""), size=1, color="red")+
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""), size=1,
                  color="blue")+  xlab("Nombre de graines") + ylab("Effectif") +theme_bw()
pdf("figure314cggplot.pdf")
print(ggplot(df.table_graines, aes(x = nbr.graines)) +
        geom_ribbon(aes(ymin=0,ymax=effectif,group=""),fill=I("red"),alpha=.5)+
        geom_line(aes(y=effectif,group=""), size=1, color="red")+
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""), size=1,
                        color="blue")+  xlab("Nombre de graines") + ylab("Effectif") +theme_bw())
dev.off()
ggplot(df.table_graines, aes(x = nbr.graines)) +
  geom_ribbon(aes(ymin=0,ymax=effectif,group=""),fill=I("gray80"))+
  geom_line(aes(y=effectif,group=""), size=1, color=I("gray40")) +
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""), size=1)+ 
  xlab("Nombre de graines") + ylab("Effectif") +theme_bw()
pdf("figure314dggplot.pdf")
print(ggplot(df.table_graines, aes(x = nbr.graines)) +
        geom_ribbon(aes(ymin=0,ymax=effectif,group=""),fill=I("gray80"))+
        geom_line(aes(y=effectif,group=""), size=1, color=I("gray40")) +
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""), size=1)+ 
        xlab("Nombre de graines") + ylab("Effectif") +theme_bw())
dev.off()

#En plus, ggplot par groupes
ggplot(data.graines.espece, aes(x = nbr.graines)) +
  geom_linerange(aes(ymin=0,ymax=effectif,group=espece), size=1.2,color=I("red"))+
  geom_line(aes(y=effectif,group=espece), size=1.2,color=I("black"))+ 
  xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)
pdf("figure314groupeAggplot.pdf")
print(ggplot(data.graines.espece, aes(x = nbr.graines)) +
        geom_linerange(aes(ymin=0,ymax=effectif,group=espece), size=1.2,color=I("red"))+
        geom_line(aes(y=effectif,group=espece), size=1.2,color=I("black"))+ 
        xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece))
dev.off()
ggplot(data.graines.espece, aes(x = nbr.graines))+
  geom_line(aes(y=effectif,group=espece), size=1.2,color=I("red")) +
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece),
                  size=1.2,color=I("blue"))+ xlab("Nombre de graines") + ylab("Effectif") +
  facet_wrap(~espece)
pdf("figure314groupeAggplot.pdf")
print(ggplot(data.graines.espece, aes(x = nbr.graines))+
        geom_line(aes(y=effectif,group=espece), size=1.2,color=I("red")) +
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece),
                        size=1.2,color=I("blue"))+ xlab("Nombre de graines") + ylab("Effectif") +
        facet_wrap(~espece))
dev.off()
ggplot(data.graines.espece, aes(x = nbr.graines)) +
  geom_ribbon(aes(ymin=0,ymax=effectif,group=espece),fill=I("red"),alpha=.5)+
  geom_line(aes(y=effectif,group=espece), size=1, color="red")+
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece), size=1,
                  color="blue")+  xlab("Nombre de graines") + ylab("Effectif") +
  facet_wrap(~espece)+theme_bw()
pdf("figure314groupeAggplot.pdf")
print(ggplot(data.graines.espece, aes(x = nbr.graines)) +
        geom_ribbon(aes(ymin=0,ymax=effectif,group=espece),fill=I("red"),alpha=.5)+
        geom_line(aes(y=effectif,group=espece), size=1, color="red")+
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece), size=1,
                        color="blue")+  xlab("Nombre de graines") + ylab("Effectif") +
        facet_wrap(~espece)+theme_bw())
dev.off()
ggplot(data.graines.espece, aes(x = nbr.graines)) +
  geom_ribbon(aes(ymin=0,ymax=effectif,group=espece),fill=I("gray80"))+
  geom_line(aes(y=effectif,group=espece), size=1, color=I("gray40")) +
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece), size=1)+ 
  xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)+theme_bw()
pdf("figure314groupeAggplot.pdf")
print(ggplot(data.graines.espece, aes(x = nbr.graines)) +
        geom_ribbon(aes(ymin=0,ymax=effectif,group=espece),fill=I("gray80"))+
        geom_line(aes(y=effectif,group=espece), size=1, color=I("gray40")) +
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece), size=1)+ 
        xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)+theme_bw())
dev.off()


#page 128
plot(cumsum(table(Mesures5$graines)),type="h",lwd=4,col="red",xlab="Nombre de graines",
     ylab="Effectif")
lines(cumsum(table(Mesures5$graines)),lwd=4)
pdf("figure315.pdf")
plot(cumsum(table(Mesures5$graines)),type="h",lwd=4,col="red",xlab="Nombre de graines",
     ylab="Effectif")
lines(cumsum(table(Mesures5$graines)),lwd=4)
dev.off()

df.cumsum.table_graines<-df.table_graines; df.cumsum.table_graines[,2] <-
  cumsum(df.table_graines[,2])
ggplot(df.cumsum.table_graines, aes(x = nbr.graines)) +
  geom_linerange(aes(ymin=0, ymax=effectif), size=1.2,color=I("red"))+
  geom_line(aes(y=effectif,group=""), size=1.2,color=I("black"))+ 
  xlab("Nombre de graines") + ylab("Effectif")
pdf("figure315ggplot.pdf")
print(ggplot(df.cumsum.table_graines, aes(x = nbr.graines)) +
        geom_linerange(aes(ymin=0, ymax=effectif), size=1.2,color=I("red"))+
        geom_line(aes(y=effectif,group=""), size=1.2,color=I("black"))+ 
        xlab("Nombre de graines") + ylab("Effectif"))
dev.off()
ggplot(df.cumsum.table_graines, aes(x = nbr.graines))+
  geom_line(aes(y=effectif,group=""), size=1.2,color=I("black")) +
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""),
                  size=1.2,color=I("red"))+ xlab("Nombre de graines") + ylab("Effectif")
pdf("figure315bggplot.pdf")
print(ggplot(df.cumsum.table_graines, aes(x = nbr.graines))+
        geom_line(aes(y=effectif,group=""), size=1.2,color=I("black")) +
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""),
                        size=1.2,color=I("red"))+ xlab("Nombre de graines") + ylab("Effectif"))
dev.off()
ggplot(df.cumsum.table_graines, aes(x = nbr.graines)) +
  geom_ribbon(aes(ymin=0,ymax=effectif,group=""),fill=I("red"),alpha=.5)+
  geom_line(aes(y=effectif,group=""), size=1, color="red")+
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""), size=1,
                  color="blue")+  xlab("Nombre de graines") + ylab("Effectif") +theme_bw()
pdf("figure315cggplot.pdf")
print(ggplot(df.cumsum.table_graines, aes(x = nbr.graines)) +
        geom_ribbon(aes(ymin=0,ymax=effectif,group=""),fill=I("red"),alpha=.5)+
        geom_line(aes(y=effectif,group=""), size=1, color="red")+
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""), size=1,
                        color="blue")+  xlab("Nombre de graines") + ylab("Effectif") +theme_bw())
dev.off()
ggplot(df.cumsum.table_graines, aes(x = nbr.graines)) +
  geom_ribbon(aes(ymin=0,ymax=effectif,group=""),fill=I("gray80"))+
  geom_line(aes(y=effectif,group=""), size=1, color=I("gray40")) +
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""), size=1)+ 
  xlab("Nombre de graines") + ylab("Effectif") +theme_bw()
pdf("figure315dggplot.pdf")
print(ggplot(df.cumsum.table_graines, aes(x = nbr.graines)) +
        geom_ribbon(aes(ymin=0,ymax=effectif,group=""),fill=I("gray80"))+
        geom_line(aes(y=effectif,group=""), size=1, color=I("gray40")) +
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=""), size=1)+ 
        xlab("Nombre de graines") + ylab("Effectif") +theme_bw())
dev.off()

#Par groupes
data.cumsum.graines.espece<-data.graines.espece
data.cumsum.graines.espece[,3] <- unlist(tapply(data.graines.espece[,3],
                                                data.graines.espece[,2],cumsum))

ggplot(data.cumsum.graines.espece, aes(x = nbr.graines)) +
  geom_linerange(aes(ymin=0,ymax=effectif,group=espece), size=1.2,color=I("red"))+
  geom_line(aes(y=effectif,group=espece), size=1.2,color=I("black"))+ 
  xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)
pdf("figure315eggplot.pdf")
print(ggplot(data.cumsum.graines.espece, aes(x = nbr.graines)) +
        geom_linerange(aes(ymin=0,ymax=effectif,group=espece), size=1.2,color=I("red"))+
        geom_line(aes(y=effectif,group=espece), size=1.2,color=I("black"))+ 
        xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece))
dev.off()
ggplot(data.cumsum.graines.espece, aes(x = nbr.graines))+
  geom_line(aes(y=effectif,group=espece), size=1.2,color=I("red")) +
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece),
                  size=1.2,color=I("blue"))+ xlab("Nombre de graines") + ylab("Effectif") +
  facet_wrap(~espece)
pdf("figure315fggplot.pdf")
print(ggplot(data.cumsum.graines.espece, aes(x = nbr.graines))+
        geom_line(aes(y=effectif,group=espece), size=1.2,color=I("red")) +
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece),
                        size=1.2,color=I("blue"))+ xlab("Nombre de graines") + ylab("Effectif") +
        facet_wrap(~espece))
dev.off()
ggplot(data.cumsum.graines.espece, aes(x = nbr.graines)) +
  geom_ribbon(aes(ymin=0,ymax=effectif,group=espece),fill=I("red"),alpha=.5)+
  geom_line(aes(y=effectif,group=espece), size=1, color="red")+
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece), size=1,
                  color="blue")+  xlab("Nombre de graines") + ylab("Effectif") +
  facet_wrap(~espece)+theme_bw()
pdf("figure315gggplot.pdf")
print(ggplot(data.cumsum.graines.espece, aes(x = nbr.graines)) +
        geom_ribbon(aes(ymin=0,ymax=effectif,group=espece),fill=I("red"),alpha=.5)+
        geom_line(aes(y=effectif,group=espece), size=1, color="red")+
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece), size=1,
                        color="blue")+  xlab("Nombre de graines") + ylab("Effectif") +
        facet_wrap(~espece)+theme_bw())
dev.off()
ggplot(data.cumsum.graines.espece, aes(x = nbr.graines)) +
  geom_ribbon(aes(ymin=0,ymax=effectif,group=espece),fill=I("gray80"))+
  geom_line(aes(y=effectif,group=espece), size=1, color=I("gray40")) +
  geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece), size=1)+ 
  xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)+theme_bw()
pdf("figure315hggplot.pdf")
print(ggplot(data.cumsum.graines.espece, aes(x = nbr.graines)) +
        geom_ribbon(aes(ymin=0,ymax=effectif,group=espece),fill=I("gray80"))+
        geom_line(aes(y=effectif,group=espece), size=1, color=I("gray40")) +
        geom_pointrange(aes(ymin=0,ymax=effectif,y=effectif,group=espece), size=1)+ 
        xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)+theme_bw())
dev.off()

pie.graines<-c(0.1000,0.3727,0.2455,0.1455,0.0909,0.0182,0.0273)

#page 129
names(pie.graines)<-c("1 graine","2 graines","3 graines","4 graines","5
                      graines","6 graines","7 graines")
pie(pie.graines,col=c("red","purple","cyan","blue","green","cornsilk","orange"))
pie(table(Mesures5$graines),labels=c("1 graine",paste(2:7,"
                                                      graines")),col=rainbow(7))
pdf("figure316.pdf")
pie(table(Mesures5$graines),labels=c("1 graine",paste(2:7,"
                                                      graines")),col=rainbow(7))
dev.off()

#ggplot pie is only a polar coord change from geom_bar
p=ggplot(data.graines.espece, aes(x="", y= effectif, fill = nbr.graines)) +
  geom_bar(stat="identity",position="fill") + xlab("Nombre de graines") +
  ylab("Effectif") + facet_wrap(~espece) + scale_fill_grey() + theme_bw()
p
q <- p+coord_polar(theta="y")
q
q + scale_fill_hue()
q + scale_fill_brewer()

pdf("figure316aggplot.pdf")
print(q)
dev.off()
pdf("figure316bggplot.pdf")
print(q + scale_fill_hue())
dev.off()
pdf("figure316cggplot.pdf")
print(q + scale_fill_brewer())
dev.off()

#page 130
hist(Mesures$masse)
histo<-hist(Mesures$masse,ylab="Effectif",xlab="Masse",main="Histogramme des
            masses")

#en plus ggplot
g=ggplot(Mesures,aes(x=masse))+geom_histogram()
g
pdf("figure317aggplot.pdf")
g
dev.off()
g1 = g +
  geom_histogram(binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse)
  ) #R\`egle de Sturges 
g1
pdf("figure317bggplot.pdf")
g1
dev.off()
ggplot(Mesures,aes(x=masse))+geom_histogram(aes(fill=..count..))+
  scale_fill_gradient("Count", low = "green", high = "red")
pdf("figure317cggplot.pdf")
print(ggplot(Mesures,aes(x=masse))+geom_histogram(aes(fill=..count..))+
        scale_fill_gradient("Count", low = "green", high = "red"))
dev.off()
g+geom_histogram(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse))+scale_fill_gradient("Count",low = "green", high ="red")
pdf("figure317dggplot.pdf")
print(g+geom_histogram(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse))+scale_fill_gradient("Count", low = "green", high = "red"))
dev.off()
ggplot(Mesures,aes(x=masse))+geom_histogram(aes(fill=..count..))+
  scale_fill_gradient("Count", low = "grey80", high = "black")
pdf("figure317eggplot.pdf")
print(ggplot(Mesures,aes(x=masse))+geom_histogram(aes(fill=..count..))+
        scale_fill_gradient("Count", low = "grey80", high = "black"))
dev.off()
g+geom_histogram(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse))+scale_fill_gradient("Count", low = "grey80", high = "black")
pdf("figure317fggplot.pdf")
print(g+geom_histogram(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse))+scale_fill_gradient("Count", low = "grey80", high = "black"))
dev.off()

#page 131
histo<-hist(Mesures$masse)
histo

#page 133
library(lattice)
histogram(~masse|espece,data=Mesures)
pdf("figure318lattice.pdf")
histogram(~masse|espece,data=Mesures)
dev.off()

#en plus
ggplot(Mesures, aes(x = masse)) +
  geom_histogram(binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse)
  ) + xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece)
pdf("figure318ggplot.pdf")
print(ggplot(Mesures, aes(x = masse)) +
        geom_histogram(binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse)
        ) + xlab("Nombre de graines") + ylab("Effectif") + facet_wrap(~espece))
dev.off()
ggplot(Mesures, aes(x = masse)) +
  geom_histogram(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse)) + xlab("Nombre de graines") + ylab("Effectif") +
  facet_wrap(~espece) + scale_fill_gradient("Count", low = "green", high = "red")
pdf("figure318aggplot.pdf")
print(ggplot(Mesures, aes(x = masse)) +
  geom_histogram(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse)) + xlab("Nombre de graines") + ylab("Effectif") +
  facet_wrap(~espece) + scale_fill_gradient("Count", low = "green", high = "red"))
dev.off()
g=ggplot(Mesures, aes(x = masse)) +
  geom_histogram(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse)) + xlab("Nombre de graines") + ylab("Effectif") +
  facet_wrap(~espece)
g
pdf("figure318bggplot.pdf")
print(g)
dev.off()

histo<-hist(Mesures$masse,ylab="Effectif",xlab="Masse",main="Polygone des
            effectifs des masses")
lines(histo$mids,histo$counts,lwd=2)
points(histo$mids,histo$counts,cex=1.2,pch=19)
pdf("figure319.pdf")
histo<-hist(Mesures$masse,ylab="Effectif",xlab="Masse",main="Polygone des
            effectifs des masses")
lines(histo$mids,histo$counts,lwd=2)
points(histo$mids,histo$counts,cex=1.2,pch=19)
dev.off()

#En plus ggplot
g=ggplot(Mesures, aes(x = masse)) +
  geom_histogram(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse),boundary=0) + xlab("Nombre de graines") + ylab("Effectif")
g
pdf("figure319ggplot.pdf")
print(g)
dev.off()
g1=g+geom_line(binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),
               size=2,alpha=.60,color="blue",stat="bin",boundary=0)
g1
pdf("figure319aggplot.pdf")
g1
dev.off()
g+stat_bin(binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),
           size=2,alpha=.60,color="blue",geom="line",boundary=0)
pdf("figure319bggplot.pdf")
print(g+stat_bin(binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse),size=2,alpha=.60,color="blue",geom="line",boundary=0))
dev.off()
g1+ scale_fill_gradient(low="white", high="black")
pdf("figure319cggplot.pdf")
print(g1+ scale_fill_gradient(low="white", high="black"))
dev.off()

if(!("scales" %in% rownames(installed.packages()))){install.packages("scales")}
library(scales)
g1+ scale_fill_gradient2(low=muted("red"), mid="white",
                         high=muted("blue"),midpoint=40)
pdf("figure319dggplot.pdf")
g1+ scale_fill_gradient2(low=muted("red"), mid="white",
                         high=muted("blue"),midpoint=40)
dev.off()
g1+ scale_fill_gradientn(colours = c("darkred", "orange", "yellow", "white"))
pdf("figure319eggplot.pdf")
g1+ scale_fill_gradientn(colours = c("darkred", "orange", "yellow", "white"))
dev.off()

#Par groupe
g=ggplot(Mesures, aes(x = masse)) +
  geom_histogram(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse),boundary=0) + xlab("Nombre de graines") +
  ylab("Effectif") + facet_wrap(~espece)
g
pdf("figure319fggplot.pdf")
print(g)
dev.off()
g+geom_freqpoly(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse),size=2,alpha=.60,color="blue")+
  scale_fill_gradientn(colours = c("darkred", "orange", "yellow", "white"))
pdf("figure319fggplot.pdf")
print(g+geom_freqpoly(aes(fill=..count..),binwidth=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse),size=2,alpha=.60,color="blue")+
  scale_fill_gradientn(colours = c("darkred", "orange", "yellow", "white")))
dev.off()

#page 135
histo<-hist(Mesures$masse,plot=FALSE)
barplot<-barplot(cumsum(histo$counts),ylab="Effectif",xlab="Masse",main="
                 Polygone des effectifs cumul\'es des masses")
lines(barplot,cumsum(histo$counts),lwd=2)
points(barplot,cumsum(histo$counts),cex=1.2,pch=19)
pdf("figure320.pdf")
barplot<-barplot(cumsum(histo$counts),ylab="Effectif",xlab="Masse",main="
                 Polygone des effectifs cumul\'es des masses")
lines(barplot,cumsum(histo$counts),lwd=2)
points(barplot,cumsum(histo$counts),cex=1.2,pch=19)
dev.off()

#Effectifs et polygone des fr\'equences cumul\'ees
library(qcc)
pareto.chart(table(Mesures5$graines))
pdf("figure320qcc.pdf")
pareto.chart(table(Mesures5$graines))
dev.off()

consmw=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse)
consmw

consmw.espece=cbind(espece=names(unlist(lapply(split(Mesures$masse,Mesures$
  espece),function(xxx) return(diff(range(xxx))/nclass.Sturges(xxx))))),
  consmw.espece=unlist(lapply(split(Mesures$masse,Mesures$espece),function(xxx)
  return(diff(range(xxx))/nclass.Sturges(xxx)))))
consmw.espece

Mesures.binw<-merge(cbind(Mesures,consmw=diff(range(Mesures$masse))/
  nclass.Sturges(Mesures$masse)),consmw.espece)

g=ggplot(Mesures.binw, aes(x = masse)) 
g +
  geom_histogram(data=Mesures.binw,aes(y=5.355556*..density..,fill=..density..),
  binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),boundary =
  min(Mesures$masse)) + xlab("Masse") + ylab("Fr\'equence")
pdf("figure320ggplot.pdf")
print(g +
  geom_histogram(data=Mesures.binw,aes(y=5.355556*..density..,fill=..density..),
  binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),boundary =
  min(Mesures$masse)) + xlab("Masse") + ylab("Fr\'equence"))
dev.off()

g +
  geom_histogram(data=Mesures.binw,aes(y=5.355556*..count..,fill=..count..),
  binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),boundary =
  min(Mesures$masse)) + xlab("Masse") + ylab("D\'enombrement")
pdf("figure320aggplot.pdf")
print(g +
  geom_histogram(data=Mesures.binw,aes(y=5.355556*..count..,fill=..count..),
  binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),boundary =
  min(Mesures$masse)) + xlab("Masse") + ylab("D\'enombrement"))
dev.off()

g + stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,direction="vh") +
  stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,direction="vh",geom="linerange",ymin
  =0,aes(ymax=..y..))
pdf("figure320bggplot.pdf")
print(g + stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,direction="vh") +
  stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,direction="vh",geom="linerange",ymin
  =0,aes(ymax=..y..)))
dev.off()

g +
  geom_histogram(aes(y=5.355556*..density..,fill=..density..),binwidth=diff(range(
    Mesures$masse))/nclass.Sturges(Mesures$masse),boundary = min(Mesures$masse)) +
  xlab("Masse") + ylab("Fr\'equence") +
  stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,direction="vh") +
  stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,direction="vh",geom="linerange",ymin
            =0,aes(ymax=..y..))
pdf("figure320cggplot.pdf")
print(g +
  geom_histogram(aes(y=5.355556*..density..,fill=..density..),binwidth=diff(range(
  Mesures$masse))/nclass.Sturges(Mesures$masse),boundary = min(Mesures$masse)) +
  xlab("Masse") + ylab("Fr\'equence") +
  stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,direction="vh") +
  stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,direction="vh",geom="linerange",ymin
  =0,aes(ymax=..y..)))
dev.off()

#depuis ggplot2 2.0 ne freqpoly n'est plus un geom acceptable
#g+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("rect"),fill="blue",aes(
#ymax=..y..,ymin=0,xmax=..x..,xmin=..x..-diff(range(BioStatR::Mesures$masse))/
#grDevices::nclass.Sturges(BioStatR::Mesures$masse)),alpha=.5,colour="blue")+
#stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("freqpoly"),fill="blue",aes(
#x=masse-5.355556/2,y=..y..))+geom_histogram(aes(y=5.355556*..density..,fill=..
#density..),binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),
#alpha=.35,boundary = min(Mesures5$masse))
#pdf("figure320dggplot.pdf")
#print(g+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("rect"),fill="blue",
#aes(ymax=..y..,ymin=0,xmax=..x..,xmin=..x..-diff(range(BioStatR::Mesures$masse)
#)/grDevices::nclass.Sturges(BioStatR::Mesures$masse)),alpha=.5,colour="blue")+
#stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("freqpoly"),fill="blue",aes(
#x=masse-5.355556/2,y=..y..))+geom_histogram(aes(y=5.355556*..density..,fill=..
#density..),binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),
#alpha=.35,boundary = min(Mesures5$masse)))
#dev.off()
#
g+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("bar"),fill="blue",
  aes(x=masse-5.355556/2,width=5.355556),alpha=.5,colour="blue")+stat_ecdf(n=
  nclass.Sturges(Mesures$masse)+1,geom=c("line"),fill="blue",aes(x=masse-5.355556/2,
  y=..y..))+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("point"),fill="blue",
  aes(x=masse-5.355556/2,y=..y..))+geom_histogram(aes(y=5.355556*..density..,
  fill=..density..),binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),
  alpha=.35,boundary = min(Mesures5$masse))
pdf("figure320eggplot.pdf")
print(g+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("bar"),fill="blue",
  aes(x=masse-5.355556/2,width=5.355556),alpha=.5,colour="blue")+stat_ecdf(n=
  nclass.Sturges(Mesures$masse)+1,geom=c("line"),fill="blue",aes(x=masse-5.355556/2,
  y=..y..))+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("point"),fill="blue",
  aes(x=masse-5.355556/2,y=..y..))+geom_histogram(aes(y=5.355556*..density..,
  fill=..density..),binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),
  alpha=.35,boundary = min(Mesures5$masse)))
dev.off()

g+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("bar"),fill="grey50",aes(x=masse-
  5.355556/2,width=5.355556),alpha=.5,colour="black")+stat_ecdf(n=
  nclass.Sturges(Mesures$masse)+1,geom=c("line"),fill="grey50",aes(x=masse-5.355556/2,
  y=..y..))+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("point"),fill="black",
  aes(x=masse-5.355556/2,y=..y..))+geom_histogram(aes(y=5.355556*..density..,
  fill=..density..),binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),
  alpha=.35,boundary = min(Mesures5$masse))+ scale_fill_gradient(low="white", high="black")
pdf("figure320fggplot.pdf")
print(g+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("bar"),fill="grey50", aes(x=
  masse-5.355556/2,width=5.355556),alpha=.5,colour="black")+stat_ecdf(n=
  nclass.Sturges(Mesures$masse)+1,geom=c("line"),fill="grey50",aes(x=masse-5.355556/2,
  y=..y..))+stat_ecdf(n=nclass.Sturges(Mesures$masse)+1,geom=c("point"),
  fill="black",aes(x=masse-5.355556/2,y=..y..))+geom_histogram(aes(y=5.355556*..density..,
  fill=..density..),binwidth=diff(range(Mesures$masse))/nclass.Sturges(Mesures$masse),
  alpha=.35,boundary = min(Mesures5$masse))+scale_fill_gradient(low="white", high="black"))
dev.off()

#Par groupe
g+stat_ecdf(n=9+1,geom=c("bar"),fill="blue",aes(x=masse-5.355556/2,width=5.355556/2),
alpha=.5,colour="blue",binwidth=5.355556)+stat_ecdf(n=9+1,geom=c("line"),fill="blue",
  aes(x=masse-5.355556/2,y=..y..))+stat_ecdf(n=9+1,geom=c("point"),fill="blue",
  aes(x=masse-5.355556/2,y=..y..))+facet_wrap(~espece)+geom_histogram(aes(y=
  5.355556*..density..,fill=..density..),binwidth=5.355556,alpha=.35)
pdf("figure320gggplot.pdf")
print(g+stat_ecdf(n=9+1,geom=c("bar"),fill="blue",aes(x=masse-5.355556/2,width=5.355556/2),
alpha=.5,colour="blue",binwidth=5.355556)+stat_ecdf(n=9+1,geom=c("line"),fill="blue",
aes(x=masse-5.355556/2,y=..y..))+stat_ecdf(n=9+1,geom=c("point"),fill="blue",
aes(x=masse-5.355556/2,y=..y..))+facet_wrap(~espece)+geom_histogram(aes(y=5.355556*..density..,
fill=..density..),binwidth=5.355556,alpha=.35))
dev.off()
g+stat_ecdf(n=9+1,geom=c("bar"),fill="blue",aes(x=masse-5.355556/2,width=5.355556/2),
  alpha=.5,colour="blue")+stat_ecdf(n=9+1,geom=c("line"),fill="blue",aes(x=masse-5.355556/2,
  y=..y..))+stat_ecdf(n=9+1,geom=c("point"),fill="blue",aes(x=masse-5.355556/2,y=..y..))+
  facet_wrap(~espece,scales="free_x")+geom_histogram(aes(y=5.355556*..density..,
  fill=..density..),binwidth=5.355556,alpha=.35)
pdf("figure320hggplot.pdf")
print(g+stat_ecdf(n=9+1,geom=c("bar"),fill="blue",aes(x=masse-5.355556/2,width=5.355556/2),
  alpha=.5,colour="blue")+stat_ecdf(n=9+1,geom=c("line"),fill="blue",aes(x=masse-5.355556/2,
  y=..y..))+stat_ecdf(n=9+1,geom=c("point"),fill="blue",aes(x=masse-5.355556/2,y=..y..))+
  facet_wrap(~espece,scales="free_x")+geom_histogram(aes(y=5.355556*..density..,
  fill=..density..),binwidth=5.355556,alpha=.35))
dev.off()

#page 137
boxplot(Mesures$masse)
title("Bo^ite \`a moustaches de la variable masse")
pdf("figure321.pdf")
boxplot(Mesures$masse)
title("Bo^ite \`a moustaches de la variable masse")
dev.off()

#En plus ggplot
ggplot(Mesures, aes(x="",y=masse)) + geom_boxplot()
pdf("figure321ggplot.pdf")
print(ggplot(Mesures, aes(x="",y=masse)) + geom_boxplot())
dev.off()
#remove label axe x
ggplot(Mesures, aes(x="",y=masse)) + geom_boxplot() + xlab("")
pdf("figure321aggplot.pdf")
print(ggplot(Mesures, aes(x="",y=masse)) + geom_boxplot() + xlab(""))
dev.off()

ggplot(Mesures, aes(x="",y=masse)) + geom_boxplot() + coord_flip() + xlab("")
pdf("figure321bggplot.pdf")
print(ggplot(Mesures, aes(x="",y=masse)) + geom_boxplot() + coord_flip() +
  xlab(""))
dev.off()

ggplot(Mesures, aes(x="", y=masse)) + geom_boxplot(width=.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") +
  xlab("")
pdf("figure321cggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) + geom_boxplot(width=.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white") +
  xlab(""))
dev.off()

ggplot(Mesures, aes(x="", y=masse)) + geom_violin() + geom_boxplot(width=.1,
  fill="black") + stat_summary(fun.y=mean, geom="point", fill="white", shape=21, size=2.5)
pdf("figure321dggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) + geom_violin() +
  geom_boxplot(width=.1, fill="black") + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5))
dev.off()

#Without extreme values
ggplot(Mesures, aes(x="", y=masse)) + geom_violin() + geom_boxplot(width=.1,
  fill="black", outlier.colour=NA) + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)
pdf("figure321eggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) + geom_violin() +
  geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=21, size=2.5))
dev.off()

#Gaussian kernel is the default and very (too) smooth for a finite population
ggplot(Mesures, aes(x="", y=masse)) + geom_violin(kernel="rectangular") +
  geom_boxplot(width=.1, fill="black") + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)
pdf("figure321fggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) + geom_violin(kernel="rectangular") +
  geom_boxplot(width=.1, fill="black") + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5))
dev.off()

#Without extreme values
ggplot(Mesures, aes(x="", y=masse)) + geom_violin(kernel="rectangular") +
  geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=21, size=2.5)
pdf("figure321gggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) + geom_violin(kernel="rectangular") +
        geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
        stat_summary(fun.y=mean, geom="point", fill="white", shape=21, size=2.5))
dev.off()

#page 138
boxplot.stats(Mesures$masse)
boxplot(Mesures$masse~Mesures$espece)
pdf("figure322.pdf")
boxplot(Mesures$masse~Mesures$espece)
dev.off()

#page 139
pdf("figure322color.pdf")
boxplot(Mesures$masse~Mesures$espece,col=rainbow(4))
dev.off()

#En plus lattice par groupe
bwplot(masse~espece,data=Mesures,pch="|")
bwplot(~masse|espece,data=Mesures,pch="|")
pdf("figure322lattice.pdf")
bwplot(masse~espece,data=Mesures,pch="|")
dev.off()
pdf("figure322latticegroupe.pdf")
bwplot(~masse|espece,data=Mesures,pch="|")
dev.off()

#En plus ggplot par groupe
ggplot(Mesures, aes(x=espece,y=masse)) + geom_boxplot()
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x=espece,y=masse)) + geom_boxplot())
dev.off()
ggplot(Mesures, aes(x=espece,y=masse)) + geom_boxplot() + coord_flip()
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x=espece,y=masse)) + geom_boxplot() + coord_flip())
dev.off()
ggplot(Mesures, aes(x=espece,y=masse,fill=espece)) + geom_boxplot()
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x=espece,y=masse,fill=espece)) + geom_boxplot())
dev.off()
ggplot(Mesures, aes(x=espece,y=masse,fill=espece)) + geom_boxplot() +
  coord_flip() + scale_fill_brewer(palette="Set1")
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x=espece,y=masse,fill=espece)) + geom_boxplot() +
  coord_flip() + scale_fill_brewer(palette="Set1"))
dev.off()

ggplot(Mesures, aes(x="", y=masse)) + geom_violin(kernel="rectangular") +
  geom_boxplot(width=.1, fill="black", outlier.colour="black") +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=21,
  size=2.5)+facet_wrap(~espece)
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) + geom_violin(kernel="rectangular") +
  geom_boxplot(width=.1, fill="black", outlier.colour="black") +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=21,
  size=2.5)+facet_wrap(~espece))
dev.off()
ggplot(Mesures, aes(x="", y=masse)) +
  geom_violin(aes(fill=espece),kernel="rectangular",alpha=.2) +
  geom_boxplot(aes(fill=espece),width=.1) + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)+facet_wrap(~espece)
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) +
  geom_violin(aes(fill=espece),kernel="rectangular",alpha=.2) +
  geom_boxplot(aes(fill=espece),width=.1) + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)+facet_wrap(~espece))
dev.off()
ggplot(Mesures, aes(x="", y=masse)) +
  geom_violin(aes(fill=espece),kernel="rectangular",alpha=.2) +
  geom_boxplot(aes(fill=espece),width=.1) + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)+facet_wrap(~espece)+scale_fill_brewer(palette="Set1")
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) +
  geom_violin(aes(fill=espece),kernel="rectangular",alpha=.2) +
  geom_boxplot(aes(fill=espece),width=.1) + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)+facet_wrap(~espece)+scale_fill_brewer(palette="Set1"))
dev.off()
ggplot(Mesures, aes(x="", y=masse)) +
  geom_violin(aes(fill=espece,kernel="rectangular"),alpha=.2) +
  geom_boxplot(aes(fill=espece),width=.1) + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)+facet_wrap(~espece)+scale_fill_brewer(palette="Set2")
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) +
  geom_violin(aes(fill=espece,kernel="rectangular"),alpha=.2) +
  geom_boxplot(aes(fill=espece),width=.1) + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)+facet_wrap(~espece)+scale_fill_brewer(palette="Set2"))
dev.off()
ggplot(Mesures, aes(x="", y=masse)) +
  geom_violin(aes(fill=espece,kernel="rectangular"),alpha=.2) +
  geom_boxplot(aes(fill=espece),width=.1) + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)+facet_wrap(~espece)+scale_fill_brewer(palette="Set3")
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) +
  geom_violin(aes(fill=espece,kernel="rectangular"),alpha=.2) +
  geom_boxplot(aes(fill=espece),width=.1) + stat_summary(fun.y=mean, geom="point",
  fill="white", shape=21, size=2.5)+facet_wrap(~espece)+scale_fill_brewer(palette="Set3"))
dev.off()

#Without extreme values and with Gaussian kernel
ggplot(Mesures, aes(x="", y=masse)) + geom_violin() + geom_boxplot(width=.1,
 fill="black", outlier.colour=NA) + stat_summary(fun.y=mean, geom="point",
 fill="white", shape=21, size=2.5)+facet_wrap(~espece)
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) + geom_violin() +
        geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
        stat_summary(fun.y=mean, geom="point", fill="white", shape=21,
                     size=2.5)+facet_wrap(~espece))
dev.off()
ggplot(Mesures, aes(x="", y=masse)) + geom_violin(aes(fill=espece),alpha=.2) +
  geom_boxplot(aes(fill=espece),width=.1,outlier.color=NA) +
  stat_summary(fun.y=mean, geom="point", fill="white", shape=21,
               size=2.5)+facet_wrap(~espece)+ scale_fill_brewer(palette="Set3")
pdf("figure322ggplot.pdf")
print(ggplot(Mesures, aes(x="", y=masse)) +
        geom_violin(aes(fill=espece),alpha=.2) +
        geom_boxplot(aes(fill=espece),width=.1,outlier.color=NA) +
        stat_summary(fun.y=mean, geom="point", fill="white", shape=21,
                     size=2.5)+facet_wrap(~espece)+ scale_fill_brewer(palette="Set3"))
dev.off()

#page 140
stem(Mesures$masse)

#page 142
hist(Mesures$masse,ylab="Effectif",xlab="Masse",main="Histogramme des masses")
histo<-hist(Mesures$masse,plot=FALSE)
classes<-histo$breaks
classes

#page 143
effectifs<-histo$counts
effectifs
which(histo$density==max(histo$density))
median(Mesures$masse)
quantile(Mesures$masse,0.5,type=6)

#page 144
quantile(Mesures$masse,0.25,type=6)
quantile(Mesures$masse,0.75,type=6)
quantile(Mesures$masse,c(0.25,0.5,0.75),type=6)

#page 145
quantile(Mesures$masse,type=6)

#page 146
options(digits=7)
mean(Mesures$masse)
summary(Mesures$masse)

#page 147
max(Mesures$masse)-min(Mesures$masse)
diff(range(Mesures$masse))
IQR(Mesures$masse,type=6)

#page 149
var(Mesures$masse)
var(Mesures$masse)*length(Mesures$masse)/(length(Mesures$masse)-1)

#page 150
sd(Mesures$masse)

#page 151
mad(Mesures$masse,constant=1)
mad(Mesures$masse,quantile(Mesures$masse,type=1,probs=.5),constant=1)
median(abs(Mesures$masse-quantile(Mesures$masse,type=1,probs=.5)))
mad(Mesures$masse,constant=1,low=TRUE)

#page 152
quantile(abs(Mesures$masse-median(Mesures$masse)),type=1,probs=.5)
mad(Mesures$masse,quantile(Mesures$masse,type=1,probs=.5),constant=1,low=TRUE)
quantile(abs(Mesures$masse-quantile(Mesures$masse,type=1,probs=.5)),type=1,probs
         =.5)

#mads par rapport \`a une autre r\'ef\'erence
mad(Mesures$masse,quantile(Mesures$masse,type=4,probs=.5),constant=1)
mad(Mesures$masse,quantile(Mesures$masse,type=6,probs=.5),constant=1)
mad(Mesures$masse,quantile(Mesures$masse,type=7,probs=.5),constant=1)

#Autre example de calculs \`a partir d'un petit \'echantillon
x <- c(1,2,3,5,7,8)
sort(abs(x - median(x)))
c(mad(x, constant = 1),
  mad(x, constant = 1, low = TRUE),
  mad(x, constant = 1, high = TRUE))
quantile(x,type=1,probs=.5)
quantile(x,type=2,probs=.5)
mad(x,constant=1,low = TRUE)

sort(abs(x-quantile(x,type=1,probs=.5)))
quantile(abs(x-quantile(x,type=1,probs=.5)),type=1,probs=.5)

library(BioStatR)
cvar(Mesures$masse)

#page 154
# Asym\'etrie et aplatissement d'un \'echantillon
if(!("agricolae" %in%
     rownames(installed.packages()))){install.packages("agricolae")}
library(agricolae)
skewness(Mesures$masse)
kurtosis(Mesures$masse)
#Pour retirer la biblioth\`eque agricolae de la m\'emoire de R avant de charger e1071
detach(package:agricolae)
if(!("e1071" %in% rownames(installed.packages()))){install.packages("e1071")}
library(e1071)
# Asym\'etrie et aplatissement d'une s\'erie statistique (=population)
skewness(Mesures$masse,type=1)
kurtosis(Mesures$masse,type=1)
# Asym\'etrie et aplatissement d'un \'echantillon (comme agricolae)
skewness(Mesures$masse,type=2)
kurtosis(Mesures$masse,type=2)
detach(package:e1071)

#Exercice 3.1
#page 164
#1)
Variete<-c(rep(1,4),rep(2,4),rep(3,4))
Variete
Jutosite<-c(4,6,3,5,7,8,7,6,8,6,5,6)
Jutosite
Pommes<-data.frame(Variete,Jutosite)
Pommes

#page 165
#2)
str(Pommes)
class(Pommes$Variete)
#3)
Variete<-factor(Variete)
Pommes<-data.frame(Variete,Jutosite)
rm(Variete)
rm(Jutosite)
str(Pommes)

#page 166
class(Pommes$Variete)
Pommes
#4)
Variete<-factor(c(rep(1,4),rep(2,4),rep(3,4)))
Jutosite<-c(4,6,3,5,7,8,7,6,8,6,5,6)
Pommes<-data.frame(Variete,Jutosite)
str(Pommes)
#5)
Variete<-factor(c(rep(1,4),rep(2,4),rep(3,4)),labels=c("V1","V2","V3"))
Jutosite<-c(4,6,3,5,7,8,7,6,8,6,5,6)
Pommes<-data.frame(Variete,Jutosite)
Pommes

#page 167
str(Pommes)
#6)
Variete<-as.factor(c(rep(1,4),rep(2,4),rep(3,4)))
Jutosite<-c(4,6,3,5,7,8,7,6,8,6,5,6)
Pommes<-data.frame(Variete,Jutosite)
Pommes
str(Pommes)

#page 168
#7)
tapply(Jutosite,Variete,mean)
tapply(Jutosite,Variete,sd)
tapply(Jutosite,Variete,quantile,type=6)
tapply(Jutosite,Variete,summary)

#Exercice 3.2
#page 169
#1)
options(digits=3)
hist(Mesures$masse,breaks=5,plot=FALSE)

#page 170
#2)
hist(Mesures$masse,breaks=c(0,5,10,15,20,50),plot=FALSE)

#page 171
#3)
brk <- c(0,5,10,15,20,50)
table(cut(Mesures$masse, brk))
head(cut(Mesures$masse,brk))
data.frame(table(cut(Mesures$masse, brk)))
#4)
if(!("Hmisc" %in% rownames(installed.packages()))){install.packages("Hmisc")}
library(Hmisc)
brk <- c(0,5,10,15,20,50)
res <- cut2(Mesures$masse, brk)
head(res)

#page 172
table(res)
table(cut2(Mesures$masse, g=10))
table(cut2(Mesures$masse, m=50))

#Exercice 3.3
#1)
library(BioStatR)
head(Mesures$masse)
#head(masse)
#
#page 173
#2)
attach(Mesures)
head(masse)
detach(Mesures)
#head(masse)
#
#Exercice 3.4
options(digits=7)
#1)
head(Europe)
#2)
str(Europe)

#page 174
#3)
class(Europe)
dim(Europe)
#4)
summary(Europe$Duree)

#page 175
histo<-hist(Europe$Duree,xlab="Dur\'ee en heures",ylab="Nombre de pays",
            main="Histogramme de la variable Duree")
histo<-hist(Europe$Duree)
classe<-histo$breaks
classe

#page 176
which(histo$density==max(histo$density))
#5)
sd(Europe$Duree)
cvar(Europe$Duree)
diff(range(Europe$Duree))
#6)
boxplot(Europe$Duree,ylab="Dur\'ee en heures")
points(1,mean(Europe$Duree),pch=1)

#page 177
#7)
pdf(file="boxplot.pdf")
boxplot(Europe$Duree,ylab="Dur\'ee en heures")
points(1,mean(Europe$Duree),pch=1)
dev.off()

#page 178
postscript(file="boxplot.ps")
boxplot(Europe$Duree,ylab="Dur\'ee en heures")
points(1,mean(Europe$Duree),pch=1)
dev.off()

#Probl\`eme 3.1
#1)
Femmes<-c(105,110,112,112,118,119,120,120,125,126,127,128,130,132,133,
          134,135,138,138,138,138,142,145,148,148,150,151,154,154,158)
Femmes

#page 179
Hommes<-c(141,144,146,148,149,150,150,151,153,153,153,154,155,156,156,
          160,160,160,163,164,164,165,166,168,168,170,172,172,176,179)
Hommes
#2)
histo.fem<-hist(Femmes,breaks=c(104,114,124,134,144,154,164,174,184))
effectif.fem<-histo.fem$counts
effectif.fem
sum(effectif.fem)
histo.frm<-hist(Femmes,breaks=c(104,114,124,134,144,154,164,174,184))
frequence.fem<-effectif.fem/sum(effectif.fem)
print(frequence.fem,digits=3)

#page 180
histo.hom<-hist(Hommes,breaks=c(104,114,124,134,144,154,164,174,184))
effectif.hom<-histo.hom$counts
effectif.hom
histo.hom<-hist(Hommes,breaks=c(104,114,124,134,144,154,164,174,184))
frequence.hom<-effectif.hom/sum(effectif.hom)
print(frequence.hom,digits=3)

#page 181
#3)
histo<-hist(Femmes,breaks=c(104,114,124,134,144,154,164,174,184),
            main="Histogramme de la variable taux d'h\'emoglobine pour les
            Femmes",
            xlab="Taux d'h\'emoglobine",ylab="Effectif")

#page 182
histo<-hist(Hommes,breaks=c(104,114,124,134,144,154,164,174,184),
            main="Histogramme de la variable taux d'h\'emoglobine pour les
            Hommes",
            xlab="Taux d'h\'emoglobine",ylab="Effectif")
library(lattice)
Ensemble.df <- make.groups(Femmes,Hommes)
colnames(Ensemble.df) <- c("Taux","Sexe")
histogram(~Taux|Sexe,xlab="Taux d'h\'emoglobine",data=Ensemble.df,
          breaks=c(104,114,124,134,144,154,164,174,184),layout=c(1,2))

#page 183
histogram(~Taux|Sexe,xlab="Taux d'h\'emoglobine",data=Ensemble.df,
          breaks=c(104,114,124,134,144,154,164,174,184))

#page 184
#4)
Ensemble<-c(Femmes,Hommes)
Ensemble
mean(Ensemble)
mean(Femmes)
mean(Hommes)
#5)
histo.ens<-hist(Ensemble,breaks=c(104,114,124,134,144,154,164,174,184))
sum(histo.ens$counts*histo.ens$mids)/length(Ensemble)

#page 185
sum(histo.fem$counts*histo.fem$mids)/length(Femmes)
sum(histo.hom$counts*histo.hom$mids)/length(Hommes)
#6)
quantile(Ensemble,0.50,type=6)
quantile(Femmes,0.50,type=6)
quantile(Hommes,0.50,type=6)
#M^eme r\'esultats avec la fonction median
median(Ensemble)
median(Femmes)
median(Hommes)

#page 186
#7)
IQR(Ensemble,type=6)
IQR(Femmes,type=6)
IQR(Hommes,type=6)
#8)
var(Ensemble)*(length(Ensemble)-1)/length(Ensemble)
var(Femmes)*(length(Femmes)-1)/length(Femmes)

#page 187
var(Hommes)*(length(Hommes)-1)/length(Hommes)
sd(Ensemble)*sqrt((length(Ensemble)-1)/length(Ensemble))
sd(Femmes)*sqrt((length(Femmes)-1)/length(Femmes))
sd(Hommes)*sqrt((length(Hommes)-1)/length(Hommes))
#9)
# Asym\'etrie et aplatissement d'une s\'erie statistique (=population)
if(!("e1071" %in% rownames(installed.packages()))){install.packages("e1071")}
library(e1071)
skewness(Femmes,type=1)

#page 188
kurtosis(Femmes,type=1)









