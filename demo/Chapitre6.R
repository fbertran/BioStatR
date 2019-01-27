#' ---
#' title: "Initiation \`a la statistique avec R, code et compl\'ements chapitre 6"
#' author: "Fr\'ed\'eric Bertrand et Myriam Maumy-Bertrand"
#' date: "11 d\'ecembre 2018"
#' ---

#Chapitre 6
#page 260
require(BioStatR)
glycine.blanche<-subset(Mesures,subset=(Mesures$espece=="glycine blanche"))
mean(glycine.blanche$taille)

#page 261
var(glycine.blanche$taille)

#page 262
(var(glycine.blanche$taille))*((length(glycine.blanche$taille)-1)/
  length(glycine.blanche$taille))
glycine.blanche<-subset(Mesures5,subset=(Mesures5$espece=="glycine blanche"))

#page 263
effectif.cumule<-cumsum(table(glycine.blanche$graines))
effectif.cumule
37/54

#page 264
qnorm(0.975)

#page 265
glycine.blanche<-subset(Mesures,subset=(Mesures$espece=="glycine blanche"))
shapiro.test(glycine.blanche$taille)

#page 239
length((glycine.blanche$taille))
qqnorm(glycine.blanche$taille)
qqline(glycine.blanche$taille)
pdf("figch61A.pdf")
qqnorm(glycine.blanche$taille)
qqline(glycine.blanche$taille)
dev.off()

#argument: un dataframe et le nom d'une variable
gg_qqplot(glycine.blanche,"taille")
library(ggplot2)
pdf("figch61B.pdf")
gg_qqplot(glycine.blanche,"taille")
dev.off()

#En plus : autre mani\`ere de construire le diagramme quantile-quantile
#bas\'e sur la loi normale centr\'ee et r\'eduite
ggplot(glycine.blanche, aes(sample = taille)) + stat_qq()
ggplot(glycine.blanche, aes(sample = taille)) + geom_point(stat = "qq")
#ou avec le fonction pr\'ec\'edente et l'option qq.line=FALSE
gg_qqplot(glycine.blanche,"taille",qq.line=FALSE)

#page 267
lauriers.roses<-subset(Mesures,subset=(Mesures$espece=="laurier rose"))
shapiro.test(lauriers.roses$taille) 

#pas issu d'une loi normal au risque alpha=5%
gg_qqplot(lauriers.roses,"taille")

#page 268
#essayons un qqplot avec une autre loi, ici Student (car dist = qt) dont on estime les ddl
if(!("MASS" %in% rownames(installed.packages()))){install.packages("MASS")}
library(MASS)
params <- as.list(fitdistr(lauriers.roses$taille, "t")$estimate)
gg_qqplot(lauriers.roses,"taille",qt,list(df=params$df))

#En plus : autre mani\`ere de construire le diagramme quantile-quantile
#bas\'e sur la loi de student
ggplot(lauriers.roses, aes(sample = taille)) + stat_qq(distribution = stats::qt,
  dparams = list(df=params[[3]]))

#En plus essayons un qqplot avec une loi gamma
params <- as.list(fitdistr(lauriers.roses$taille,"gamma")$estimate)
ggplot(lauriers.roses, aes(sample = taille)) + stat_qq(distribution = stats::qgamma, 
  dparams = params)
#avec la droite
gg_qqplot(lauriers.roses,"taille",qgamma,params)

#essayons un qqplot avec une loi du chi-deux
params <- list(df=fitdistr(lauriers.roses$taille,"chi-squared",start=list(df=5),
  method="Brent",lower=1,upper=40)$estimate)
ggplot(lauriers.roses, aes(sample = taille)) + stat_qq(distribution = qchisq,
  dparams = params)
#avec la droite
gg_qqplot(lauriers.roses,"taille",qchisq,params)


if(!("gridExtra" %in% rownames(installed.packages()))){install.packages("gridExtra")}
library(gridExtra)
params <- as.list(fitdistr(lauriers.roses$taille, "t")$estimate)
p1=gg_qqplot(lauriers.roses,"taille",qt,list(df=params$df))
params <- list(df=fitdistr(lauriers.roses$taille,"chi-squared",start=list(df=5),
  method="Brent",lower=1,upper=40)$estimate)
p2=gg_qqplot(lauriers.roses,"taille",qchisq,params)

pdf("fig61Cggplot")
grid.arrange(p1, p2, nrow = 1)
dev.off()

#En plus : graphique avec les quatre qqplots
p0=gg_qqplot(lauriers.roses,"taille")+ggtitle("qqplot normal")
params <- as.list(fitdistr(lauriers.roses$taille,"gamma")$estimate)
p3=gg_qqplot(lauriers.roses,"taille",qgamma,params)+ggtitle("qqplot gamma")

grid.arrange(p1+ggtitle("qqplot student"), p2+ggtitle("qqplot chi-deux"), p0, p3, nrow=2)

(moyenne<-mean(glycine.blanche$taille))

#page 269
(quantile<-qt(0.975,53))
(ecart.type<-sd(glycine.blanche$taille))
moyenne-quantile*(ecart.type/sqrt(length(glycine.blanche$taille)))
moyenne+quantile*(ecart.type/sqrt(length(glycine.blanche$taille)))
t.test(glycine.blanche$taille)

#page 271
glycine.blanche<-subset(Mesures,subset=(Mesures$espece=="glycine blanche"))
shapiro.test(glycine.blanche$taille)
length(glycine.blanche$taille)

#page 272
(variance<-var(glycine.blanche$taille))
qchisq(0.975,53)
qchisq(0.025,53)
((length(glycine.blanche$taille)-1)*variance)/qchisq(0.975,53)
((length(glycine.blanche$taille)-1)*variance)/qchisq(0.025,53)

#page 273
binom.test(x=5,n=10,p=0.5,alternative=c("two.sided","less","greater"),conf.level=0.95)

#page 274
binom.ci(x=5,n=10,conf.level=0.95,method="exact")
prop.test(x=5,n=10,p=0.5,alternative=c("two.sided","less","greater"),conf.level=0.95)

#page 275
binom.ci(x=5,n=10,conf.level=0.95,method="Wilson")
binom.ci(x=5,n=10,conf.level=0.95,method="Wald")

#page 283
#Exercice 6.1
#1)
toxine<-c(1.2,0.8,0.6,1.1,1.2,0.9,1.5,0.9,1.0)
str(toxine)
mean(toxine)
sd(toxine)
#2)
t.test(toxine)

#page 284
#4)
variance<-var(toxine)
((length(toxine)-1)*variance)/qchisq(0.975,8)
((length(toxine)-1)*variance)/qchisq(0.025,8)
sqrt(((length(toxine)-1)*variance)/qchisq(0.975,8))

#page 285
sqrt(((length(toxine)-1)*variance)/qchisq(0.025,8))

#Exercice 6.3
#page 286
#1)
lambda_n<-(1*11+2*41+3*27+4*16+5*10+6*2+7*3)/110
lambda_n
#2)
echantillon<-rep(0:8,c(0,11,41,27,16,10,2,3,0))
echantillon
poi.ci(echantillon)

#Probl\`eme 6.1
#page 287
library(BioStatR)
#1)
glycine<-subset(Mesures,subset=(Mesures$espece=="glycine blanche"))
#2)
layout(t(1:2))
histo<-hist(glycine$taille,ylab="Nombre de gousses de glycine blanche",
  main="Histogramme de la taille\n d'une gousse de glycine blanche",
  xlab="Taille d'une gousse de glycine blanche en cm")
boxplot(glycine$taille,ylab="Taille d'une gousse de glycine blanche en cm",
  main="Bo^ite \`a moustaches de la taille\n d'une gousse de glycine blanche")

pdf("chap5fig62.pdf")
layout(t(1:2))
histo<-hist(glycine$taille,ylab="Nombre de gousses de glycine blanche",
  main="Histogramme de la taille\n d'une gousse de glycine blanche",
  xlab="Taille d'une gousse de glycine blanche en cm")
boxplot(glycine$taille,ylab="Taille d'une gousse de glycine blanche en cm",
  main="Bo^ite \`a moustaches de la taille\n d'une gousse de glycine blanche")
dev.off()

#page 288
#4)
shapiro.test(glycine$taille)

#page 289
length(glycine$taille)
#5)
classes<-histo$breaks
classes
effectifs<-histo$counts
effectifs
#6)
mean(glycine$taille)

#page 290
sd(glycine$taille)
#7)
t.test(glycine$taille)
#8)
15.67395-13.87050

#page 291
1.80345/2
(8*1.96/((15.67395-13.87050)/2))^2

