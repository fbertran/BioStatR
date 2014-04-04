pkgname <- "BioStatR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('BioStatR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("Europe")
### * Europe

flush(stderr()); flush(stdout())

### Name: Europe
### Title: Durées de travail en Europe
### Aliases: Europe
### Keywords: datasets

### ** Examples

data(Europe)



cleanEx()
nameEx("Extrait_Taille")
### * Extrait_Taille

flush(stderr()); flush(stdout())

### Name: Extrait_Taille
### Title: Mesures de fruits d'arbustes
### Aliases: Extrait_Taille
### Keywords: datasets

### ** Examples

data(Extrait_Taille)



cleanEx()
nameEx("Mesures")
### * Mesures

flush(stderr()); flush(stdout())

### Name: Mesures
### Title: Mesures de fruits d'arbustes
### Aliases: Mesures
### Keywords: datasets

### ** Examples

data(Mesures)



cleanEx()
nameEx("Mesures5")
### * Mesures5

flush(stderr()); flush(stdout())

### Name: Mesures5
### Title: Mesures de fruits d'arbustes
### Aliases: Mesures5
### Keywords: datasets

### ** Examples

data(Mesures5)



cleanEx()
nameEx("Quetelet")
### * Quetelet

flush(stderr()); flush(stdout())

### Name: Quetelet
### Title: Indices de Quetelet
### Aliases: Quetelet
### Keywords: datasets

### ** Examples

data(Quetelet)



cleanEx()
nameEx("binom.ci")
### * binom.ci

flush(stderr()); flush(stdout())

### Name: binom.ci
### Title: Intervalles de confiance pour une proportion
### Aliases: binom.ci
### Keywords: univar

### ** Examples

binom.ci(5,10,method="all")



cleanEx()
nameEx("cvar")
### * cvar

flush(stderr()); flush(stdout())

### Name: cvar
### Title: Coefficient de variation
### Aliases: cvar
### Keywords: univar

### ** Examples

data(Europe)
cvar(Europe[,2])



cleanEx()
nameEx("eta2")
### * eta2

flush(stderr()); flush(stdout())

### Name: eta2
### Title: Calcul du rapport de corrélation eta carré
### Aliases: eta2
### Keywords: univar

### ** Examples

eta2(Mesures5$taille,Mesures5$espece)



cleanEx()
nameEx("gg_qqplot")
### * gg_qqplot

flush(stderr()); flush(stdout())

### Name: gg_qqplot
### Title: Graphique des quantiles (qqplot) et droite interquartile
### Aliases: gg_qqplot
### Keywords: univar

### ** Examples

glycine.blanche<-subset(Mesures,subset=(Mesures$espece=="glycine blanche"))
gg_qqplot(glycine.blanche,"taille")

#bonus ajustement avec une autre loi (ici Student (car dist = qt) dont on estime les ddl)
lauriers.roses<-subset(Mesures,subset=(Mesures$espece=="laurier rose"))
shapiro.test(lauriers.roses$taille) 
#pas issu d'une loi normal au risque alpha=5%
gg_qqplot(lauriers.roses,"taille")
gg_qqplot(lauriers.roses,"taille",qq.line=FALSE)
#essayons un qqplot avec une loi de Student
## Not run: 
##D require(MASS)
##D params <- as.list(fitdistr(lauriers.roses$taille, "t")$estimate)
##D #avec la droite
##D gg_qqplot(lauriers.roses,"taille",qt,params)
##D #essayons un qqplot avec une loi gamma
##D params <- as.list(fitdistr(lauriers.roses$taille,"gamma")$estimate)
##D #avec la droite
##D gg_qqplot(lauriers.roses,"taille",qgamma,params)
##D #essayons un qqplot avec une loi du chi-deux
##D params <- list(df=fitdistr(lauriers.roses$taille,"chi-squared",start=list(df=5),method="Brent",lower=1,upper=40)$estimate)
##D #avec la droite
##D gg_qqplot(lauriers.roses,"taille",qchisq,params)
## End(Not run)



cleanEx()
nameEx("panel.hist")
### * panel.hist

flush(stderr()); flush(stdout())

### Name: panel.hist
### Title: Histrogammes
### Aliases: panel.hist
### Keywords: univar

### ** Examples

data(Mesures5)
pairs(Mesures5,diag.panel="panel.hist")



cleanEx()
nameEx("plotcdf2")
### * plotcdf2

flush(stderr()); flush(stdout())

### Name: plotcdf2
### Title: Représentation bivariée des variables discrètes ou des variables
###   continues groupées en classes.
### Aliases: plotcdf2
### Keywords: univar

### ** Examples

xx=c(1.83,1.72,1.65,1.70,2.05,1.92,1.85,1.70,1.75,1.9)
yy=c(75,70,70,60,90,92,75,68,71,87)
plotcdf2(xx,yy,f=0,"taille en m","poids en kg")

xx=seq(2,12)
yy=seq(1,6)                 
p=c(1/36,0,0,0,0,0,
2/36,0,0,0,0,0,
2/36,1/36,0,0,0,0,
2/36,2/36,0,0,0,0,
2/36,2/36,1/36,0,0,0,
2/36,2/36,2/36,0,0,0,
0,2/36,2/36,1/36,0,0,
0,0,2/36,2/36,0,0,
0,0,0,2/36,1/36,0,
0,0,0,0,2/36,0,
0,0,0,0,0,1/36)
p=matrix(p,byrow=TRUE,ncol=6)
plotcdf2(xx,yy,p,"somme des des","valeur du plus petit")



cleanEx()
nameEx("poi.ci")
### * poi.ci

flush(stderr()); flush(stdout())

### Name: poi.ci
### Title: Intervalle de confiance pour le paramètre d'une loi de Poisson
### Aliases: poi.ci
### Keywords: univar

### ** Examples

poi.ci(rpois(20,10))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
