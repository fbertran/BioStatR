\name{gg_qqplot}
\alias{gg_qqplot}
\title{
Graphique des quantiles (qqplot) et droite interquartile
}
\description{
Dessine le graphique des quantiles ou \code{\link{qqplot}} et la droite interquartile (passant par le premier et le troisi?me quartile ? la mani?re de la fonction \code{\link{qqline}}) avec la biblioth?que graphique \code{ggplot2}.
}
\usage{
gg_qqplot(df,var,qdist=qnorm,params=list(),qq.line=TRUE,color="red",alpha=.5) 
}
\arguments{
  \item{df}{Un jeu de donn?es (dataframe)}
  \item{var}{Le nom d'une variable de df}
  \item{qdist}{La fonction quantile d'une (famille de) distribution. Par d?faut celle de la famille des lois normales.}
  \item{params}{Une liste de param?tres pour sp?cifier la loi ? utiliser. Par d?faut la loi normale centr?e et r?duite. Les param?tres peuvent ?tre estim?s avec la fonction \code{\link[MASS]{fitdistr}} de la biblioth?que MASS.}
  \item{qq.line}{Une valeur logique. Affiche ou masque la droite interquartile.}
  \item{color}{Le nom d'une couleur. Sp?cifie la couleur ? utiliser pour la droite interquartile.}
  \item{alpha}{Indice de transparence. Sp?cifie la transparence ? utiliser pour repr?senter les valeurs de l'?chantillon.}
}
\value{
\item{ggplot}{Un graphique utilisant la biblioth?que ggplot2. Affiche les valeurs des quartiles th?oriques par lesquels passe la droite ainsi que son ordonn?e ? l'origine et sa pente si la trac? de celle-ci est demand?.}
}
\references{F. Bertrand, M. Maumy-Bertrand, Initiation ? la Statistique avec R, Dunod, 2e, 2014.}
\author{\enc{Fr?d?ric}{Fr\'ed\'eric} Bertrand\cr
\email{frederic.bertrand@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~fbertran/}
\enc{Myriam}{Myriam} Maumy-Bertrand\cr
\email{myriam.maumy@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~mmaumy/}
}
\seealso{\code{\link{qqplot}}, \code{\link{qqline}}}
\examples{
glycine.blanche<-subset(Mesures,subset=(Mesures$espece=="glycine blanche"))
gg_qqplot(glycine.blanche,"taille")

#bonus ajustement avec une autre loi (ici Student (car dist = qt) dont on estime les ddl)
lauriers.roses<-subset(Mesures,subset=(Mesures$espece=="laurier rose"))
shapiro.test(lauriers.roses$taille) 
#pas issu d'une loi normal au risque alpha=5%
gg_qqplot(lauriers.roses,"taille")
gg_qqplot(lauriers.roses,"taille",qq.line=FALSE)
#essayons un qqplot avec une loi de Student
\dontrun{
require(MASS)
params <- as.list(fitdistr(lauriers.roses$taille, "t")$estimate)
#avec la droite
gg_qqplot(lauriers.roses,"taille",qt,params)
#essayons un qqplot avec une loi gamma
params <- as.list(fitdistr(lauriers.roses$taille,"gamma")$estimate)
#avec la droite
gg_qqplot(lauriers.roses,"taille",qgamma,params)
#essayons un qqplot avec une loi du chi-deux
params <- list(df=fitdistr(lauriers.roses$taille,"chi-squared",start=list(df=5),
method="Brent",lower=1,upper=40)$estimate)
#avec la droite
gg_qqplot(lauriers.roses,"taille",qchisq,params)
}
}
\keyword{univar}
