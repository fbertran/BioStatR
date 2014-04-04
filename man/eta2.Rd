\name{eta2}
\alias{eta2}
\title{
Calcul du rapport de corrélation eta carré}
\description{
Cette fonction calcule le rapport de corrélation \eqn{\eta^2} qui est une mesure d'association importante entre une variable quantitative et une variable qualitative. 
}
\usage{
eta2(x, y)
}
\arguments{
  \item{x}{Un vecteur associé à la variable quantitative}
  \item{y}{Un facteur associé à la variable qualitative}
}
\value{
\item{num}{La valeur du rapport de corrélation empirique}
}
\references{F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec R, Dunod, 2010.}
\author{\enc{Frédéric}{Fr\'ed\'eric} Bertrand\cr
\email{frederic.bertrand@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~fbertran/}
\enc{Myriam}{Myriam} Maumy-Bertrand\cr
\email{myriam.maumy@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~mmaumy/}
}
\examples{
eta2(Mesures5$taille,Mesures5$espece)
}
\keyword{univar}
