\name{panel.hist}
\alias{panel.hist}
\title{Histrogammes}
\description{Sert à représenter des histogrammes dans les graphiques matriciels}
\usage{
panel.hist(x, ...)
}
\arguments{
  \item{x}{Un vecteur numérique}
  \item{\dots}{Des arguments à transmettre à la fonction qui créé les histogrammes}
}
\details{
Cette fonction s'utilise avec la fonctions graphique pairs.
}
\references{F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec R, Dunod, 2010.}
\author{\enc{Frédéric}{Fr\'ed\'eric} Bertrand\cr
\email{frederic.bertrand@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~fbertran/}
\enc{Myriam}{Myriam} Maumy-Bertrand\cr
\email{myriam.maumy@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~mmaumy/}
}
\seealso{\code{\link{pairs}}, \code{\link{hist}}}
\examples{
data(Mesures5)
pairs(Mesures5,diag.panel="panel.hist")
}
\keyword{univar}
