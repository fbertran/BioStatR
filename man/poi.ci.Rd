\name{poi.ci}
\alias{poi.ci}
\title{
Intervalle de confiance pour le paramètre d'une loi de Poisson
}
\description{
Créé un intervalle de confiance pour le paramètre d'une loi de Poisson.
}
\usage{
poi.ci(x, conf.level = 0.95)
}
\arguments{
  \item{x}{Un vecteur de données}
  \item{conf.level}{Niveau de confiance de l'intervalle}
}
\value{
\item{matrix}{Limites des intervalles de confiance demandés.}
}
\references{F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec R, Dunod, 2010.}
\author{\enc{Frédéric}{Fr\'ed\'eric} Bertrand\cr
\email{frederic.bertrand@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~fbertran/}
\enc{Myriam}{Myriam} Maumy-Bertrand\cr
\email{myriam.maumy@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~mmaumy/}
}
\seealso{\code{\link{binom.test}}, \code{\link{binom.ci}}, \code{\link{poi.ci}}}
\examples{
poi.ci(rpois(20,10))
}
\keyword{univar}
