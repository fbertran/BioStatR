\name{binom.ci}
\alias{binom.ci}
\title{Intervalles de confiance pour une proportion}
\description{
Cette fonction permet de calculer plusieurs types d'intervalles de confiance pour une proportion.
}
\usage{
binom.ci(x, n, conf.level = 0.95, method = c("Wilson", "exact", "Wald", "all"))
}
\arguments{
  \item{x}{Nombre de succès}
  \item{n}{Nombre d'essais}
  \item{conf.level}{Niveau de confiance recherché pour l'intervalle}
  \item{method}{Type d'intervalle de confiance à calculer : intervalle de "Wilson", intervalle "exact" de Clopper-Pearson, intervalle asymptotique de "Wald" ou tous les trois "all"}
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
binom.ci(5,10,method="all")
}
\keyword{univar}
