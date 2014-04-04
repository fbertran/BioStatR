\name{cvar}
\alias{cvar}
\title{Coefficient de variation}
\description{Calcule coefficent de variation d'une série statistique}
\usage{
cvar(x)
}
\arguments{
  \item{x}{Un vecteur numérique}
}
\details{
Le coefficient de variation est égal à l'écart-type corrigé divisé par la moyenne. Il est exprimé en pourcents.
}
\value{
\item{num}{Valeur du coefficient de variation exprimé en pourcents}
}
\references{F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec R, Dunod, 2010.}
\author{\enc{Frédéric}{Fr\'ed\'eric} Bertrand\cr
\email{frederic.bertrand@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~fbertran/}
\enc{Myriam}{Myriam} Maumy-Bertrand\cr
\email{myriam.maumy@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~mmaumy/}
}
\seealso{\code{\link{mean}}, \code{\link{sd}}}
\examples{
data(Europe)
cvar(Europe[,2])
}
\keyword{univar}
