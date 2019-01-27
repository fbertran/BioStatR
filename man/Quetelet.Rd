\name{Quetelet}
\alias{Quetelet}
\docType{data}
\title{Indices de Quetelet}
\description{
Ce jeu de données contient des mesures de masse et de taille pour permettre le calcul de l'indice de masse corporelle (aussi dit de Quetelet).
}
\usage{Quetelet}
\format{
  Un data frame avec 66 observations de 3 variables.
  \describe{
    \item{\code{sexe}}{un facteur donnant le sexe de l'individu}
    \item{\code{poids}}{le poids de l'individu}
    \item{\code{taille}}{la hauteur de l'individu}
  }
}
\details{Le poids est exprimée en kg et la hauteur en cm}
\references{F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec R, Dunod, 3ème edition, 2018.}
\examples{
data(Quetelet)
}
\keyword{datasets}
