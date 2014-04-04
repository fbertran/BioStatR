\name{plotcdf2}
\alias{plotcdf2}
\title{
Représentation bivariée des variables discrètes ou des variables continues groupées en classes.
}
\description{
Cette fonction construit un stéréogramme permettant de juger de l'association entre deux variables discrètes ou groupées en classes.
}
\usage{
plotcdf2(x, y, f, xaxe, yaxe, col=NULL, border=FALSE, Nxy=200, theme="0")
}
\arguments{
  \item{x}{Valeurs observées ou modalités de la première variable discrète}
  \item{y}{Valeurs observées ou modalités de la seconde variable discrète}
  \item{f}{Si \code{f=0} (donc \code{length(f)=0}), \code{x} et \code{y} sont deux séries statistiques. Si \code{length(f)>1}, f est un tableau de fréquences et \code{x} et \code{y} les noms des lignes et des colonnes de \code{f}.}
  \item{xaxe}{Nom de l'axe des abscisses}
  \item{yaxe}{Nom de l'axe des ordonnées}
  \item{col}{Couleur du stéréogramme}
  \item{border}{Le maillage du graphique doit-il être affiché ?}
  \item{Nxy}{Pas du maillage pour chaque axe}
  \item{theme}{Le thème détermine la palette de couleurs utilisées. Il y a quatre choix possibles en couleurs "0", "1", "2", "3" et un en nuances de gris "bw"}
}
\value{Un stéréogramme des deux séries statistiques groupées ou des deux variables discrètes étudiées.}
\references{F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec R, Dunod, 2010.}
\author{\enc{Frédéric}{Fr\'ed\'eric} Bertrand\cr
\email{frederic.bertrand@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~fbertran/}
\enc{Myriam}{Myriam} Maumy-Bertrand\cr
\email{myriam.maumy@math.unistra.fr}\cr
\url{http://www-irma.u-strasbg.fr/~mmaumy/}
}
\examples{
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
}
\keyword{univar}
