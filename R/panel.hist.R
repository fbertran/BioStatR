#' Histrogammes
#' 
#' Sert à représenter des histogrammes dans les graphiques matriciels
#' 
#' Cette fonction s'utilise avec la fonctions graphique pairs.
#' 
#' @param x Un vecteur numérique
#' @param \dots Des arguments à transmettre à la fonction qui créé les
#' histogrammes
#' @author Frédéric Bertrand\cr \email{frederic.bertrand@@lecnam.net}\cr
#' \url{https://fbertran.github.io/homepage/}\cr 
#' Maumy-Bertrand\cr \email{myriam.maumy@@ehesp.fr}\cr
#' \url{https://www.ehesp.fr/annuaire/enseignement-recherche/myriam-maumy/}
#' @seealso \code{\link{pairs}}, \code{\link{hist}}
#' @references F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec
#' R, Dunod, 4ème édition, 2023.
#' @keywords univar
#' @examples
#' 
#' data(Mesures5)
#' pairs(Mesures5,diag.panel="panel.hist")
#' 
#' @export panel.hist
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par("usr"))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(xleft=breaks[-nB], ybottom=0, xright=breaks[-1], 
         ytop=y, col="cyan", ...)
}
