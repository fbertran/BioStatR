#' Histrogammes
#' 
#' Sert à représenter des histogrammes dans les graphiques matriciels
#' 
#' Cette fonction s'utilise avec la fonctions graphique pairs.
#' 
#' @param x Un vecteur numérique
#' @param \dots Des arguments à transmettre à la fonction qui créé les
#' histogrammes
#' @author Frédéric Bertrand\cr \email{frederic.bertrand@@utt.fr}\cr
#' \url{http://www-irma.u-strasbg.fr/~fbertran/}\cr 
#' Maumy-Bertrand\cr \email{myriam.maumy@@utt.fr}\cr
#' \url{http://www-irma.u-strasbg.fr/~mmaumy/}
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
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
