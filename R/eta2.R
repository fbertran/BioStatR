#' Calcul du rapport de corrélation eta carré
#' 
#' Cette fonction calcule le rapport de corrélation \eqn{\eta^2} qui est une
#' mesure d'association importante entre une variable quantitative et une
#' variable qualitative.
#' 
#' 
#' @param x Un vecteur associé à la variable quantitative
#' @param y Un facteur associé à la variable qualitative
#' @return \item{num}{La valeur du rapport de corrélation empirique}
#' @author Frédéric Bertrand\cr \email{frederic.bertrand@@lecnam.net}\cr
#' \url{https://fbertran.github.io/homepage/}\cr 
#' Maumy-Bertrand\cr \email{myriam.maumy@@ehesp.fr}\cr
#' \url{https://www.ehesp.fr/annuaire/enseignement-recherche/myriam-maumy/}
#' @references F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec
#' R, Dunod, 4ème édition, 2023.
#' @keywords univar
#' @examples
#' 
#' eta2(Mesures5$taille,Mesures5$espece)
#' 
#' @export eta2
eta2 <- function(x,y) {
  if (!is.factor(y)) {
    stop("`y` must be a factor (qualitative variable) for eta2().", call. = FALSE)
  }
  return(summary(lm(as.formula(x~y)))$r.squared)
}
