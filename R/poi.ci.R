#' Intervalle de confiance pour le paramètre d'une loi de Poisson
#' 
#' Créé un intervalle de confiance pour le paramètre d'une loi de Poisson.
#' 
#' 
#' @param x Un vecteur de données
#' @param conf.level Niveau de confiance de l'intervalle
#' @return \item{matrix}{Limites des intervalles de confiance demandés.}
#' @author Frédéric Bertrand\cr \email{frederic.bertrand@@utt.fr}\cr
#' \url{http://www-irma.u-strasbg.fr/~fbertran/}\cr 
#' Maumy-Bertrand\cr \email{myriam.maumy@@utt.fr}\cr
#' \url{http://www-irma.u-strasbg.fr/~mmaumy/}
#' @seealso \code{\link{binom.test}}, \code{\link{binom.ci}},
#' \code{\link{poi.ci}}
#' @references F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec
#' R, Dunod, 3ème edition, 2018.
#' @keywords univar
#' @examples
#' 
#' poi.ci(rpois(20,10))
#' 
#' @export poi.ci
poi.ci <- function (x, conf.level = 0.95) 
{
    nn <- length(x)
    LCI <- qchisq((1 - conf.level)/2, 2 * sum(x))/2/nn
    UCI <- qchisq(1 - (1 - conf.level)/2, 2 * (sum(x) + 1))/2/nn
    res <- cbind(mean(x), LCI, UCI)
    ci.prefix <- paste(round(100 * conf.level, 1), "%", sep = "")
    colnames(res) <- c("PointEst", paste(ci.prefix, "LCI"), paste(ci.prefix, 
        "UCI"))
    res
}                 
