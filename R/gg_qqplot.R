#' Graphique des quantiles (qqplot) et droite interquartile
#' 
#' Dessine le graphique des quantiles ou \code{\link{qqplot}} et la droite
#' interquartile (passant par le premier et le troisième quartile à la manière
#' de la fonction \code{\link{qqline}}) avec la bibliothèque graphique
#' \code{ggplot2}.
#' 
#' 
#' @param df Un jeu de données (dataframe)
#' @param var Le nom d'une variable de df
#' @param qdist La fonction quantile d'une (famille de) distribution. Par
#' défaut celle de la famille des lois normales.
#' @param params Une liste de paramètres pour spécifier la loi à utiliser. Par
#' défaut la loi normale centrée et réduite. Les paramètres peuvent être
#' estimés avec la fonction \code{\link[MASS]{fitdistr}} de la bibliothèque
#' MASS.
#' @param qq.line Une valeur logique. Affiche ou masque la droite
#' interquartile.
#' @param color Le nom d'une couleur. Spécifie la couleur à utiliser pour la
#' droite interquartile.
#' @param alpha Indice de transparence. Spécifie la transparence à utiliser
#' pour représenter les valeurs de l'échantillon.
#' @return \item{ggplot}{Un graphique utilisant la bibliothèque ggplot2.
#' Affiche les valeurs des quartiles théoriques par lesquels passe la droite
#' ainsi que son ordonnée à l'origine et sa pente si le tracé de celle-ci est
#' demandé.}
#' @author Frédéric Bertrand\cr \email{frederic.bertrand@@math.unistra.fr}\cr
#' \url{http://www-irma.u-strasbg.fr/~fbertran/}\cr 
#' Maumy-Bertrand\cr \email{myriam.maumy@@math.unistra.fr}\cr
#' \url{http://www-irma.u-strasbg.fr/~mmaumy/}
#' @seealso \code{\link{qqplot}}, \code{\link{qqline}}
#' @references F. Bertrand, M. Maumy-Bertrand, Initiation à la Statistique avec
#' R, Dunod, 3e, 2018.
#' @keywords univar
#' @examples
#' 
#' glycine.blanche<-subset(Mesures,subset=(Mesures$espece=="glycine blanche"))
#' gg_qqplot(glycine.blanche,"taille")
#' 
#' #bonus ajustement avec une autre loi (ici Student (car dist = qt) dont on estime les ddl)
#' lauriers.roses<-subset(Mesures,subset=(Mesures$espece=="laurier rose"))
#' shapiro.test(lauriers.roses$taille) 
#' #pas issu d'une loi normale au risque alpha=5%
#' gg_qqplot(lauriers.roses,"taille")
#' gg_qqplot(lauriers.roses,"taille",qq.line=FALSE)
#' #essayons un qqplot avec une loi de Student
#' \dontrun{
#' require(MASS)
#' params <- as.list(fitdistr(lauriers.roses$taille, "t")$estimate)
#' #avec la droite
#' gg_qqplot(lauriers.roses,"taille",qt,params)
#' #essayons un qqplot avec une loi gamma
#' params <- as.list(fitdistr(lauriers.roses$taille,"gamma")$estimate)
#' #avec la droite
#' gg_qqplot(lauriers.roses,"taille",qgamma,params)
#' #essayons un qqplot avec une loi du chi-deux
#' params <- list(df=fitdistr(lauriers.roses$taille,"chi-squared",start=list(df=5),
#' method="Brent",lower=1,upper=40)$estimate)
#' #avec la droite
#' gg_qqplot(lauriers.roses,"taille",qchisq,params)
#' }
#' 
#' @export gg_qqplot
gg_qqplot <- function(df,var,qdist=qnorm,params=list(),qq.line=TRUE,color="red",alpha=.5) 
{
  requireNamespace("ggplot2")
  force(params)
  y <- quantile((df[var])[!is.na(df[var])], c(0.25, 0.75))
  mf <- names(formals(qdist))
  m <- match(names(formals(qdist)), names(params), 0L)
  uparams <- params[m]
  x <- do.call("qdist",c(list(p=c(0.25, 0.75)),uparams))
  if(qq.line){
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
    }
  p <- ggplot2::ggplot(df, aes_string(sample=var)) + ggplot2::stat_qq(alpha = alpha,distribution=qdist,dparams=params)
  if(qq.line){
    p <- p + ggplot2::geom_abline(slope = slope, intercept = int, color=color)  
  cat(paste(c("1st quartile : ",x[1],"\n3rd quartile : ",x[2],"\nIntercept : ",int,"\nSlope : ",slope,"\n"),sep=""))
    }
  return(p)
}
