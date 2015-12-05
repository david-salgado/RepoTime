#' Extrae partes de un objeto de clase RepoTimeInt.
#' 
#' \code{[} extrae o reemplaza partes de un objeto de clase 
#' \linkS4class{RepoTimeInt}.
#' 
#' Se trata del método \code{[} para la clase \linkS4class{RepoTimeInt}. Este 
#' método obtiene subconjuntos del objeto de clase \code{RepoTimeInt} 
#' especificado como entrada en base a su slot \code{Repo}. Devuelve, por tanto,
#'  un objeto de la misma clase \code{RepoTimeInt} del que se extrae un 
#'  determinado subconjunto del slot \code{Repo}.
#' 
#' 
#' @param x objeto de clase \linkS4class{RepoTimeInt} del que se van a extraer 
#' los elementos.
#'
#' @param i,j, ... índices correspondientes a los elementos a extraer. 
#' Los índices son \code{vectores} de clase \code{numeric} o de clase 
#' \code{character} o de clase \code{\link{missing}} o \code{\link{NULL}}. Los 
#' valores \code{numeric} son forzados a \code{integer} mediante
#' \code{\link{as.integer}} (y por tanto, truncados hacia cero). Los vectores de
#'  clase \code{character} corresponderán a los nombres de los objetos (o para 
#'  matrices/arrays, los dimnames).
#'       
#' @param drop Incluido por coherencia.
#'
#' @return Objeto de clase \linkS4class{RepoTimeInt}, que consiste en un 
#' subconjunto del objeto \code{RepoTimeInt} de entrada.
#'  
#' @examples
#' Ejemplo <- newRepoTime(paste0('MM', c('03', '04'), '2014'))
#' Ejemplo[1]
#' 
#' @include RepoTimeInt-class.R newRepoTime.R
#' 
#' @import data.table
#' 
#' @export
setMethod(
  f = "[",
  signature = c("RepoTimeInt"),
  function(x, i, j, ..., drop = TRUE){
    
    mc <- match.call()
    mc[['x']] <- x@Repo
    output <- eval(mc, envir = parent.frame())
    output <- newRepoTime(output)
    return(output)
    
  }
)