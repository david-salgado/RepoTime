#' Constructor de objetos de clase RepoTimeInt
#' 
#' Este constructor devuelve un objeto de clase 
#' \code{\linkS4class{RepoTimeInt}} a partir de un vector de períodos temporales
#' según la notación del repositorio de microdatos.
#' 
#' @param Time un vector \code{character} con el período temporal siguiendo la 
#' notación del repositorio de microdatos.
#' 
#' @return Objeto de clase \linkS4class{RepoTimeInt}.
#' 
#' @examples
#' x <- newRepoTime(c('MM042015', 'MM052015')) 
#' x
#' str(x) 
#'
#' @include RepoTimeInt-class.R RepoTimeTolubri.R
#' 
#' @export
    newRepoTime <- function(Time){
    
    lubriInt <- RepoTimeTolubri(Time)
    output <- new(Class = 'RepoTimeInt', Repo = Time, lubriInt = lubriInt)
    return(output)
    
}