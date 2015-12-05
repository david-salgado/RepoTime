#' Devuelve el slot lubriInt de un objeto de clase RepoTimeInt.
#' 
#' \code{getlubriInt} devuelve el slot \code{lubriInt} del input 
#' \code{\linkS4class{RepoTimeInt}}.
#' 
#' Esta función devuelve únicamente los datos del slot \code{lubriInt} del objeto 
#' \code{RepoTimeInt} de entrada como un vector. 
#' 
#' 
#' @param object Objeto de clase \code{\linkS4class{RepoTimeInt}} del que se desea 
#' extraer el slot Repo.
#'
#' @return Una lista de objetos de clase \code{\link{lubridate::Interval}}.
#'  
#' @examples
#' getlubriInt(new(Class = 'RepoTimeInt', 'TT12015'))
#' 
#' @include RepoTimeInt-class.R
#' 
#' @export
setGeneric("getlubriInt", function(object){standardGeneric("getlubriInt")})

#' @rdname getlubriInt
#' 
#' @include RepoTimeInt-class.R
#' 
#' 
#' @export
setMethod(
  f = "getlubriInt",
  signature = c("RepoTimeInt"),
  function(object){ object@lubriInt }
)

