#' Devuelve el slot Repo de un objeto de clase RepoTimeInt.
#' 
#' \code{getRepo} devuelve el slot \code{Repo} del input 
#' \code{\linkS4class{RepoTimeInt}}.
#' 
#' Esta función devuelve únicamente los datos del slot \code{Repo} del objeto 
#' \code{RepoTimeInt} de entrada como un vector. 
#' 
#' 
#' @param object Objeto de clase \code{\linkS4class{RepoTimeInt}} del que se desea 
#' extraer el slot Repo.
#'
#' @return Un vector de clase \code{\link{character}} de longitud 1.
#'  
#' @examples
#' getRepo(new(Class = 'RepoTimeInt', 'TT12015'))
#' 
#' @include RepoTimeInt-class.R
#' 
#' @export
setGeneric("getRepo", function(object){standardGeneric("getRepo")})

#' @rdname getRepo
#' 
#' @include RepoTimeInt-class.R
#' 
#' 
#' @export
setMethod(
  f = "getRepo",
  signature = c("RepoTimeInt"),
  function(object){ object@Repo }
)

