#' Método 'Intersect' para la clase RepoTimeInt.
#'
#' \code{Intersect} da la intersección de dos objetos de clase \code{\linkS4class{RepoTimeInt}}
#' y longitud 1 o de los elementos de un objeto de clase \code{\linkS4class{RepoTimeInt}}
#' y longitud mayor o igual a 1.
#'
#'
#' @param x Objeto de clase \code{\linkS4class{RepoTimeInt}}.
#'
#' @param y Objeto de clase \code{\linkS4class{RepoTimeInt}}.
#'
#' @return Objeto de clase \code{\linkS4class{RepoTimeInt}} el resultado de hacer la
#' intersección de los periodos de tiempo.
#'
#' @examples
#' RepoPeriod1 <- newRepoTime('MM012015')
#' RepoPeriod2 <- newRepoTime('MM022015')
#' Intersect(RepoPeriod1,RepoPeriod2)
#'
#' @include RepoTimeInt-class.R
#'
#'
#' @export
setGeneric("Intersect", function(x, y){standardGeneric("Intersect")})

#' @rdname Intersect
#'
#' @include RepoTimeInt-class.R
#'
#' @export
setMethod(
  f = "Intersect",
  signature = c("RepoTimeInt"),
  definition = function(x, y){

    if (missing(y) && length(x@Repo) == 1) return(x)
    if (missing(y) && length(x@Repo) >= 2) {

      output <- Reduce(intersect, x@lubriInt)
      if(!is.na(output)){
        output <- unlist(lubriToRepoTime(output))
        output <- newRepoTime(output)
      }
      return(output)
    }

    if (class(y) == 'RepoTimeInt' && length(x@Repo) == 1 && length(y@Repo) == 1){

      output <- intersect(x@lubriInt[[1]], y@lubriInt[[1]])
      if(!is.na(output)){
        output <- unlist(lubriToRepoTime(output))
        output <- newRepoTime(output)
      }

      return(output)

    }

    stop('[Intersect RepoTimeInt] Los parámetros de la función Intersect deben ser (i)
         bien un objeto de clase RepoTimeInt (ii) bien dos objetos de clase RepoTimeInt de longitud 1.')
  }

)
