#' Método 'Union' para la clase RepoTimeInt.
#'
#' \code{Union} suma dos objetos de clase \code{\linkS4class{RepoTimeInt}} y longitud 1
#' o los elementos de un objeto de clase \code{\linkS4class{RepoTimeInt}} y longitud
#' mayor o igual a 1.
#'
#'
#' @param x Objeto de clase \code{\linkS4class{RepoTimeInt}}.
#'
#' @param y Objeto de clase \code{\linkS4class{RepoTimeInt}}.
#'
#' @return Objeto de clase \code{\linkS4class{RepoTimeInt}} el resultado de sumar
#' los periodos de tiempo.
#'
#' @examples
#' RepoPeriod1 <- newRepoTime('TT12015')
#' RepoPeriod2 <- newRepoTime('TT22015')
#' Union(RepoPeriod1,RepoPeriod2)
#'
#' @include RepoTimeInt-class.R
#'
#' @export
setGeneric("Union", function(x, y){standardGeneric("Union")})

#' @rdname Union
#'
#' @include RepoTimeInt-class.R
#'
#'
#' @export
setMethod(
  f = "Union",
  signature = c("RepoTimeInt"),
  definition = function(x, y){

    if (missing(y) && length(x@Repo) == 1) return(x)
    if (missing(y) && length(x@Repo) >= 2) {

      output <- Reduce(lubridate::union, x@lubriInt)
      output <- unlist(lubriToRepoTime(output))
      output <- newRepoTime(output)
      return(output)
    }

    if (class(y) == 'RepoTimeInt' && length(x@Repo) == 1 && length(y@Repo) == 1){

      output <- lubridate::union(x@lubriInt[[1]], y@lubriInt[[1]])
      output <- unlist(lubriToRepoTime(output))
      output <- newRepoTime(output)
      return(output)

    }

    stop('[Union RepoTimeInt] Los parámetros de la función Union deben ser (i)
         bien un objeto de clase RepoTimeInt (ii) bien dos objetos de clase RepoTimeInt de longitud 1.')
  }

)
