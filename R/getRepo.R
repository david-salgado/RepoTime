#' @name getRepo
#' @aliases getRepo
#' 
#' @title Return the slot Repo
#' 
#' @description \code{getRepo} returns the slot \code{Repo} of an object of 
#' class \linkS4class{RepoTimeInt}. 
#' 
#' @param object Object of class \code{\linkS4class{RepoTimeInt}}.
#'
#' @return A character vector with the time intervals in the repo notation.
#'  
#' @examples
#' getRepo(newRepoTime('TT12015'))
#' 
#' @include RepoTimeInt-class.R
#' 
#' @seealso \code{\link{RepoTimeInt-class}}, \code{\link{getlubriInt}}
#' 
#' @export
setGeneric("getRepo", function(object){standardGeneric("getRepo")})

#' @rdname getRepo
#' 
#' @include RepoTimeInt-class.R
#' 
#' @export
setMethod(
  f = "getRepo",
  signature = c("RepoTimeInt"),
  function(object){ object@Repo }
)

