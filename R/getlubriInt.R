#' @title Return the slot lubriInt
#' 
#' @description \code{getlubriInt} returns the slot \code{lubriInt} of an object
#'  of class \linkS4class{RepoTimeInt}.
#' 
#' @param object Object of class \linkS4class{RepoTimeInt}.
#'
#' @return A named list of objects of class Interval from package lubridate.
#'  
#' @examples
#' getlubriInt(newRepoTime('TT12015'))
#' 
#' @include RepoTimeInt-class.R
#' 
#' @export
setGeneric("getlubriInt", function(object){standardGeneric("getlubriInt")})

#' @rdname getlubriInt
#' 
#' @include RepoTimeInt-class.R
#' 
#' @export
setMethod(
  f = "getlubriInt",
  signature = c("RepoTimeInt"),
  function(object){ object@lubriInt }
)

