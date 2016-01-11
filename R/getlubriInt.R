#' @name getlubriInt
#' @aliases getlubriInt
#' 
#' @title Return the slot lubriInt
#' 
#' @description \code{getlubriInt} returns the slot \code{lubriInt} of an object
#'  of class \linkS4class{RepoTimeInt}.
#' 
#' @param object Object of class \linkS4class{RepoTimeInt}.
#'
#' @return A named list of objects of class \linkS4class{Interval} from 
#' \link[lubridate]{lubridate-package}.
#'  
#' @examples
#' getlubriInt(newRepoTime('TT12015'))
#' 
#' @include RepoTimeInt-class.R
#' 
#' @seealso \code{\linkS4class{RepoTimeInt}}, \code{\link{getRepo}}
#' 
#' @name getlubriInt
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

