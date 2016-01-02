#' @name Intersect
#' @aliases Intersect
#' 
#' @title Intersect of two objects of class \linkS4class{RepoTimeInt}
#'
#' @description \code{Intersect} provides the intersection between two objects 
#' of class \linkS4class{RepoTimeInt} and length 1 or of all elements of an 
#' object of class \linkS4class{RepoTimeInt} and length greater than 1.
#'
#' @param x Object of class \linkS4class{RepoTimeInt}.
#'
#' @param y Object of class \linkS4class{RepoTimeInt}.
#'
#' @return Object of class \linkS4class{RepoTimeInt} resulting from intersecting
#' the slot(s) \code{lubriInt} of the argument(s).
#' 
#' @details If argument \code{y} is missing, \code{intersect} is applied 
#' iteratively on every successive component of the list of 
#' \code{\link[lubridate]{Interval-class}} objects. If argument \code{y} is 
#' not missing, the length of both arguments must be 1. 
#' 
#' @examples
#' RepoPeriod1 <- newRepoTime('MM012015')
#' RepoPeriod2 <- newRepoTime('MM022015')
#' Intersect(RepoPeriod1, RepoPeriod2)
#'
#' RepoPeriod1 <- newRepoTime('MM012015')
#' RepoPeriod2 <- newRepoTime('AA2015')
#' Intersect(RepoPeriod1, RepoPeriod2)
#'
#' @include RepoTimeInt-class.R
#'
#'
#' @export
setGeneric("Intersect", function(x, y){standardGeneric("Intersect")})

#' @rdname Intersect
#'
#' @include RepoTimeInt-class.R lubriToRepoTime.R newRepoTime.R getlubriInt.R
#'
#' @importFrom lubridate intersect
#'
#' @export
setMethod(
  f = "Intersect",
  signature = c("RepoTimeInt"),
  definition = function(x, y){

    if (missing(x = y) && length(x = x@Repo) == 1) return(value = x)
    if (missing(x = y) && length(x = x@Repo) >= 2) {

      output <- Reduce(f = intersect, x = x@lubriInt)
      if(!is.na(x = output)){
        output <- unlist(x = lubriToRepoTime(lubriInterval = output))
        output <- newRepoTime(Time = output)
      }
      return(value = output)
    }

    if (class(x = y) == 'RepoTimeInt' && 
        length(x = x@Repo) == 1 && 
        length(x = y@Repo) == 1){

      output <- intersect(x = getlubriInt(x)[[1]], y = getlubriInt(y)[[1]])
      if (length(x = output) == 0) {
          output <- newRepoTime(Time = character(0))
          return(value = output)        
      }
      if(!is.na(x = output)){
        output <- unlist(x = lubriToRepoTime(lubriInterval = output))
        output <- newRepoTime(Time = output)
      }

      return(value = output)

    }

    stop('[RepoTimeInt::Intersect] Arguments of Intersect must be either (i) an
          object of class RepoTimeInt or (ii) two objects of class RepoTimeInt 
          and length 1.')
  }

)
