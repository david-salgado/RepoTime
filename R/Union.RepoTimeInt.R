#' @title Union of two objects of class \linkS4class{RepoTimeInt}
#'
#' @description \code{Union} takes two objects of class 
#' \linkS4class{RepoTimeInt} and produces a new object of this class with the 
#' slot \code{lubriInt} resulting from applying the set operation 
#' \link[base]{union} to their corresponding slots \code{lubriInt}.
#'
#' @param x Object of class \linkS4class{RepoTimeInt}.
#'
#' @param y Object of class \linkS4class{RepoTimeInt}.
#'
#' @return Object of class \linkS4class{RepoTimeInt}.
#'
#' @details If argument \code{y} is missing, \code{union} is applied iteratively
#' on every successive component of the list of 
#' \code{\link[lubridate]{Interval-class}} objects. If argument \code{y} is 
#' not missing, the length of both arguments must be 1. 
#' 
#' It is important to remind that \code{union} "fills in" the time gap between 
#' their time interval arguments, if ever. It includes intervening time 
#' intervals between the initial and final input time intervals.
#' 
#' @name Union
#' @aliases Union
#' 
#'
#' @examples
#' RepoPeriod1 <- newRepoTime('TT12015')
#' RepoPeriod2 <- newRepoTime('TT22015')
#' Union(RepoPeriod1, RepoPeriod2)
#'
#' Months <- newRepoTime(paste0('MM', 10:12, '2014'))
#' Union(Months)
#' 
#' @include RepoTimeInt-class.R
#'
#' @export
setGeneric("Union", function(x, y){standardGeneric("Union")})

#' @rdname Union
#'
#' @include RepoTimeInt-class.R lubriToRepoTime.R newRepoTime.R getlubriInt.R
#' 
#' @export
setMethod(
  f = "Union",
  signature = c("RepoTimeInt"),
  definition = function(x, y){

    if (missing(x = y) && length(x = x@Repo) == 1) return(value = x)
    if (missing(x = y) && length(x = x@Repo) >= 2) {

      output <- Reduce(f = lubridate::union, x = getlubriInt(object = x))
      output <- unlist(x = lubriToRepoTime(lubriInterval = output))

      output <- newRepoTime(Time = output)
      return(value = output)
    }

    if (class(x = y) == 'RepoTimeInt' && 
        length(x = x@Repo) == 1 && 
        length(x = y@Repo) == 1){

      output <-  lubridate::union(x = x@lubriInt[[1]], y = y@lubriInt[[1]])
      output <- unlist(x = lubriToRepoTime(lubriInterval = output))
      output <- newRepoTime(Time = output)
      return(value = output)

    }

    stop('[RepoTime::Union] Arguments of Union must be (i) either an object of
         class RepoTimeInt or (ii) two objects of class RepoTimeInt and length 1
         .')
  }

)
