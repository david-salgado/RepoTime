#' @name newRepoTime
#' @aliases newRepoTime
#' 
#' @title Constructor of objects of class \linkS4class{RepoTimeInt}
#' 
#' @description This constructor takes as argument a character vector with the 
#' time intervals in the repo notation and returns the corresponding 
#' \linkS4class{RepoTimeInt} object.
#' 
#' @param Time Character vector with the time intervals in the repo notation.
#' 
#' @details  The repo notation of time intervals is explained in the details 
#' section of \code{\link{RepoTimeInt-class}}.
#' 
#' @return Object of class \linkS4class{RepoTimeInt}.
#' 
#' @examples
#' x <- newRepoTime(c('MM042015', 'MM052015')) 
#' x
#' str(x) 
#'
#' # It works even with lists instead of vector
#' newRepoTime(list('AA2015', 'TT12012'))
#'
#' @include RepoTimeInt-class.R RepoTimeTolubri.R
#' 
#' @importFrom methods new
#' 
#' @export
newRepoTime <- function(Time){
    
    names(Time) <- NULL
    lubriInt <- RepoTimeTolubri(RepoTime = Time)
    output <- new(Class = 'RepoTimeInt', Repo = as.character(Time), lubriInt = lubriInt)
    return(value = output)
    
}