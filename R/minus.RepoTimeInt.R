#' @title Subtract of a \linkS4class{Period} class object to a 
#' \linkS4class{RepoTimeInt} class object
#'
#' @description \code{-} subtracts an object of class \linkS4class{Period} 
#' of \link[lubridate]{lubridate-package} to an object of class 
#' \linkS4class{RepoTimeInt}.
#'
#' @details This method overloads the operator \link{-} and builds an object of
#' class \linkS4class{RepoTimeInt}.
#'
#' @param e1 Object of class \linkS4class{RepoTimeInt}.
#'
#' @param e2 Object of class \linkS4class{Period}.
#'
#' @return Object of class \linkS4class{RepoTimeInt} resulting from subtracting the
#' time period \code{e2} to the initial time interval \code{e1}.
#'
#' @examples
#' RepoPeriod <- newRepoTime('MM022015')
#' RepoPeriod - months(1)
#' RepoPeriod - lubridate::years(1)
#' RepoPeriod - lubridate::weeks(2)
#'
#' @include RepoTimeInt-class.R
#'
#' @export
setMethod(
    f = "-",
    signature = c("RepoTimeInt", "Period"),
    definition = function(e1, e2){
      
      e2 <- -e2
      output <- e1 + e2

      
    return(value = output)

    }
)
