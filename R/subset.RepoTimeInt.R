#' @title Extract parts of an object of class \linkS4class{RepoTimeInt}
#' 
#' @description \code{[} extracts parts of an object of class 
#' \linkS4class{RepoTimeInt}. 
#' 
#' @param x Object of class \linkS4class{RepoTimeInt} from which to extract 
#' parts.
#'
#' @param i,j,... indices corresponding to the elements to extract. 
#'       
#' @param drop Included for coherence.
#'
#' @return Object of class \linkS4class{RepoTimeInt}.
#'  
#' @examples
#' Ejemplo <- newRepoTime(paste0('MM', c('03', '04'), '2014'))
#' Ejemplo[1]
#' 
#' @include RepoTimeInt-class.R newRepoTime.R
#' 
#' @export
setMethod(
  f = "[",
  signature = c("RepoTimeInt"),
  function(x, i, j, ..., drop = TRUE){
    
    mc <- match.call()
    newx <- getRepo(object = x)
    names(newx) <- newx
    mc[['x']] <- newx
    output <- eval(expr = mc, envir = parent.frame())
    output <- newRepoTime(Time = output)
    return(eval = output)
    
  }
)