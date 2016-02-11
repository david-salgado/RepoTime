#' @title Show an object of class \linkS4class{RepoTimeInt}
#' 
#' @description \code{show} displays only the slot \code{Repo} of its input argument 
#' of class \linkS4class{RepoTimeInt}.
#' 
#' @param object Object of class \linkS4class{RepoTimeInt}.
#'
#' @return Object of class \code{NULL}.
#'
#' @details It is indeed the method \code{\link[methods]{show}} adapted to class 
#' \linkS4class{RepoTimeInt}.
#'      
#' @examples
#' show(newRepoTime('TT12015'))
#' 
#' @include RepoTimeInt-class.R getRepo.R
#' 
#' @export
setMethod(
    f = "show",
    signature = c("RepoTimeInt"),
    function(object){
        
        show(object = getRepo(object = object))
        
        return(value = invisible(x = NULL))
    }
)

