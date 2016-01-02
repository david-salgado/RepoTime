#' @name Length
#' @aliases Length
#' 
#' @title Length of an object of class \linkS4class{RepoTimeInt}
#' 
#' @description \code{Length} gives the number of time intervals contained in an
#' object of class \linkS4class{RepoTimeInt}.
#' 
#' @param x Object of class \linkS4class{RepoTimeInt}
#' 
#' @return A non-negative integer.
#'  
#' @examples
#' RepoPeriod1 <- newRepoTime('MM012015')
#' Length(RepoPeriod1)
#' 
#' RepoPeriod2 <- newRepoTime(c('MM012015', 'MM022015'))
#' Length(RepoPeriod2) 
#'  
#' @include RepoTimeInt-class.R getRepo.R
#' 
#' @export
setGeneric("Length", function(x){standardGeneric("Length")})

#' @rdname Length
#' 
#' @include RepoTimeInt-class.R getRepo.R
#' 
#' @export
setMethod(
    f = "Length",
    signature = c("RepoTimeInt"),
    definition = function(x){
        
        output <- length(x = getRepo(object = x))
        return(value = output)
    }
    
)