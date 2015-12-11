#' @title Method for the S4 class \linkS4class{RepoTimeInt}
#' 
#' @description \code{Seq} generates a list of objects of class 
#' \linkS4class{RepoTimeInt} ranging from the initial and final time interval 
#' arguments.
#' 
#' @param x Object of class \linkS4class{RepoTimeInt} with the initial time 
#' interval.
#' 
#' @param y Object of class \linkS4class{RepoTimeInt} with the final time 
#' interval.
#'  
#' @return A list of objects of class \linkS4class{RepoTimeInt} ranging from
#' the initial time interval until the final time interval.
#'  
#' @examples
#' RepoPeriod1 <- newRepoTime('MM012015')
#' RepoPeriod2 <- newRepoTime('MM022016')
#' Seq(RepoPeriod1, RepoPeriod2)
#'  
#' @include RepoTimeInt-class.R
#' 
#' @importFrom lubridate duration as.interval
#' 
#' @export
setGeneric("Seq", function(x, y){standardGeneric("Seq")})

#' @rdname Seq
#' 
#' @include RepoTimeInt-class.R
#' 
#' 
#' @export
setMethod(
  f = "Seq",
  signature = c("RepoTimeInt"),
  definition = function(x, y){
    
    if (class(x = x) != 'RepoTimeInt' || 
        class(x = y) != 'RepoTimeInt' || 
        length(x = x@Repo) != 1 || 
        length(x = y@Repo) != 1){
          stop('[RepoTimeInt::Seq] Arguments of Seq must be objects of class
               RepoTimeInt and length 1.')
    }
    
    if (substr(x = x@Repo, start = 1, stop = 2) != 
        substr(x = y@Repo, start = 1, stop = 2)) {
            stop('[RepoTimeInt::Seq] Arguments of Seq must be objects of class
                 RepoTimeInt with the same time reference (AA, MM, QQ, etc)')
    }
      
    if (substr(x = x@Repo, start = 1, stop = 2) %in% c('QQ', 'QR')){ 
        byParam <- '15 days'
    }
    if (substr(x = x@Repo, start = 1, stop = 2) %in% c('MM', 'MR')){ 
        byParam <- '1 month'
    }
    if (substr(x = x@Repo, start = 1, stop = 2) %in% c('BB', 'BR')){ 
        byParam <- '2 month'
    }
    if (substr(x = x@Repo, start = 1, stop = 2) %in% c('TT', 'TR')){ 
        byParam <- '3 month'
    }
    if (substr(x = x@Repo, start = 1, stop = 2) %in% c('EE', 'ER')){ 
        byParam <- '6 month'
    }
    if (substr(x = x@Repo, start = 1, stop = 2) %in% c('AA', 'AR')){ 
        byParam <- '1 year'
    }
    
    startinterval <- seq(from = getlubriInt(x)[[1]]@start, 
                         to = getlubriInt(y)[[1]]@start, 
                         by = byParam)  
    
    monthduration <- duration(days = 30)
    seqInterval <- lapply(X = startinterval, 
                          FUN = function(x) as.interval(x = monthduration,
                                                        start = x))
    output <- lapply(X = seqInterval, 
                     FUN = function(x) lubriToRepoTime(lubriInterval = x))
    output <- lapply(X = output, FUN = newRepoTime)
    
    aux <- unlist(x = lapply(X = output, FUN = getRepo))
    rot <- aux[substr(x = aux, start = 3 , stop = 4) == 12]
    if (length(x = rot)>0) {rot <- gsub(pattern = 'MM', 
                                        replacement = 'MR', 
                                        x = rot)
                        aux <- c(aux, rot)
                        output <- lapply(X = aux, FUN = newRepoTime)
    }
    
    #names(output) <- unlist(lapply(output, getRepo))
    
    return(value = output)
  }
  
)