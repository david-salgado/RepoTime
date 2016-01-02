#' @name Seq
#' @aliases Seq
#' 
#' @title Seq method for the S4 class \linkS4class{RepoTimeInt}
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
#' @param Rot Logical vector of length 1 indicating whether periods with rotated
#' samples are to be included in the sequence (default) or not.
#' 
#' @param RotPer Character vector of length 1 with the numeric code of the time
#' period containing both unrotated and rotated samples.
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
setGeneric("Seq", function(x, y, Rot = TRUE, RotPer = '12'){standardGeneric("Seq")})

#' @rdname Seq
#' 
#' @include RepoTimeInt-class.R
#' 
#' @export
setMethod(
  f = "Seq",
  signature = c("RepoTimeInt"),
  definition = function(x, y, Rot = TRUE, RotPer = '12'){
    
    if (class(x = x) != 'RepoTimeInt' || 
        class(x = y) != 'RepoTimeInt' || 
        length(x = x@Repo) != 1 || 
        length(x = y@Repo) != 1){
          stop('[RepoTimeInt::Seq] Arguments x and y of Seq must be objects of 
                class RepoTimeInt and length 1.')
    }
    
    if (substr(x = x@Repo, start = 1, stop = 1) != 
        substr(x = y@Repo, start = 1, stop = 1)) {
            stop('[RepoTimeInt::Seq] Arguments of Seq must be objects of class
                 RepoTimeInt with the same time reference (AA, MM, SS, ...)')
    }
      
    if (substr(x = x@Repo, start = 1, stop = 1) == 'Q'){ 
        byParam <- '15 days'
        prefix <- 'Q'
        days <- 15
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'M'){ 
        byParam <- '1 month'
        prefix <- 'M'
        days <- 30
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'B'){ 
        byParam <- '2 month'
        prefix <- 'B'
        days <- 60
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'T'){ 
        byParam <- '3 month'
        prefix <- 'T'
        days <- 91
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'E'){ 
        byParam <- '6 month'
        prefix <- 'E'
        days <- 182
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'A'){ 
        byParam <- '1 year'
        prefix <- 'A'
        days <- 365
    }
      
    startinterval <- seq(from = getlubriInt(x)[[1]]@start, 
                         to = getlubriInt(y)[[1]]@start, 
                         by = byParam)  
    
    PeriodDuration <- duration(days = days)

    seqInterval <- lapply(X = startinterval, 
                          FUN = function(x) as.interval(x = PeriodDuration,
                                                        start = x))
    
    output <- unlist(lapply(X = seqInterval, 
                     FUN = function(x) lubriToRepoTime(lubriInterval = x)))
    if (Rot) {
        
        if (nchar(x = RotPer) == 1) {
            Rot.index <- which(x = substr(x = output, 
                                          start = 3, 
                                          stop = 3) == RotPer)
        }
        
        if (nchar(x = RotPer) == 2) {
            Rot.index <- which(x = substr(x = output, 
                                          start = 3, 
                                          stop = 4) == RotPer)
        }
        if (nchar(x = RotPer) > 2) stop('[RepoTime::Seq] RotPer must be 1 or 2 
                                    digits.')
        Breaks <- unique(x = c(1, Rot.index, length(x = output)))
        newoutput <- c()
        ini.index <- 1
        for (Break in Breaks[-1]){
            Rot <- output[Break]
            aux <- output[ini.index:Break]
            newoutput <- c(newoutput, aux)
            if (Break == Breaks[length(x = Breaks)]) {
                
                if (substr(x = getRepo(object = y), start = 2, stop = 2) == 'R'){
                    
                    newoutput <- c(newoutput, getRepo(object = y))
                }
                
                break
            }
            newoutput <- c(newoutput, gsub(pattern = paste0(prefix, prefix), 
                                           replacement = paste0(prefix, 'R'), 
                                           x = Rot))
            ini.index <- Break + 1
        }
            
        if (substr(x = getRepo(object = x), start = 2, stop = 2) == 'R'){
            
            newoutput[1] <- getRepo(object = x)
        }
        output <- as.list(newoutput)
        
    }
    output <- lapply(X = output, FUN = newRepoTime)
    
    names(output) <- unlist(x = lapply(X = output, FUN = getRepo))
    
    return(value = output)
  }
  
)