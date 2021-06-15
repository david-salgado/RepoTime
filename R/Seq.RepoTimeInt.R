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
          stop('[RepoTimeInt::Seq] Arguments x and y of Seq must be objects of class RepoTimeInt and length 1.',
               call. = FALSE)
    }
    
    if (substr(x = x@Repo, start = 1, stop = 1) != 
        substr(x = y@Repo, start = 1, stop = 1)) {
            stop('[RepoTimeInt::Seq] Arguments of Seq must be objects of class RepoTimeInt with the same time reference (AA, MM, SS, ...)',
                 call. = FALSE)
    }
      
    if (substr(x = x@Repo, start = 1, stop = 1) == 'Q'){ 
        stop('[RepoTimeInt::Seq] Fortnights not implemented for this function.',
             call. = FALSE)
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'M'){ 
        byParam <- '1 month'
        prefix <- 'M'
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'B'){ 
        byParam <- '2 month'
        prefix <- 'B'
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'T'){ 
        byParam <- '3 month'
        prefix <- 'T'
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'S'){ 
        byParam <- '6 month'
        prefix <- 'S'
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'A'){ 
        byParam <- '1 year'
        prefix <- 'A'
    }
      
    startinterval <- seq(from = getlubriInt(x)[[1]]@start, 
                         to = getlubriInt(y)[[1]]@start, 
                         by = byParam)  

    Months <- month(startinterval)
    BiMonths <- unlist(lapply(as.list(Months), 
                              function(m){
                                  if (m %in% c(1, 2)) return(1)
                                  if (m %in% c(3, 4)) return(2)
                                  if (m %in% c(5, 6)) return(3)
                                  if (m %in% c(7, 8)) return(4)
                                  if (m %in% c(9, 10)) return(5)
                                  if (m %in% c(11, 12)) return(6)
                              }))
    Terms <- unlist(lapply(as.list(Months), 
                           function(m){
                               if (m %in% c(1, 2, 3)) return(1)
                               if (m %in% c(4, 5, 6)) return(2)
                               if (m %in% c(7, 8, 9)) return(3)
                               if (m %in% c(10, 11, 12)) return(4)
                           }))
    Sems <- unlist(lapply(as.list(Months), 
                           function(m){
                               if (m %in% c(1, 2, 3, 4, 5, 6)) return(1)
                               if (m %in% c(7, 8, 9, 10, 11, 12)) return(2)
                           }))
    
    Months <- ifelse(nchar(Months) == 1, paste0('0', Months), Months)
    Years <- lubridate::year(startinterval)

    if (substr(x = x@Repo, start = 1, stop = 1) == 'M'){
        
        output <- paste0('MM', Months, Years)
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'B'){
        
        output <- paste0('BB', BiMonths, Years)
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'T'){ 
        
        output <- paste0('TT', Terms, Years)
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'S'){ 
        
        output <- paste0('SS', Sems, Years)
    }
    if (substr(x = x@Repo, start = 1, stop = 1) == 'A'){
        
        output <- paste0('AA', Years)
    }

    if (Rot) {
        
        if (substr(x = x@Repo, start = 1, stop = 1) == 'M'){
            
            Rot.index <- which(Months %in% RotPer)
        }
        
        if (substr(x = x@Repo, start = 1, stop = 1) == 'B'){
            
            Rot.index <- which(BiMonths %in% RotPer)
        }
        
        if (substr(x = x@Repo, start = 1, stop = 1) == 'T'){
            
            Rot.index <- which(Terms %in% RotPer)
        }
    
        if (substr(x = x@Repo, start = 1, stop = 1) == 'S'){
            
            Rot.index <- which(Sems %in% RotPer)
        }

        if (substr(x = x@Repo, start = 1, stop = 1) == 'A'){
            
            Rot.index <- which(Years %in% RotPer)
        }
        
        Breaks <- unique(c(1, Rot.index, length(output)))

        newoutput <- c()
        ini.index <- 1
        if (Breaks[1] %in% Rot.index) {
            
            newoutput <- c(output[1], gsub(pattern = paste0(prefix, prefix), 
                                           replacement = paste0(prefix, 'R'), 
                                           x = output[1]))
            ini.index <- 2
        }

        for (Break in Breaks[-1]){
            
            Rot <- output[Break]
            aux <- output[ini.index:Break]
            newoutput <- c(newoutput, aux)
            if (Break == Breaks[length(Breaks)] && !Break %in% Rot.index){
                
                break
            }
            newoutput <- c(newoutput, gsub(pattern = paste0(prefix, prefix), 
                                           replacement = paste0(prefix, 'R'), 
                                           x = Rot))
            ini.index <- Break + 1
        }
        
        if(length(newoutput) > 0){
            output <- newoutput
        }
        
    } 

    output <- newRepoTime(output)
    
    
    return(output)
  }
  
)