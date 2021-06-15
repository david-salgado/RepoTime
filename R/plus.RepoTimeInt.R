#' @title Sum of a \linkS4class{Period} class object to a 
#' \linkS4class{RepoTimeInt} class object
#'
#' @description \code{+} sums an object of class \linkS4class{Period} 
#' of \link[lubridate]{lubridate-package} to an object of class 
#' \linkS4class{RepoTimeInt}.
#'
#' @details This method overloads the operator \link{+} and builds an object of
#' class \linkS4class{RepoTimeInt}.
#'
#' @param e1 Object of class \linkS4class{RepoTimeInt}.
#'
#' @param e2 Object of class \linkS4class{Period}.
#'
#' @return Object of class \linkS4class{RepoTimeInt} resulting from summing the
#' time period \code{e2} to the initial time interval \code{e1}.
#'
#' @examples
#' RepoPeriod <- newRepoTime('MM022015')
#' RepoPeriod + months(1)
#' RepoPeriod + years(1)
#' RepoPeriod + weeks(2)
#'
#' @include RepoTimeInt-class.R getlubriInt.R getRepo.R
#'
#' @export
setMethod(
    f = "+",
    signature = c("RepoTimeInt", "Period"),
    definition = function(e1, e2){

      St_Instant <- lapply(
        X = getlubriInt(e1),
        FUN = function(x){
          lubridate::add_with_rollback(
            lubridate::int_start(int = x), e2) })
      St_Repo <- getRepo(e1)
        
      RepoTimeString <- function(i){

        ThisInstant <- St_Instant[[i]]

        if (substr(x = St_Repo[i], start = 1, stop = 2) == 'QQ'){

          FortNight <- ifelse(test = day(x = ThisInstant) > 15, 
                              yes = '2', 
                              no = '1')
          if (day(x = ThisInstant) < 29) {
              
            ThisInstant <- lubridate::floor_date(x = ThisInstant, unit = 'month')
          
          } else {
            
            ThisInstant <- lubridate::ceiling_date(x = ThisInstant, unit = 'month')
          
          }
          Month <- months(x = ThisInstant)
          if (nchar(x = Month) == 1) Month <- paste0('0', Month)
          Year <- lubridate::year(x = ThisInstant)
          output <- paste0('QQ', FortNight, Month, Year)
        }
        if (substr(x = St_Repo[i], start = 1, stop = 2) == 'BB'){

          if (months(x = ThisInstant) %in% c(1, 2))   BiMonth <- 1
          if (months(x = ThisInstant) %in% c(3, 4))   BiMonth <- 2
          if (months(x = ThisInstant) %in% c(5, 6))   BiMonth <- 3
          if (months(x = ThisInstant) %in% c(7, 8))   BiMonth <- 4
          if (months(x = ThisInstant) %in% c(9, 10))  BiMonth <- 5
          if (months(x = ThisInstant) %in% c(11, 12)) BiMonth <- 6
          Year <- lubridate::year(x = ThisInstant)
          output <- paste0('BB', BiMonth, Year)
        }
        if (substr(x = St_Repo[i], start = 1, stop = 2) == 'MM'){

          Month <- months(x = ThisInstant)
          if (nchar(x = Month) == 1) Month <- paste0('0', Month)
          Year <- lubridate::year(x = ThisInstant)
          output <- paste0('MM', Month, Year)
        }
        if (substr(x = St_Repo[i], start = 1, stop = 2) == 'TT'){

          if (months(x = ThisInstant) %in% c(1, 2, 3))    Term <- 1
          if (months(x = ThisInstant) %in% c(4, 5, 6))    Term <- 2
          if (months(x = ThisInstant) %in% c(7, 8, 9))    Term <- 3
          if (months(x = ThisInstant) %in% c(10, 11, 12)) Term <- 4
          Year <- lubridate::year(x = ThisInstant)
          output <- paste0('TT', Term, Year)
        }
        if (substr(x = St_Repo[i], start = 1, stop = 2) == 'SS'){

          if (months(x = ThisInstant) %in% 1:6)  Sem <- 1
          if (months(x = ThisInstant) %in% 7:12) Sem <- 2
          Year <- lubridate::year(x = ThisInstant)
          output <-paste0('SS', Sem, Year)
        }
        if (substr(x = St_Repo[i], start = 1, stop = 2) == 'AA'){

          Year <- lubridate::year(x = ThisInstant)
          output <- paste0('AA', Year)
        }

        return(value = output)

    }

    output <- unlist(x = lapply(X = seq(along = getRepo(e1)), 
                                FUN = RepoTimeString))
    output <- newRepoTime(Time = output)
    return(value = output)

    }
)
