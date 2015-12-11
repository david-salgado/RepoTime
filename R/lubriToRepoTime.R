#' @title Transformation of interval-class objects into time intervals under 
#' \emph{PPp...p} notation
#' 
#' @description \code{lubriToRepoTime} transforms time intervals expressed as 
#' objects of class interval from the lubridate package into the so-called repo 
#' notation (see details)
#' 
#' @param lubriInterval object of class interval from package lubridate
#' 
#' @details  The repo notation of time intervals 
#' amounts to denoting them by the string PPp...p, where
#' \itemize{
#'  \item PP is 
#'      \tabular{ll}{
#'          QQ,QR\tab Fortnight (\emph{Quincena} in Spanish)\cr
#'          MM,MR\tab Month\cr
#'          BB,BR\tab Bimonth\cr
#'          TT,TR\tab Term\cr
#'          EE,ER\tab Semester\cr
#'          AA,AR\tab Year (\emph{A\~no} in Spanish)
#'      }
#' \item p...p is
#'      \tabular{lll}{
#'         p...p\tab Values\tab Periodicity\cr
#'         qmmyyyy\tab q=1,2; mm=01,...,12; yyyy=year\tab Fortnight\cr
#'         mmyyyy\tab mm=01,...,12; yyyy=year\tab Month\cr
#'         byyyy\tab b=1,...,6; yyyy=year\tab Bimonth\cr
#'         tyyyy\tab t=1,...,4; yyyy=year\tab Term\cr
#'         syyyy\tab s=1,2; yyyy=year\tab Semester\cr
#'         yyyy\tab yyyy=year\tab Year
#'       }
#'  }#' The span of years possibly covered by the repo notation has been internally 
#' fixed between the years 1 a.d. and 3000 a.d.
#'
#' @examples
#' library(lubridate)
#' lubriToRepoTime(interval('2015-04-01', '2015-04-30', tz = 'Europe/Madrid'))
#' lubriToRepoTime(interval('2015-01-01', '2015-03-31', tz = 'Europe/Madrid'))
#'
#' @include RepoTimeTolubri.R
#'
#' @importFrom lubridate int_start days mday month year
#'
#' @export
lubriToRepoTime <- function(lubriInterval){

    if (length(x = lubriInterval) == 1){

        if (class(x = lubriInterval) != 'Interval') 
            stop('[RepoTime::lubriToRepoTime] Argument is not an inteval-class 
                 object from package lubridate.')

        IniTime <- int_start(int = lubriInterval)
        NDays <- lubriInterval %/% days(x = 1)

        if (NDays <= 17) {

            PP <- 'QQ'
            MonthDay <- mday(x = IniTime)
            if (MonthDay <= 14) {

                p <- 1

            } else {

                p <- 2
            }
            Month <- month(x = IniTime)
            Month <- ifelse(nchar(Month) == 1, paste0('0', Month), Month)
            Year <- year(IniTime)
            RepoTime <- paste0(PP, p, Month, Year)
            return(RepoTime)

        }

        if (NDays > 17 & NDays <= 31){

            PP <- 'MM'
            Month <- month(IniTime)
            Month <- ifelse(test = (nchar(x = Month) == 1), 
                            yes = paste0('0', Month), 
                            no = Month)
            Year <- year(x = IniTime)
            RepoTime <- paste0(PP, Month, Year)
            return(value = RepoTime)

        }

        if (NDays > 31 & NDays <= 62){

            PP <- 'BB'
            Month <- month(x = IniTime)
            if (Month %in% 1:2) BiM <- 1
            if (Month %in% 3:4) BiM <- 2
            if (Month %in% 5:6) BiM <- 3
            if (Month %in% 7:8) BiM <- 4
            if (Month %in% 9:10) BiM <- 5
            if (Month %in% 11:12) BiM <- 6
            Year <- year(x = IniTime)
            RepoTime <- paste0(PP, BiM, Year)
            return(value = RepoTime)

        }

        if (NDays > 62 & NDays <= 93){

            PP <- 'TT'
            Month <- month(x = IniTime)
            if (Month %in% 1:3) Term <- 1
            if (Month %in% 4:6) Term <- 2
            if (Month %in% 7:9) Term <- 3
            if (Month %in% 10:12) Term <- 4
            Year <- year(x = IniTime)
            RepoTime <- paste0(PP, Term, Year)
            return(value = RepoTime)

        }

        if (NDays > 93 & NDays <= 186){

            PP <- 'SS'
            Month <- month(x = IniTime)
            if (Month %in% 1:6) Sem <- 1
            if (Month %in% 7:12) Sem <- 2
            Year <- year(x = IniTime)
            RepoTime <- paste0(PP, Sem, Year)
            return(value = RepoTime)

        }

        if (NDays > 186){

            PP <- 'AA'
            Year <- year(x = IniTime)
            RepoTime <- paste0(PP, Year)
            return(value = RepoTime)

        }
        cat('[RepoTime::lubriToRepoTime] Time interval not valid.\n\n')
        return(value = invisible(NULL))

    } else {

        output <- lapply(X = lubriInterval, FUN = lubriToRepoTime)
        return(value = output)

    }
}
