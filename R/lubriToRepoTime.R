#' @name lubriToRepoTime
#' @aliases lubriToRepoTime
#' 
#' @title Transformation of \linkS4class{Interval} class objects into time 
#' intervals under the repo notation
#' 
#' @description \code{lubriToRepoTime} transforms time intervals expressed as 
#' objects of class \linkS4class{Interval} from the 
#' \link[lubridate]{lubridate-package} into the so-called repo notation.
#' 
#' @param lubriInterval Object of class \linkS4class{Interval} from 
#' \link[lubridate]{lubridate-package}.
#' 
#' @param Rot Logical vector of length 1 indicating whether periods with 
#' rotated samples are to be included in the sequence or not (default).
#' 
#' @details The repo notation of time intervals is explained in the details 
#' section of \code{\link{RepoTimeInt-class}}.
#'
#' @examples
#' library(lubridate)
#' lubriToRepoTime(interval('2015-04-01', '2015-04-30', tz = 'Europe/Madrid'))
#' lubriToRepoTime(interval('2015-01-01', '2015-03-31', tz = 'Europe/Madrid'))
#'
#' @include RepoTimeTolubri.R
#'
#' @seealso \code{\link{RepoTimeTolubri}}
#'
#' @importFrom lubridate int_start int_end days mday month year leap_year
#'
#' @export
lubriToRepoTime <- function(lubriInterval, Rot = FALSE){

    if (length(x = lubriInterval) == 1){

        if (class(x = lubriInterval) != 'Interval') {
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': Argument is not an inteval-class object from package lubridate.'),
                 call. = FALSE)
        
        }
        IniTime <- int_start(int = lubriInterval)
        if (day(IniTime) != 1 & day(IniTime) != 16) {
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must start on the 1st or 16th day of a month.'),
                 call. = FALSE)
            
        }
        
        FinTime <- int_end(int = lubriInterval)
        monthFinTime <- month(FinTime)
        dayFinTime <- day(FinTime)
        if (monthFinTime == 1 && !dayFinTime %in% c(15, 31)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 31st day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 2 && leap_year(FinTime) && !dayFinTime %in% c(15, 29)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 29th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 2 && !leap_year(FinTime) && !dayFinTime %in% c(15, 28)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 29th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 3 && !dayFinTime %in% c(15, 31)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 31st day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 4 && !dayFinTime %in% c(15, 30)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 30th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 5 && !dayFinTime %in% c(15, 31)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 30th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 6 && !dayFinTime %in% c(15, 30)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 30th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 7 && !dayFinTime %in% c(15, 31)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 30th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 8 && !dayFinTime %in% c(15, 31)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 30th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 9 && !dayFinTime %in% c(15, 30)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 30th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 10 && !dayFinTime %in% c(15, 31)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 30th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 11 && !dayFinTime %in% c(15, 30)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 30th day of a month.'),
                 call. = FALSE)
        }
        if (monthFinTime == 12 && !dayFinTime %in% c(15, 31)){
            
            stop(paste0('[RepoTime::lubriToRepoTime] ', 
                        lubriInterval, 
                        ': This input time interval must end on the 15th or 30th day of a month.'),
                 call. = FALSE)
        }
        NDays <- lubriInterval %/% days(x = 1)

        if (NDays <= 15) {

            PP <- ifelse(test = Rot, yes = 'QR', no ='QQ')
            MonthDay <- mday(x = IniTime)
            if (MonthDay <= 14) {

                p <- 1

            } else {

                p <- 2
            }
            Month <- month(x = IniTime)
            Month <- ifelse(test = nchar(Month) == 1, 
                            yes = paste0('0', Month), 
                            no = Month)
            Year <- year(x = IniTime)
            RepoTime <- paste0(PP, p, Month, Year)
            return(value = RepoTime)

        }

        if (NDays > 15 & NDays <= 31){

            PP <- ifelse(test = Rot, yes = 'MR', no = 'MM')
            Month <- month(IniTime)
            Month <- ifelse(test = (nchar(x = Month) == 1), 
                            yes = paste0('0', Month), 
                            no = Month)
            Year <- year(x = IniTime)
            RepoTime <- paste0(PP, Month, Year)
            return(value = RepoTime)

        }

        if (NDays > 31 & NDays <= 62){

            PP <- ifelse(test = Rot, yes = 'BR', no = 'BB')
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

            PP <- ifelse(test = Rot, yes = 'TR', no = 'TT')
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

            PP <- ifelse(test = Rot, yes = 'SR', no = 'SS')
            Month <- month(x = IniTime)
            if (Month %in% 1:6) Sem <- 1
            if (Month %in% 7:12) Sem <- 2
            Year <- year(x = IniTime)
            RepoTime <- paste0(PP, Sem, Year)
            return(value = RepoTime)

        }

        if (NDays > 186 & NDays <= 365){

            PP <- ifelse(test = Rot, yes = 'AR', no = 'AA')
            Year <- year(x = IniTime)
            RepoTime <- paste0(PP, Year)
            return(value = RepoTime)

        }
        
        stop(paste0('[RepoTime::lubriToRepoTime] ', 
             lubriInterval, 
             ': Time interval not valid.\n\n'),
             call. = FALSE)

    } else {

        output <- lapply(X = lubriInterval, FUN = lubriToRepoTime)
        return(value = output)

    }
}
