#' @name RepoTimeTolubri
#' @aliases RepoTimeTolubri
#' 
#' @title Transformation of time intervals in repo notation into a list of
#' \link[lubridate]{Interval-class} objects of the 
#' \link[lubridate]{lubridate-package}
#' 
#' @description \code{RepoTimeTolubri} transforms time intervals expressed in 
#' the so-called repo notation (see details) into a list of 
#' \link[lubridate]{Interval-class} objects of the 
#' \link[lubridate]{lubridate-package}. When the time interval is not recognized
#' as a valid string, it returns the corresponding message.
#' 
#' @param RepoTime Character vector with the time interval in repo notation.
#' 
#' @param TimeZone Time zone component of a date-time as expected in function
#' \code{\link[lubridate]{tz}} of the \link[lubridate]{lubridate-package} 
#' (default value = 'Europe/Madrid').
#' 
#' @details  The repo notation of time intervals is explained in the details 
#' section of \code{\link{RepoTimeInt-class}}.
#' 
#' @examples
#' RepoTimeTolubri('MM022014')
#' \dontrun{
#' RepoTimeTolubri('MM22014')
#' }
#' 
#' @importFrom lubridate tz ymd years weeks days interval
#' 
#' @export
RepoTimeTolubri <- function(RepoTime, TimeZone = 'Europe/Madrid'){
    
    names(RepoTime) <- NULL
    Months <- c(paste0('0', 1:9), 10:12)
    Years <- 1:3000
    
    if (length(x = RepoTime) == 1) {

        if (substr(x = RepoTime, start = 1, stop = 2) %in% c('QQ', 'QR')) {
            
            if (nchar(x = RepoTime) != 9) stop('[RepoTime:RepoTimeTolubri] The
            number of characters for a QQ or QR repo time interval must be 9.')
            
            FortNight <- substr(x = RepoTime, start = 3, stop = 3)
            if (!FortNight %in% c(1, 2)){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            Month <- substr(x = RepoTime, start = 4, stop = 5)
            if(!Month %in% Months){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            
            Year <- substr(x = RepoTime, start = 6, stop = 9)
            if (!Year %in% Years){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            if (FortNight == '1' ) {
                St_Instant <- ymd(paste0(Year, '-', Month, '-01'), 
                                  tz = TimeZone)
                End_Instant <- St_Instant + weeks(x = 2) - days(x = 1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (FortNight == '2' ) {
                St_Instant <- ymd(paste0(Year, '-', Month, '-15'), 
                                  tz = TimeZone)
                St_Instant_Month <- ymd(paste0(Year, '-', Month, '-01'))
                End_Instant <- St_Instant_Month + months(1, abbreviate = FALSE) 
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }  
        
        
        if (substr(x = RepoTime, start = 1, stop = 2) %in% c('MM', 'MR')) {
            
            if (nchar(x = RepoTime) != 8) stop('[RepoTime:RepoTimeTolubri] The
            number of characters for a MM or MR repo time interval must be 7.')
            Month <- substr(x = RepoTime, start = 3, stop = 4)
            if(!Month %in% Months){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            Year <- substr(x = RepoTime, start = 5, stop = 8)
            if (!Year %in% Years){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            St_Instant <- ymd(paste0(Year, '-', Month, '-01'), tz = TimeZone)
            End_Instant <- St_Instant + months(1, abbreviate = FALSE) - days(1)
            
            lubriInterval <- interval(St_Instant, End_Instant)
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }
        
        if (substr(x = RepoTime, start = 1, stop = 2) %in% c('BB', 'BR')) {
            
            if (nchar(RepoTime) != 7) stop('[RepoTime:RepoTimeTolubri] The
            number of characters for a BB or BR repo time interval must be 7.')
            BiMonth <- substr(x = RepoTime, start = 3, stop = 3)
            if (!BiMonth %in% 1:6){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            Year <- substr(x = RepoTime, start = 4, stop = 7)
            if (!Year %in% Years){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            if (BiMonth == '1' ) {
                St_Instant <- ymd(paste0(Year, '-', '01-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(2, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (BiMonth == '2' ) {
                St_Instant <- ymd(paste0(Year, '-', '03-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(2, abbreviate = FALSE)
                                - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (BiMonth == '3' ) {
                St_Instant <- ymd(paste0(Year, '-', '05-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(2, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (BiMonth == '4' ) {
                St_Instant <- ymd(paste0(Year, '-', '07-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(2, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (BiMonth == '5' ) {
                St_Instant <- ymd(paste0(Year, '-', '09-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(2, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (BiMonth == '6' ) {
                St_Instant <- ymd(paste0(Year, '-', '11-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(x = 2, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }  
        
        if (substr(x = RepoTime, start = 1, stop = 2) %in% c('TT', 'TR')) {
            
            if (nchar(x = RepoTime) != 7) stop('[RepoTime:RepoTimeTolubri] The
            number of characters for a TT or TR repo time interval must be 7.')
            Term <- substr(x = RepoTime, start = 3, stop = 3)
            if (!Term %in% 1:4){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            Year <- substr(x = RepoTime, start = 4, stop = 7)
            if (Term == '1' ) {
                St_Instant <- ymd(paste0(Year, '-', '01-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(x = 3, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (Term == '2' ) {
                St_Instant <- ymd(paste0(Year, '-', '04-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(x = 3, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (Term == '3' ) {
                St_Instant <- ymd(paste0(Year, '-', '07-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(x = 3, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (Term == '4' ) {
                St_Instant <- ymd(paste0(Year, '-', '10-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(x = 3, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }  
        
        if (substr(RepoTime, start = 1, stop = 2) %in% c('SS', 'SR')) {
            
            if (nchar(x = RepoTime) != 7) stop('[RepoTime:RepoTimeTolubri] The
            number of characters for a SS or SR repo time interval must be 7.')
            Sem <- substr(RepoTime, start = 3, stop = 3)
            if (!Sem %in% 1:2){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            Year <- substr(RepoTime, start = 4, stop = 7)
            if (!Year %in% Years){
                
                cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
                return(value = invisible(NULL))
            }
            if (Sem == '1' ) {
                St_Instant <- ymd(paste0(Year, '-', '01-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(x = 6, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            if (Sem == '2' ) {
                St_Instant <- ymd(paste0(Year, '-', '07-01'), tz = TimeZone)
                End_Instant <- St_Instant + months(x = 6, abbreviate = FALSE)
                               - days(1)
                
                lubriInterval <- interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }  
        
        if (substr(RepoTime, start = 1, stop = 2) %in% c('AA', 'AR')) {
            
            if (nchar(x = RepoTime) != 6) stop('[RepoTime:RepoTimeTolubri] The
            number of characters for an AA or AR repo time interval must be 6.')
            Year <- substr(RepoTime, start = 3, stop = 6)
            
            St_Instant <- ymd(paste0(Year, '-', '01-01'), tz = TimeZone)
            End_Instant <- St_Instant + years(1) - days(1)
            
            lubriInterval <- interval(St_Instant, End_Instant)
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }
        cat('[RepoTime::RepoTimeTolubri] Time interval not valid.\n\n')
        return(value = invisible(NULL))
        
    } else {
         
        output <- lapply(X = as.list(x = RepoTime), 
                         FUN = function(x){RepoTimeTolubri(RepoTime = x, 
                                                     TimeZone = TimeZone)[[1]]})
        names(x = output) <- RepoTime
        return(value = output)
        
    }
}