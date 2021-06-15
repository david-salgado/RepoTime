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
#' RepoTimeTolubri('MR122014')
#' RepoTimeTolubri('TT20141')
#' 
#' @export
RepoTimeTolubri <- function(RepoTime, TimeZone = 'Europe/Madrid'){
    
    names(RepoTime) <- NULL
    Months <- c(paste0('0', 1:9), 10:12)
    Years <- stringr::str_pad(1:9999, 4, 'left', '0')
    
    if (length(x = RepoTime) == 0) {
        
        stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                    RepoTime, 
                    ': Zero-length repo time intervals are not valid.\n'), 
             call. = FALSE)
    }
    if (length(x = RepoTime) == 1) {
        
        if (is.na(RepoTime)) return(interval(NA, NA))
        
        if (nchar(x = RepoTime) < 6) {
            
            stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                        RepoTime, 
                        ': The number of characters for a valid repo time interval must be at least 6. Check syntax PPp...p or PRp...p.\n'), 
                 call. = FALSE)
        }
        if (substr(x = RepoTime, start = 1, stop = 2) %in% c('QQ', 'QR')) {
            
            if (nchar(x = RepoTime) != 9) {
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': The number of characters for a QQ or QR repo time interval must be 9. Check syntax QQqmmaaaa or QRqmmaaaa.\n'), 
                     call. = FALSE)
            }
            FortNight <- substr(x = RepoTime, start = 3, stop = 3)
            if (!FortNight %in% c(1, 2)){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': Time interval not valid. Chech syntax QQqmmaaaa or QRqmmaaaa.\n'), 
                     call. = FALSE)
            }
            Month <- substr(x = RepoTime, start = 4, stop = 5)
            if(!Month %in% Months){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': Time interval not valid. Chech syntax QQqmmaaaa or QRqmmaaaa.\n'), 
                     call. = FALSE)
            }
            
            Year <- substr(x = RepoTime, start = 6, stop = 9)
            if (as.integer(Year) < 1900 | as.integer(Year) > 2100){
                
                warning(paste0('[RepoTime::RepoTimeTolubri] ', 
                               RepoTime,
                               ': Input year is before 1900 or after 2100. Check syntax QQqmmaaaa or QRqmmaaaa.\n'),
                        call. = FALSE)
                
            }
            if (!Year %in% Years){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': Time interval not valid. Chech syntax QQqmmaaaa or QRqmmaaaa.\n'), 
                     call. = FALSE)
            }
            if (FortNight == '1' ) {
                St_Instant <- ymd(paste0(Year, '-', Month, '-01'), 
                                  tz = TimeZone)
                End_Instant <- St_Instant + lubridate::weeks(x = 2)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (FortNight == '2' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', Month, '-16'), 
                                  tz = TimeZone)
                St_Instant_Month <- lubridate::ymd(paste0(Year, '-', Month, '-01'),
                                        tz = TimeZone)

                End_Instant <- strftime(St_Instant_Month + 
                                        lubridate::month(1, abbr = FALSE) - 
                                        lubridate::days(1))

                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }  
        
        
        if (substr(x = RepoTime, start = 1, stop = 2) %in% c('MM', 'MR')) {
            
            if (nchar(x = RepoTime) != 8) {
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': The number of characters for an MM or MR repo time interval must be 7. Check syntax MMmmaaaa or MRmmaaaa.\n'), 
                     call. = FALSE)
            }
            Month <- substr(x = RepoTime, start = 3, stop = 4)
            if(!Month %in% Months){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime,
                            ': Time interval not valid. Chech syntax MMmmaaaa or MRmmaaaa.\n'), 
                     call. = FALSE)
            }
            Year <- substr(x = RepoTime, start = 5, stop = 8)
            if (as.integer(Year) < 1900 | as.integer(Year) > 2050){
                
                warning(paste0('[RepoTime::RepoTimeTolubri] ', 
                               RepoTime,
                               ': Input year is before 1900 or after 2050. Check syntax MMqmmaaaa or MRqmmaaaa.\n'),
                        call. = FALSE)
                
            }
            if (!Year %in% Years){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime,
                            ': Time interval not valid. Chech syntax MMmmaaaa or MRmmaaaa.\n'), 
                     call. = FALSE)
            }
            St_Instant <- lubridate::ymd(paste0(Year, '-', Month, '-01'), tz = TimeZone)
            End_Instant <- St_Instant + lubridate::month(1, abbr = FALSE) - lubridate::days(1)
            
            lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }
        
        if (substr(x = RepoTime, start = 1, stop = 2) %in% c('BB', 'BR')) {
            
            if (nchar(RepoTime) != 7) {
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': The number of characters for a BB or BR repo time interval must be 7. Check syntax BBbaaaaa or BRbaaaa.\n'), 
                     call. = FALSE)
            }
            BiMonth <- substr(x = RepoTime, start = 3, stop = 3)
            if (!BiMonth %in% 1:6){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': Time interval not valid. Chech syntax BBbaaaa or BRbaaaa.\n'), 
                     call. = FALSE)
            }
            Year <- substr(x = RepoTime, start = 4, stop = 7)
            if (as.integer(Year) < 1900 | as.integer(Year) > 2050){
                
                warning(paste0('[RepoTime::RepoTimeTolubri] ', 
                               RepoTime,
                               ': Input year is before 1900 or after 2050. Check syntax BBbaaaa or BRbaaaa.\n'),
                        call. = FALSE)
                
            }
            if (!Year %in% Years){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': Time interval not valid. Chech syntax BBbaaaa or BRbaaaa.\n'), 
                     call. = FALSE)
            }
            if (BiMonth == '1' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '01-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(2, abbr = FALSE)- 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (BiMonth == '2' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '03-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(2, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (BiMonth == '3' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '05-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(2, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (BiMonth == '4' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '07-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(2, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (BiMonth == '5' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '09-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(2, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (BiMonth == '6' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '11-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(x = 2, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }  
        
        if (substr(x = RepoTime, start = 1, stop = 2) %in% c('TT', 'TR')) {
            
            if (nchar(x = RepoTime) != 7) {
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': The number of characters for a TT or TR repo time interval must be 7. Check syntax TTtaaaa or TRtaaaa.\n'), 
                     call. = FALSE)
            }
            Term <- substr(x = RepoTime, start = 3, stop = 3)
            if (!Term %in% 1:4){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': Time interval not valid. Chech syntax TTtaaaa or TRtaaaa.\n'), 
                     call. = FALSE)
            }
            Year <- substr(x = RepoTime, start = 4, stop = 7)
            if (as.integer(Year) < 1900 | as.integer(Year) > 2050){
                
                warning(paste0('[RepoTime::RepoTimeTolubri] ', 
                               RepoTime,
                               ': Input year is before 1900 or after 2050. Check syntax TTtaaaa or TRtaaaa.\n'),
                        call. = FALSE)
                
            }
            if (!Year %in% Years){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime,
                            ': Time interval not valid. Chech syntax TTtaaaa or TRtaaaa.\n'),
                     call. = FALSE)
            }
            if (Term == '1' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '01-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(x = 3, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (Term == '2' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '04-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(x = 3, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (Term == '3' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '07-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(x = 3, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (Term == '4' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '10-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(x = 3, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }  
        
        if (substr(RepoTime, start = 1, stop = 2) %in% c('SS', 'SR')) {
            
            if (nchar(x = RepoTime) != 7) {
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime, 
                            ': The number of characters for an SS or SR repo time interval must be 7. Check syntax SSsaaaa or SRsaaaa.\n'), 
                     call. = FALSE)
            }
            Sem <- substr(RepoTime, start = 3, stop = 3)
            if (!Sem %in% 1:2){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime,
                            ': Time interval not valid. Chech syntax SSsaaaa or SRsaaaa.\n'), 
                     call. = FALSE)
            }
            Year <- substr(RepoTime, start = 4, stop = 7)
            if (as.integer(Year) < 1900 | as.integer(Year) > 2050){
                
                warning(paste0('[RepoTime::RepoTimeTolubri] ', 
                               RepoTime,
                               ': Input year is before 1900 or after 2050. Check syntax SSsaaaa or SRsaaaa.\n'),
                        call. = FALSE)
                
            }
            if (!Year %in% Years){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                            RepoTime,
                            ': Time interval not valid. Chech syntax SSsaaaa or SRsaaaa.\n'), 
                     call. = FALSE)
            }
            if (Sem == '1' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '01-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(x = 6, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            if (Sem == '2' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '07-01'), tz = TimeZone)
                End_Instant <- St_Instant + 
                               lubridate::month(x = 6, abbr = FALSE) - 
                               lubridate::days(1)
                
                lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }  
        
        if (substr(RepoTime, start = 1, stop = 2) %in% c('AA', 'AR')) {
            
            if (nchar(x = RepoTime) != 6) {
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ',
                            RepoTime,
                            ': The number of characters for an AA or AR repo time interval must be 6. Check syntax AAaaaa or ARaaaa.\n'),
                     call. = FALSE)
            }
            Year <- substr(RepoTime, start = 3, stop = 6)
            if (as.integer(Year) < 1900 | as.integer(Year) > 2050){
                
                warning(paste0('[RepoTime::RepoTimeTolubri] ', 
                               RepoTime,
                               ': Input year is before 1900 or after 2050. Check syntax AAaaaa or ARaaaa.\n'),
                        call. = FALSE)
                
            }
            if (!Year %in% Years){
                
                stop(paste0('[RepoTime::RepoTimeTolubri] ',
                            RepoTime,
                            ': Time interval not valid. Chech syntax AAaaaa or ARaaaa.\n'), 
                     call. = FALSE)
            }
            St_Instant <- lubridate::ymd(paste0(Year, '-', '01-01'), tz = TimeZone)
            End_Instant <- St_Instant + lubridate::years(1) - lubridate::days(1)
            
            lubriInterval <- lubridate::interval(St_Instant, End_Instant)
            output <- list(lubriInterval)
            names(x = output) <- RepoTime
            return(value = output)
        }
        
        stop(paste0('[RepoTime::RepoTimeTolubri] ', 
                    RepoTime, 
                    ': Two initial characters of input time interval not valid. Check uppercase.\n'), 
             call. = FALSE)
        
    } else {
         
        output <- lapply(X = as.list(x = RepoTime), 
                         FUN = function(x){RepoTimeTolubri(RepoTime = x, 
                                                     TimeZone = TimeZone)[[1]]})
        names(x = output) <- RepoTime
        return(value = output)
        
    }
}