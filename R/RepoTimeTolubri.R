#' Transformación de un intervalo temporal en notación del repositorio a 
#' del paquete lubridate
#'  
#' La notación de intervalos temporales del repositorio consiste en denotarlos
#' por la cadena de caracteres PPp...p, donde
#' \itemize{
#'  \item PP es de la forma 
#'          QQ,QR 	Quincenal
#'          MM,MR 	Mensual
#'          BB,BR 	Bimensual
#'          TT,TR 	Trimestral
#'          EE,ER 	Semestral
#'          AA,AR 	Anual
#' \item p...p es de la forma
#'         p….p 	Valores 	Periodicidad
#'         qmmaaaa  q=1,2;      mm=01,…,12; aaaa=año  Quincenal
#'         mmaaaa   mm=01,…,12; aaaa=año              Mensual
#'         baaaa    b=1,…,6;    aaaa=año 	          Bimensual
#'         taaaa    t=1,…,4;    aaaa=año 	          Trimestral
#'         saaaa 	s=1,2;      aaaa=año 	          Semestral
#'         aaaa     aaaa=año 	                      Anual
#'}
#' 
#' @examples
#' RepoTimeTolubri('MM022014')
#' RepoTimeTolubri('MM22014')
#' 
#' @import lubridate 
#' 
#' @export
RepoTimeTolubri <- function(RepoTime){
    
    Months <- c(paste0('0', 1:9), 10:12)
    Years <- 1900:2100
    
    if (length(RepoTime) == 1) {

        if (substr(RepoTime, 1, 2) %in% c('QQ', 'QR')) {
            
            FortNight <- substr(RepoTime, 3, 3)
            if (!FortNight %in% c(1, 2)){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            Month <- substr(RepoTime, 4, 5)
            if(!Month %in% Months){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            
            Year <- substr(RepoTime, 6, 9)
            if (!Year %in% Years){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            if (FortNight == '1' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', Month, '-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + lubridate::weeks(2) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (FortNight == '2' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', Month, '-15'), tz ='Europe/Madrid')
                St_Instant_Month <- lubridate::ymd(paste0(Year, '-', Month, '-01'))
                End_Instant <- St_Instant_Month + months(1) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(output) <- RepoTime
            return(output)
        }  
        
        
        if (substr(RepoTime, 1, 2) %in% c('MM', 'MR')) {
            
            Month <- substr(RepoTime, 3, 4)
            if(!Month %in% Months){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            Year <- substr(RepoTime, 5, 8)
            if (!Year %in% Years){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            St_Instant <- lubridate::ymd(paste0(Year, '-', Month, '-01'), tz = 'Europe/Madrid')
            End_Instant <- St_Instant + months(1) - lubridate::days(1)
            tz(End_Instant) <- 'Europe/Madrid'
            lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            output <- list(lubriInterval)
            names(output) <- RepoTime
            return(output)
        }
        
        if (substr(RepoTime, 1, 2) %in% c('BB', 'BR')) {
            
            BiMonth <- substr(RepoTime, 3, 3)
            if (!BiMonth %in% 1:6){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            Year <- substr(RepoTime, 4, 7)
            if (!Year %in% Years){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            if (BiMonth == '1' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '01-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(2) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (BiMonth == '2' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '03-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(2) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (BiMonth == '3' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '05-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(2) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (BiMonth == '4' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '07-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(2) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (BiMonth == '5' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '09-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(2) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (BiMonth == '6' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '11-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(2) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(output) <- RepoTime
            return(output)
        }  
        
        if (substr(RepoTime, 1, 2) %in% c('TT', 'TR')) {
            
            Term <- substr(RepoTime, 3, 3)
            if (!Term %in% 1:4){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            Year <- substr(RepoTime, 4, 7)
            if (Term == '1' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '01-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(3) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (Term == '2' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '04-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(3) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (Term == '3' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '07-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(3) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (Term == '4' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '10-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(3) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(output) <- RepoTime
            return(output)
        }  
        
        if (substr(RepoTime, 1, 2) %in% c('SS', 'SR')) {
            
            Sem <- substr(RepoTime, 3, 3)
            if (!Sem %in% 1:2){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            Year <- substr(RepoTime, 4, 7)
            if (!Year %in% Years){
                
                cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
                return(invisible(NULL))
            }
            if (Sem == '1' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '01-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(6) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            if (Sem == '2' ) {
                St_Instant <- lubridate::ymd(paste0(Year, '-', '07-01'), tz ='Europe/Madrid')
                End_Instant <- St_Instant + months(6) - lubridate::days(1)
                tz(End_Instant) <- 'Europe/Madrid'
                lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            }
            output <- list(lubriInterval)
            names(output) <- RepoTime
            return(output)
        }  
        
        if (substr(RepoTime, 1, 2) %in% c('AA', 'AR')) {
            
            Year <- substr(RepoTime, 3, 6)
            
            St_Instant <- lubridate::ymd(paste0(Year, '-', '01-01'), tz ='Europe/Madrid')
            End_Instant <- St_Instant + years(1) - lubridate::days(1)
            tz(End_Instant) <- 'Europe/Madrid'
            lubriInterval <- lubridate::new_interval(St_Instant, End_Instant)
            output <- list(lubriInterval)
            names(output) <- RepoTime
            return(output)
        }
        cat('[RepoTimeTolubri RepoTime] El período indicado no se reconoce.\n\n')
        return(invisible(NULL))
        
    } else {
         
        output <- lapply(as.list(RepoTime), function(x){RepoTimeTolubri(x)[[1]]})
        names(output) <- RepoTime
        return(output)
        
    }
}