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
#' lubriToRepoTime(RepoTimeTolubri('MM022014'))
#' lubriToRepoTime(RepoTimeTolubri('QQ1032015'))
#'
#' include RepoTimeTolubri.R
#'
#' @importFrom lubridate int_start days mday month year
#'
#' @export
lubriToRepoTime <- function(lubriInterval){

    if (length(lubriInterval) == 1){

        if (class(lubriInterval) != 'Interval') stop('[lubriToRepoTime RepoTime] El argumento no es un objeto de clase Interval del paquete lubridate.')

        IniTime <- int_start(lubriInterval)
        NDays <- lubriInterval %/% days(1)

        if (NDays <= 17) {

            PP <- 'QQ'
            MonthDay <- mday(IniTime)
            if (MonthDay <= 14) {

                p <- 1

            } else {

                p <- 2
            }
            Month <- month(IniTime)
            Month <- ifelse(nchar(Month) == 1, paste0('0', Month), Month)
            Year <- year(IniTime)
            RepoTime <- paste0(PP, p, Month, Year)
            return(RepoTime)

        }

        if (NDays > 17 & NDays <= 31){

            PP <- 'MM'
            Month <- month(IniTime)
            Month <- ifelse(nchar(Month) == 1, paste0('0', Month), Month)
            Year <- year(IniTime)
            RepoTime <- paste0(PP, Month, Year)
            return(RepoTime)

        }

        if (NDays > 31 & NDays <= 62){

            PP <- 'BB'
            Month <- month(IniTime)
            if (Month %in% 1:2) BiM <- 1
            if (Month %in% 3:4) BiM <- 2
            if (Month %in% 5:6) BiM <- 3
            if (Month %in% 7:8) BiM <- 4
            if (Month %in% 9:10) BiM <- 5
            if (Month %in% 11:12) BiM <- 6
            Year <- year (IniTime)
            RepoTime <- paste0(PP, BiM, Year)
            return(RepoTime)

        }

        if (NDays > 62 & NDays <= 93){

            PP <- 'TT'
            Month <- month(IniTime)
            if (Month %in% 1:3) Term <- 1
            if (Month %in% 4:6) Term <- 2
            if (Month %in% 7:9) Term <- 3
            if (Month %in% 10:12) Term <- 4
            Year <- year (IniTime)
            RepoTime <- paste0(PP, Term, Year)
            return(RepoTime)

        }

        if (NDays > 93 & NDays <= 186){

            PP <- 'SS'
            Month <- month(IniTime)
            if (Month %in% 1:6) Sem <- 1
            if (Month %in% 7:12) Sem <- 2
            Year <- year (IniTime)
            RepoTime <- paste0(PP, Sem, Year)
            return(RepoTime)

        }

        if (NDays > 186){

            PP <- 'AA'
            Year <- year (IniTime)
            RepoTime <- paste0(PP, Year)
            return(RepoTime)

        }
        cat('[lubriToRepoTime RepoTime] El intervalo temporal no se reconoce.')
        return(invisible(NULL))

    } else {

        output <- lapply(lubriInterval, lubriToRepoTime)
        return(output)

    }
}
