#' Clase S4 de intervalos temporales en la notación del repositorio
#'
#' Definición de una clase S4 \code{RepoTimeInt} de intervalos temporales en la
#' notación del repositorio:
#' QQ 	Quincenal
#' MM 	Mensual
#' BB 	Bimensual
#' TT 	Trimestral
#' EE 	Semestral
#' AA 	Anual
#'
#' La estructura de la clase S4 \code{RepoTimeInt} se compone de 2 slots:
#'
#' (i) el slot \code{Repo} que es un vector \code{character} con el intervalo
#' temporal en la notación del repositorio de microdatos;
#'
#' (ii) el slot \code{lubriInt} que es un intervalo temporal de clase
#' \linkS4class{lubridate:Interval} del paquete \code{lubridate}.
#'
#' @slot Repo vector \code{character} con el intervalo temporal en la notación del
#' repositorio de microdatos: PPp...p
#'
#' QQ 	Quincenal
#' MM 	Mensual
#' BB 	Bimensual
#' TT 	Trimestral
#' EE 	Semestral
#' AA 	Anual
#'
#' p….p 	Valores 	Periodicidad
#' qmmaaaa  q=1,2;      mm=01,…,12; aaaa=año  Quincenal
#' mmaaaa   mm=01,…,12; aaaa=año              Mensual
#' baaaa    b=1,…,6;    aaaa=año 	          Bimensual
#' taaaa    t=1,…,4;    aaaa=año 	          Trimestral
#' saaaa 	s=1,2;      aaaa=año 	          Semestral
#' aaaa     aaaa=año 	                      Anual
#'
#' @slot lubriInt Objeto de clase \code{\linkS4class{lubridate:Interval}}.
#'
#' @examples
#' x <- new(Class = 'RepoTimeInt', 'MM042015')
#' x
#' str(x)
#'
#' @include RepoTimeTolubri.R
#'
#' @import lubridate
#'
#' @export
setClass(Class = "RepoTimeInt",
         slots = c(Repo = 'character',
                   lubriInt = 'list'),
         validity = function(object){

            if (!all(unlist(lapply(object@lubriInt, class)) == 'Interval')) stop('[Validación RepoTime] El slot lubriInt debe ser una lista de Interval.')
            RepoTolubri <- RepoTimeTolubri(object@Repo)
            if (!identical(RepoTolubri, object@lubriInt))stop('[Validity RepoTimeInt] Ambos slots no coinciden.')

            return(TRUE)
         }
)
