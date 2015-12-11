#' @title S4 definition for class RepoTimeInt
#'
#' @description An S4 class \code{RepoTimeInt} of time intervals expressed in 
#' the so-called repo notation is provided. The structure of the class comprises
#' two attributes (slots):
#'
#' (i) slot \code{Repo}, which is a character vector with the time interval 
#' expressed in the repo notation (see details);
#'
#' (ii) slot \code{lubriInt}, which is a time interval of class interval of the
#' lubridate package
#'
#' @slot Repo character vector with the time intervals expressed in the repo 
#' notation
#'
#' @slot lubriInt object of class interval of the lubridate package
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
#'}
#' The span of years possibly covered by the repo notation has been internally 
#' fixed between the years 1 a.d. and 3000 a.d.
#'
#'
#' @examples
#' RepoNot <- 'MM042015'
#' lubriNot <- RepoTimeTolubri('MM042015')
#' x <- new(Class = 'RepoTimeInt', Repo = RepoNot, lubriInt = lubriNot)
#' x
#' str(x)
#'
#' @include RepoTimeTolubri.R
#'
#' @importFrom lubridate tz
#' 
#' @export
setClass(Class = "RepoTimeInt",
         slots = c(Repo = 'character',
                   lubriInt = 'list'),
         validity = function(object){

            if (!all(
                unlist(x = lapply(X = object@lubriInt, class)) == 'Interval')) 
                stop('[RepoTime::validity RepoTimeInt] The slot lubriInt must be
                     a list of objects of class lubridate::interval.')
            RepoTolubri <- RepoTimeTolubri(RepoTime = object@Repo,
                                           TimeZone = tz(
                                               x = object@lubriInt[[1]]@start))
            if (!identical(x = RepoTolubri, 
                           y = object@lubriInt))
                stop('[RepoTime::validity RepoTimeInt] Slots do not correspond 
                     to each other.')

            return(TRUE)
         }
)
