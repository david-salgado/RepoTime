#' @title S4 class \code{RepoTimeInt}
#' 
#' @description An S4 class \code{RepoTimeInt} of time intervals expressed in 
#' the so-called repo notation in this package is provided. The structure of the
#'  class comprises two attributes (slots):
#'
#' (i) attribute \code{Repo}, which is a character vector with the time interval 
#' expressed in the repo notation (see details);
#'
#' (ii) attribute \code{lubriInt}, which is a list of time intervals of class
#' \linkS4class{Interval} of the \link[lubridate]{lubridate-package}.
#'
#' @slot Repo Character vector with the time intervals expressed in the repo 
#' notation.
#'
#' @slot lubriInt List of objects of \linkS4class{Interval} class of the 
#' \link[lubridate]{lubridate-package}.
#'
#' @details  The repo notation of time intervals for this package amounts to 
#' denoting them by the string PPp...p, where
#' \itemize{
#'  \item PP is 
#'      \tabular{ll}{
#'          \strong{PP}\tab \strong{Periodicity}\cr
#'          QQ,QR\tab Fortnight (\emph{Quincena} in Spanish)\cr
#'          MM,MR\tab Month\cr
#'          BB,BR\tab Bimonth\cr
#'          TT,TR\tab Term\cr
#'          SS,SR\tab Semester\cr
#'          AA,AR\tab Year (\emph{A\~no} in Spanish)
#'      }
#'  \emph{R} stands for those cases where the data set contains a rotated sample.
#' \item p...p is
#'      \tabular{lll}{
#'         \strong{p...p}\tab \strong{Values}\tab \strong{Periodicity}\cr
#'         qmmyyyy\tab q=1,2; mm=01,...,12; yyyy=\emph{year}\tab Fortnight\cr
#'         mmyyyy\tab mm=01,...,12; yyyy=\emph{year}\tab Month\cr
#'         byyyy\tab b=1,...,6; yyyy=\emph{year}\tab Bimonth\cr
#'         tyyyy\tab t=1,...,4; yyyy=\emph{year}\tab Term\cr
#'         syyyy\tab s=1,2; yyyy=\emph{year}\tab Semester\cr
#'         yyyy\tab yyyy=\emph{year}\tab Year
#'       }
#'}
#' The span of years possibly covered by the repo notation has been internally 
#' fixed between the years 1 a.d. and 3000 a.d.
#'
#' @examples
#' # Empty object
#' x <- new(Class = 'RepoTimeInt')
#' x
#' str(x)
#' 
#' # Example with one time interval
#' RepoNot <- 'MM042015'
#' lubriNot <- RepoTimeTolubri('MM042015')
#' x <- new(Class = 'RepoTimeInt', Repo = RepoNot, lubriInt = lubriNot)
#' x
#' str(x)
#'
#' @seealso \code{\link{RepoTimeTolubri}}, \code{\link{lubriToRepoTime}}
#'
#' @include RepoTimeTolubri.R
#' 
#' @export
setClass(Class = "RepoTimeInt",
         slots = c(Repo = 'character',
                   lubriInt = 'list'),
         prototype = list(Repo = character(0), lubriInt = list()),
         validity = function(object){

            if (!all(
                unlist(x = lapply(X = object@lubriInt, class)) == 'Interval')) 
                stop('[RepoTime::validity RepoTimeInt] The slot lubriInt must be
                     a list of objects of class lubridate::Interval.')
            RepoTolubri <- RepoTimeTolubri(RepoTime = object@Repo,
                                           TimeZone = lubridate::tz(
                                               x = object@lubriInt[[1]]@start))
            if (!identical(x = RepoTolubri, 
                           y = object@lubriInt))
                stop('[RepoTime::validity RepoTimeInt] Slots do not correspond 
                     to each other.')

            return(value = TRUE)
         }
)
