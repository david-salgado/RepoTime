#' @title Constructor of objects of class \linkS4class{RepoTimeInt}
#' 
#' @description This constructor takes as argument a character vector with the 
#' timw intervals in the repo notation (see details) and returns the 
#' corresponding \linkS4class{RepoTimeInt} object.
#' 
#' @param Time a character vector with the time intervals in the repo notation.
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
#'  }
#' The span of years possibly covered by the repo notation has been internally 
#' fixed between the years 1 a.d. and 3000 a.d.
#' 
#' @return Object of class \linkS4class{RepoTimeInt}.
#' 
#' @examples
#' x <- newRepoTime(c('MM042015', 'MM052015')) 
#' x
#' str(x) 
#'
#' @include RepoTimeInt-class.R RepoTimeTolubri.R
#' 
#' @export
    newRepoTime <- function(Time){
    
    lubriInt <- RepoTimeTolubri(RepoTime = Time)
    output <- new(Class = 'RepoTimeInt', Repo = Time, lubriInt = lubriInt)
    return(value = output)
    
}