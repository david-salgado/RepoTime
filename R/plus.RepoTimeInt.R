#' Método '+' para la clase RepoTimeInt.
#'
#' \code{+} suma a un objeto de clase \code{\linkS4class{RepoTimeInt}} un objeto
#' de clase \code{\linkS4class{lubridate::Period}} del paquete \code{lubridate}
#' con el plazo temporal de duración especificada como input.
#'
#' Este método sobrecarga el operador \link{+} y construye un nuevo objeto
#' \code{RepoTimeInt}.
#'
#' @param e1 Objeto de clase \code{\linkS4class{RepoTimeInt}}.
#'
#' @param e2 Objeto de clase \code{\linkS4class{lubridate::Period}}.
#'
#' @return Objeto de clase \code{\linkS4class{RepoTimeInt}} el resultado de sumar
#' al período inicial el plazo temporal especificado.
#'
#' @examples
#' RepoPeriod <- newRepoTime('MM022015')
#' RepoPeriod + months(1)
#' RepoPeriod + years(1)
#' RepoPeriod + weeks(1)
#'
#' @include RepoTimeInt-class.R
#'
#' @import lubridate
#'
#' @export
setMethod(
    f = "+",
    signature = c("RepoTimeInt", "Period"),
    definition = function(e1, e2){

      RepoTimeString <- function(i){

        ThisInstant <- St_Instant[[i]]

        if (substr(St_Repo[i], 1, 2) == 'QQ'){

          FortNight <- ifelse(lubridate::day(ThisInstant) > 14, '2', '1')
          if (day(ThisInstant) < 29) {
            ThisInstant <- lubridate::floor_date(ThisInstant, 'month')
          } else {
            ThisInstant <- lubridate::ceiling_date(ThisInstant, 'month')
          }
          Month <- month(ThisInstant)
          if (nchar(Month) == 1) Month <- paste0('0', Month)
          Year <- year(ThisInstant)
          output <- paste0('QQ', FortNight, Month, Year)
        }
        if (substr(St_Repo[i], 1, 2) == 'BB'){

          if (month(ThisInstant) %in% c(1, 2))   BiMonth <- 1
          if (month(ThisInstant) %in% c(3, 4))   BiMonth <- 2
          if (month(ThisInstant) %in% c(5, 6))   BiMonth <- 3
          if (month(ThisInstant) %in% c(7, 8))   BiMonth <- 4
          if (month(ThisInstant) %in% c(9, 10))  BiMonth <- 5
          if (month(ThisInstant) %in% c(11, 12)) BiMonth <- 6
          Year <- year(ThisInstant)
          output <- paste0('BB', BiMonth, Year)
        }
        if (substr(St_Repo[i], 1, 2) == 'MM'){

          Month <- month(ThisInstant)
          if (nchar(Month) == 1) Month <- paste0('0', Month)
          Year <- year(ThisInstant)
          output <- paste0('MM', Month, Year)
        }
        if (substr(St_Repo[i], 1, 2) == 'TT'){

          if (month(ThisInstant) %in% c(1, 2, 3))   Term <- 1
          if (month(ThisInstant) %in% c(4, 5, 6))   Term <- 2
          if (month(ThisInstant) %in% c(7, 8, 9))   Term <- 3
          if (month(ThisInstant) %in% c(10, 11, 12)) Term <- 4
          Year <- year(ThisInstant)
          output <- paste0('TT', Term, Year)
        }
        if (substr(St_Repo[i], 1, 2) == 'SS'){

          if (month(ThisInstant) %in% 1:6)  Sem <- 1
          if (month(ThisInstant) %in% 7:12) Sem <- 2
          Year <- year(ThisInstant)
          output <-paste0('SS', Sem, Year)
        }
        if (substr(St_Repo[i], 1, 2) == 'AA'){

          Year <- year(ThisInstant)
          output <- paste0('AA', Year)
        }

        return(output)

    }


    St_Instant <- lapply(e1@lubriInt, function(x){lubridate::int_start(x) + e2 })
    St_Repo <- e1@Repo

    output <- unlist(lapply(seq(along = e1@Repo), RepoTimeString))
    output <- newRepoTime(output)
    return(output)


    }
)
