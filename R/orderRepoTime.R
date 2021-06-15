#' @name orderRepoTime
#' 
#' @title Order of RepoTime Period
#' 
#' @description \code{orderRepoTime} returns a character vector with the order of the time periods 
#' in the input parameter.
#' 
#' @param RepoTime Character vector with time periods in repo notation.
#'
#' @return An integer vector with the same length as the input parameter RepoTime with the order of 
#' the time periods in the input parameter.
#'  
#' @examples
#' orderRepoTime(paste0('MM', c('04','06', '01'), '2016'))
#' orderRepoTime(paste0('QQ', rep(c('1', '2'), 3),c('04','06', '01'), '2016'))
#' orderRepoTime(paste0('AA', c('2016', '2001', '2014', '2012')))
#' 
#' @seealso \code{\link{RepoTimeInt-class}}, \code{\link{getlubriInt}}
#' 
#' @import data.table
#' 
#' @export
orderRepoTime <- function(RepoTime){
    
    FinalOrder <- NULL
    
    Prefix <- unique(substr(RepoTime, 1, 1))
    if (length(Prefix) != 1)
        stop()
    
    
    if (Prefix == "Q") {
        auxDT <- data.table(OrigOrder = seq(along = RepoTime),
                            Fortnight = as.integer(substr(RepoTime, 3, 3)),
                            Months = as.integer(substr(RepoTime, 4, 5)),
                            Years = as.integer(substr(RepoTime, 6, 9)),
                            Period = RepoTime)
        setkeyv(auxDT, c("Years", "Months", "Fortnight"))
        auxDT[, FinalOrder := seq(along = RepoTime)]
        setkeyv(auxDT, 'OrigOrder')
        output <- auxDT$FinalOrder
        return(output)
    }
    
    
    if (Prefix == "M") {
        auxDT <- data.table(OrigOrder = seq(along = RepoTime),
                            Months = as.integer(substr(RepoTime, 3, 4)),
                            Years = as.integer(substr(RepoTime, 5, 8)),
                            Period = RepoTime)
        setkeyv(auxDT, c("Years", "Months"))
        auxDT[, FinalOrder := seq(along = RepoTime)]
        setkeyv(auxDT, 'OrigOrder')
        output <- auxDT$FinalOrder
        return(output)
    }
    

    if (Prefix == "B") {
        auxDT <- data.table(OrigOrder = seq(along = RepoTime),
                            BiMonths = as.integer(substr(RepoTime, 3, 3)),
                            Years = as.integer(substr(RepoTime, 4, 7)),
                            Period = RepoTime)
        setkeyv(auxDT, c("Years", "BiMonths"))
        auxDT[, FinalOrder := seq(along = RepoTime)]
        setkeyv(auxDT, 'OrigOrder')
        output <- auxDT$FinalOrder
        return(output)
    }
    
    if (Prefix == "T") {
        auxDT <- data.table(OrigOrder = seq(along = RepoTime),
                            Term = as.integer(substr(RepoTime, 3, 3)),
                            Years = as.integer(substr(RepoTime, 4, 7)),
                            Period = RepoTime)
        setkeyv(auxDT, c("Years", "Term"))
        auxDT[, FinalOrder := seq(along = RepoTime)]
        setkeyv(auxDT, 'OrigOrder')
        output <- auxDT$FinalOrder
        return(output)
    }
    
    if (Prefix == "S") {
        auxDT <- data.table(OrigOrder = seq(along = RepoTime),
                            Semesters = as.integer(substr(RepoTime, 3, 3)),
                            Years = as.integer(substr(RepoTime, 4, 7)),
                            Period = RepoTime)
        setkeyv(auxDT, c("Years", "Semester"))
        auxDT[, FinalOrder := seq(along = RepoTime)]
        setkeyv(auxDT, 'OrigOrder')
        output <- auxDT$FinalOrder
        return(output)
    }
    
    if (Prefix == "A") {
        auxDT <- data.table(OrigOrder = seq(along = RepoTime),
                            Years = as.integer(substr(RepoTime, 3, 6)),
                            Period = RepoTime)
        setkeyv(auxDT, "Years")
        auxDT[, FinalOrder := seq(along = RepoTime)]
        setkeyv(auxDT, 'OrigOrder')
        output <- auxDT$FinalOrder
        return(output)
    }
    
}