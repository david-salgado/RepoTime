#' Método 'Seq' para la clase RepoTimeInt.
#' 
#' \code{Seq} genera una colección de objetos de clase \code{\linkS4class{RepoTimeInt}}
#' 
#' 
#' @param x Objeto de clase \code{\linkS4class{RepoTimeInt}}.
#' 
#' @param y Objeto de clase \code{\linkS4class{RepoTimeInt}}.
#'  
#' @return Objeto de clase \code{\linkS4class{RepoTimeInt}} el resultado de sumar
#' los periodos de tiempo.   
#'  
#' @examples
#' RepoPeriod1 <- newRepoTime('MM012015')
#' RepoPeriod2 <- newRepoTime('MM022016')
#' Seq(RepoPeriod1,RepoPeriod2)
#'  
#' @include RepoTimeInt-class.R
#' 
#' 
#' @export
setGeneric("Seq", function(x, y){standardGeneric("Seq")})

#' @rdname Seq
#' 
#' @include RepoTimeInt-class.R
#' 
#' 
#' @export
setMethod(
  f = "Seq",
  signature = c("RepoTimeInt"),
  definition = function(x, y){
    
    if (class(x) != 'RepoTimeInt' || class(y) != 'RepoTimeInt' || length(x@Repo) != 1 || length(y@Repo) != 1){
          stop('[Seq RepoTimeInt] Los parámetros de la función Seq deben ser objetos de clase RepoTimeInt de longitud 1.')}
    
    if (substr(x@Repo, 1, 2) != substr(y@Repo, 1, 2)) {stop('[Seq RepoTimeInt] Los parámetros de la función Seq deben ser objetos de clase RepoTime
con la misma referencia temporal (MM, QQ, etc)')}
      
      
    if (substr(x@Repo, 1, 2) %in% c('QQ', 'QR')){ byParam <- '15 days'}
    if (substr(x@Repo, 1, 2) %in% c('MM', 'MR')){ byParam <- '1 month'}
    if (substr(x@Repo, 1, 2) %in% c('BB', 'BR')){ byParam <- '2 month'}
    if (substr(x@Repo, 1, 2) %in% c('TT', 'TR')){ byParam <- '3 month'}
    if (substr(x@Repo, 1, 2) %in% c('EE', 'ER')){ byParam <- '6 month'}
    if (substr(x@Repo, 1, 2) %in% c('AA', 'AR')){ byParam <- '1 year'}
    
    startinterval <- seq(from = x@lubriInt[[1]]@start, to = y@lubriInt[[1]]@start, by = byParam)  
    
    monthduration <- new_duration(days = 30)
    seqInterval <- lapply(startinterval, function(x) as.interval(monthduration, x))
    output <- lapply(seqInterval,function(x) lubriToRepoTime(x))
    output <- lapply(output, newRepoTime)
    
    aux <- unlist(lapply(output, getRepo))
    rot <- aux[substr(aux, 3 , 4) == 12]
    if (length(rot)>0) {rot <- gsub('MM', 'MR', rot)
                        aux <- c(aux, rot)
                        output <- lapply(aux, newRepoTime)
    }
    
    names(output) <- unlist(lapply(output, getRepo))
    
    return(output)
  }
  
)