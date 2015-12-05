#' Muestra un objeto de clase RepoTimeInt.
#' 
#' \code{show.RepoTimeInt} muestra el slot \code{Repo} del input 
#' \code{\linkS4class{RepoTimeInt}}.
#' 
#' Esta función muestra únicamente los datos del slot \code{Repo} del objeto 
#' \code{RepoTimeInt} de entrada como un vector. 
#' 
#' Se trata en realidad del método \code{\linkS4class{show}} adaptado para la 
#' clase \code{\linkS4class{RepoTimeInt}}.
#' 
#' @param object Objeto de clase \code{\linkS4class{RepoTimeInt}} que se desea 
#' mostrar.
#'
#' @return Objeto de clase \code{\link{NULL}}.
#'  
#' @examples
#' show(new(Class = 'RepoTimeInt', 'TT12015'))
#' 
#' @include RepoTimeInt-class.R
#' 
#' @export
setMethod(
    f = "show",
    signature = c("RepoTimeInt"),
    function(object){
        
        show(object@Repo)
        
        invisible(NULL)
    }
)

