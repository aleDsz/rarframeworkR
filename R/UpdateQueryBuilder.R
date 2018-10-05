#' Classe para manipulação de String SQL para a função UPDATE
#'
#' @aliases UpdateQueryBuilder
#' @importFrom methods setRefClass
#' @export UpdateQueryBuilder UpdateQueryBuilder
#' @exportClass UpdateQueryBuilder
#'
UpdateQueryBuilder <- setRefClass(
    "UpdateQueryBuilder",
    
    contains = c("DefaultQueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- paste0("UPDATE ", getFromClause())
                sSql <- paste0(sSql, " SET ", getSetClause())
                
                if (length(whereList) > 0) {
                    sSql <- paste0(sSql, " WHERE ", getWhereClause())
                }
                
                return (gsub("\r\n", "", paste(sSql, collapse = " ")))
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
