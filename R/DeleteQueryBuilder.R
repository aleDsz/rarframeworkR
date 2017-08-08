#' Classe para manipulação de String SQL para a função DELETE
#'
#' @aliases DeleteQueryBuilder
#' @importFrom methods setRefClass
#' @exportClass DeleteQueryBuilder
#'
DeleteQueryBuilder <- setRefClass(
    "DeleteQueryBuilder",
    
    contains = c("QueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- "DELETE \r\n"
                sSql <- c(sSql, "  FROM ", getFromClause(), "\r\n")
                
                if (length(whereList) > 0) {
                    sSql <- c(sSql, " WHERE ", getWhereClause())
                }
                
                return (paste(sSql, collapse = " "))
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
