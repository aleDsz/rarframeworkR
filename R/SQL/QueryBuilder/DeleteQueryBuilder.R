#' Classe para manipulação de String SQL para a função DELETE
#'
#' @aliases DeleteQueryBuilder DeleteQueryBuilder class
#' @importFrom methods setRefClass
#' @export DeleteQueryBuilder DeleteQueryBuilder class
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
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
