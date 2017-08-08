#' Classe para manipulação de String SQL para a função UPDATE
#'
#' @aliases UpdateQueryBuilder UpdateQueryBuilder class
#' @importFrom methods setRefClass
#' @export UpdateQueryBuilder UpdateQueryBuilder class
#'
UpdateQueryBuilder <- setRefClass(
    "UpdateQueryBuilder",
    
    contains = c("QueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- c("UPDATE ", getFromClause(), "\r\n")
                sSql <- c("   SET ", getSetClause(),  "\r\n")
                
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
