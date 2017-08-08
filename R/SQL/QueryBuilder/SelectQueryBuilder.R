#' Classe para manipulação de String SQL para a função SELECT
#'
#' @aliases SelectQueryBuilder SelectQueryBuilder class
#' @importFrom methods setRefClass
#' @export SelectQueryBuilder SelectQueryBuilder class
#'
SelectQueryBuilder <- setRefClass(
    "SelectQueryBuilder",
    
    contains = c("QueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- character(0)
                
                if (length(fieldList) > 0) {
                    sSql <- c("SELECT ", getFieldClause(), "\r\n")
                } else {
                    sSql <- "SELECT *\r\n"
                }
                
                if (length(fromList) > 0) {
                    sSql <- c(sSql, "  FROM ", getFromClause(), "\r\n")
                }
                
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
