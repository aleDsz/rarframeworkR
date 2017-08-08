#' Classe para manipulação de String SQL para a função UPDATE
#'
#' @aliases UpdateQueryBuilder
#' @importFrom methods setRefClass
#' @exportClass UpdateQueryBuilder
#'
UpdateQueryBuilder <- setRefClass(
    "UpdateQueryBuilder",
    
    contains = c("DefaultQueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- c("UPDATE ", getFromClause(), "\r\n")
                sSql <- c("   SET ", getSetClause(),  "\r\n")
                
                if (length(whereList) > 0) {
                    sSql <- c(sSql, " WHERE ", getWhereClause())
                }
                
                return (gsub("\r\n", "", paste(sSql, collapse = " ")))
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
