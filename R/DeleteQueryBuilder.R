#' Classe para manipulação de String SQL para a função DELETE
#'
#' @aliases DeleteQueryBuilder
#' @importFrom methods setRefClass
#' @export DeleteQueryBuilder DeleteQueryBuilder
#' @exportClass DeleteQueryBuilder
#'
DeleteQueryBuilder <- setRefClass(
    "DeleteQueryBuilder",
    
    contains = c("DefaultQueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- "DELETE"
                sSql <- paste0(sSql, "  FROM ", getFromClause())
                
                if (length(whereList) > 0) {
                    sSql <- paste0(sSql, " WHERE ", getWhereClause())
                }
                
                return (paste(sSql, collapse = " "))
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
