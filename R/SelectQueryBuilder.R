#' Classe para manipulação de String SQL para a função SELECT
#'
#' @aliases SelectQueryBuilder
#' @importFrom methods setRefClass
#' @exportClass SelectQueryBuilder
#'
SelectQueryBuilder <- setRefClass(
    "SelectQueryBuilder",
    
    contains = c("DefaultQueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- character(0)
                
                if (length(fieldList) > 0) {
                    sSql <- c("SELECT ", trimws(getFieldClause()), "\r\n")
                } else {
                    sSql <- "SELECT *\r\n"
                }
                
                if (length(fromList) > 0) {
                    sSql <- c(sSql, " FROM ", trimws(getFromClause()), "\r\n")
                }
                
                if (length(whereList) > 0) {
                    sSql <- c(sSql, "WHERE ", trimws(getWhereClause()))
                }
                
                return (gsub("\r\n", "", paste(sSql, collapse = " ")))
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
