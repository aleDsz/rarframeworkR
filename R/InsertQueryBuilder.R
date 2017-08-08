#' Classe para manipulação de String SQL para a função INSERT
#'
#' @aliases InsertQueryBuilder
#' @importFrom methods setRefClass
#' @exportClass InsertQueryBuilder
#'
InsertQueryBuilder <- setRefClass(
    "InsertQueryBuilder",
    
    contains = c("DefaultQueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- character(0)
                
                sSql <- c("INSERT INTO ", getFromClause(), " ")
                sSql <- c(sSql, "(", getFieldClause(), ")")
                sSql <- c(sSql, " VALUES ")
                sSql <- c(sSql, "(", getValueClause(), ")")
                
                return (gsub("\r\n", "", paste(sSql, collapse = " ")))
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
