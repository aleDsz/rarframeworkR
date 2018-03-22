#' Classe para manipulação de String SQL para a função INSERT
#'
#' @aliases InsertQueryBuilder
#' @importFrom methods setRefClass
#' @export InsertQueryBuilder InsertQueryBuilder
#' @exportClass InsertQueryBuilder
#'
InsertQueryBuilder <- setRefClass(
    "InsertQueryBuilder",
    
    contains = c("DefaultQueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- paste0("INSERT INTO ", getFromClause(), " (", getFieldClause(), ") VALUES (", getValueClause(), ")")
                
                return (gsub("\r\n", "", paste(sSql, collapse = " ")))
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
