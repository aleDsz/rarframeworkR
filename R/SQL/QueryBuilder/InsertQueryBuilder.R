#' Classe para manipulação de String SQL para a função INSERT
#'
#' @aliases InsertQueryBuilder InsertQueryBuilder class
#' @importFrom methods setRefClass
#' @export InsertQueryBuilder InsertQueryBuilder class
#'
InsertQueryBuilder <- setRefClass(
    "InsertQueryBuilder",
    
    contains = c("QueryBuilder"),
    
    methods = list(
        
        toString = function() {
            tryCatch({
                sSql <- character(0)
                
                sSql <- c("INSERT INTO ", getFromClause(), " ")
                sSql <- c(sSql, "(", getFieldClause(), ")")
                sSql <- c(sSql, " VALUES ")
                sSql <- c(sSql, "(", getValueClause(), ")")
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
