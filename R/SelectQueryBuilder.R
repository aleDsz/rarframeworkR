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
                    sSql <- paste0("SELECT ", trimws(getFieldClause()))
                } else {
                    sSql <- "SELECT *"
                }
                
                if (length(fromList) > 0) {
                    sSql <- paste0(sSql, " FROM ", trimws(getFromClause()))
                }
                
                if (length(whereList) > 0) {
                    sSql <- paste0(sSql, " WHERE ", trimws(getWhereClause()))
                }
                
                if (length(groupList) > 0) {
                    sSql <- paste0(sSql, " GROUP BY ", trimws(getGroupByClause()))
                }
                
                return (gsub("\r\n", "", paste(sSql, collapse = " ")))
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
