#' Classe para manipulação de String SQL para a função SELECT
#'
#' @aliases QueryBuilder QueryBuilder class
#' @importFrom methods setRefClass
#' @export QueryBuilder QueryBuilder class
#'
QueryBuilder <- setRefClass(
    "QueryBuilder",
    
    fields = list(
        
        valueList = "character",
        fieldList = "character",
        fromList  = "character",
        whereList = "character"
        
    ),
    
    methods = list(
        
        addValue = function(value) {
            tryCatch({
                
                valueList <<- c(valueList, value)
                
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        addField = function(field) {
            tryCatch({
                
                fieldList <<- c(fieldList, field)
                
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        addFrom = function(from) {
            tryCatch({
                
                fromList <<- c(fromList, from)
                
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        addWhere = function(where) {
            tryCatch({
                
                whereList <<- c(whereList, where)
                
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getValueClause = function() {
            tryCatch({
                sSql  <- character(0)
                comma <- ", "
                
                for (value in valueList) {
                    sSql <- c(sSql, ifelse(is.na(value), "null", trimws(shQuote(value))), comma)
                }
                
                sSql <- substring(sSql, 1, nchar(sSql) - nchar(comma))
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getSetClause = function() {
            tryCatch({
                sSql  <- character(0)
                comma <- ",\r\n       "
                
                for (i in { 1 : length(valueList) }) {
                    sSql <- c(sSql, fieldList[i], " = ", ifelse(is.na(valueList[i]), "null", trimws(shQuote(valueList[i]))), comma)
                }
                
                sSql <- substring(sSql, 1, nchar(sSql) - nchar(comma))
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getFieldClause = function() {
            tryCatch({
                sSql  <- character(0)
                comma <- ", "
                
                for (field in fieldList) {
                    sSql <- c(sSql, field, comma)
                }
                
                sSql <- substring(sSql, 1, nchar(sSql) - nchar(comma))
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getFromClause = function() {
            tryCatch({
                sSql  <- character(0)
                comma <- ", "
                
                for (from in fromList) {
                    sSql <- c(sSql, from, comma)
                }
                
                sSql <- substring(sSql, 1, nchar(sSql) - nchar(comma))
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getWhereClause = function() {
            tryCatch({
                sSql   <- character(0)
                andSql <- "\r\n   AND "
                
                for (where in whereList) {
                    sSql <- c(sSql, where, andSql)
                }
                
                sSql <- substring(sSql, 1, nchar(sSql) - nchar(andSql))
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
