#' Classe para manipulação de String SQL para a função SELECT
#'
#' @aliases QueryBuilder
#' @importFrom methods setRefClass
#' @exportClass QueryBuilder
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
        
        initialize = function () {
            tryCatch({
                
                valueList  <<- character(1)
                fieldList  <<- character(1)
                fromList   <<- character(1)
                whereList  <<- character(1)
                
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
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
                sSql       <- character(0)
                comma      <- ", "
                valueList ->> valueList
                sSql       <- paste(trimws(shQuote(valueList)), collapse = comma)
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getSetClause = function() {
            tryCatch({
                sSql       <- character(0)
                comma      <- ",\r\n       "
                valueList ->> valueList
                fieldList ->> fieldList
                sSql       <- paste(fieldList, " = ", valueList, collapse = comma)
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getFieldClause = function() {
            tryCatch({
                comma      <- ", "
                fieldList ->> fieldList
                sSql       <- paste(fieldList, collapse = comma)
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getFromClause = function() {
            tryCatch({
                comma     <- ", "
                fromList ->> fromList
                sSql      <- paste(fromList, collapse = comma)
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getWhereClause = function() {
            tryCatch({
                andSql     <- "\r\n   AND "
                whereList ->> whereList
                sSql       <- paste(whereList, collapse = andSql)
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
