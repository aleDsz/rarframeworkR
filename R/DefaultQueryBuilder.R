#' Classe para manipulação de String SQL genérica
#'
#' @aliases DefaultQueryBuilder
#' @importFrom methods setRefClass
#' @export DefaultQueryBuilder DefaultQueryBuilder
#' @exportClass DefaultQueryBuilder
#'
DefaultQueryBuilder <- setRefClass(
    "DefaultQueryBuilder",
    
    fields = list(
        
        valueList = "character",
        fieldList = "character",
        fromList  = "character",
        whereList = "character",
        groupList = "character"
        
    ),
    
    methods = list(
        
        initialize = function () {
            tryCatch({
                
                valueList  <<- character(0)
                fieldList  <<- character(0)
                fromList   <<- character(0)
                whereList  <<- character(0)
                groupList  <<- character(0)
                
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
        
        addGroup = function(group) {
            tryCatch({
                
                groupList <<- c(groupList, group)
                
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
                sSql       <- paste(trimws(shQuote(.self$valueList, type = "sh")), collapse = comma, sep = "")
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getSetClause = function() {
            tryCatch({
                sSql       <- character(0)
                comma      <- ", "
                sSql       <- paste(.self$fieldList, " = ", trimws(paste0("'", .self$valueList, "'")), collapse = comma, sep = "")
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getFieldClause = function() {
            tryCatch({
                comma      <- ", "
                sSql       <- paste(.self$fieldList, collapse = comma, sep = "")
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getGroupByClause = function() {
            tryCatch({
                comma      <- ", "
                sSql       <- paste(.self$groupList, collapse = comma, sep = "")
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getFromClause = function() {
            tryCatch({
                comma     <- ", "
                sSql      <- paste(.self$fromList, collapse = comma, sep = "")
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getWhereClause = function() {
            tryCatch({
                andSql     <- " AND "
                sSql       <- paste(.self$whereList, collapse = andSql, sep = "")
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
