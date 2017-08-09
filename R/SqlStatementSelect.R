#' Classe para manipulação de String SQL para a função SELECT
#'
#' @aliases SqlStatementSelect
#' @importFrom methods setRefClass
#' @exportClass SqlStatementSelect
#'
SqlStatementSelect <- setRefClass(
    "SqlStatementSelect",
    
    contains = c("SqlStatement"),
    
    fields = list(
      
        object = "ANY",
        sSql   = "character"
        
    ),
    
    methods = list(
        
        initialize = function(Object = NULL) {
            object <<- Object
            sSql   <<- character(1)
        },
        
        createSql = function(isList) {
            tryCatch({
                objectContext <- ObjectContext$new(object)
                listProps     <- objectContext$getProperties()
                
                if (!isList) {
                    countPks <- 0
                    
                    for (prop in listProps) {
                        if (!is.null(prop$value)) {
                            if (prop$primaryKey) {
                                countPks <- countPks + 1
                            }
                        }
                    }
                    
                    if (countPks == 0)
                        stop ("Informar pelo menos 1 Primary Key")
                }
                
                selectQueryBuilder <- SelectQueryBuilder$new()
                selectQueryBuilder$addFrom(objectContext$getTableName())
                
                for (prop in listProps) {
                    selectQueryBuilder$addField(prop$fieldName)
                    
                    if (length(prop$value) > 0) {
                        
                        selectQueryBuilder$addWhere(paste0(prop$fieldName, " ", getQuotedValue(prop$value, prop$type)))
                    }
                }
                
                sSql <<- selectQueryBuilder$toString()
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getSql = function(isList = F) {
            tryCatch({
                createSql(isList)
                
                return (sSql)
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
