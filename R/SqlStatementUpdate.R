#' Classe para manipulação de String SQL para a função UPDATE
#'
#' @aliases SqlStatementUpdate
#' @importFrom methods setRefClass
#' @export SqlStatementUpdate SqlStatementUpdate
#' @exportClass SqlStatementUpdate
#'
SqlStatementUpdate <- setRefClass(
    "SqlStatementUpdate",
    
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
                objectContext      <- ObjectContext$new(object)
                listProps          <- objectContext$getProperties()
                listPks            <- list()
                listNonPks         <- list()
                updateQueryBuilder <- UpdateQueryBuilder$new()
                updateQueryBuilder$addFrom(objectContext$getTableName())
                
                for (prop in listProps) {
                    if (prop$primaryKey) {
                        if (length(prop$value) > 0 & prop$fieldName == "id") {
                            listPks <- c(listPks, prop)
                        } else {
                            listNonPks <- c(listNonPks, prop)
                        }
                    } else {
                        listNonPks <- c(listNonPks, prop)
                    }
                }
                
                if (length(listPks) == 0)
                    stop ("Informar pelo menos 1 Primary Key")
                
                for (prop in listNonPks) {
                    updateQueryBuilder$addField(prop$fieldName)
                    updateQueryBuilder$addValue(as.character(prop$value))
                }
                
                for (prop in listPks) {
                    updateQueryBuilder$addWhere(paste0(prop$fieldName, " ", getQuotedValue(as.character(prop$value), prop$type)))
                }
                
                sSql <<- updateQueryBuilder$toString()
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
