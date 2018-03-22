#' Classe para manipulação de String SQL para a função DELETE
#'
#' @aliases SqlStatementDelete
#' @importFrom methods setRefClass
#' @export SqlStatementDelete SqlStatementDelete
#' @exportClass SqlStatementDelete
#'
SqlStatementDelete <- setRefClass(
    "SqlStatementDelete",
    
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
                objectContext      <- ObjectContext$new(object);
                listProps          <- objectContext$getProperties();
                listPks            <- list();
                deleteQueryBuilder <- DeleteQueryBuilder$new()
                deleteQueryBuilder$addFrom(objectContext$getTableName())
                
                for (prop in listProps) {
                    if (length(prop$value) > 0) {
                        if (prop$primaryKey) {
                            listPks <- c(listPks, prop)
                        }
                    }
                }
                
                if (length(listPks) == 0)
                    stop ("Informar pelo menos 1 Primary Key")
                
                for (prop in listPks) {
                    deleteQueryBuilder$addWhere(paste0(prop$fieldName, " ", getQuotedValue(prop$value, prop$type)))
                }
                
                sSql <<- deleteQueryBuilder$toString()
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
