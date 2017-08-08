#' Classe para manipulação de String SQL para a função DELETE
#'
#' @aliases SqlStatementDelete SqlStatementDelete class
#' @importFrom methods setRefClass
#' @export SqlStatementDelete SqlStatementDelete class
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
        }
        
        createSql = function(isList) {
            tryCatch({
                objectContext      <- ObjectContext$new(object);
                listProps          <- objectContext$getProperties();
                listPks            <- list();
                deleteQueryBuilder <- DeleteQueryBuilder$new()
                deleteQueryBuilder$addFrom(objectContext$getTableName())
                
                for (prop in listProps) {
                    if (!is.null(prop$value)) {
                        if (prop$primaryKey) {
                            listPks[length(listPks) + 1] <- prop
                        }
                    }
                }
                
                if (length(listPks) == 0)
                    stop ("Informar pelo menos 1 Primary Key")
                
                for (prop in listPks) {
                    deleteQueryBuilder$addWhere(c(prop$fieldName, " ", getQuotedValue(prop$value, prop$type)));
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
