#' Classe para manipulação de String SQL para a função UPDATE
#'
#' @aliases SqlStatementUpdate
#' @importFrom methods setRefClass
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
                objectContext      <- ObjectContext$new(object);
                listProps          <- objectContext$getProperties();
                listPks            <- list();
                listNonPks         <- list();
                updateQueryBuilder <- UpdateQueryBuilder$new()
                updateQueryBuilder$addFrom(objectContext$getTableName())
                
                for (prop in listProps) {
                    if (!is.null(prop$value)) {
                        if (prop$primaryKey) {
                            listPks[length(listPks) + 1]       <- prop
                        } else {
                            listNonPks[length(listNonPks) + 1] <- prop
                        }
                    }
                }
                
                if (length(listPks) == 0)
                    stop ("Informar pelo menos 1 Primary Key")
                
                for (prop in listNonPks) {
                    updateQueryBuilder$addField(prop$fieldName);
                    updateQueryBuilder$addValue(prop$value);
                }
                
                for (prop in listPks) {
                    updateQueryBuilder$addWhere(c(prop$fieldName, " ", getQuotedValue(prop$value, prop$type)));
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
