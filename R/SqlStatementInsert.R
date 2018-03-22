#' Classe para manipulação de String SQL para a função INSERT
#'
#' @aliases SqlStatementInsert
#' @importFrom methods setRefClass
#' @export SqlStatementInsert SqlStatementInsert
#' @exportClass SqlStatementInsert
#'
SqlStatementInsert <- setRefClass(
    "SqlStatementInsert",
    
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
                insertQueryBuilder <- InsertQueryBuilder$new()
                insertQueryBuilder$addFrom(objectContext$getTableName())
                
                for (prop in listProps) {
                    insertQueryBuilder$addField(prop$fieldName)
                    insertQueryBuilder$addValue(prop$value)
                }
                
                sSql <<- insertQueryBuilder$toString()
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
