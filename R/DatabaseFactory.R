#' Fabricador de instâncias de Database
#'
#' @aliases DatabaseFactory
#' @importFrom methods setRefClass
#' @exportClass DatabaseFactory
#'
DatabaseFactory <- setRefClass(
    "DatabaseFactory",
    
    fields = list(
        
        dataContextInstance = "DataContext"
        
    ),
    
    methods = list(
        
        getDataContextInstance = function(databaseName = "common") {
            tryCatch({
                if (is.null(.self$dataContextInstance)) {
                    .self$dataContextInstance <- DataContext$new(databaseName)
                }
                
                return (.self$dataContextInstance)
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)
