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
        
        getDataContextInstance = function() {
            tryCatch({
                dataContextInstance ->> dataContextInstance
                
                if (is.null(dataContextInstance)) {
                    dataContextInstance <<- DataContext$new()
                }
                
                return (dataContextInstance)
            }, error = function(ex) {
                stop (ex$message)
            })
        }
        
    )
)