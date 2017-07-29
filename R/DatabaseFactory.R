#' DatabaseFactory class
#'
#' @aliases DatabaseFactory DatabaseFactory
#' @export DatabaseFactory DatabaseFactory

DatabaseFactory <- methods::setRefClass(
    "DatabaseFactory",

    fields = list(

        dataContextInstance = "ANY"

    ),

    methods = list(

        getDataContextInstace = function() {
            tryCatch({
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