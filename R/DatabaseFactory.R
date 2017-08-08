#' DatabaseFactory class
#'
#' @aliases DatabaseFactory DatabaseFactory class
#' @importFrom methods setRefClass
#' @export DatabaseFactory DatabaseFactory class
#'
DatabaseFactory <- setRefClass(
    "DatabaseFactory",

    fields = list(

        dataContextInstance = "ANY"

    ),

    methods = list(

        getDataContextInstance = function() {
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