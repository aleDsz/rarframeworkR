#' DataContext class
#'
#' @aliases DataContext DataContext
#' @export DataContext DataContext

DataContext <- methods::setRefClass(
    "DataContext",

    fields = list(

        databaseConnection = "ANY"

    ),

    methods = list(

        initialize = function() {
            tryCatch({
                dataContextFactory <- DataContextFactory$new()
                databaseConnection <<- dataContextFactory$getConnection()
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        connect = function() {
            tryCatch({
                dataContextFactory <- DataContextFactory$new()
                databaseConnection <<- dataContextFactory$getConnection()
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        disconnect = function() {
            tryCatch({
                if (!is.null(databaseConnection))
                    databaseConnection$dbDisconnect()
            }, error = function (ex) {
                stop (ex$message)
            })
        },

    )
)