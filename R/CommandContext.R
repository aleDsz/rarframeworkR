#' Classe para execução de instruções SQL
#'
#' @aliases CommandContext
#' @importFrom methods setRefClass
#' @export CommandContext CommandContext
#' @exportClass CommandContext
#'
CommandContext <- setRefClass(
    "CommandContext",

    fields = list(

        command      = "ANY",
        dataContext  = "ANY"

    ),

    methods = list(

        initialize = function(sSql = character(1), databaseName = "common") {
            tryCatch({
                .self$dataContext  <- DataContext$new(databaseName)
                .self$command      <- sSql
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        setSql = function(sSql) {
            tryCatch({
                .self$command <- sSql
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        executeQuery = function() {
            tryCatch({
                dataContext$executeQuery(command)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        executeReader = function() {
            tryCatch({
                return (dataContext$executeReader(command))
            }, error = function (ex) {
                stop (ex$message)
            })
        }

    )
)