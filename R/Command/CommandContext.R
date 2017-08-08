#' Classe para execução de instruções SQL
#'
#' @aliases CommandContext CommandContext class
#' @importFrom methods setRefClass
#' @export CommandContext CommandContext class
#'
CommandContext <- setRefClass(
    "CommandContext",

    fields = list(

        command     = "ANY",
        dataContext = "ANY"

    ),

    methods = list(

        initialize = function(sSql = character(1)) {
            tryCatch({
                databaseFactory <- DatabaseFactory$new()
                dataContext    <<- databaseFactory$getDataContextInstance()
                command        <<- sSql
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        setSql = function(sSql) {
            tryCatch({
                command        <<- sSql
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