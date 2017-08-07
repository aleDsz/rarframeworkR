#' Classe para acesso ao banco de dados
#' Manipulação de transações
#' Conexão
#' Execução de instrução SQL
#'
#' @aliases DataContext DataContext class
#' @importFrom methods setRefClass
#' @export DataContext DataContext class
#'
DataContext <- setRefClass(
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