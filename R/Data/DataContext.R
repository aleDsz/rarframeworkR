#' Classe para acesso ao banco de dados
#' Manipulação de transações
#' Conexão
#' Execução de instrução SQL
#'
#' @aliases DataContext DataContext class
#' @importFrom methods setRefClass
#' @import DBI
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
                    dbDisconnect(databaseConnection)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        beginTransaction = function() {
            tryCatch({
                if (is.null(databaseConnection)) {
                    connect()
                }

                dbBegin(databaseConnection)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        commitTransaction = function() {
            tryCatch({
                if (is.null(databaseConnection)) {
                    stop ("Conexão não encontrada")
                }

                dbCommit(databaseConnection)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        rollbackTransaction = function() {
            tryCatch({
                if (is.null(databaseConnection)) {
                    stop ("Conexão não encontrada")
                }

                dbRollback(databaseConnection)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        executeReader = function(sSql) {
            tryCatch({
                if (is.null(databaseConnection)) {
                    connect()
                }

                rs        <- dbSendQuery(databaseConnection, sSql)
                fetchData <- dbFetch(rs)
                rowCount  <- dbGetRowCount(fetchData)
                dbClearResult(rs)

                return (fetchData)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        executeQuery = function(sSql) {
            tryCatch({
                if (is.null(databaseConnection)) {
                    connect()
                }

                rs       <- dbSendStatement(databaseConnection, sSql)
                rowCount <- dbGetRowsAffected(rs)

                dbClearResult(rs)
            }, error = function (ex) {
                stop (ex$message)
            })
        }

    )
)