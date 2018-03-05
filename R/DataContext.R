#' Classe para acesso ao banco de dados
#' Manipulação de transações
#' Conexão
#' Execução de instrução SQL
#'
#' @aliases DataContext
#' @importFrom methods setRefClass
#' @importFrom jsonlite fromJSON
#' @import DBI
#' @import RMySQL
#' @import RMariaDB
#' @exportClass DataContext
#'
DataContext <- setRefClass(
    "DataContext",
    
    fields = list(
        databaseConnection = "DBIConnection",
        databaseName = "character"
    ),
    
    methods = list(

        initialize = function(databaseName = "common") {
            tryCatch({
                .self$databaseName <- databaseName
                
                if (is.null(.self$databaseConnection))
                    .self$createConnection()
            }, error = function (ex) {
                stop (ex$message)
            })
        },
        
        createConnection = function() {
            tryCatch({
                databaseConfig <- NULL
                
                if (file.exists(paste0(getwd(), "/databaseConfig.json")))
                    databaseConfig <- fromJSON(paste0(getwd(), "/databaseConfig.json"))
                
                if (is.list(databaseConfig)) {
                    
                    if (is.na(.self$databaseName)) {
                        .self$databaseName <- "common"
                    }
                    
                    host <- unlist(databaseConfig[[.self$databaseName]][["host"]])
                    port <- unlist(databaseConfig[[.self$databaseName]][["port"]])
                    user <- unlist(databaseConfig[[.self$databaseName]][["user"]])
                    pwd  <- unlist(databaseConfig[[.self$databaseName]][["pwd"]])
                    db   <- unlist(databaseConfig[[.self$databaseName]][["db"]])
                    type <- unlist(databaseConfig[[.self$databaseName]][["type"]])
                    
                    switch (type,
                            mysql   = {
                                .self$databaseConnection <- dbConnect(MySQL(), user = user, password = pwd, dbname = db, host = host, post = port)
                            },
                            
                            mariadb = {
                                .self$databaseConnection <- dbConnect(MariaDB(), user = user, password = pwd, dbname = db, host = host, port = port)
                            },
                            
                            sqlite  = {
                                .self$databaseConnection <- dbConnect(SQLite(), host = host)
                            },
                            
                            pgsql   = {
                                .self$databaseConnection <- dbConnect(PostgreSQL(), user = user, password = pwd, dbname = db, host = host, post = port)
                            }
                    )
                }
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        connect = function() {
            tryCatch({
                createConnection()
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        disconnect = function() {
            tryCatch({
                lapply(dbListConnections(MySQL()), dbDisconnect)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        beginTransaction = function() {
            tryCatch({
                if (is.null(.self$databaseConnection)) {
                    connect()
                }

                dbBegin(.self$databaseConnection)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        commitTransaction = function() {
            tryCatch({
                if (is.null(.self$databaseConnection)) {
                    stop ("Conexão não encontrada")
                }

                dbCommit(.self$databaseConnection)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        rollbackTransaction = function() {
            tryCatch({
                if (is.null(.self$databaseConnection)) {
                    stop ("Conexão não encontrada")
                }

                dbRollback(.self$databaseConnection)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        executeReader = function(sSql) {
            tryCatch({
                .self$connect()
                
                message(paste0("[", format(Sys.time(), "%d/%m/%Y %X"), "] [rarframeworkR:::DataContext$executeReader] [TRACE] - Executing SQL: ", sSql))

                resultStatement <- dbGetQuery(.self$databaseConnection, sSql)
                rowCount <- nrow(resultStatement)
                
                .self$disconnect()
                
                message(paste0("[", format(Sys.time(), "%d/%m/%Y %X"), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - ", rowCount, " row(s) affected"))

                return (resultStatement)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        executeQuery = function(sSql) {
            tryCatch({
                .self$connect()
                
                message(paste0("[", format(Sys.time(), "%d/%m/%Y %X"), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - Executing SQL: ", sSql))

                resultStatement <- dbSendStatement(.self$databaseConnection, sSql)
                rowCount <- dbGetRowsAffected(resultStatement)
                
                .self$disconnect()
                
                message(paste0("[", format(Sys.time(), "%d/%m/%Y %X"), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - ", rowCount, " row(s) affected"))
            }, error = function (ex) {
                stop (ex$message)
            })
        }

    )
)