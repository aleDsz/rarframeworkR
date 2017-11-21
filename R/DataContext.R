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
#' @exportClass DataContext
#'
DataContext <- setRefClass(
    "DataContext",
    
    fields = list(
        
        databaseConnection = "MySQLConnection",
        databaseName       = "character"
        
    ),
    
    methods = list(

        initialize = function(databaseName) {
            tryCatch({
                .self$databaseName <- databaseName
                
                if (is.null(databaseConnection))
                    createConnection()
            }, error = function (ex) {
                stop (ex$message)
            })
        },
        
        createConnection = function() {
            tryCatch({
                databaseConfig     <- NULL
                
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
                                .self$databaseConnection <- dbConnect(MySQL(),
                                                                      user     = user,
                                                                      password = pwd,
                                                                      dbname   = db,
                                                                      host     = host,
                                                                      post     = port)
                            },
                            
                            sqlite  = {
                                .self$databaseConnection <- dbConnect(SQLite(), host = host)
                            },
                            
                            pgsql   = {
                                .self$databaseConnection <- dbConnect(PostgreSQL(),
                                                                      user     = user,
                                                                      password = pwd,
                                                                      dbname   = db,
                                                                      host     = host,
                                                                      post     = port)
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
                dbDisconnect(.self$databaseConnection)
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
                connect()
                
                print(paste0("[", format(Sys.time(), "%d/%m/%Y %X"), "] [rarframeworkR:::DataContext$executeReader] [TRACE] - Executing SQL: ", sSql))

                resultStatement <- dbGetQuery(databaseConnection, sSql)
                rowCount        <- nrow(resultStatement)
                
                disconnect()
                
                print(paste0("[", format(Sys.time(), "%d/%m/%Y %X"), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - ", rowCount," row(s) affected(s)"))

                return (resultStatement)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        executeQuery = function(sSql) {
            tryCatch({
                connect()
                
                print(paste0("[", format(Sys.time(), "%d/%m/%Y %X"), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - Executing SQL: ", sSql))

                resultStatement <- dbSendStatement(databaseConnection, sSql)
                rowCount        <- dbGetRowsAffected(resultStatement)
                
                disconnect()
                
                print(paste0("[", format(Sys.time(), "%d/%m/%Y %X"), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - ", rowCount," row(s) affected(s)"))
            }, error = function (ex) {
                stop (ex$message)
            })
        }

    )
)