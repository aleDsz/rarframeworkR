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
#' @export DataContext DataContext
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
                
                if (nchar(Sys.getenv("RARFRAMEWORK_CONFIG")) > 0) {
                    databaseConfig <- fromJSON(Sys.getenv("RARFRAMEWORK_CONFIG"))
                } else if (file.exists(paste0(getwd(), "/databaseConfig.json"))) {
                    databaseConfig <- fromJSON(paste0(getwd(), "/databaseConfig.json"))
                } else {
                    stop ("databaseConfig.json not found")
                }
                
                if (is.list(databaseConfig)) {
                    
                    if (is.na(.self$databaseName)) {
                        .self$databaseName <- "common"
                    }
                    
                    databaseConfig = databaseConfig[[.self$databaseName]]
                    
                    if (is.null(databaseConfig[["url"]])) {
                        host <- unlist(databaseConfig[["host"]])
                        port <- unlist(databaseConfig[["port"]])
                        user <- unlist(databaseConfig[["user"]])
                        pwd  <- unlist(databaseConfig[["pwd"]])
                        db   <- unlist(databaseConfig[["db"]])
                        type <- unlist(databaseConfig[["type"]])
                    } else if (databaseConfig[["url"]] != "") {
                        type <- strsplit(databaseConfig[["url"]], ":")[[1]][[1]]
                        db <- strsplit(databaseConfig[["url"]], "/")[[1]]
                        user <- strsplit(db[[3]], "@")[[1]][[1]]
                        host <- strsplit(db[[3]], "@")[[1]][[2]]
                        pwd <- strsplit(user, ":")[[1]][[2]]
                        user <- strsplit(user, ":")[[1]][[1]]
                        port <- strsplit(host, ":")[[1]][[2]]
                        host <- strsplit(host, ":")[[1]][[1]]
                        db <- db[[length(db)]]
                    }
                    
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
                            },
                            
                            postgres = {
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