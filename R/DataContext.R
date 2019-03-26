#' Classe para acesso ao banco de dados
#' Manipulação de transações
#' Conexão
#' Execução de instrução SQL
#'
#' @aliases DataContext
#' @importFrom methods setRefClass
#' @importFrom jsonlite fromJSON
#' @importFrom RJDBC JDBC
#' @importFrom RMongo mongoDbConnect
#' @import DBI
#' @export DataContext DataContext
#' @exportClass DataContext
#'
DataContext <- setRefClass(
    "DataContext",
    
    fields = list(
        databaseConnection = "DBIConnection",
        databaseName = "character",
        databaseDriver = "ANY"
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
                
                if (nchar(Sys.getenv("RARFRAMEWORK_ENV")) > 0) {
                    databaseConfig <- fromJSON(paste0(getwd(), "/configs/", Sys.getenv("RARFRAMEWORK_ENV"), ".json"))
                } else if (nchar(Sys.getenv("RARFRAMEWORK_CONFIG")) > 0) {
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
                    
                    host <- unlist(databaseConfig[[.self$databaseName]][["host"]])
                    port <- unlist(databaseConfig[[.self$databaseName]][["port"]])
                    user <- unlist(databaseConfig[[.self$databaseName]][["user"]])
                    pwd  <- unlist(databaseConfig[[.self$databaseName]][["pwd"]])
                    db   <- unlist(databaseConfig[[.self$databaseName]][["db"]])
                    type <- unlist(databaseConfig[[.self$databaseName]][["type"]])
                    
                    switch (type,
                            mysql = {
                                .self$databaseConnection <- dbConnect(RMySQL::MySQL(), user = user, password = pwd, dbname = db, host = host, post = port)
                                .self$databaseDriver <- RMySQL::MySQL()
                            },
                            
                            mariadb = {
                                .self$databaseConnection <- dbConnect(RMariaDB::MariaDB(), user = user, password = pwd, dbname = db, host = host, port = port)
                                .self$databaseDriver <- RMariaDB::MariaDB()
                            },
                            
                            sqlite = {
                                .self$databaseConnection <- dbConnect(RSQLite::SQLite(), host = host)
                                .self$databaseDriver <- RSQLite::SQLite()
                            },
                            
                            pgsql = {
                                .self$databaseConnection <- dbConnect(RPostgreSQL::PostgreSQL(), user = user, password = pwd, dbname = db, host = host, port = port)
                                .self$databaseDriver <- RPostgreSQL::PostgreSQL()
                            },
                            
                            cassandra = {
                                Cassandra <- JDBC("org.apache.cassandra.cql.jdbc.CassandraDriver", list.files("./lib", pattern = "jar$", full.names = T))
                                .self$databaseConnection <- dbConnect(Cassandra, paste0("jdbc:cassandra://", host, ":", port, "/", db))
                                .self$databaseDriver <- Cassandra
                            },
                            
                            cassandra = {
                                .self$databaseConnection <- RMongo::mongoDbConnect(db, host, port)
                                .self$databaseDriver <- NULL
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
                if (!is.null(.self$databaseDriver)) {
                    lapply(dbListConnections(.self$databaseDriver), dbDisconnect)
                }
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
