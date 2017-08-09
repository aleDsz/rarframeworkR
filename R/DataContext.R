#' Classe para acesso ao banco de dados
#' Manipulação de transações
#' Conexão
#' Execução de instrução SQL
#'
#' @aliases DataContext
#' @importFrom methods setRefClass
#' @importFrom jsonlite fromJSON
#' @import DBI
#' @exportClass DataContext
#'
DataContext <- setRefClass(
    "DataContext",

    fields = list(

        databaseConnection = "ANY"

    ),

    methods = list(

        initialize = function() {
            tryCatch({
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
                
                databaseConnection ->> databaseConnection
                
                if (is.list(databaseConfig)) {
                    host <- databaseConfig["host"]
                    port <- databaseConfig["port"]
                    user <- databaseConfig["user"]
                    pwd  <- databaseConfig["pwd"]
                    db   <- databaseConfig["db"]
                    type <- databaseConfig["type"]
                    
                    switch (type,
                            mysql   = {
                                databaseConnection <<- DBI::dbConnect(MySQL(),
                                                                      user     = user,
                                                                      password = pwd,
                                                                      dbname   = db,
                                                                      host     = host,
                                                                      post     = port)
                            },
                            
                            sqlite  = {
                                databaseConnection <<- DBI::dbConnect(SQLite(),
                                                                      host     = host)
                            },
                            
                            pgsql   = {
                                databaseConnection <<- DBI::dbConnect(PostgreSQL(),
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
                rowCount  <- dbGetRowCount(rs)
                fetchData <- dbFetch(rs)
                dbClearResult(rs)
                
                cat("[", as.character(Sys.Date()), "] [rarframeworkR:::DataContext$executeReader] [TRACE] - ", rowCount," registro(s) encontrado(s)")

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
                
                cat("[", as.character(Sys.Date()), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - ", rowCount," registro(s) afetado(s)")

                dbClearResult(rs)
            }, error = function (ex) {
                stop (ex$message)
            })
        }

    )
)