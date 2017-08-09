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
        
        databaseConnection = "MySQLConnection"
        
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
                
                if (is.list(databaseConfig)) {
                    host <- unlist(databaseConfig[["host"]])
                    port <- unlist(databaseConfig[["port"]])
                    user <- unlist(databaseConfig[["user"]])
                    pwd  <- unlist(databaseConfig[["pwd"]])
                    db   <- unlist(databaseConfig[["db"]])
                    type <- unlist(databaseConfig[["type"]])
                    
                    switch (type,
                            mysql   = {
                                databaseConnection <<- dbConnect(MySQL(),
                                                                 user     = user,
                                                                 password = pwd,
                                                                 dbname   = db,
                                                                 host     = host,
                                                                 post     = port)
                            },
                            
                            sqlite  = {
                                databaseConnection <<- dbConnect(SQLite(),
                                                                 host     = host)
                            },
                            
                            pgsql   = {
                                databaseConnection <<- dbConnect(PostgreSQL(),
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

                rs        <- dbGetQuery(databaseConnection, sSql)
                rowCount  <- nrow(rs)
                
                print(paste0("[", as.character(Sys.Date()), "] [rarframeworkR:::DataContext$executeReader] [TRACE] - ", rowCount," registro(s) encontrado(s)"))

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
                
                print(paste0("[", as.character(Sys.Date()), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - ", rowCount," registro(s) afetado(s)"))
            }, error = function (ex) {
                stop (ex$message)
            })
        }

    )
)