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
                
                if (length(dbListConnections(MySQL())) > 0)
                    .self$databaseConnection <- dbListConnections(MySQL())[1]
                
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
                    host <- unlist(databaseConfig[["host"]])
                    port <- unlist(databaseConfig[["port"]])
                    user <- unlist(databaseConfig[["user"]])
                    pwd  <- unlist(databaseConfig[["pwd"]])
                    db   <- unlist(databaseConfig[["db"]])
                    type <- unlist(databaseConfig[["type"]])
                    
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
                                .self$databaseConnection <- dbConnect(SQLite(),
                                                                      host     = host)
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
                if (length(dbListConnections(MySQL())) > 0)
                    .self$databaseConnection <- dbListConnections(MySQL())[1]
                
                if (is.null(databaseConnection))
                    createConnection()
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        disconnect = function() {
            tryCatch({
                if (length(dbListConnections(MySQL())) > 0)
                    .self$databaseConnection <- dbListConnections(MySQL())[1]
                
                if (!is.null(databaseConnection))
                    dbDisconnect(databaseConnection)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        beginTransaction = function() {
            tryCatch({
                if (length(dbListConnections(MySQL())) > 0)
                    .self$databaseConnection <- dbListConnections(MySQL())[1]
                
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
                if (length(dbListConnections(MySQL())) > 0)
                    databaseConnection <<- dbListConnections(MySQL())[1]
                
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
                if (length(dbListConnections(MySQL())) > 0)
                    .self$databaseConnection <- dbListConnections(MySQL())[1]
                
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
                if (length(dbListConnections(MySQL())) > 0)
                    .self$databaseConnection <- dbListConnections(MySQL())[1]
                
                if (is.null(databaseConnection)) {
                    connect()
                }
                
                print(paste0("[", as.character(Sys.Date(), format = "%d/%m/%Y"), "] [rarframeworkR:::DataContext$executeReader] [TRACE] - Executing SQL: ", sSql))

                resultStatement <- dbGetQuery(databaseConnection, sSql)
                rowCount        <- nrow(resultStatement)
                
                print(paste0("[", as.character(Sys.Date(), format = "%d/%m/%Y"), "] [rarframeworkR:::DataContext$executeReader] [TRACE] - ", rowCount," registro(s) encontrado(s)"))

                return (resultStatement)
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        executeQuery = function(sSql) {
            tryCatch({
                if (length(dbListConnections(MySQL())) > 0)
                    .self$databaseConnection <- dbListConnections(MySQL())[1]
                
                if (is.null(databaseConnection)) {
                    connect()
                }
                
                print(paste0("[", as.character(Sys.Date(), format = "%d/%m/%Y"), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - Executing SQL: ", sSql))

                resultStatement <- dbSendStatement(databaseConnection, sSql)
                rowCount        <- dbGetRowsAffected(resultStatement)
                
                print(paste0("[", as.character(Sys.Date(), format = "%d/%m/%Y"), "] [rarframeworkR:::DataContext$executeQuery] [TRACE] - ", rowCount," registro(s) afetado(s)"))
            }, error = function (ex) {
                stop (ex$message)
            })
        }

    )
)