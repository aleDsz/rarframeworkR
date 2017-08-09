#' Fabricador da classe PDO para acessar aos drivers de banco de dados
#'
#' @aliases DataContextFactory
#' @importFrom methods setRefClass
#' @importFrom jsonlite fromJSON
#' @import DBI
#' @importFrom RMySQL MySQL
#' @importFrom RSQLite SQLite
#' @importFrom RPostgreSQL PostgreSQL
#' @exportClass DataContextFactory
#'
DataContextFactory <- setRefClass(
    "DataContextFactory",

    fields = list(

        databaseConfig = "list"

    ),

    methods = list(

        loadConfig = function() {
            tryCatch({
                if (file.exists(paste0(getwd(), "/databaseConfig.json")))
                    databaseConfig <<- fromJSON(paste0(getwd(), "/databaseConfig.json"))
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        getConnection = function() {
            tryCatch({
                databaseConfig    ->> databaseConfig
                databaseConnection <- NULL
                
                if (length(databaseConfig) == 0)
                    loadConfig()

                if (!is.list(databaseConfig))
                    stop("Arquivo de configuração não encontrado: databaseConfig.json")

                host <- databaseConfig["host"]
                port <- databaseConfig["port"]
                user <- databaseConfig["user"]
                pwd  <- databaseConfig["pwd"]
                db   <- databaseConfig["db"]
                type <- databaseConfig["type"]
                
                switch (type,
                    mysql   = {
                        databaseConnection <- DBI::dbConnect(MySQL(),
                                                             user     = user,
                                                             password = pwd,
                                                             dbname   = db,
                                                             host     = host,
                                                             post     = port)
                    },
                    
                    sqlite  = {
                        databaseConnection <- DBI::dbConnect(SQLite(),
                                                             host     = host)
                    },
                    
                    pgsql   = {
                        databaseConnection <- DBI::dbConnect(PostgreSQL(),
                                                             user     = user,
                                                             password = pwd,
                                                             dbname   = db,
                                                             host     = host,
                                                             post     = port)
                    }
                )
                
                return (databaseConnection)
            }, error = function (ex) {
                stop (ex$message)
            })
        }

    )
)
