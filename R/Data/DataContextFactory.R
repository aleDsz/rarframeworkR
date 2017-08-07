#' Fabricador da classe PDO para acessar aos drivers de banco de dados
#'
#' @aliases DataContextFactory DataContextFactory class
#' @importFrom methods setRefClass
#' @importFrom jsonlite fromJSON
#' @import DBI
#' @import RMySQL
#' @import RSQLite
#' @import RPostgreSQL
#' @import RSQLServer
#' @import rJava
#' @export DataContextFactory DataContextFactory class
#'
DataContextFactory <- setRefClass(
    "DataContextFactory",

    fields = list(

        databaseConfig = "list"

    ),

    methods = list(

        loadConfig = function() {
            tryCatch({
                databaseConfig <<- fromJSON("databaseConfig.json")
            }, error = function (ex) {
                stop (ex$message)
            })
        },

        getConnection = function() {
            tryCatch({
                loadConfig()

                if (is.null(databaseConfig))
                    stop("Arquivo de configuração não encontrado: databaseConfig.json")

                host = databaseConfig["host"];
                port = databaseConfig["port"];
                user = databaseConfig["user"];
                pwd  = databaseConfig["pwd"];
                db   = databaseConfig["db"];
                type = databaseConfig["type"];

                switch (type,
                    mariadb = ,
                    mysql   = return (dbConnect(MySQL(),
                                                user     = user,
                                                password = pwd,
                                                dbname   = db,
                                                host     = host,
                                                post     = port)),

                    sqlite  = return (dbConnect(SQLite(),
                                                host     = host)),

                    pgsql   = return (dbConnect(PostgreSQL(),
                                                user     = user,
                                                password = pwd,
                                                dbname   = db,
                                                host     = host,
                                                post     = port))
                )
            }, error = function (ex) {
                stop (ex$message)
            })
        },

    )
)