#' Classe para manipulação de String SQL com métodos comuns
#' - Obter valores das propriedades pra utilizar no SQL
#'
#' @aliases SqlStatement SqlStatement class
#' @importFrom methods setRefClass
#' @export SqlStatement SqlStatement class
#'
SqlStatement <- setRefClass(
    "SqlStatement",

    methods = list(

        getQuotedValue = function(propValue, type) {
            tryCatch({
                sqlValue <- character(0)
                comma    <- ", "

                if (!is.na(propValue) & !is.na(type)) {
                    switch(type,
                        numeric = ,
                        integer = {
                            if (is.array(propValue) | is.list(propValue)) {
                                sqlValue <- c("IN (")
                                
                                for (prop in propValue) {
                                    sqlValue <- c(sqlValue, prop, comma)
                                }
                                
                                sqlValue <- substring(sqlValue, 1, nchar(sqlValue) - nchar(comma))
                                sqlValue <- c(sqlValue, ")")
                            } else {
                                sqlValue <- propValue
                            }
                        }
                        
                        "default" = {
                            if (startsWith(propValue, "%") | endsWith(propValue, "%")) {
                                sqlValue <- c("LIKE ", trimws(shQuote(sqlValue)))
                            } else {
                                if (is.array(propValue) | is.list(propValue)) {
                                    sqlValue <- c("IN (")
                                    
                                    for (prop in propValue) {
                                        sqlValue <- c(sqlValue, trimws(shQuote(prop)), comma)
                                    }
                                    
                                    sqlValue <- substring(sqlValue, 1, nchar(sqlValue) - nchar(comma))
                                    sqlValue <- c(sqlValue, ")")
                                } else {
                                    sqlValue <- trimws(shQuote(sqlValue))
                                }
                            }
                        }
                    )
                }

                return (sqlValue)
            }, error = function(ex) {
                stop (ex$message)
            })
        }

    )
)
