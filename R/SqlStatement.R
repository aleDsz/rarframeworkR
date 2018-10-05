#' Classe para manipulação de String SQL com métodos comuns
#' - Obter valores das propriedades pra utilizar no SQL
#'
#' @aliases SqlStatement
#' @importFrom methods setRefClass
#' @exportClass SqlStatement
#'
SqlStatement <- setRefClass(
    "SqlStatement",

    methods = list(

        getQuotedValue = function(propValue, type ) {
            tryCatch({
                sqlValue <- character(0)
                comma    <- ", "

                if (length(propValue) == 1) {
                    if (!is.null(propValue) & !is.null(type)) {
                        switch(type,
                               numeric = ,
                               integer = {
                                   sqlValue <- paste0("= ", propValue)
                               },
                               
                               logical = {
                                   if (propValue) {
                                       propValue <- 1
                                   } else {
                                       propValue <- 0
                                   }
                                   
                                   sqlValue <- paste0("= ", propValue)
                               },
                               
                               character = {
                                   if (startsWith(propValue, "%") | endsWith(propValue, "%")) {
                                       sqlValue <- paste0("LIKE ", trimws(shQuote(sqlValue, type = "sh")))
                                   } else {
                                       sqlValue <- paste0("= ", trimws(shQuote(propValue, type = "sh")))
                                   }
                               },
                               
                               Date = {
                                   sqlValue <- paste0("= ", trimws(shQuote(propValue, type = "sh")))
                               }
                        )
                    }
                } else {
                    if (length(propValue[is.na(propValue)]) == 0 & !is.null(type)) {
                        switch(type,
                               numeric = ,
                               integer = {
                                   sqlValue <- paste0("IN (")
                                   
                                   for (prop in propValue) {
                                       sqlValue <- paste0(sqlValue, prop, comma)
                                   }
                                   
                                   sqlValue <- substring(sqlValue, 1, nchar(sqlValue) - nchar(comma))
                                   sqlValue <- paste0(sqlValue, ")")
                               },
                               
                               logical = {
                                   sqlValue <- paste0("IN (")
                                   
                                   for (prop in propValue) {
                                       if (prop) {
                                           prop <- 1
                                       } else {
                                           prop <- 0
                                       }
                                       
                                       sqlValue <- paste0(sqlValue, prop, comma)
                                   }
                                   
                                   sqlValue <- substring(sqlValue, 1, nchar(sqlValue) - nchar(comma))
                                   sqlValue <- paste0(sqlValue, ")")
                               },
                               
                               character = {
                                   sqlValue <- paste0("IN (")
                                   
                                   for (prop in propValue) {
                                       sqlValue <- paste0(sqlValue, trimws(shQuote(prop, type = "sh")), comma)
                                   }
                                   
                                   sqlValue <- substring(sqlValue, 1, nchar(sqlValue) - nchar(comma))
                                   sqlValue <- paste0(sqlValue, ")")
                               },
                               
                               Date = {
                                   if (propValue[1] == propValue[2]) {
                                       sqlValue <- paste0("= ", trimws(shQuote(propValue[1], type = "sh")))
                                   } else {
                                       sqlValue <- paste0("BETWEEN ", trimws(shQuote(propValue[1], type = "sh")), " AND ", trimws(shQuote(propValue[2], type = "sh")))
                                   }
                               }
                        )
                    }
                }

                return (sqlValue)
            }, error = function(ex) {
                stop (ex$message)
            })
        }

    )
)
