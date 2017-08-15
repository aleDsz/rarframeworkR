#' Classe para manipulação de objetos
#' - Obter propriedades
#' - Obter valores
#' - Obter objeto(s) populados
#'
#' @aliases ObjectContext
#' @importFrom methods setRefClass
#' @exportClass ObjectContext
#'
ObjectContext <- setRefClass(
    "ObjectContext",

    fields = list(

        object = "ANY"

    ),

    methods = list(

        initialize = function(Object = NULL) {
            .self$object <- Object
        },

        getProperties = function() {
            tryCatch({
                listProps  <- list()
                properties <- as.list(names((.self$object$getClass())@fieldClasses))

                if (length(properties) > 0) {
                    for (prop in properties) {
                        listProps <- c(listProps, getCustomAttributes(prop))
                    }
                }

                return (as.list(listProps))
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getObject = function(dataReader = data.frame()) {
            tryCatch({
                listProps <- getProperties()

                if (!is.null(dataReader) & nrow(dataReader) > 0) {
                    for (prop in listProps) {
                        .self$object[[prop$fieldName]] <- dataReader[1, prop$fieldName]
                    }
                }

                return (object)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getObjects = function(dataReader = data.frame()) {
            tryCatch({
                # TEMP FIX
                return (dataReader)
                
                objeto      <- NULL
                listProps   <- getProperties()
                listObjects <- list()

                if (!is.null(dataReader) & nrow(dataReader) > 0) {
                    for (i in { 1 : nrow(dataReader) }) {
                        objeto <- new(getTableName())
                        
                        for (prop in listProps) {
                            fieldName           <- prop$fieldName
                            objeto[[fieldName]] <- dataReader[i, prop$fieldName]
                        }

                        listObjects <- c(listObjects, objeto)
                    }
                }

                return (listObjects)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getCustomAttributes = function(propertyName) {
            tryCatch({
                property <- NULL

                if (nchar(propertyName) > 0) {
                    property <- Property$new()

                    property$setValues(
                        propertyName,
                        ifelse(grepl("date", propertyName), "Date", class(.self$object[[propertyName]])),
                        ifelse(grepl("id$", propertyName), TRUE, FALSE),
                        .self$object[[propertyName]]
                    )
                }

                return (property)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getTableName = function() {
            tryCatch({
                return (unlist(as.list((.self$object$getClass())@className)[1]))
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        fillObject = function(dataFrame) {
            tryCatch({
                listProps  <- getProperties()
                fieldNames <- as.list(names(dataFrame))
                
                for (i in { 1 : length(fieldNames) }) {
                    fieldName <- fieldNames[[i]]
                    prop      <- listProps[sapply(listProps, function(x) x$fieldName == fieldName)][[1]]
                    
                    if (length(prop) > 0) {
                        switch(prop$type,
                               numeric = {
                                   if (!is.na(dataFrame[1, fieldName])) {
                                       .self$object[[fieldName]] <- as.numeric(dataFrame[1, fieldName])
                                   }
                               },
                               
                               integer = {
                                   if (!is.na(dataFrame[1, fieldName])) {
                                       .self$object[[fieldName]] <- as.integer(dataFrame[1, fieldName])
                                   }
                               },
                               
                               character = {
                                   if (!is.na(dataFrame[1, fieldName])) {
                                       .self$object[[fieldName]] <- as.character(dataFrame[1, fieldName])
                                   }
                               },
                               
                               Date = {
                                   if (!is.na(dataFrame[1, fieldName])) {
                                       .self$object[[fieldName]] <- as.Date(dataFrame[1, fieldName])
                                   }
                               },
                               
                               POSIXct = {
                                   if (!is.na(dataFrame[1, fieldName])) {
                                       .self$object[[fieldName]] <- as.POSIXct(dataFrame[1, fieldName])
                                   }
                               }
                        )
                    }
                }

                return (.self$object)
            }, error = function(ex) {
                stop (ex$message)
            })
        }

    )
)
