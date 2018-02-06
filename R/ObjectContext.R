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
                listProps <- list()
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
                } else {
                    return (NULL)
                }

                return (.self$object)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getObjects = function(dataReader = data.frame()) {
            tryCatch({
                for (field in names(dataReader)) {
                    if (grepl("date", field)) {
                        dataReader[, field] <- as.Date(dataReader[, field])
                    }
                }
                
                return (dataReader)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getCustomAttributes = function(propertyName) {
            tryCatch({
                property <- NULL

                if (nchar(propertyName) > 0) {
                    property <- Property$new()

                    if (grepl("date", propertyName)) {
                        property$setValues(
                            propertyName,
                            "Date",
                            ifelse(grepl("id$", propertyName), TRUE, FALSE),
                            .self$object[[propertyName]]
                        )
                    } else {
                        property$setValues(
                            propertyName,
                            class(.self$object[[propertyName]]),
                            ifelse(grepl("id$", propertyName), TRUE, FALSE),
                            .self$object[[propertyName]]
                        )
                    }
                }

                return (property)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getTableName = function() {
            tryCatch({
                className <- unlist(as.list((.self$object$getClass())@className)[1])
                
                if (grepl("[.]", className)) {
                    return (as.list(strsplit(className, "[.]")[[1]])[[2]])
                } else {
                    return (className)
                }
            }, error = function(ex) {
                stop (ex$message)
            })
        },
        
        getDatabaseName = function() {
            tryCatch({
                className <- unlist(as.list((.self$object$getClass())@className)[1])
                
                if (grepl("[.]", className)) {
                    return (as.list(strsplit(className, "[.]")[[1]])[[1]])
                } else {
                    return ("common")
                }
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        fillObject = function(dataFrame) {
            tryCatch({
                listProps <- .self$getProperties()
                fieldNames <- as.list(names(dataFrame))
                
                for (i in { 1 : length(fieldNames) }) {
                    fieldName <- fieldNames[[i]]
                    prop <- listProps[sapply(listProps, function(x) x$fieldName == fieldName)][[1]]
                    
                    switch(prop$type,
                           numeric = {
                               if (!is.na(dataFrame[1, fieldName])) {
                                   .self$object[[fieldName]] <- as.numeric(dataFrame[[fieldName]])
                               }
                           },
                           
                           integer = {
                               if (!is.na(dataFrame[1, fieldName])) {
                                   .self$object[[fieldName]] <- as.integer(dataFrame[[fieldName]])
                               }
                           },
                           
                           character = {
                               if (!is.na(dataFrame[1, fieldName])) {
                                   .self$object[[fieldName]] <- as.character(dataFrame[[fieldName]])
                               }
                           },
                           
                           Date = {
                               if (!is.na(dataFrame[1, fieldName])) {
                                   .self$object[[fieldName]] <- as.Date(dataFrame[[fieldName]])
                               }
                           },
                           
                           POSIXct = {
                               if (!is.na(dataFrame[1, fieldName])) {
                                   .self$object[[fieldName]] <- as.POSIXct(dataFrame[[fieldName]])
                               }
                           }
                    )
                }

                return (.self$object)
            }, error = function(ex) {
                stop (ex$message)
            })
        }
    )
)
