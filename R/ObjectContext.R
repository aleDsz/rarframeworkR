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
            object <<- Object
        },

        getProperties = function() {
            tryCatch({
                object    ->> object
                listProps  <- list()
                properties <- as.list(names((object$getClass())@fieldClasses))

                if (length(properties) > 0) {
                    for (i in { 1 : length(properties) }) {
                        listProps[i] <- getCustomAttributes(unlist(properties[i]))
                    }
                }

                return (listProps)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getObject = function(dataReader = data.frame()) {
            tryCatch({
                object    ->> object
                listProps  <- getProperties()

                if (!is.null(dataReader) & nrow(dataReader) > 0) {
                    for (prop in listProps) {
                        fieldName            <- prop$fieldName
                        object[[fieldName]] <<- dataReader[1, prop$fieldName]
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
                object     ->> object
                listProps   <- getProperties()
                listObjects <- list()

                if (!is.null(dataReader) & nrow(dataReader) > 0) {
                    for (i in { 1 : nrow(dataReader) }) {
                        objeto <- new(getTableName())
                        
                        for (prop in listProps) {
                            fieldName           <- prop$fieldName
                            objeto[[fieldName]] <- dataReader[i, prop$fieldName]
                        }

                        listObjects[i] <- objeto
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
                object  ->> object

                if (nchar(propertyName) > 0) {
                    property <- Property$new()

                    property$setValues(
                        propertyName,
                        class(object[[propertyName]]),
                        ifelse(grepl("id$", propertyName), TRUE, FALSE),
                        object[[propertyName]]
                    )
                }

                return (property)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getTableName = function() {
            tryCatch({
                return (unlist(as.list((object$getClass())@className)[1]))
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        fillObject = function(dataFrame = data.frame()) {
            tryCatch({
                fieldNames <- as.list(names(dataFrame))
                object    ->> object

                for (fieldName in fieldNames) {
                    object[[fieldName]] <<- dataFrame[1, fieldName]
                }

                return (object)
            }, error = function(ex) {
                stop (ex$message)
            })
        }

    )
)
