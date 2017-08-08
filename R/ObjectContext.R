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
                objeto     <- NULL
                listProps  <- getProperties()

                if (!is.null(dataReader) & nrow(dataReader) > 0) {
                    objeto <- new(getTableName())

                    for (prop in listProps) {
                        attr(object, prop$fieldName) <<- dataReader[1, prop$fieldName]
                    }
                }

                return (objeto)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getObjects = function(dataReader = data.frame()) {
            tryCatch({
                objeto      <- NULL
                object     ->> object
                listProps   <- getProperties()
                listObjects <- list()

                if (!is.null(dataReader) & nrow(dataReader) > 0) {
                    for (i in { 1 : nrow(dataReader) }) {
                        objeto <- new(getTableName())
                        
                        for (prop in listProps) {
                            attr(objeto, prop$fieldName) <- dataReader[i, prop$fieldName]
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
                        class(attr(object, propertyName)),
                        ifelse(propertyName == "id", TRUE, FALSE),
                        attr(object, propertyName)
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
                object ->> object

                for (i in { 1 : length(fieldNames) }) {
                    attr(object, fieldNames[i]) <<- dataFrame[1, fieldNames[i]]
                }

                return (object)
            }, error = function(ex) {
                stop (ex$message)
            })
        }

    )
)
