#' ObjectContext class
#'
#' @aliases ObjectContext ObjectContext class
#' @importFrom methods setRefClass
#' @export ObjectContext ObjectContext class
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
                fieldNames <- as.list(names(dataFrame))
                objeto     <- NULL
                listProps  <- getProperties()

                if (!is.null(dataReader) & nrow(dataReader) > 0) {
                    objeto <- new(getTableName())

                    for (i in { 1 : length(listProps) }) {
                        object$field(listProps[i]$fieldName) <- dataReader[1, listProps[x]$fieldName]
                    }
                }

                return (objeto)
            }, error = function(ex) {
                stop (ex$message)
            })
        },

        getObjects = function(dataReader = data.frame()) {
            tryCatch({
                fieldNames  <- as.list(names(dataFrame))
                objeto      <- NULL
                listProps   <- getProperties()
                listObjects <- list()

                if (!is.null(dataReader) & nrow(dataReader) > 0) {
                    for (i in { 1 : nrow(dataReader) }) {
                        objeto <- new(getTableName())

                        for (x in { 1 : length(listProps) }) {
                            object$field(listProps[x]$fieldName) <- dataReader[i, listProps[x]$fieldName]
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

                if (nchar(propertyName) > 0) {
                    property <- Property$new()

                    property$setValues(
                        propertyName,
                        class(object$field(propertyName)),
                        ifelse(propertyName == "id", TRUE, FALSE),
                        object$field(propertyName)
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

                for (i in { 1 : length(fieldNames) }) {
                    object$field(fieldNames[i]) <- dataFrame[1, fieldNames[i]]
                }

                return (object)
            }, error = function(ex) {
                stop (ex$message)
            })
        }

    )
)
