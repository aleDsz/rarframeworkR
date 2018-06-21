#' Classe para armazenar metadados dos objetos
#'
#' @aliases Property
#' @importFrom methods setRefClass
#'
Property <- setRefClass(
    "Property",

    fields = list(
        fieldName = "character",
        type = "character",
        primaryKey = "logical",
        value = "ANY"
    ),

    methods = list(
        initialize = function() {
            .self$fieldName <- character(1)
            .self$type <- character(1)
            .self$primaryKey <- FALSE
            .self$value <- NULL
        },

        setValues = function(field_name, type, primary_key, value) {
            tryCatch({
                .self$fieldName <- field_name
                .self$type <- type
                .self$primaryKey <- primary_key
                .self$value <- value
            }, error = function(ex) {
                stop (ex$message)
            })
        }
    )
)
