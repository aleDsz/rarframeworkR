#' Property class
#'
#' @aliases Property Property class
#' @importFrom methods setRefClass
#' @export Property Property class
#'
Property <- setRefClass(
    "Property",

    fields = list(

        fieldName	= "character",
        type		= "character",
        primaryKey	= "logical",
        value		= "ANY"

    ),

    methods = list(

        initialize = function() {
            fieldName	<<- character(1)
            type		<<- character(1)
            primaryKey	<<- FALSE
            value		<<- NULL
        },

        setValues = function(FieldName, Type, PrimaryKey, Value) {
            tryCatch({
                fieldName   <<- FieldName
                type        <<- Type
                primaryKey  <<- PrimaryKey
                value       <<- Value
            }, error = function(ex) {
                stop (ex$message)
            })
        }

    )
)
