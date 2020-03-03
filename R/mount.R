new_query <- function () {
  setRefClass("rarframeworkR.Query",
              fields = list(
                from = "ANY",
                where = "ANY",
                order_by = "ANY"
              )
  )
  
  new("rarframeworkR.Query")
}

mount_query <- function (query = new_query(), ...) {
  dots <- list(...)
  properties <- names(dots)
  
  for (property in properties) {
    query[[property]] <- dots[[property]]
  }
  
  return (query)
}
