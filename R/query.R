#' @examples 
#' from(
#'   schema,
#'   "x.id == 'abc'"
#' )
from <- function (table) {
  if (is_schema(table)) {
    table <- get_table_from_schema(table)
  }
  
  mount_query(from = table)
}

where <- function (.query, where) {
  where <- enquos(where)
  mount_query(.query, where = where)
}

order_by <- function (.query, ...) {
  order_by <- quos(...)
  mount_query(.query, order_by = order_by)
}
