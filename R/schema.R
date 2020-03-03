is_schema <- function (table) {
  if (is.object(table)) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

get_table_from_schema <- function (schema) {
  class_name <- unlist(as.list((schema$getClass())@className)[1])
  
  if (grepl("[.]", class_name)) {
    class_name <- as.list(strsplit(class_name, "[.]")[[1]])
    
    return (class_name[[length(class_name)]])
  } else {
    return (class_name)
  }
}
