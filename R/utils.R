remove_leading_slash <- function(x) gsub("^/", "", x)

add_trailing_slash <- function(x) if (!grepl("/$", x)) paste0(x, "/") else x
