#' Get a database connection
#'
#' @param dbfile Path to the SQLite database file
#' @export
connect <- function(dbfile){
  return(DBI::dbConnect(RSQLite::SQLite(), dbfile))
}


#' Get Grambank table as tibble
#'
#' @param conn A database connection as obtained via `connect`
#' @param name CLDF component name (or CSV filename for custom tables)
#' @return tibble
#' @export
table <- function(conn, name){
  return(dplyr::tbl(conn, name))
}


#' @export
grouped_values <- function(conn, grouper, strategy){
  values <- table(conn, 'ValueTable')
  #
  # Now join grouper and select according to strategy
  #
  return(values)
}
