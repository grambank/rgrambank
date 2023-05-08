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

#'
#'
#' @param conn A database connection as obtained via `connect`
#' @param grouper A table with two columns, `cldf_languageReference` and `group`
#' @param merge_strategy "random_merged"|"random_language"|"best_covered_language"
#' @export
merged_values <- function(conn, grouper, merge_strategy){
  values <- table(conn, 'ValueTable')
  #
  # Now join grouper and select according to strategy
  #
  return(values)
}
