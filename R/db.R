#' Get a database connection
#'
#' @param dbfile Path to the SQLite database file
#' @export
connect <- function(dbfile){
  return(DBI::dbConnect(RSQLite::SQLite(), dbfile))
}

#'
#'
#' @param conn A database connection as obtained via `connect`
#' @param grouper A table with two columns, `cldf_languageReference` and `group`
#' @param merge_strategy "random_merged"|"random_language"|"best_covered_language"
#' @export
merged_values <- function(conn, grouper, merge_strategy){
  values <- dplyr::tbl(conn, 'ValueTable')
  #
  # Now join grouper and select according to strategy
  #
  return(values)
}
