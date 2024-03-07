#' Extract a wide dataframe from a grambank value table
#' @param ValueTable a ValueTable dataframe containing the columns Language_ID, Parameter_ID, and Value
#' @return a wide dataframe.
#' @importFrom dplyr %>%
#' @export
#' @examples
#' gb <- rcldf::cldf(system.file("tests/testthat/fixtures/testdata/", "StructureDataset-metadata.json", package = "rgrambank"))
#' values <- as_grambank_wide(gb$tables$ValueTable)
#' # or:
#' values <- gb$tables$ValueTable %>% as_grambank_wide()
as_grambank_wide <- function(ValueTable = NULL) {
    if (!inherits(ValueTable, "data.frame")) stop("'ValueTable' must be a dataframe.")

    if (!all(c('Language_ID', 'Parameter_ID', 'Value') %in% colnames(ValueTable))) {
        stop("Invalid table format - ValueTable needs to have columns Language_ID/Parameter_ID/Value")
    }

    ValueTable %>%
        dplyr::select("Language_ID", "Parameter_ID", "Value") %>%
        tidyr::spread(key = "Parameter_ID", value = "Value", drop = FALSE)
}
