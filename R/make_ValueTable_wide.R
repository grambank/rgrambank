#' Makes a long cldf-value table wide.
#'
#' @param data-frame data-frame of cldf value table in long format
#'  

make_ValueTable_wide <- function(table = NULL){
  table %>% 
    dplyr::select(Language_ID, Parameter_ID, Value) %>% 
    spread(key = Parameter_ID, value = Value, drop = FALSE) 
}
