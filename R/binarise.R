binary_parameters <- c(
    "GB024a", "GB024b",
    "GB025a", "GB025b",
    "GB065a", "GB065b",
    "GB130a","GB130b",
    "GB193a","GB193b",
    "GB203a", "GB203b")

multistate_parameters <- c(
                          "GB024",
                          "GB025",
                          "GB065",
                          "GB130",
                          "GB193",
                          "GB203")

# functions for turning 4 of the multistate features into binarised version. These features don't have the 0 option.
#GB024 multistate 1; Num-N; 2: N-Num; 3: both.
#GB025 multistate 1: Dem-N; 2: N-Dem; 3: both.
#GB065 multistate 1:Possessor-Possessed; 2:Possessed-Possessor; 3: both
#GB130 multistate 1: SV; 2: VS; 3: both
binarise_GBXXX_to_GBXXXa_without_zero <- function(values) {
    if("0" %in% values){
        stop("Feature contains zero-values which are not permitted.")
    }

    dplyr::case_match(values, "1" ~ "1", "2" ~ "0", "3" ~ "1", "?" ~ "?",  NA ~ NA)
}

binarise_GBXXX_to_GBXXXb_without_zero <- function(values) {
    if("0" %in% values){
        stop("Feature contains zero-values which are not permitted.")
    }
        dplyr::case_match(values, "1" ~ "0", "2" ~ "1", "3" ~ "1",  "?" ~ "?", NA ~ NA)
}

# functions for turning 2 of the multistate features into binarised version. These features have the 0 option.
# we can just use this function for all multistate, since the other ones shouldn't legally have 0's in them at all. However, to be conservative I (Hedvig) separated them out so that if anything weird happens and somehow GB065 has a 0 value, the code breaks rather than does the wrong thing.

#GB193 multistate 0: they cannot be used attributively, 1: ANM-N; 2: N-ANM; 3: both.
#GB203 multistate 0: no UQ, 1: UQ-N; 2: N-UQ; 3: both.
binarise_GBXXX_to_GBXXXa_with_zero <- function(values) {
    dplyr::case_match(values, "0"~"0", "1" ~ "1", "2" ~ "0", "3" ~ "1", "?" ~ "?",  NA ~ NA)
}

binarise_GBXXX_to_GBXXXb_with_zero <- function(values) {
    dplyr::case_match(values, "0"~"0", "1" ~ "0", "2" ~ "1", "3" ~ "1",  "?" ~ "?", NA ~ NA)
}

gb_recode <- function(ValueTable, oldvariable, newvariable, func) {
    ValueTable %>% dplyr::filter(Parameter_ID==oldvariable) %>%
        dplyr::mutate(
            ID=paste0(newvariable, "-", Language_ID),
            Parameter_ID=newvariable,
            Value=func(Value)
        ) %>%
        mutate(Code_ID = paste0(Parameter_ID, "-", Value)) %>%
        rbind(ValueTable)
}


#' Makes multi-state Grambank-features binary in the appropriate manner.
#'
#' @param ValueTable data frame of the ValueTable from grambank-cldf (long).
#' @param drop_multistate If TRUE,the multistate parent features of the binarised features are kept. If FALSE, they are dropped from the resulting data-frame.
#' @param keep_raw_binary If TRUE and if the value table already contains some binarised features, they are kept. If false, they are overriden and replaced by values derived from the multistate features.
#' @return Data-frame (long ValueTable)
#' @export
binarise <- function(ValueTable = NULL, drop_multistate = TRUE, keep_raw_binary = TRUE){
    if (!inherits(ValueTable, "data.frame")) stop("'ValueTable' must be a dataframe.")

    # if there are binary features coded directly by coders, i.e. not derived from multistate features,
    # we call these "raw binary". users can choose to drop these and only used derived, or use a mix,
    if (keep_raw_binary == FALSE) {
        ValueTable <- ValueTable %>%
            dplyr::filter(!(Parameter_ID %in% binary_parameters))

    }

    if (keep_raw_binary == TRUE) {
        ValueTable_raw_binary <- ValueTable %>%
            dplyr::filter(Parameter_ID %in% binary_parameters)

    }



# BINARISING MULTISTATE FEATUYES

    ValueTable <- gb_recode(ValueTable, 'GB024', 'GB024a', binarise_GBXXX_to_GBXXXa_without_zero)
    ValueTable <- gb_recode(ValueTable, 'GB024', 'GB024b', binarise_GBXXX_to_GBXXXb_without_zero)
    ValueTable <- gb_recode(ValueTable, 'GB025', 'GB025a', binarise_GBXXX_to_GBXXXa_without_zero)
    ValueTable <- gb_recode(ValueTable, 'GB025', 'GB025b', binarise_GBXXX_to_GBXXXb_without_zero)
    ValueTable <- gb_recode(ValueTable, 'GB065', 'GB065a', binarise_GBXXX_to_GBXXXa_without_zero)
    ValueTable <- gb_recode(ValueTable, 'GB065', 'GB065b', binarise_GBXXX_to_GBXXXb_without_zero)
    ValueTable <- gb_recode(ValueTable, 'GB130', 'GB130a', binarise_GBXXX_to_GBXXXa_without_zero)
    ValueTable <- gb_recode(ValueTable, 'GB130', 'GB130b', binarise_GBXXX_to_GBXXXb_without_zero)
    ValueTable <- gb_recode(ValueTable, 'GB193', 'GB193a', binarise_GBXXX_to_GBXXXa_with_zero)
    ValueTable <- gb_recode(ValueTable, 'GB193', 'GB193b', binarise_GBXXX_to_GBXXXb_with_zero)
    ValueTable <- gb_recode(ValueTable, 'GB203', 'GB203a', binarise_GBXXX_to_GBXXXa_with_zero)
    ValueTable <- gb_recode(ValueTable, 'GB203', 'GB203b', binarise_GBXXX_to_GBXXXb_with_zero)



    if (keep_raw_binary == TRUE) {
        ValueTable <- ValueTable %>%
            dplyr::anti_join(dplyr::select(ValueTable_raw_binary,
                                           Language_ID, Parameter_ID),
                             by = join_by(Language_ID, Parameter_ID)) %>%
            full_join(ValueTable_raw_binary, by = join_by(ID, Language_ID, Parameter_ID, Value, Code_ID, Comment, Source, Source_comment, Coders))

    }
        if(drop_multistate == T) {
        ValueTable <- ValueTable %>%
            dplyr::filter(!(Parameter_ID %in% multistate_parameters))
        }

ValueTable
}