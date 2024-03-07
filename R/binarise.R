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
    if ("0" %in% values) {
        stop("Feature contains zero-values which are not permitted.")
    }
    dplyr::case_match(values, "1" ~ "1", "2" ~ "0", "3" ~ "1", "?" ~ "?",  NA ~ NA)
}


binarise_GBXXX_to_GBXXXb_without_zero <- function(values) {
    if ("0" %in% values) {
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
    ValueTable %>% dplyr::filter(Parameter_ID == oldvariable) %>%
        dplyr::mutate(
            ID=paste0(newvariable, "-", Language_ID),
            Parameter_ID=newvariable,
            Value=func(Value)
        ) %>%
        dplyr::mutate(Code_ID = paste0(Parameter_ID, "-", Value)) %>%
        rbind(ValueTable)
}


#' Makes multi-state Grambank-features binary in the appropriate manner.
#'
#' @param ValueTable data frame of the ValueTable from grambank-cldf (long).
#' @param drop_multistate logical vector. If TRUE,the multistate parent features of the binarised features are dropped, only binary and/or binarised features remain. If FALSE, they are kept alongside their binarised versions.
#' @param keep_raw_binary logical vector. If TRUE and if the value table already contains some binarised features, they are kept. If false, they are overriden and replaced by values derived from the multi-state features. Note that raw binary coding and binarised coding principally differs in terms of ? and 0 coding. See note.
#' @param  trim_to_only_raw_binary logical vector. If TRUE, multi-state features are dropped and not binarised.
#' @note The Grambank questionnaire contains multi-state features, all related to word-order. They ask: "Is the order 1) X~Y, 2) Y~X or 3) both?". This function turns them into sets of two binary features: "Is the order X~Y?" and "Is the order Y~X?". If the multi-state feature is coded as "1", the binarised features are "1" and "0" respectively. Please note that absence is inferred, we recode to "1" and "0", not to "1" and "?". Since summer 2023, Grambank coders can also code the binary features from scratch, i.e. code the binary features directly and skip the multi-state. We call this "raw binary". If they find clear evidence for presence of one order but not as clear absence of the other, they may code "1" and "?". This means that released version after 1.0 has raw binary coding as well as multi-state coding which can be binarised, for the same phenomena for different languages. If you prefer to only have the recoded binarised feature values, set keep_raw_binary to FALSE. If you prefer to ONLY have the raw binary features, set trim_to_only_raw_binary to TRUE. If you prefer a mix, set keep_raw_binary to TRUE and trim_to_only_raw_binary to FALSE. The last option is the default. There are much fewer raw binary feature coding than there are multi-state-coding.
#'
#' @return Data-frame (long ValueTable)
#' @export
binarise <- function(ValueTable = NULL,
                     drop_multistate = TRUE,
                     keep_raw_binary = TRUE,
                     trim_to_only_raw_binary = FALSE){
    if (!inherits(ValueTable, "data.frame")) stop("'ValueTable' must be a dataframe.")


    if (trim_to_only_raw_binary == TRUE) {
        ValueTable <- ValueTable %>%
            dplyr::filter(!(Parameter_ID %in% multistate_parameters))

        if(!(any(Parameter_ID %in% binary_parameters))){
            stop("There is no raw binary coding at all.")
        }

    } else {

    if (keep_raw_binary == FALSE) {
        ValueTable <- ValueTable %>%
            dplyr::filter(!(Parameter_ID %in% binary_parameters))
    } else {
        ValueTable_raw_binary <- ValueTable %>%
            dplyr::filter(Parameter_ID %in% binary_parameters)
    }

    # BINARISING MULTISTATE FEATURES
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
            dplyr::anti_join(
                dplyr::select(ValueTable_raw_binary, Language_ID, Parameter_ID),
                     by = c("Language_ID", "Parameter_ID")) %>%
            dplyr::full_join(
                ValueTable_raw_binary,
                by = c("ID", "Language_ID", "Parameter_ID", "Value", "Code_ID", "Comment", "Source", "Source_comment", "Coders"))

    }
    if (drop_multistate == TRUE) {
        ValueTable <- ValueTable %>%
            dplyr::filter(!(Parameter_ID %in% multistate_parameters))
        }
    }
ValueTable
}