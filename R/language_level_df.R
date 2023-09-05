#' Reduce dialects and other duplicates which have the same glottcode in dataframe to one per language, and give it the glottocode of the parent that is at level language. Requires a cldf-value table and language-table. Simplifies combination of dataset.
#'
#' @param ValueTable data-frame, long format. ValueTable from cldf.
#' @param LanguageTable data-frame of a cldf LanguageTable. Needs to have columns Glottocode and Language_ID or Language_level_ID.
#' @param method character vector, choice between "singular_least_missing_data", "combine_random", "singular_random". combine_random = combine all datapoints for all the dialects/duplicates and if there is more than one datapoint for a given feature/word/variable choose randomly between them, singular_random = choose randomly between the dialects/duplicates, singular_least_missing_data = choose the dialect/duplicate which has the most datapoints.
#' @return data-frame of ValueTable without duplicates
#' @export

language_level_df <- function(ValueTable, LanguageTable,
                              method = c("singular_least_missing_data", "combine_random", "singular_random")) {

    if (length(method) != 1 & !(method %in% c("singular_least_missing_data", "combine_random", "singular_random"))) {
        stop("Method of merging is not defined.")
    }

    if (!all(c('Language_ID', 'Parameter_ID', 'Value') %in% colnames(ValueTable))) {
        stop("Invalid table format - ValueTable needs to have columns Language_ID/Parameter_ID/Value")
    }

    # The language level ID column is named different in different, CLDF datasets.
    # setting them to the same
    if (!("Language_level_ID" %in% colnames(LanguageTable))) {
        LanguageTable   <- LanguageTable %>%
            dplyr::select(Language_ID = Glottocode, Language_level_ID = Language_ID)
    }

    if ("Language_level_ID" %in% colnames(LanguageTable) & "ID" %in% colnames(LanguageTable)) {
        LanguageTable   <- LanguageTable %>%
            dplyr::select(Language_ID = Glottocode, Language_level_ID)
    }

    # if there is a missing language level ID, which it can be in some datasets where only
    # dialects get language level IDs and languages and families don't, then replace those
    # with the content in the Language_ID column.
    LanguageTable   <- LanguageTable %>%
        dplyr::mutate(Language_level_ID = ifelse(
            is.na(Language_level_ID) | Language_level_ID == "", Language_ID, Language_level_ID)
        )

    ## MERGE FOR LEAST MISSING DATA
    if (method == "singular_least_missing_data") {
        levelled_ValueTable <- ValueTable %>%
            dplyr::left_join(LanguageTable, by = "Language_ID") %>%
            dplyr::group_by(Language_ID) %>%
            dplyr::mutate(n = dplyr::n()) %>%
            dplyr::arrange(desc(n)) %>%
            dplyr::ungroup() %>%
            dplyr::distinct(Language_level_ID, .keep_all = T) %>%
            dplyr::distinct(Language_ID) %>%
            dplyr::inner_join(ValueTable, by = "Language_ID") %>%
            dplyr::left_join(LanguageTable, by = "Language_ID") %>%
            dplyr::select(-Language_ID) %>%
            dplyr::rename(Language_ID = Language_level_ID)

    # MERGE BY MAKING A FRANKENSTEIN COMBINATION OF ALL THE DIALECTS
    } else if (method == "combine_random") {

        #in order to do this, we need to first make it long again.
        # making vector of columns to make long

        ValueTable_grouped <- ValueTable %>%
            dplyr::filter(!is.na(Value)) %>%
            dplyr::left_join(LanguageTable, by = "Language_ID",
                      relationship = "many-to-many") %>%
            dplyr::group_by(Language_level_ID, Parameter_ID) %>%
            dplyr::mutate(n = dplyr::n()) %>%
            dplyr::ungroup() %>%
            dplyr::select(-Language_ID)

        # it's faster if we do slice_sample (choose randomly) only on those that have more than 1
        # value per language than if we do it on all.
        ValueTable_long_n_greater_than_1 <- ValueTable_grouped %>%
            dplyr::filter(n > 1) %>%
            dplyr::group_by(Language_level_ID, Parameter_ID) %>%
            dplyr::slice_sample(n = 1) %>%
            dplyr::ungroup()

        levelled_ValueTable <- ValueTable_grouped %>%
            dplyr::filter(n == 1) %>%
            dplyr::full_join(ValueTable_long_n_greater_than_1, by = join_by(
                ID, Parameter_ID, Value, Code_ID, Comment, Source, Source_comment, Coders, Language_level_ID, n))  %>%
            dplyr::mutate(Language_ID = Language_level_ID) %>%
            dplyr::select(-Language_level_ID, -n)

    # MERGE BY PICKING DIALECTS WHOLLY AT RANDOM
    } else if (method == "singular_random") {
        levelled_ValueTable <- ValueTable %>%
            dplyr::left_join(LanguageTable, by = "Language_ID") %>%
            dplyr::group_by(Language_level_ID) %>%
            dplyr::slice_sample(n = 1) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Language_ID = Language_level_ID) %>%
            dplyr::select(-Language_level_ID)
    } else {
        stop("invalid method")
    }

    levelled_ValueTable
}

