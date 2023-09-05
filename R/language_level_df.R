#' Reduce duplicates which have the same glottcode in a CLDF-datset ValueTable to one. Simplifies combination of datasets.
#'
#' @param ValueTable data-frame, long format. ValueTable from cldf.
#' @param LanguageTable data-frame of a cldf LanguageTable from the same cldf-dataset as ValueTable. Needs to minimally have the columns "ID" (for matching to ValueTable) and "Glottocode" (for identification of duplicates).
#' @param merge_dialects logical. In the case of multiple dialects of the same language, if TRUE they are replaced by the glottocode of their language-parent and all but one is dropped as with other duplicate glottocodes.
#' @param LanguageTable2 data-frame. If merge_dialects is TRUE and LanguageTable does not have the columns "Language_ID" or "Language_level_ID", then the function will need an additional LanguageTable with the necessary columns and it should be supplied here. Needs to minimally have the columns "Glottocode" and "Language_ID" or "Language_level_ID". Glottolog-cldf LanguageTable recommended.
#' @param method character vector, choice between "singular_least_missing_data", "combine_random", "singular_random". combine_random = combine all datapoints for all the dialects/duplicates and if there is more than one datapoint for a given feature/word/variable choose randomly between them, singular_random = choose randomly between the dialects/duplicates, singular_least_missing_data = choose the dialect/duplicate which has the most datapoints.
#' @return data-frame of ValueTable without duplicates
#' @export
#'

# ValueTable <- readr::read_csv("https://github.com/cldf-datasets/wals/raw/master/cldf/values.csv")
# LanguageTable <- readr::read_csv("https://github.com/cldf-datasets/wals/raw/master/cldf/languages.csv")
# LanguageTable2 <-readr::read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv")
# method = "singular_least_missing_data"
# merge_dialects = TRUE

language_level_df <- function(ValueTable,
                              LanguageTable,
                              merge_dialects = TRUE,
                              LanguageTable2,
                              method = c("singular_least_missing_data", "combine_random", "singular_random"),
                              ) {

    if (!(method %in% c("singular_least_missing_data", "combine_random", "singular_random"))) {
        stop("Method of merging is not defined.")
    }

## Check necessary columns in ValueTable

    if (!all(c('Language_ID', 'Parameter_ID', 'Value') %in% colnames(ValueTable))) {
        stop("Invalid table format - ValueTable needs to have columns Language_ID/Parameter_ID/Value")
    }

## Check if LanguageTables are able to be used for merging dialects (if merge_dialects == TRUE) and set-up LanguageTable for use later.
if(merge_dialects == TRUE){
    if (
        all(!"Language_ID" %in% colnames(LanguageTable),
        !"Language_level_ID" %in% colnames(LanguageTable)) &
        !exists(x = "LanguageTable2")
        ){

        stop("Not possible to merge dialects because LanguageTable does not have the columns 'Language_ID' or 'Language_level_ID' and no LanguageTable2 supplied.")

        if(all(!"Language_ID" %in% colnames(LanguageTable2),
               !"Language_level_ID" %in% colnames(LanguageTable2))){

            stop("Not possible to merge dialects because LanguageTable2 does not have the columns 'Language_ID' or 'Language_level_ID'")
            }
        }

# The language level ID column is named different in different, CLDF datasets.

if(!exists("LanguageTable2")){
        if (!("Language_level_ID" %in% colnames(LanguageTable))) {
            LanguageTable   <- LanguageTable %>%
                dplyr::select(Language_ID = ID, Glottocode, Language_level_ID = Language_ID)
        }

        if ("Language_level_ID" %in% colnames(LanguageTable) & "ID" %in% colnames(LanguageTable)) {
            LanguageTable   <- LanguageTable %>%
                dplyr::select(Language_ID = ID, Glottocode, Language_level_ID)
        }

    }else{
        if (!("Language_level_ID" %in% colnames(LanguageTable2))) {
            LanguageTable   <- LanguageTable2 %>%
                dplyr::select(Glottocode, Language_level_ID = Language_ID) %>%
                right_join(LanguageTable, by = "Glottocode") %>%
                dplyr::select(Language_ID = ID, Glottocode, Language_level_ID)

        }

        if ("Language_level_ID" %in% colnames(LanguageTable2) & "ID" %in% colnames(LanguageTable)) {
            LanguageTable   <- LanguageTable2 %>%
                dplyr::select(Glottocode, Language_level_ID) %>%
                right_join(LanguageTable, by = "Glottocode") %>%
                dplyr::select(Language_ID = ID, Glottocode, Language_level_ID)
        }

        }

    # if there is a missing language level ID, which it can be in some datasets where only
    # dialects get language level IDs and languages and families don't, then replace those
    # with the content in the Language_ID column.
    LanguageTable   <- LanguageTable %>%
        dplyr::mutate(Language_level_ID = ifelse(
            is.na(Language_level_ID) | Language_level_ID == "", Glottocode, Language_level_ID)
        )

# Still in the merge_dialect == TRUE if loop
    # Replacing the col glottocode with Language_level_ID merges dialects for the rest of the duplicate pruning
        LanguageTable <- LanguageTable %>%
        dplyr::select(Language_ID, Glottocode = Language_level_ID)


}


    ## MERGE FOR LEAST MISSING DATA
    if (method == "singular_least_missing_data") {
        levelled_ValueTable <- ValueTable %>%
            dplyr::left_join(LanguageTable, by = "Language_ID") %>%
            dplyr::group_by(Language_ID) %>%
            dplyr::mutate(n = dplyr::n()) %>%
            dplyr::arrange(desc(n)) %>%
            dplyr::ungroup() %>%
            dplyr::distinct(Glottocode, .keep_all = T) %>%
            dplyr::distinct(Language_ID) %>%
            dplyr::inner_join(ValueTable, by = "Language_ID") %>%
            dplyr::left_join(LanguageTable, by = "Language_ID")

    # MERGE BY MAKING A FRANKENSTEIN COMBINATION OF ALL THE DIALECTS
    } else if (method == "combine_random") {

        #in order to do this, we need to first make it long again.
        # making vector of columns to make long

        ValueTable_grouped <- ValueTable %>%
            dplyr::filter(!is.na(Value)) %>%
            dplyr::left_join(LanguageTable, by = "Language_ID",
                      relationship = "many-to-many") %>%
            dplyr::group_by(Glottocode, Parameter_ID) %>%
            dplyr::mutate(n = dplyr::n()) %>%
            dplyr::ungroup() %>%
            dplyr::select(-Language_ID)

        # it's faster if we do slice_sample (choose randomly) only on those that have more than 1
        # value per language than if we do it on all.
        ValueTable_long_n_greater_than_1 <- ValueTable_grouped %>%
            dplyr::filter(n > 1) %>%
            dplyr::group_by(Glottocode, Parameter_ID) %>%
            dplyr::slice_sample(n = 1) %>%
            dplyr::ungroup()

        levelled_ValueTable <- ValueTable_grouped %>%
            dplyr::filter(n == 1) %>%
            dplyr::full_join(ValueTable_long_n_greater_than_1, by = join_by(
                ID, Parameter_ID, Value, Code_ID, Comment, Source, Source_comment, Coders, Glottocode, n))  %>%
            dplyr::mutate(Language_ID = Language_level_ID) %>%
            dplyr::select(-Language_level_ID, -n)

    # MERGE BY PICKING DIALECTS WHOLLY AT RANDOM
    } else if (method == "singular_random") {
        levelled_ValueTable <- ValueTable %>%
            dplyr::left_join(LanguageTable, by = "Language_ID") %>%
            dplyr::group_by(Glottocode) %>%
            dplyr::slice_sample(n = 1) %>%
            dplyr::ungroup()

    } else {
        stop("invalid method")
    }

    levelled_ValueTable
}

