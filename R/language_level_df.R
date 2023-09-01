
# TODO SJG:
#   reading over this -- I wondered if it's better to run this on a grambank rcldf object
#  e.g. gb <- rcldf("tests/testdata/fixtures/testdata/")
#       gb.subset <- to_language_level(gb, method="xxx")

## Hedvig response: I disagree. I believe that this function should take dataframes of ValueTables, and the user can give those to the function however they'd like. I think requiring a rcldf object class is unnecessary, especially since it's possible to point to a df within that object (gb$tables$ValueTable). I do not see what the advantage would be. If you could explain that, I'll consider again. I spoke to Angela, and she agrees with me. I haven't heard back from the other future users on this specific point.


#' Reduce dialects in dataframe to one per language, and give it the glottocode of the parent that is at level language. Requires a cldf-value table and language-table.
#'
#' @param ValueTable data-frame with wide data from cldf value-table. One column needs to be "Language_ID" and all other columns need to be the relevant variables. There should not be columns with Language meta-data like Longitude, Family_name etc. The data-frame needs to be wide, use function make_ValueTable_wide if need be. The function currently does not support data-sets where Language_ID is not glottocodes (e.g. WALS).
#' @param method combine_random = combine all datapoints for all the dialects and if there is more than one datapoint for a given feature/word/variable choose randomly between them, singular_random = choose randomly between the dialects, singular_least_missing_data = choose the dialect which has the most datapoints
#' @param LanguageTable character vector denoting a filepath to a csv-sheet of a table with glottocodes and language-level glottocodes of the type LanguageTable in the cldf-ontology. If NULL, it fetches table from glottolog-cldf online on GitHub (v4.8).
#' #' @param drop_question_marks logical indicating wether to turn ? into missing values (NA) or not.
#' @return language-leveled dataset
#' @export

# TODO SJG: can we fix the indentation here?
# Hedvig response: Sure, to me that's a aesthtics thing, we can do it however you'd like.


language_level_df <- function(ValueTable = NULL,
                              method = c( "singular_least_missing_data", "combine_random", "singular_random"),
                              drop_question_marks = TRUE,
                              LanguageTable_fn = "https://github.com/glottolog/glottolog-cldf/raw/v4.8/cldf/languages.csv"){

    if(length(method) != 1 & !(method %in% c( "singular_least_missing_data",
                                              "combine_random", "singular_random"))){
        stop("Method of merging is not defined.")
    }

    ### WRANGLING LANGUAGE TABLE
    # ValueTable = readr::read_csv("tests/testthat/fixtures/testdata/values.csv")

    # TODO SJG: should add some checks here to make sure we have ALL the columns we need
    #  or we stop()
    # Hedvig response: Sure.

    if (!all(c('Language_ID', 'Parameter_ID', 'Value') %in% colnames(ValueTable))) {
        stop("Invalid table format - ValueTable needs to have columns Language_ID/Parameter_ID/Value")
    }

LanguageTable <- read.delim(LanguageTable_fn, sep = ",")

# The language level ID column is named different in different, CLDF datasets.
# setting them to the same
if(!("Language_level_ID" %in% colnames(LanguageTable))){
    LanguageTable   <- LanguageTable %>%
        dplyr::select(Language_ID = Glottocode, Language_level_ID = Language_ID)
}

if("Language_level_ID" %in% colnames(LanguageTable) & "ID" %in% colnames(LanguageTable)){
    LanguageTable   <- LanguageTable %>%
        dplyr::select(Language_ID = Glottocode, Language_level_ID)
}

# if there is a missing language level ID, which it can be in some datasets where only dialects get language level IDs and languages and families don't, then replace those with the content in the Language_ID column.
LanguageTable   <- LanguageTable %>%
  dplyr::mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID))

## QUESTION MARK ACTION

#turn question marks into missing values if set to TRUE
if(drop_question_marks == T){
    ValueTable <- ValueTable %>%
        dplyr::mutate(Value = ifelse(Value == "?", NA, Value)) %>%
        dplyr::filter(!is.na(Value))
}

## MERGE FOR LEAST MISSING DATA
if( method == "singular_least_missing_data"){

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

}


# MERGE BY MAKING A FRANKENSTEIN COMBINATION OF ALL THE DIALECTS
if( method == "combine_random"){

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

    #it's faster if we do slice_sample (choose randomly) only on those that have more than 1 value per language than if we do it on all.
    ValueTable_long_n_greater_than_1 <- ValueTable_grouped %>%
        dplyr::filter(n > 1) %>%
        dplyr::group_by(Language_level_ID, Parameter_ID) %>%
        dplyr::slice_sample(n = 1) %>%
        dplyr::ungroup()

    levelled_ValueTable <- ValueTable_grouped %>%
        dplyr::filter(n == 1) %>%
        dplyr::full_join(ValueTable_long_n_greater_than_1, by = join_by(ID,
                                                                 Parameter_ID,
                                                                 Value,
                                                                 Code_ID,
                                                                 Comment,
                                                                 Source,
                                                                 Source_comment,
                                                                 Coders,
                                                                 Language_level_ID,
                                                                 n))  %>%
        dplyr::mutate(Language_ID = Language_level_ID) %>%
        dplyr::select(-Language_level_ID, -n)
}

# MERGE BY PICKING DIALECTS WHOLLY AT RANDOM
if( method == "singular_random"){
    levelled_ValueTable <- ValueTable %>%
        dplyr::left_join(LanguageTable, by = "Language_ID") %>%
        dplyr::group_by(Language_level_ID) %>%
        dplyr::slice_sample(n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Language_ID = Language_level_ID) %>%
        dplyr::select(-Language_level_ID)
}


levelled_ValueTable
}

