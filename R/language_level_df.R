
# TODO SJG:
#   reading over this -- I wondered if it's better to run this on a grambank rcldf object
#  e.g. gb <- rcldf("tests/testdata/fixtures/testdata/")
#       gb.subset <- to_language_level(gb, method="xxx")

## Hedvig response: I disagree. I believe that this function should take dataframes of ValueTables, and the user can give those to the function however they'd like. I think requiring a rcldf object class is unnecessary, especially since it's possible to point to a df within that object (gb$tables$ValueTable). I do not see what the advantage would be. If you could explain that, I'll consider again. I spoke to Angela, and she agrees with me. I haven't heard back from the other future users on this specific point.


#' Reduce dialects in dataframe to one per language, and give it the glottocode of the parent that is at level language. Requires a cldf-value table and language-table.
#'
#' @param dataset variable name for a wide data frame in the environment. One column needs to be "Language_ID" and all other columns need to be the relevant variables. There should not be columns with Language meta-data like Longitude, Family_name etc. The data-frame needs to be wide, use function as.grambank.wide if need be. The function currently does not support datasets where Language_ID is not glottocodes (e.g. WALS).
#' @param method combine_random = combine all datapoints for all the dialects and if there is more than one datapoint for a given feature/word/variable choose randomly between them, singular_random = choose randomly between the dialects, singular_least_missing_data = choose the dialect which has the most datapoints
#' @param LanguageTable a filepath to a csv-sheet of a table with glottocodes and language-level glottocodes of the type LanguageTable in the cldf-ontology. if nothing is specified, it fetches table from glottolog-cldf online on github (i.e. most recent version)
#' #' @param question_mark_action There is ?-values and missing values. Should they be merged or kept separate
#' @return language-leveled dataset
#' @export

# TODO SJG: can we fix the indentation here?
# Hedvig response: Sure, to me that's a aesthtics thing, we can do it however you'd like.

language_level_df <- function(dataset = NULL, method = c( "singular_least_missing_data", "combine_random", "singular_random"), drop_question_marks = F,  LanguageTable_fn = "https://raw.githubusercontent.com/glottolog/glottolog-cldf/master/cldf/languages.csv"){

    # TODO SJG: should add some checks here to make sure we have ALL the columns we need
    #  or we stop()
    # Hedvig response: Sure.

    if (!all(c('Language_ID', 'Parameter_ID', 'Value') %in% colnames(ValueTable))) {
        stop("Invalid table format - ValueTable needs to have columns Language_ID/Parameter_ID/Value")
    }

LanguageTable <- read_csv(LanguageTable_fn, show_col_types = F)

# The language level ID column is named different in different, CLDF datasets.
# setting them to the same
if("Language_ID" %in% colnames(LanguageTable)){
  LanguageTable   <- LanguageTable %>%
    dplyr::select(Language_ID = ID, Language_level_ID = Language_ID)
}

if("Language_level_ID" %in% colnames(LanguageTable)){
  LanguageTable   <- LanguageTable %>%
  dplyr::select(Language_ID = ID, Language_level_ID)
}

# if there is a missing language level ID, which it can be in some datasets where only dialects get language level IDs and languages and families don't, then replace those with the content in the Language_ID column.
LanguageTable   <- LanguageTable %>%
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID))

## QUESTION MARK ACTION

#drop_question_marks", "keep_question_mark
if(drop_question_marks == T){
  dataset <- dataset[dataset == "?"] <- NA

}

## MERGE FOR LEAST MISSING DATA
if( method == "singular_least_missing_data"){
  dataset$na_prop <- apply(dplyr::select(dataset, -Language_ID), 1, function(x) mean(is.na(x)))

  # TODO SJG: add namespaces
levelled_dataset <- dataset %>%
  left_join(LanguageTable, by = "Language_ID") %>%
  arrange(na_prop) %>%
   distinct(Language_level_ID, .keep_all = T) %>%
  mutate(Language_ID = Language_level_ID) %>%
  dplyr::select(-Language_level_ID)
}


# MERGE BY MAKING A FRANKENSTEIN COMBINATION OF ALL THE DIALECTS
if( method == "combine_random"){

# TODO SJG: going between wide and long a lot smells wrong. Let's not do that if we can.
# ...suggests we should make this function work on long data frames rather than widen.

#in order to do this, we need to first make it long again.

# making vector of columns to make long
    cols <- dataset %>%
    dplyr::select(-Language_ID) %>%
    colnames()

dataset_long <- dataset %>%
    pivot_longer(names_to = "Parameter_ID", values_to = "Value", cols = all_of(cols)) %>%
    filter(!is.na(Value)) %>%
    left_join(LanguageTable, by = "Language_ID") %>%
    group_by(Language_level_ID, Parameter_ID) %>%
    mutate(n = n()) %>%
   ungroup() %>%
  dplyr::select(-Language_ID)

#it's faster if we do slice_sample (choose randomly) only on those that have more than 1 value per language than if we do it on all.
dataset_long_n_greater_than_1 <- dataset_long %>%
  filter(n > 1) %>%
  group_by(Language_level_ID, Parameter_ID) %>%
  slice_sample(n = 1) %>%
  ungroup()

levelled_dataset <- dataset_long %>%
  filter(n == 1) %>%
  full_join(dataset_long_n_greater_than_1 , by = c("Parameter_ID", "Value", "Language_level_ID", "n"))  %>%
  mutate(Language_ID = Language_level_ID) %>%
  dplyr::select(-Language_level_ID, -n)  %>%
  as.grambank.wide()
}

# MERGE BY PICKING DIALECTS WHOLLY AT RANDOM
if( method == "singular_random"){
levelled_dataset <- dataset %>%
    left_join(LanguageTable, by = "Language_ID") %>%
    group_by(Language_level_ID) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(Language_ID = Language_level_ID) %>%
    dplyr::select(-Language_level_ID)
}

levelled_dataset
}

