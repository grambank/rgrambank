#' Reduce dialects in dataframe to one per language, and give it the glottocode of the parent that is at level language. Requires a cldf-value table and language-table.
#'
#' @param ValueTable data-frame with wide data from cldf value-table. One column needs to be "Language_ID" and all other columns need to be the relevant variables. There should not be columns with Language meta-data like Longitude, Family_name etc. The data-frame needs to be wide, use function make_ValueTable_wide if need be. The function currently does not support data-sets where Language_ID is not glottocodes (e.g. WALS).
#' @param method combine_random = combine all datapoints for all the dialects and if there is more than one datapoint for a given feature/word/variable choose randomly between them, singular_random = choose randomly between the dialects, singular_least_missing_data = choose the dialect which has the most datapoints
#' @param LanguageTable character vector denoting a filepath to a csv-sheet of a table with glottocodes and language-level glottocodes of the type LanguageTable in the cldf-ontology. If NULL, it fetches table from glottolog-cldf online on GitHub (v4.8).
#' #' @param drop_question_marks logical indicating wether to turn ? into missing values (NA) or not.
#' @return language-leveled dataset


language_level_df <- function(ValueTable = NULL,
                              method = c( "singular_least_missing_data", "combine_random", "singular_random"),
                              drop_question_marks = TRUE,
                              LanguageTable_fn = "https://github.com/glottolog/glottolog-cldf/raw/v4.8/cldf/languages.csv"){

### WRANGLING LANGUAGE TABLE
# ValueTable = read_csv("https://github.com/grambank/grambank/raw/master/cldf/values.csv")

if(length(method) != 1){
    stop("Method of merging is not defined.")
}

#LanguageTable wrangling
LanguageTable <- read_csv(LanguageTable_fn, show_col_types = FALSE)

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
  mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID))

## QUESTION MARK ACTION

#turn question marks into missing values if set to TRUE
if(drop_question_marks == T){
  ValueTable <- ValueTable %>%
      mutate(Value = ifelse(Value == "?", NA, Value)) %>%
      filter(!is.na(Value))
}

## MERGE FOR LEAST MISSING DATA
if( method == "singular_least_missing_data"){

levelled_ValueTable <- ValueTable %>%
  left_join(LanguageTable, by = "Language_ID") %>%
    group_by(Language_ID) %>%
    mutate(n = n()) %>%
    arrange(desc(n)) %>%
    ungroup() %>%
    distinct(Language_level_ID, .keep_all = T) %>%
    distinct(Language_ID) %>%
    inner_join(ValueTable, by = "Language_ID") %>%
    left_join(LanguageTable, by = "Language_ID") %>%
    dplyr::select(-Language_ID) %>%
    rename(Language_ID = Language_level_ID)

}

# MERGE BY MAKING A FRANKENSTEIN COMBINATION OF ALL THE DIALECTS
if( method == "combine_random"){

#in order to do this, we need to first make it long again.

# making vector of columns to make long

ValueTable_grouped <- ValueTable %>%
    filter(!is.na(Value)) %>%
    left_join(LanguageTable, by = "Language_ID",
              relationship = "many-to-many") %>%
    group_by(Language_level_ID, Parameter_ID) %>%
    mutate(n = n()) %>%
   ungroup() %>%
  dplyr::select(-Language_ID)

#it's faster if we do slice_sample (choose randomly) only on those that have more than 1 value per language than if we do it on all.
ValueTable_long_n_greater_than_1 <- ValueTable_grouped %>%
  filter(n > 1) %>%
  group_by(Language_level_ID, Parameter_ID) %>%
  slice_sample(n = 1) %>%
  ungroup()

levelled_ValueTable <- ValueTable_grouped %>%
  filter(n == 1) %>%
  full_join(ValueTable_long_n_greater_than_1, by = join_by(ID,
                                                           Parameter_ID,
                                                           Value,
                                                           Code_ID,
                                                           Comment,
                                                           Source,
                                                           Source_comment,
                                                           Coders,
                                                            Language_level_ID,
                                                            n))  %>%
  mutate(Language_ID = Language_level_ID) %>%
  dplyr::select(-Language_level_ID, -n)
}

# MERGE BY PICKING DIALECTS WHOLLY AT RANDOM
if( method == "singular_random"){
levelled_ValueTable <- ValueTable %>%
    left_join(LanguageTable, by = "Language_ID") %>%
    group_by(Language_level_ID) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    mutate(Language_ID = Language_level_ID) %>%
    dplyr::select(-Language_level_ID)
}

levelled_ValueTable
}