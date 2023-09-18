#' Adds a column with the name of the language family.
#'
#' @param LanguageTable data-frame of CLDF table with the columns  "Family_ID", "Name" and "Glottocode".
#' @param LanguageTable2 data-frame of CLDF-table with additional information on Families in case LanguageTable is lacking information on the names of some/all families.
#' @return data-frame with Family_name column.
#' @note It is necessary that for every unique glottocode in Family_ID there is a row with a Glottocode and Name to match that. If there isn't, languages will have missing values for their Family_name even though they are not isolates.
#'  If The current LanguageTable lacks the required columns, consider using a combination of the LanguageTable and ValueTable of glottolog-cldf.
#' @export

# LanguageTable <- readr::read_csv("tests/testthat/fixtures/testdata/languages.csv", show_col_types = F) %>%
# dplyr::rename(Family_ID = Family_level_ID) %>%
# dplyr::select(-Family_name)

#LanguageTable2 <- read_tsv("tests/testthat/fixtures/cldf_wide_df_glottolog_4.8.tsv")

add_family_name_column <- function(LanguageTable = NULL, LanguageTable2 = NULL){
    if(!all(c("Family_ID", "Name", "Glottocode") %in% colnames(LanguageTable))){
        stop("LanguageTable needs to have all of these columns: Name, Glottocode and Family_ID.")
    }

    if(!exists("LanguageTable2") &
       !all(c("Family_ID", "Name", "Glottocode") %in% colnames(LanguageTable))){
        stop("LanguageTable2 needs to have all of these columns: Name, Glottocode and Family_ID.")
    }


    if(exists("LanguageTable2")){
        LanguageTable2 <- LanguageTable2 %>%
            dplyr::select(Family_ID, Name, Glottocode)
    }

    LanguageTable <-  LanguageTable %>%
        {if(!exists("LanguageTable2")) dplyr::full_join(x = ., LanguageTable2,
                                                       by = c("Name", "Glottocode", "Family_ID")) else . } %>%
        dplyr::distinct(Family_ID) %>%
        dplyr::filter(!is.na(Family_ID)) %>%
        dplyr::filter(Family_ID != "") %>%
        dplyr::rename(Glottocode = Family_ID) %>%
        dplyr::inner_join(LanguageTable, by = "Glottocode") %>%
        {if(!exists("LanguageTable2")) dplyr::full_join(x = ., LanguageTable2,
                                                by = c("Name", "Glottocode", "Family_ID")) else . } %>%
        dplyr::select(Family_ID = Glottocode, Family_name = Name) %>%
        dplyr::distinct(Family_ID, Family_name) %>%
        dplyr::right_join(LanguageTable, by = "Family_ID")

    LanguageTable

    LanguageTable
}