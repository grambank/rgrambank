#' Adds info about isolates to tables of languages with glottocodes.
#'
#' @param Table data-frame. Required columns: level, Family_ID, Glottocode and Language_ID or Language_level_ID.
#' @param add_isolate_column logical. If TRUE, a column is added called "Isolate" and isolate languages and their dialects are coded "yes".
#' @param set_isolates_Family_ID_as characther vector. Choice between "themselves", "missing" or another string, like "isolate". If set to 'themselves', the missing values for Family_ID for isolates is replaced with their glottocode, e.g. basq1248 gets the Family_ID basq1248. If 'missing' their Family_ID is set to NA. If any other string besides "themselves" or "missing", they will get that string as their Family_ID, e.g. "isolate"/"Isolates"/"Isolate" etc.
#' @return Data-frame with desired modifications.
#' @note If The current LanguageTable lacks the required columns, consider using a combination of the LanguageTable and ValueTable of glottolog-cldf.
#' @export

add_isolate_info <- function(Table = NULL,
                             add_isolate_column = TRUE,
                             set_isolates_Family_ID_as = c("themselves", "missing", "isolate")
        ){

    if(!("level" %in% colnames(Table)) &
       !("Family_ID" %in% colnames(Table))&
       !("Glottocode" %in% colnames(Table))&
       !("Language_ID" %in% colnames(Table))|
       !("Language_level_ID" %in% colnames(Table))){
        stop("The Table needs to have all of these columns: level, Glottocode, Family_ID and Language_ID or Language_level_ID.")
    }


    if(add_isolate_column == TRUE & "Language_level_ID" %in% colnames(Table)){
        Table <- Table %>%
            dplyr::mutate(Isolate = ifelse(is.na(Family_ID)|
                                               Family_ID == "" & level == "language",
                                           "yes", "no")) %>%
            dplyr::mutate(Isolate = ifelse(Family_ID == Language_level_ID & level == "dialect",
                                           "yes", "no"))
    }

    if(set_isolates_Family_ID_as == "themselves"){
        Table <- Table %>%
            dplyr::mutate(Family_ID = ifelse(is.na(Family_ID)|
                                                 Family_ID == "" & level == "language",
                                             yes = Glottocode, no = Family_ID))

    }



    if(set_isolates_Family_ID_as == "missing"){
        Table <- Table %>%
            dplyr::mutate(Family_ID = ifelse(is.na(Family_ID)|
                                                 Family_ID == "" & level == "language",
                                             yes = NA, no = Family_ID)) %>%
            dplyr::mutate(Family_ID = ifelse(Family_ID == Language_level_ID & level == "dialect",
                                             yes = NA, no = Family_ID))

    }


    if(set_isolates_Family_ID_as != "themselves" &
       set_isolates_Family_ID_as != "missing"){
        Table <- Table %>%
            dplyr::mutate(Family_ID = ifelse(is.na(Family_ID)|
                                                 Family_ID == "" & level == "language",
                                             yes = {{set_isolates_family_as}}, no = Family_ID)) %>%
            dplyr::mutate(Family_ID = ifelse(Family_ID == Language_level_ID & level == "dialect",
                                             yes = {{set_isolates_family_as}}, no = Family_ID))

    }


Table
            }






