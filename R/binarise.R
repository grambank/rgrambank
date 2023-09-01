
#GB024 multistate 1; Num-N; 2: N-Num; 3: both.
binarise_gb024_to_gb024a <- function(values) {
    dplyr::case_match(values, "1" ~ "1", "2" ~ "0", "3" ~ "1", "?" ~ "?")
}

binarise_gb024_to_gb024b <- function(values) {
    dplyr::case_match(values, "2" ~ "1", "3" ~ "1", "1" ~ "0", "?" ~ "?")
}


#' Makes multi-state Grambank-features binary in the appropriate manner.
#'
#' @param ValueTable data frame of the ValueTable from grambank-cldf (long).
#' @param drop_multistate If TRUE,the multistate parent features of the binarised features are kept. If FALSE, they are dropped from the resulting data-frame.
#' @param keep_raw_binary If TRUE and if the value table already contains some binarised features, they are kept. If false, they are overriden and replaced by values derived from the multistate features.
#' @return Data-frame (long or wide depending on 'wide' argument)
binarise <- function(ValueTable = NULL, wide = TRUE, drop_multistate = TRUE, keep_raw_binary = TRUE){
    if (!inherits(ValueTable, "data.frame")) stop("'ValueTable' must be a dataframe.")

    # if there are binary features coded directly by coders, i.e. not derived from multistate features,
    # we call these "raw binary". users can choose to drop these and only used derived, or use a mix,
    if (keep_raw_binary == FALSE) {
        ValueTable <-   ValueTable %>%
            anti_join(dplyr::select(binary_parameters_df, Parameter_ID = ID), by = join_by(Parameter_ID))
    }

    # making the long table wide, it's easier for the binarisation. The table can be made long again with 
    # comments etc later.
    wide_values <- make_ValueTable_wide(long_values)

    #if the binary raw feature isn't there, make a col with just NA values to start with. This will allow us to       blend raw and derived binary data later.
    for (binary_col in binary_parameters_df$ID) {
        if(!(binary_col %in% colnames(wide_values))){
            wide_values[[paste0(binary_col)]] <- rep(NA, nrow(wide_values))
        }
    }
    
    ######### DERIVING binary values from multistate ones
    wide_values$GB024a <- binarise_gb024_to_gb024a(wide_values$GB024)
    wide_values$GB024b <- binarise_gb024_to_gb024b(wide_values$GB024)

    #GB025 multistate 1: Dem-N; 2: N-Dem; 3: both.
    if("GB025" %in% colnames(wide_values)){
      wide_values$GB025a <- if_else(wide_values$GB025 == "1"|
                                      wide_values$GB025 == "3"&
                                      is.na(wide_values$GB025a), "1", ifelse(wide_values$GB025 == "2", "0", wide_values$GB025a))

      wide_values$GB025a <- if_else(wide_values$GB025 == "?"&
                                      is.na(wide_values$GB025a), "?", wide_values$GB025a)


      wide_values$GB025b <- ifelse(wide_values$GB025 == "2"|
                                     wide_values$GB025 == "3"&
                                     is.na(wide_values$GB025b), "1", ifelse(wide_values$GB025 == "1", "0", wide_values$GB025b))

      wide_values$GB025b <- if_else(wide_values$GB025 == "?"&
                                      is.na(wide_values$GB025b), "?", wide_values$GB025b)

    }

    #GB065 multistate 1:Possessor-Possessed; 2:Possessed-Possessor; 3: both
    if("GB065" %in% colnames(wide_values)){
      wide_values$GB065a <- if_else(wide_values$GB065 == "1"|
                                      wide_values$GB065 == "3"&
                                      is.na(wide_values$GB065a), "1", ifelse(wide_values$GB065 == "2", "0", wide_values$GB065a))

      wide_values$GB065a <- if_else(wide_values$GB065 == "?"&
                                      is.na(wide_values$GB065a), "?", wide_values$GB065a)


      wide_values$GB065b <- if_else(wide_values$GB065 == "2"|
                                      wide_values$GB065 == "3" &
                                      is.na(wide_values$GB065b), "1", ifelse(wide_values$GB065 == "1", "0", wide_values$GB065b))

      wide_values$GB065b <- if_else(wide_values$GB065 == "?" &
                                      is.na(wide_values$GB065b), "?", wide_values$GB065b)

    }

    #GB130 multistate 1: SV; 2: VS; 3: both
    if("GB130" %in% colnames(wide_values)){
      wide_values$GB130a <- if_else(wide_values$GB130 == "1"|
                                      wide_values$GB130 == "3"&
                                      is.na(wide_values$GB130a), "1", ifelse(wide_values$GB130 == "2", "0", wide_values$GB130a))

      wide_values$GB130a <- if_else(wide_values$GB130 == "?"&
                                      is.na(wide_values$GB130a), "?", wide_values$GB130a)


      wide_values$GB130b <- if_else(wide_values$GB130 == "2"|
                                      wide_values$GB130 == "3" &
                                      is.na(wide_values$GB130b), "1", ifelse(wide_values$GB130 == "1", "0", wide_values$GB130b))

      wide_values$GB130b <- if_else(wide_values$GB130 == "?"&
                                      is.na(wide_values$GB130b), "?", wide_values$GB130b)

    }

    #GB193 multistate 0: they cannot be used attributively, 1: ANM-N; 2: N-ANM; 3: both.
    if("GB193" %in% colnames(wide_values)){
      wide_values$GB193a <- if_else(wide_values$GB193 == "1"|
                                      wide_values$GB193 == "3"&
                                      is.na(wide_values$GB193a), "1", ifelse(wide_values$GB193 == "2"|wide_values$GB193 == "0", "0", wide_values$GB193a))

      wide_values$GB193a <- if_else(wide_values$GB193 == "?"&
                                      is.na(wide_values$GB193a), "?", wide_values$GB193a)

      wide_values$GB193b <- if_else(wide_values$GB193 == "2"|
                                      wide_values$GB193 == "3"&
                                      is.na(wide_values$GB193b), "1", ifelse(wide_values$GB193 == "1"|wide_values$GB193 == "0", "0", wide_values$GB193b))

      wide_values$GB193b <- if_else(wide_values$GB193 == "?"&
                                      is.na(wide_values$GB193b), "?", wide_values$GB193b)

    }
    #GB203 multistate 0: no UQ, 1: UQ-N; 2: N-UQ; 3: both.
    if("GB203" %in% colnames(wide_values)){
      wide_values$GB203a <- if_else(wide_values$GB203 == "1"|
                                      wide_values$GB203 == "3"&
                                      is.na(wide_values$GB203a), "1", ifelse(wide_values$GB203 == "2"|wide_values$GB203 == "0", "0", wide_values$GB203a))

      wide_values$GB203a <- if_else(wide_values$GB203 == "?"&
                                      is.na(wide_values$GB203a), "?", wide_values$GB203a)


      wide_values$GB203b <- if_else(wide_values$GB203 == "2"|
                                      wide_values$GB203 == "3"&
                                      is.na(wide_values$GB203b), "1", ifelse(wide_values$GB203 == "1"|wide_values$GB203 == "0", "0", wide_values$GB203b))

      wide_values$GB203b <- if_else(wide_values$GB203 == "?"&
                                      is.na(wide_values$GB203b), "?", wide_values$GB203b)

    }


    if(drop_multistate == T) {
        wide_values <- wide_values %>%
        dplyr::select(-parameters_binary_df$ID_multistate_parent)

    }

}



binary_parameters_df <- data.frame(
    ID = c(
        "GB024a", "GB024b",
        "GB025a", "GB025b",
        "GB065a", "GB065b",
        "GB130a","GB130b",
        "GB193a","GB193b",
        "GB203a", "GB203b"
    ),
    ID_multistate_parent = c(
        "GB024", "GB024",
        "GB025", "GB025",
        "GB065", "GB065",
        "GB130","GB130",
        "GB193","GB193",
        "GB203", "GB203"
    ),
    Grambank_ID_desc = c(
        "GB024a NUMOrder_Num-N",
        "GB024b NUMOrder_N-Num",
        "GB025a DEMOrder_Dem-N",
        "GB025b DEMOrder_N-Dem",
        "GB065a POSSOrder_PSR-PSD",
        "GB065b POSSOrder_PSD-PSR",
        "GB130a IntransOrder_SV",
        "GB130b IntransOrder_VS",
        "GB193a ANMOrder_ANM-N",
        "GB193b ANMOrder_N-ANM",
        "GB203a UQOrder_UQ-N",
        "GB203b UQOrder_N-UQ"
    ),
    Name = c(
        "Is the order of the numeral and noun Num-N?",
        "Is the order of the numeral and noun N-Num?",
        "Is the order of the adnominal demonstrative and noun Dem-N?",
        "Is the order of the adnominal demonstrative and noun N-Dem?",
        "Is the pragmatically unmarked order of adnominal possessor noun and possessed noun PSR-PSD?",
        "Is the pragmatically unmarked order of adnominal possessor noun and possessed noun PSD-PSR?",
        "Is the pragmatically unmarked order of S and V in intransitive clauses S-V?",
        "Is the pragmatically unmarked order of S and V in intransitive clauses V-S?",
        "Is the order of the adnominal property word (ANM) and noun ANM-N?",
        "Is the order of the adnominal property word (ANM) and noun N-ANM?",
        "Is the order of the adnominal collective universal quantifier (UQ) and noun UQ-N?",
        "Is the order of the adnominal collective universal quantifier (UQ) and noun N-QU?"
    ),
    `OV vs VO types (excl affixes)`= c(
        "OV",
        "VO",
        "OV",
        "VO",
        "OV",
        "VO",
        NA,
        NA,
        "OV",
        "VO",
        "OV",
        "VO"
    ),
    `OV VO score for counting`= c(
        0,
        1,
        0,
        1,
        0,
        1,
        NA,
        NA,
        0,
        1,
        0,
        1
    )
)

