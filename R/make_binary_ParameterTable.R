#' Makes a version of the Grambank ParameterTable with information on binarised features
#' @param ParameterTable data-frame, long format. ParameterTable from cldf.
#' @param keep_multi_state_features logical. If TRUE, rows with the multistate version of the features remain, if FALSE only binary or binarised features remain in the ParameterTable.
#' @return data-frame of ParameterTable with added rows for binarised version of multi-state features
#' @export

make_binary_ParameterTable<- function(ParameterTable,
                                      keep_multi_state_features = TRUE){

#ParameterTable <- grambank_cldf_object $tables$ParameterTable

Parameter_binary <- data.frame(
    ID = c(
        "G024", "G024",
        "G025", "G025",
        "G065", "G065",
        "G130","G130",
        "G193","G193",
        "G203", "G203"
    ),

        ID_binary = c(
        "GB024a", "GB024b",
        "GB025a", "GB025b",
        "GB065a", "GB065b",
        "GB130a","GB130b",
        "GB193a","GB193b",
        "GB203a", "GB203b"
    ),
    Grambank_ID_desc_binary = c(
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
    Name_binary = c("Is the order of the numeral and noun Num-N?",
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
             "Is the order of the adnominal collective universal quantifier (UQ) and noun N-QU?" ),

    "Word_Order_binary"= c(
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
    ),
    Binary_Multistate = c("Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised","Binarised"))


multistate_features <- c("GB024", "GB025", "GB065", "GB130", "GB193", "GB203")

ParameterTable_new <- ParameterTable %>%
    full_join(Parameter_binary, by = "ID") %>%
    mutate(ID = ifelse(!is.na(ID_binary), ID_binary, ID)) %>%
    mutate(Name = ifelse(!is.na(Name_binary), Name_binary, Name)) %>%
    mutate(Grambank_ID_desc = ifelse(!is.na(Grambank_ID_desc_binary), Grambank_ID_desc_binary, Grambank_ID_desc)) %>%
    mutate(Word_Order = ifelse(!is.na(Word_Order_binary), Word_Order_binary, Word_Order)) %>%
    dplyr::select(-c("ID_binary", Name_binary, Grambank_ID_desc_binary, Word_Order_binary)) %>%
    mutate(Binary_Multistate= ifelse(ID %in% multistate_features, "Multi", Binary_Multistate)) %>%
    mutate(Binary_Multistate = ifelse(is.na(Binary_Multistate), "Binary", Binary_Multistate))

if(keep_multi_state_features == FALSE){
ParameterTable_new <-     ParameterTable_new %>%
    filter(!(ID %in% multistate_features))
}

ParameterTable_new
}
