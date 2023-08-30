#' Renders a data-frame with a Parameters table of binarised features. In version 1.0 of Grambank, binarised features are not included in the Parameter table. This function creates the table, for example for use in plotting (appropriate labels), making OV-sore or for rgrambank::binarise()
#' @return Data-frame (tibble) with rows for each binarised feature and columns with meta-information.


make_binary_parameters_df <-function(){
   data.frame(
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
    Name = c("Is the order of the numeral and noun Num-N?",
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
      "VO"),
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
    ))
}
