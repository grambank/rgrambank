#' Computes scores based on theoretical linguistics on grambank data.
#'
#' @param ValueTable a data-frame, long format, of Grambank values
#' @param ParameterTable_FN file path to a Grambank ParameterTable
#' @return A data-frame with theoretical scores per language.

make_theo_scores <- function(ValueTable, ParameterTable_fn){

  library(tidyverse)
  
#read in sheet with scores for whether a feature denotes fusion
ParametersTable_theo <- data.table::fread(ParameterTable_fn,
                                      encoding = 'UTF-8', 
                                      quote = "\"", header = TRUE, 
                                      sep = ",") %>% 
  dplyr::select(Parameter_ID = ID, Fusion = Boundness, Informativity, Locus_of_Marking, Word_Order, Gender_or_Noun_Class, Flexivity) %>% 		
  mutate(Fusion = as.numeric(Fusion)) %>% 
  mutate(Gender_or_Noun_Class = as.numeric(Gender_or_Noun_Class)) %>% 
  mutate(Flexivity = as.numeric(Flexivity)) %>% 
  mutate(Locus_of_Marking = as.numeric(Locus_of_Marking)) %>% 
  mutate(Word_Order = as.numeric(Word_Order))


#remove features for which there is only a feature for the free or bound kind of marking, only keep those where there is one for each type of marking

# GB_fusion_points_only_with_alternatives <- GB_fusion_points %>% 
#   filter(!is.na(Fusion))	%>% 
#   filter(!is.na(informativity))	%>% 
#   group_by(informativity) %>% 
#   dplyr::summarise(count_informativity_categories = n()) %>% 
#   filter(count_informativity_categories > 1) %>% 
#   inner_join(GB_fusion_points,  by = "informativity") %>% 
#   dplyr::select(Parameter_ID, Fusion)

ValueTable_ParametersTable_theo <- ValueTable %>% 
  inner_join(ParametersTable_theo , by = "Parameter_ID") %>% 
  mutate(Value = is.numeric(Value))  #drop out ? marking
  

#fusion counts
df_morph_count <- ValueTable_ParametersTable_theo %>% 
  filter(Fusion != 0) %>% 
  filter(!is.na(Value)) %>%
  mutate(Value_weighted = ifelse(Fusion == 0.5 & Value == 1, 0.5, Value )) %>% 
  #replacing all instances of 1 for a feature that is weighted to 0.5 bound morph points to 0.5
  #  mutate(Value_weighted = if_else(Fusion == 0, abs(Value-1), Value_weighted)) %>% # reversing the Values of the features that refer to free-standing markers 
  group_by(Language_ID) %>% 
  dplyr::summarise(mean_morph = mean(Value_weighted))


##Flexivity scores
lg_df_for_flex_count <- ValueTable_ParametersTable_theo  %>% 
  filter(!is.na(Flexivity)) %>% 
  filter(!is.na(Value)) %>% 
  #  reversing the Values of the features that have a score of 0
  mutate(Value_weighted = if_else(Flexivity == 0, abs(Value-1), Value)) %>%
  group_by(Language_ID) %>% 
  dplyr::summarise(Flexivity = mean(Value_weighted), .groups = "drop")

##`locus of marking`s
lg_df_for_HM_DM_count <- ValueTable_ParametersTable_theo %>% 
  filter(!is.na(Locus_of_Marking)) %>% 
  filter(!is.na(Value)) %>% 
  # reversing the Values of the features that have a score of 0
  mutate(Value_weighted = if_else(Locus_of_Marking == 0, abs(Value-1), Value)) %>%
  group_by(Language_ID) %>% 
  dplyr::summarise(Locus_of_Marking = mean(Value_weighted), .groups = "drop_last")

##Gender_or_Noun_Class scores
lg_df_for_gender_nc_count <- ValueTable_ParametersTable_theo  %>% 
  filter(!is.na(Gender_or_Noun_Class)) %>% 
  filter(!is.na(Value)) %>% 
  #  reversing the Values of the features that have a score of 0
  mutate(Value_weighted = if_else(Gender_or_Noun_Class == 0, abs(Value-1), Value)) %>%
  group_by(Language_ID) %>% 
  dplyr::summarise(Gender_or_Noun_Class = mean(Value_weighted), .groups = "drop_last")

##OV_VO scores
lg_df_for_OV_VO_count <- ValueTable_ParametersTable_theo  %>% 
  filter(!is.na(Word_Order)) %>% 
  filter(!is.na(Value)) %>% 
  # reversing the Values of the features that refer to free-standing markers 
  mutate(Value_weighted = if_else(Word_Order == 0, abs(Value-1), Value)) %>%
  group_by(Language_ID) %>% 
  dplyr::summarise(Word_Order = mean(Value_weighted), .groups = "drop_last")

##informativity score

lg_df_informativity_score <-  ValueTable_ParametersTable_theo  %>% 
  filter(!is.na(Informativity)) %>% 
  # reversing GB140 because 0 is the informative state
  mutate(Value = if_else(Parameter_ID == "GB140", abs(Value-1), Value)) %>%
  #grouping per language and per informativity category 
  group_by(Language_ID, Informativity) %>%
  #for each informativity cateogry for each langauge, how many are answered 1 ("yes")
  #how many of the Values per informativity category are missing
  dplyr::summarise(sum_informativity = sum(Value, na.rm = TRUE),
                   sum_na = sum(is.na(Value)), .groups = "drop_last" ) %>%
  #if there is at least one NA and the sum of Values for the entire category is 0, the 
  # informativity score should be NA because there could be a 1 hiding under the NA Value.
  mutate(sum_informativity = ifelse(sum_na >= 1 & sum_informativity == 0, NA, sum_informativity)) %>%
  mutate(informativity_score = ifelse(sum_informativity >= 1, 1, sum_informativity)) %>%
  ungroup() %>%
  group_by(Language_ID) %>%
  dplyr::summarise(Informativity = mean(informativity_score, na.rm = TRUE), .groups = "drop_last")

all_theo_scores <- lg_df_for_OV_VO_count %>%
  full_join(lg_df_for_flex_count, by = "Language_ID") %>%
  full_join(lg_df_for_gender_nc_count, by = "Language_ID") %>%
  full_join(lg_df_for_HM_DM_count, by = "Language_ID") %>%
  full_join(df_morph_count, by = "Language_ID") %>%
  full_join(lg_df_informativity_score, by = "Language_ID")


all_theo_scores
}



