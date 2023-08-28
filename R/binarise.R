#' Makes multi-state Grambank-features binary in the appropriate manner.
#' 
#' @param values_fn filename to a grambank values-table
#' @param wide If true, the resulting data-frame is wide with languoids as rows and features as columns. If false, returned data-frame is long with information such as Comment, Source etc retained.
#' @param drop_multistate If true,the multistate parent features of the binarised features are kept. If false, theya re dropped from the resulting data-frame.
#' @param keep_raw_binary If TRUE and if the value table already contains some binarised features, they are kept. If false, they are overriden and replaced by values derived from the multistate features.
#' @return Data-frame (long or wide depending on 'wide' argument)

##############
binarise <- function(values_fn = NULL, wide = T, drop_multistate = T, keep_raw_binary = T){

long_values <- read_csv(values_fn, show_col_types = F)

parameters_binary_df <- make_binary_parameters_df() 

#if there are binary features coded directly by coders, i.e. not derived from multistate features, we call these "raw binary". users can choose to drop these and only used derived, or use a mix,

if(keep_raw_binary == F){
long_values <-   long_values %>% 
  anti_join(dplyr::select(parameters_binary_df, Parameter_ID = ID), by = join_by(Parameter_ID))
  }

#making the long table wide, it's easier for the binarisation. The table can be made long again with comments etc later.
wide_values <- make_ValueTable_wide(long_values) 

#if the binary raw feature isn't there, make a col with just NA values to start with. This will allow us to blend raw and derived binary data later.
for(binary_col in parameters_binary_df$ID){

#  binary_col <- parameters_binary_df$ID[4]
  if(!(binary_col %in% colnames(wide_values))){
    
    wide_values[[paste0(binary_col)]] <- rep(NA, nrow(wide_values))
    }
}


######### DERIVING binary values from multistate ones

#GB024 multistate 1; Num-N; 2: N-Num; 3: both.


if("GB024" %in% colnames(wide_values)){
  wide_values$GB024a <- if_else(wide_values$GB024 == "1"|
                                  wide_values$GB024 == "3" &
                                  is.na(wide_values$GB024a), "1", ifelse(wide_values$GB024 == "2", "0", wide_values$GB024a)) 


  wide_values$GB024a <- if_else(wide_values$GB024 == "?" &
                                  is.na(wide_values$GB024a), "?", wide_values$GB024a) 
  
    
  wide_values$GB024b <- if_else(wide_values$GB024 == "2"|
                                  wide_values$GB024 == "3" &
                                  is.na(wide_values$GB024b)
                                , "1", ifelse(wide_values$GB024 == "1", "0", wide_values$GB024b)) 

  wide_values$GB024b <- if_else(wide_values$GB024 == "?" &
                                  is.na(wide_values$GB024b), "?", wide_values$GB024b) 
  
  }

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

if(wide == F){
  
long_again_df <-  wide_values %>% 
    reshape2::melt(id.vars = "Language_ID") %>% 
    rename(Parameter_ID = variable, Value_binary = value)
  
long_again_df_with_comments_and_source <- parameters_binary_df %>%
  dplyr::select(Parameter_ID = ID, ID_multistate_parent) %>%
  full_join(long_again_df, by = "Parameter_ID") %>%
  distinct() %>% 
  mutate(ID_multistate_parent = ifelse(is.na(ID_multistate_parent), Parameter_ID, ID_multistate_parent)) %>% 
  dplyr::rename(Parameter_ID = ID_multistate_parent, Parameter_ID_binary = Parameter_ID) %>% 
  left_join(long_values, by = c("Parameter_ID", "Language_ID")) %>% 
  dplyr::select(Language_ID, Parameter_ID = Parameter_ID_binary, Value= Value_binary, Code_ID, Source, Source_comment, Comment, Coders) %>% 
  filter(!is.na(Value))

long_again_df_with_comments_and_source  
}else{
  wide_values  
  }
}
