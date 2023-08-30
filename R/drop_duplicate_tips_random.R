#' If a language tree has tips with duplicate tip labels, drop all but one at random.
#'
#' @param tree 	an object of class "phylo".
#' @param merge_dialects a logical specifying whether to replace dialect tip labels with the glottocode of the language that is their parent, and then drop all but one
#' @param random_seed integer specifying random seed
#' @param LanguageTable data-frame of a cldf LanguageTable with columns for glottocodes per languoid and dialects language parent glottocode. If merge_dialects is set to TRUE, a LanguageTable is necessary. If NULL then Glottolog v4.8 is fetched from the internet and used.
#' @param trim_tip_label_to_first_eight a logical specifying whether we should trim the tip label to the first 8 characters. If the tip labels contain more than just glottocodes, it is necessary to trim away what is not glottocodes. If set to TRUE, only the first 8 characters are retained. This is necessary for the EDGE-trees as they contain names as well (e.g. "bamu1257_Bamu_Kiwaian"). If the tip labels only consist of glottocodes, this parameter can be set to TRUE or FALSE without difference.
#'

drop_duplicate_tips_random <- function(tree = NULL,
                                      merge_dialects = TRUE,
                                      random_seed = NULL,
                                      LanguageTable = "Glottolog_4.8",
                                      trim_tip_label_to_first_eight = TRUE){

    #There is a random element in the duplicate dropping. If you want it to give the same result everytime, set a random seed.
if(is.numeric(random_seed)){
    set.seed(random_seed)
}


#if the tip labels has glottocodes as the first 8 characters and then something else, like a name, then prune that off. This is for example true for the EDGE-trees.
if(trim_tip_label_to_first_eight == TRUE){
    tree$tip.label <- tree$tip.label %>% substr(1, 8)
}


if(merge_dialects == TRUE){

if(is.null(LanguageTable)){
    stop("LanguageTable is NULL. LanguageTable is necessary for merging dialects.\n")
}else{

    #unless otherwise specify, use the LanguageTable from glottolog-cldf v4.8 as LanguageTable for matching dialects to their parents.
    if(LanguageTable == "Glottolog_4.8"){
        LanguageTable <- read_csv("https://raw.githubusercontent.com/glottolog/glottolog-cldf/v4.8/cldf/languages.csv", show_col_types = F)
    }

if(!("Language_ID" %in% colnames(LanguageTable) | "Language_level_ID" %in% colnames(LanguageTable))){
stop("LanguageTable does not contain 'Language_ID' or 'Language_level_ID' columns.\n")
    }

# The language level ID column is named different in different, CLDF datasets.
# setting them to the same
if("Language_ID" %in% colnames(LanguageTable)){
    LanguageTable   <- LanguageTable %>%
        dplyr::select(Language_ID = ID, Language_level_ID = Language_ID)
}

if("Language_level_ID" %in% colnames(LanguageTable) & "ID" %in% colnames(LanguageTable)){
    LanguageTable   <- LanguageTable %>%
        dplyr::select(Language_ID = ID, Language_level_ID)
}

#some LanguageTables only contain values for Language_level_ID if the languoid is a dialect. Here we insert the language level glottocode if the level is language or family as well.
LanguageTable <-    LanguageTable %>%
        mutate(Language_level_ID = ifelse(is.na(Language_level_ID), Language_ID, Language_level_ID))

#rename tip labels to the glottocode of the language level if tip is dialect
tip_labels_df <- tree$tip.label %>%
    as.data.frame() %>%
    rename(Language_ID = ".") %>%
    left_join(LanguageTable, by = "Language_ID")

tree$tip.label <- tip_labels_df$Language_level_ID

}}


#keeping just one tip per language in the entire tree. Anytime where there are duplicate tip labels, only one tip is kept. Selection is random. Random seed can be set.
to_keep <- tree$tip.label %>%
              as.data.frame() %>%
              rename(tip.label = ".") %>%
                group_by(tip.label) %>%
                sample_n(1)

tree <- ape::keep.tip(tree, tip = to_keep$tip.label)

tree
}
