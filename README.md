# rgrambank

R package to access and analyse Grambank's CLDF data


```{r}
library(devtools)
install_github("SimonGreenhill/rcldf", dependencies = TRUE)
install_github("grambank/rgrambank", dependencies = TRUE)
```

Usage:

```{r}
grambank <- load_grambank()
glottolog <- load_glottolog()
```

## Show me the languages in grambank:

```{r}
grambank$tables$LanguageTable
```

## Show me the values from grambank:

```{r}
grambank$tables$ValueTable
```

## Give me a wide value table

```{r}
df.wide <- as.grambank.wide(grambank$tables$ValueTable)
```



## I want a table of languages in grambank with details from glottolog:

```{r}
library(dplyr)
grambank <- load_grambank()
glottolog <- load_grambank()

languages <- grambank$tables$LanguageTable %>%
    left_join(glottolog$tables$LanguageTable)

head(languages)
```


## I want to get the theoretical scores:

```{r}
# theo scores is a cover term for several measurements based on grambank features such as "fusion", "informativity" etc. There is an rgrambank function that does this given a ValueTable and ParameterTable.
# corresponding scripts in grambank-analysed: make_theo_scores.R and make_theo_scores_fusion.R

grambank_cldf_object <- rcldf::cldf(mdpath = "https://zenodo.org/records/7844558/files/grambank/grambank-v1.0.3.zip", #loading specifically Grambank v1.0.3
                        load_bib = FALSE) 

grambank_ValueTable <- grambank_cldf_object $tables$ValueTable
grambank_ParameterTable <- grambank_cldf_object $tables$ParameterTable

grambank_theo_scores <- rgrambank::make_theo_scores(ValueTable = grambank_ValueTable_binary_language_level, ParameterTable = grambank_ParameterTable)

# the warning about NAs introduced by coercion in rgrambank::make_theo_scores() is nothing to worry about, it will be gone when this PR is merged in: https://github.com/grambank/rgrambank/pull/36
# Note that it is advisable to crop the Grambank dataset for missing data before using the theoretical scores as otherwise it might be unreliable for languages with very few features filled in. See function rgrambank::crop_missing_data() which will be implemented if https://github.com/grambank/rgrambank/pull/37 is merged in
```

## I want to binarise multistate Grambank features
```{r}
#get Grambank
grambank_cldf_object <- rcldf::cldf(mdpath = "https://zenodo.org/records/7844558/files/grambank/grambank-v1.0.3.zip", #loading specifically Grambank v1.0.3
                        load_bib = FALSE) 

grambank_ValueTable <- grambank_cldf_object $tables$ValueTable

# Some Grambank features are not binary. They are all of the type "is the order XY, YX or both (or neither)"? They can be made binary with an rgrambank function. For more details on the arguments, see the helpfile on rgrambank::binarise()
# corresponding script in grambank-analysed: make_wide_binarized.R

grambank_ValueTable_binary <- rgrambank::binarise(ValueTable = grambank_ValueTable,
                                                     drop_multistate = TRUE,
                                                     keep_raw_binary = TRUE,
                                                     trim_to_only_raw_binary = FALSE
                                                    )
```


## I want to merge data from glottolog.

```{r}
library(dplyr)
library(rgrambank)
library(ggplot2)

grambank <- load_grambank()
glottolog <- load_grambank()

languages <- glottolog$tables$LanguageTable %>%
    right_join(theo, join_by(ID==Language_ID))

ggplot(languages, aes(x=Longitude, y=Latitude, color=Locus_of_Marking)) + 
    geom_point(alpha=0.6, size=0.7) + scale_color_viridis_b()

cor.test(languages$Latitude, languages$Locus_of_Marking)  # hey, it's significant.
````
