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
glottolog <- load_glottolog()

languages <- grambank$tables$LanguageTable %>%
    left_join(glottolog$tables$LanguageTable)

head(languages)
```


## I want to get the theoretical scores:

```{r}
grambank <- load_grambank()
theo <- make_theo_scores(grambank$tables$ValueTable, grambank$tables$ParameterTable)
head(theo)
```

## I want to merge data from glottolog.

```{r}
library(dplyr)
library(rgrambank)
library(ggplot2)

grambank <- load_grambank()
glottolog <- load_glottolog()

languages <- glottolog$tables$LanguageTable %>%
    right_join(theo, join_by(ID==Language_ID))

ggplot(languages, aes(x=Longitude, y=Latitude, color=Locus_of_Marking)) + 
    geom_point(alpha=0.6, size=0.7) + scale_color_viridis_b()

cor.test(languages$Latitude, languages$Locus_of_Marking)  # hey, it's significant.
````
