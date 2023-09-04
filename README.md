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

## I want a table of languages in grambank with details from glottolog:

```{r}
library(dplyr)
grambank <- load_grambank()
glottolog <- load_grambank()

languages <- grambank$tables$LanguageTable %>%
    left_join(glottolog$tables$LanguageTable)

head(languages)
```
