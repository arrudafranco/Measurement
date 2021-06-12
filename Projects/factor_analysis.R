library(haven)
library(tidyverse)
library(psych)
library(knitr)
library(GPArotation)

rel_vars <- c('Q22', 'Q8', 'Q45',
              'Q66b', 'Q66c', 'Q17',
              'Q48a','Q48b', 'Q48c')

rel_df <-
  read_sav("Religion in Latin America Dataset.sav",
           col_select = all_of(rel_vars))

rel_df <- as.data.frame(rel_df) %>%
  zap_missing() %>%
  zap_label() %>%
  zap_labels() %>%
  drop_na() %>%
  na_if(., "98") %>%
  na_if(., "99") %>%
  drop_na()

cor(rel_df, method = 'spearman')

fa(rel_df, rotate = 'varimax', cor = 'poly', weight = NULL,
   nfactors = 3, warnings = TRUE, fm = 'pa')