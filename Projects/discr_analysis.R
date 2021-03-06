library(haven)
library(tidyverse)
library(psych)
library(MASS)
library(knitr)
set.seed(123456789)

rel_vars <- c('Q28a', 'Q28c', 'Q46a', 'Q55a',
  'Q55b', 'Q55d', 'Q55e', 'Q55f',
  'Q60a', 'Q60e', 'Q60f', 'Q60j')
rel_df <-
  read_sav("Religion in Latin America Dataset.sav",
           col_select = all_of(rel_vars))

# Considering NAs
rel_tot <- as.data.frame(rel_df) %>%
  zap_label() %>%
  zap_labels() %>%
  mutate_all(funs(as.numeric(str_replace_all(., "2", "0")))) %>%
  na_if(., "98") %>%
  na_if(., "99")

items_tot <- rel_tot[3:12]
rel_tot <- rel_tot %>%
  mutate(score = rowSums(items_tot))

print(alpha(items_tot, cumulative = TRUE))
print(alpha(rel_tot[-2][1:11]))

model_tot = lda(Q28a ~ score, data = rel_tot,
                na.action = na.omit)
predictions_tot = predict(model_tot)
print(model_tot)

# No NAs
rel_df <- as.data.frame(rel_df) %>%
  zap_missing() %>%
  zap_label() %>%
  zap_labels() %>%
  drop_na() %>%
  mutate_all(funs(as.numeric(str_replace_all(., "2", "0")))) %>%
  na_if(., "98") %>%
  na_if(., "99") %>%
  drop_na()

test_df <- slice(rel_df, 1:10)

items <- rel_df[3:12]
rel_df <- rel_df %>%
  mutate(score = rowSums(items)) %>%
  mutate(P_C = if_else(rel_df[1] | rel_df[2] == 1, 1, 0))

print(alpha(items, cumulative = TRUE))
print(alpha(rel_df[-2][1:11]))

model_P = lda(Q28a ~ score, data = rel_df)
predictions_P = predict(model_P)
tab_lda_P = table(rel_df$Q28a,predictions_P$class)
print(model_P)
cutoff <- ceiling((5.349445 + 6.590104) / 2)
paste('Cutoff: ', cutoff)
print(tab_lda_P)
print(sum(diag(tab_lda_P))/nrow(rel_df))

model_PC = lda(P_C ~ score, data = rel_df)
predictions_PC = predict(model_PC)
tab_lda_PC = table(rel_df$P_C,predictions_PC$class)
print(model_PC)
print(tab_lda_PC)
print(sum(diag(tab_lda_PC))/nrow(rel_df))

# Cross-Validation, from example on Canvas.
n = nrow(rel_df)
nt = 2000
neval=n-nt
rep=100
hitr=dim(rep)
for(i in 1:rep){
  train=sample(1:n,nt)
  model=lda(Q28a ~ score, data = rel_df[train,])
  predict(model,rel_df[-train,])$class
  tab_lda=table(rel_df$Q28a[-train],
                predict(model,rel_df[-train,])$class)
  hitr[i]=sum(diag(tab_lda))/neval
  }
print(mean(hitr))