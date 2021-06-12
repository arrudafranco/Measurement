# G-Study and D-Study Crossed Designs

library(tidyverse)

# This function creates a source table for a
# single facet G-study crossed design.
# It takes as input a data.frame with rows as
# persons and columns as items. The output is
# a data.frame with degrees of freedom, SS, MS,
# expected MS and estimated variance component
# for persons (p), items (i) and pi.
source_table <- function(scores){
person_m <- mutate(scores,
                   mean_p = rowSums(scores) /
                     ncol(scores))
mean_i_row <- as.data.frame(t(summarise(person_m,
                                        mean_i = colSums(person_m) / nrow(person_m))))
colnames(mean_i_row) <- (colnames(person_m))
n_i <- ncol(scores)
n_p <- nrow(scores)
mean_g <- mean_i_row[,'mean_p']
SS_p <- sum((person_m['mean_p']) ^ 2 ) * n_i -
  n_i * n_p * (mean_g ^ 2)
sum_m_i <- (mean_i_row[head(seq_len(ncol(mean_i_row)),
                            -1)]) ^ 2 %>%
  sum()
SS_i <- sum_m_i * n_p - n_i * n_p * (mean_g ^ 2)
SS_pi <- sum(scores ^ 2) -
  sum((person_m['mean_p']) ^ 2 ) * n_i -
  sum_m_i * n_p + n_i * n_p * (mean_g ^ 2)
df_p <- n_p - 1
df_i <- n_i - 1
df_pi <- df_p * df_i
degree_f <- c(df_p, df_i, df_pi)
SS <- c(SS_p, SS_i, SS_pi)
source <- c('p', 'i', 'pi')
source.table <- data.frame(degree_f, SS,
                           MS = SS/degree_f,
                           row.names = source)
var_p <- (source.table['p', 'MS'] -
            source.table['pi', 'MS']) / n_i
var_i <- (source.table['i', 'MS'] -
            source.table['pi', 'MS']) / n_p
var_pi <- source.table['pi', 'MS']
est_var_comp <- c(var_p, var_i, var_pi)
E_MS_p <- var_pi + var_p * n_i
E_MS_i <- var_pi + var_i * n_p
E_MS_pi <- var_pi
E_MS <- c(E_MS_p, E_MS_i, E_MS_pi)
source.table <- bind_cols(source.table,
                          E_MS = E_MS,
                          est_var_comp = est_var_comp)
return(source.table)
}


# This function calculates the percent of total
# variance each component accounts for
# in a single facet G-study crossed design.
# It takes as input a data.frame with rows as
# persons and columns as items. The output is
# a data.frame with the percent (%) of total
# variance for persons (p), items (i) and pi.
prop_total_var <- function(scores){
  prop_total <- source_table(scores)['est_var_comp'] /
  sum(source_table(scores)['est_var_comp'])
  return(prop_total * 100)
}


# This function calculates the absolute error
# variance in a single facet D-study crossed
# design. It takes two inputs: a data.frame with
# rows as persons and columns as items, and the
# number of items in the sample of the study.
# The output is a numerical variable.
abs_error_var <- function(scores, n_sample){
  return((source_table(scores)['i', 'est_var_comp'] /
            n_sample) +
    (source_table(scores)['pi', 'est_var_comp'] /
       n_sample))
}


# This function calculates the relative error
# variance in a single facet D-study crossed
# design. It takes two inputs: a data.frame with
# rows as persons and columns as items, and the
# number of items in the sample of the study.
# The output is a numerical variable.
rel_error_var <- function(scores, n_sample){
  return(source_table(scores)['pi', 'est_var_comp'] /
              n_sample)
}


# This function calculates the generalizability
# coefficient in a single facet D-study crossed
# design. It takes two inputs: a data.frame with
# rows as persons and columns as items, and the
# number of items in the sample of the study.
# The output is a numerical variable.
gener_coef <- function(scores, n_sample){
  var_p <- (source_table(scores)['p', 'est_var_comp'])
  rel_error <- rel_error_var(scores, n_sample)
  return(var_p / (var_p + rel_error))
}

# This function calculates the dependability
# coefficient in a single facet D-study crossed
# design. It takes two inputs: a data.frame with
# rows as persons and columns as items, and the
# number of items in the sample of the study.
# The output is a numerical variable.
depend_coef <- function(scores, n_sample){
  var_p <- (source_table(scores)['p', 'est_var_comp'])
  abs_error <- abs_error_var(scores, n_sample)
  d_coef <- (var_p / (var_p + abs_error))
  return(d_coef)
}


# This function calculates a table including
# absolute error variance, relative error
# variance, dependability coefficient and
# generalizability coefficient for a single
# facet D-study crossed design.
# It takes two inputs: a data.frame with
# rows as persons and columns as items; and the
# number of items in the sample of the study
# either as a single numerical variable or
# as a vector containing multiple numerical
# variables. The output is a data.frame.
d_s_table <- function(scores, n_samples){
  table_build <- t(data.frame(abs_e_var = 0,
                              rel_e_var = 0,
  d_coef = 0, gen_coef = 0))
  for (i in seq(length(n_samples))){
    e_c <- c()
    e_c[1] <- rel_error_var(scores, n_samples[i])
    e_c[2] <- gener_coef(scores, n_samples[i])
    e_c[3] <- abs_error_var(scores, n_samples[i])
    e_c[4] <- depend_coef(scores, n_samples[i])
    table_build <- bind_cols(table_build, e_c)
  }
  table_build <- as.data.frame(table_build[-1])
  colnames(table_build) <- n_samples
  row.names(table_build) <- c('rel_e_var',
                              'gen_coef',
                              'abs_e_var',
                              'dep_coef')
  return(table_build)
}

