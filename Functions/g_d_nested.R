# G-Study and D-Study Nested Designs

library(tidyverse)

# This is a function that takes as input a data.frame,
# with items as rows and one column for EACH judge AND
# objective as columns, and a numerical value for the
# number of objectives in the study. It returns a
# data.frame with item-objective congruence indexes,
# with items as rows and objectives as columns.
item_obj_idx <- function(item_judge_df, n_objs){
  judgeXobj <- ncol(item_judge_df)
  jump <- (judgeXobj / n_objs) - 1
  item_means <- data.frame(total_mean =
                             rowSums(item_judge_df)
                           / ncol(item_judge_df))
  idx = 1
  for (i in seq(1, judgeXobj, n_objs)) {
    idx = idx + 1
    subset_df <- item_judge_df[i: (i + jump)]
    item_means[idx] = (rowSums(subset_df)/
                         ncol(subset_df))
  }
  idx_df <- item_means[-1]
  idx_df <- idx_df[,]-item_means[,1]
  calc_n <- n_objs/(2*n_objs - 2)
  idx_df <- idx_df * calc_n
  return(idx_df)
}


# This function creates a source table for a
# single facet G-study nested design.
# It takes as input a data.frame with rows as
# persons and columns as items. The output is
# a data.frame with degrees of freedom, SS, MS,
# expected MS and estimated variance component
# for persons (p), items (i) and pi.
source_table_ns <- function(scores){
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
  SS_i_p <- sum(scores ^ 2) -
    sum((person_m['mean_p']) ^ 2 ) * n_i
  df_p <- n_p - 1
  df_i_p <- (n_i - 1) * n_p
  degree_f <- c(df_p, df_i_p)
  SS <- c(SS_p, SS_i_p)
  source <- c('p', 'i:p')
  source.table <- data.frame(degree_f, SS,
                             MS = SS/degree_f,
                             row.names = source)
  var_p <- (source.table['p', 'MS'] -
              source.table['i:p', 'MS']) / n_i
  var_i_p <- source.table['i:p', 'MS']
  est_var_comp <- c(var_p, var_i_p)
  E_MS_p <- var_i_p + var_p * n_i
  E_MS_i_p <- var_i_p
  E_MS <- c(E_MS_p, E_MS_i_p)
  source.table <- bind_cols(source.table,
                            E_MS = E_MS,
                            est_var_comp = est_var_comp)
  return(source.table)
}


# This function calculates the absolute error
# variance in a single facet D-study nested
# design. It takes two inputs: a data.frame with
# rows as persons and columns as items, and the
# number of items in the sample of the study.
# The output is a numerical variable.
abs_error_var_ns <- function(scores, n_sample){
  return((source_table_ns(scores)['i:p', 'est_var_comp'] /
              n_sample))
}

# This function calculates the relative error
# variance in a single facet D-study nested
# design. It takes two inputs: a data.frame with
# rows as persons and columns as items, and the
# number of items in the sample of the study.
# The output is a numerical variable.
rel_error_var_ns <- function(scores, n_sample){
  return(source_table_ns(scores)['i:p', 'est_var_comp'] /
           n_sample)
}

# This function calculates the generalizability
# coefficient in a single facet D-study nested
# design. It takes two inputs:
# a data.frame with rows as persons and columns
# as items, and the number of items in the sample
# of the study. The output is a numerical variable.
gener_coef_ns <- function(scores, n_sample){
  var_p <- (source_table_ns(scores)['p', 'est_var_comp'])
  rel_error <- rel_error_var_ns(scores, n_sample)
  return(var_p / (var_p + rel_error))
}


# This function calculates the dependability
# coefficient in a single facet D-study nested
# design. It takes two inputs:
# a data.frame with rows as persons and columns
# as items, and the number of items in the sample
# of the study. The output is a numerical variable.
depend_coef_ns <- function(scores, n_sample){
  var_p <- (source_table_ns(scores)['p', 'est_var_comp'])
  rel_error <- abs_error_var_ns(scores, n_sample)
  return(var_p / (var_p + rel_error))
}


# This function calculates a table including
# absolute error variance, relative error
# variance, dependability coefficient and
# generalizability coefficient for a single
# facet D-study nested design.
# It takes two inputs: a data.frame with
# rows as persons and columns as items; and the
# number of items in the sample of the study
# either as a single numerical variable or
# as a vector containing multiple numerical
# variables. The output is a data.frame.
d_s_table_ns <- function(scores, n_samples){
  table_build <- t(data.frame(abs_e_var = 0,
                              rel_e_var = 0,
                              d_coef = 0, gen_coef = 0))
  for (i in seq(length(n_samples))){
    e_c <- c()
    e_c[1] <- rel_error_var_ns(scores, n_samples[i])
    e_c[2] <- gener_coef_ns(scores, n_samples[i])
    e_c[3] <- abs_error_var_ns(scores, n_samples[i])
    e_c[4] <- depend_coef_ns(scores, n_samples[i])
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

