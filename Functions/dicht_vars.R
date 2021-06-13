# Dichotomous Variable Analysis

# Item difficulties, item variances, item correlations,
# item covariances, correlation tables, composite scores
# of tests, true scores of test participants.

library(tidyverse)

# Function to find item with maximum difficulty in
# a test consisting exclusively of dichotomous items.
# Input is data frames with columns as items and
# observation as rows. In retrospect, this could
# be more modular. Inspired  by:
# https://www.geeksforgeeks.org/find-column-with-maximum-sum-in-a-matrix/
high_diff <- function(item_frame) {
  #item_diff <- data.frame(item_frame)
  # Variable to store index of column with maximum
  idx = -1
  # Variable to store maximum difficulty
  maxDiff = -10 ** 9
  for (i in seq(ncol(item_frame))){
    # Calculate difficulty of each item/column
    difficulty <- (sum(item_frame[i])/nrow(item_frame))
    #add_row(item_diff, item_diff[i] = difficulty)
    if ( difficulty >= maxDiff) {
      # Update maxDiff if it is less 
      # than current Diff
      maxDiff = difficulty
      #store column index
      idx = i
    }
  }
  return(list(idx, maxDiff))
}


# Function takes a frame with dichotomous questions as
# columns and responses as rows, generating a row of
# difficulties as output.
dicht_diff <- function(item_frame){
  difficulty_row <- c()
  for (i in seq(ncol(item_frame))){
    # Calculate difficulty of each item/column
    difficulty <- (sum(item_frame[i])/nrow(item_frame))
    item_id <- colnames(item_frame[i])
    difficulty_row[item_id] <- difficulty
  }
  return(difficulty_row)
}

# Much more efficient version of the previous function.
# Function takes a frame with dichotomous questions as
# columns and responses as rows, generating a single-
# column dataframe with item difficulties as output.
dicht_diff_better <- function(item_df){
  k <- nrow(item_df)
  p <- as.data.frame(colSums(item_df == 1) / k)
  return(p)
}

# Function takes a frame with dichotomous questions as
# columns and responses as rows, generating a row of
# variances as output.
dicht_var <- function(item_frame) {
  diff_row <- dicht_diff(item_frame)
  dicht_var_row <- diff_row*(1 - diff_row)
  return(dicht_var_row)
}


# Function takes a frame with dichotomous questions as
# columns and responses as rows, generating a row of
# standard deviation as output.
dicht_std_dev <- function(item_frame) {
  return(sqrt(dicht_var(item_frame)))
}


# This function has four inputs. As the previous ones,
# it takes a frame with dichotomous questions as
# columns and responses as rows, in addition to
# two item IDs, and the option to print or not an output table.
# As output, the function returns the correlation
# coefficient between the two given items and prints a joint-
# frequency table in case received TRUE as a fourth input.
# Definitely could more modular, separating the
# joint frequency table printing from the simple
# calculation output.
dicht_cor <- function(item_frame, itemA, itemB, tableOpt = FALSE) {
  iA_iB <- count(item_frame, item_frame[itemA] == 0 &
                   item_frame[itemB] == 0)[2, 2]
  cA_cB <- count(item_frame, item_frame[itemA] == 1 &
                   item_frame[itemB] == 1)[2, 2]
  cA_iB <- count(item_frame, item_frame[itemA] == 1 &
                   item_frame[itemB] == 0)[2, 2]
  iA_cB <- count(item_frame, item_frame[itemA] == 0 &
                   item_frame[itemB] == 1)[2, 2]
  
  joint_freq <- data.frame('cA' = c(cA_iB, cA_cB),
                           'iA' = c(iA_iB, iA_cB))
  row.names(joint_freq) <- c('iB', 'cB')
  
  joint_freq[is.na(joint_freq)] = 0
  dicht_cor_coef <- (((joint_freq[1, 2] * joint_freq[2, 1]) -
                        (joint_freq[1, 1] * joint_freq[2, 2])) /
                       sqrt((joint_freq[1, 2] + joint_freq[2, 2]) *
                              (joint_freq[1, 2] + joint_freq[1, 1]) *
                              (joint_freq[2, 1] + joint_freq[1, 1]) *
                              (joint_freq[2, 1] + joint_freq[2, 2])))
  
  if (tableOpt == TRUE){
    print(joint_freq) 
  }
  return(dicht_cor_coef[[1]])
}


# This function has three inputs. As the previous ones,
# it takes a frame with dichotomous questions as
# columns and responses as rows, in addition to
# two item IDs. As output, the function returns
# the covariance between the two given items.
dicht_covar <- function(item_frame, itemA, itemB){
  corAB <- dicht_cor(item_frame, itemA, itemB, FALSE)
  std_all <- dicht_std_dev(item_frame)
  stdA <- std_all[itemA]
  stdB <- std_all[itemB]
  covarAB <- corAB * stdA * stdB
  return(covarAB)
}


# This function takes two inputs: a correlation
# table in data.frame format, and single-column
# data.frame with variances. It then creates a
# variance-covariance matrix as output, also in
# data.frame format.
var_covar_maker <- function(cor_frame, var_col) {
  var_covar_table <- cor_frame
  for (i in seq(ncol(var_covar_table))) {
    var_covar_table[,i] <-
      var_covar_table[,i] * sqrt(var_col[i,1])
  }
  for (i in seq(nrow(var_covar_table))) {
    var_covar_table[i,] <-
      var_covar_table[i,] * sqrt(var_col[i,1])
  }
  
  colnames(var_covar_table) <-
    seq(ncol(var_covar_table))
  return(var_covar_table)
}


# This function also takes two inputs: a correlation
# table in data.frame format, and single-column
# data.frame with variances. But then it sums
# the composite variance of the test as an output
# instead of the whole table.
composite_variance <- function(cor_frame, var_cor) {
  return(sum(var_covar_maker(cor_frame, var_cor)))
}


# Takes in two inputs: 
# a table, with expected scores for each item as
# columns and observations as rows; and a vector
# with ordered list of scores.
hypot_true_scores <- function(score_frame, scores) {
  score_frame['true_score'] = 0
  for (i in seq(ncol(score_frame)-1)) {
    score_frame['true_score'] =
      score_frame['true_score'] + score_frame[i] *
      scores[i]
  }
  return(score_frame['true_score'])
}


# Just an off-the-cuff function to calculate
# the variance of true scores.
hypot_true_score_var <- function(true_scores_table) {
  true_scores_mean <-
    sum(true_scores_table)/nrow(true_scores_table)
  sum_score_variation = 0
  for (i in seq(nrow(true_scores_table))) {
    sum_score_variation = sum_score_variation +
      (true_scores_table[i,] - true_scores_mean) ^ 2
  }
  return(sum_score_variation/nrow(true_scores_table))
}
