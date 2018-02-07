# This script extracts bag-of-word chunks of tokens and their variable values for selected chunk sizes.

# all_files_comb_vars.R

library(tidyverse)
library(caret)
library(magrittr)

list_of_tibbles <- readRDS(file = "all_files_comb_vars.R")
tib_1 <- list_of_tibbles[[1]]

tib_1 %>%
  head() %>%
  View()





part_tibs <- function(tib, tokens) { # function to create n-fold partition of input tibble
  k_folds <- tib %>%
    nrow() %>%
    divide_by(tokens) %>%
    ceiling()
  
  p <- tib %>%
    nrow() %>%
    seq_len() %>%
    createFolds(k_folds)
  
  return(p)
  
}


tokens <- 1000

x <- part_tibs(list_of_tibbles[[1]], 3000)

tib_1[x[[1]],] %>%
  head() %>%
  View()




folds <- NULL

for(i in seq_along(list_of_tibbles)) {
  folds <- part_tibs(list_of_tibbles[[i]], 3000) %>%
    length() %>%
    append(folds, .)
  
  
  
}

total_folds <- sum(folds)

chunk.list <- vector(mode = "list", total_folds)
n <- 1

for(i in seq_along(list_of_tibbles)) {
  tib_part <- list_of_tibbles[[i]] %>%
    part_tibs( 3000)
  
  for(j in seq_along(tib_part)) {
    target.tib <- list_of_tibbles[[i]]
    chunk.list[[n]] <- target.tib[tib_part[[j]], ]
    n <- n + 1
    
  }
}

chunk.list %>%
  lengths()



chunk.list[[201]] %>%
  head() %>%
  View()

lengths(chunk.list)


