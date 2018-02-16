# script to test performance of chunk population

library(tidyverse)
library(caret)
library(magrittr)

list_of_tibbles <- readRDS(file = "all_files_comb_vars.R")
culled_names <- readRDS(file = "culed_names.rds")

removal.v <- ls()
removal.v <- removal.v[-3]

rm(list = removal.v)
gc()


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


add_chunk <- function(token_name, chunk_number) {
  gsub("token.*", "", token_name)
  
}



start_time <- Sys.time()
folds <- NULL

for(i in seq_along(list_of_tibbles)) {
  folds <- part_tibs(list_of_tibbles[[i]], 50) %>%
    length() %>%
    append(folds, .)
  
}


end_time <-Sys.time()
print(end_time - start_time)




start_time <- Sys.time()

total_folds <- sum(folds)

chunk.list <- vector(mode = "list", total_folds)
n <- 1

for(i in seq_along(list_of_tibbles)) {
  tib_part <- list_of_tibbles[[i]] %>%
    part_tibs( 50)
  
  for(j in seq_along(tib_part)) {
    target.tib <- list_of_tibbles[[i]]
    chunk.list[[n]] <- target.tib[tib_part[[j]], ]
    n <- n + 1
    
  }
}


end_time <-Sys.time()
print(end_time - start_time)




start_time <- Sys.time()
n <- 1

for(i in seq_along(folds)) {
  
  for(j in seq_len(folds[i])) {
    
    chunk_id <- paste0(sapply(chunk.list[[n]][, 1], add_chunk), "chunk_", j)
    
    chunk.list[[n]] <- chunk.list[[n]][, 4:ncol(chunk.list[[n]])]
    
    chunk.list[[n]] <- chunk_id %>%
      as_tibble() %>%
      bind_cols(., chunk.list[[n]])
    
    var_names <- colnames(chunk.list[[n]])
    var_names[1] <- "chunk_id"
    chunk.list[[n]] <- set_colnames(chunk.list[[n]], var_names)
    
  n <- n + 1
    
  }
  
}



folds[i]

chunk.list[[100]] %>%
  head() %>%
  View()


end_time <-Sys.time()
print(end_time - start_time)



combined_rel_freq.tib <- tibble(chunk_id="a", combined = "a")


start_time <- Sys.time()
for(i in seq_along(chunk.list)) {
  
  start_time2 <- Sys.time()
  freq.tib <- gather(chunk.list[[i]], variable_name, variable_value, -chunk_id, na.rm = TRUE) %>%
    mutate(combined = paste(paste(variable_name, variable_value, sep = "=="))) %>%
    select(chunk_id, combined) %>%
    group_by(chunk_id, combined) %>%
    summarize(n())  %>%
    arrange(desc(`n()`))
  
  freq.tib <- freq.tib %>%
    filter(combined %in% culled_names)
  
  rel_freq.tib <- freq.tib
  rel_freq.tib$`n()` <- freq.tib$`n()` %>%
    divide_by(nrow(chunk.list[[i]])) %>%
    multiply_by(100)
  
  
  combined_rel_freq.tib <-bind_rows(combined_rel_freq.tib, rel_freq.tib)
  end_time2 <- Sys.time()
  print(paste("completed loop ", i))
  print(end_time2 - start_time2)
  
}

end_time <- Sys.time()
end_time - start_time

rm(chunk.list)




start_time <- Sys.time()
result <- xtabs(`n()` ~ chunk_id+combined, data = comb_3.tib, sparse = FALSE)  ## works maybe
end_time <- Sys.time()
end_time - start_time

object.size(result)

start_time <- Sys.time()
result_2 <- apply(result, 2, as.numeric)
end_time <-Sys.time()
print(end_time - start_time)

names_for_rows <- NULL
n <- 1
for(i in seq_along(folds)) {
  for(j in seq_len(folds[i])) {
    names_for_rows <- chunk.list[[n]][1, 1] %>%
      unlist() %>%
      append(names_for_rows, .)
    n <- n + 1
  }
    
}


start_time <- Sys.time()
result_2 <- as_tibble(result_2)
end_time <-Sys.time()
print(end_time - start_time)

order_for_cols <- colSums(result_2) %>%
  order(decreasing = TRUE)

result_2 <- result_2 %>%
  select(order_for_cols)
  
  
x <- comb_3.tib %>%
   transmute(new_id = paste0("a_", chunk_id))

comb_a.tib <- comb_3.tib
comb_a.tib[, 1] <- x
 
 rm(comb_a.tib, x)
 
 comb_3.tib <- bind_rows(comb_a.tib, comb_3.tib)
 
 
saveRDS(combined_rel_freq.tib, file = "long_tib_200_vars.rds")
removal.v
