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



x <- part_tibs(list_of_tibbles[[1]], 3000)

tib_1[x[[1]],] %>%
  head() %>%
  View()




folds <- NULL

for(i in seq_along(list_of_tibbles)) {
  folds <- part_tibs(list_of_tibbles[[i]], 2000) %>%
    length() %>%
    append(folds, .)
  
}




total_folds <- sum(folds)

chunk.list <- vector(mode = "list", total_folds)
n <- 1

for(i in seq_along(list_of_tibbles)) {
  tib_part <- list_of_tibbles[[i]] %>%
    part_tibs( 2000)
  
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


tib_2 <- chunk.list[[1]]

tib_3 %>%
  head() %>%
  View()

x <- tib_2[, 1] %>% map( ~gsub("token.*", "", tib_2[, 1]))

x <- sapply(tib_2[, 1], extract)

add_chunk <- function(token_name, chunk_number) {
  gsub("token.*", "", token_name)
  
}

seq_len(folds[1])

chunk_id <- paste0(sapply(x, add_chunk), "chunk_", 1)


########## add chunk id as column to tibble

n <- 1
for(i in seq_along(folds)) {
  
  for(j in seq_len(folds[i])) {
    
    chunk_id <- paste0(sapply(chunk.list[[n]][, 1], add_chunk), "chunk_", j)
    
    chunk.list[[n]] <- chunk_id %>%
      as_tibble() %>%
      bind_cols(., chunk.list[[n]])
    
    var_names <- colnames(chunk.list[[n]])
    var_names[1] <- "chunk_id"
    chunk.list[[n]] <- set_colnames(chunk.list[[n]], var_names)
    
    n <- n + 1
    
  }
  
}


###########################################




i <- 1
j <- 1

x <- chunk.list[[235]]

x %>%
  head() %>%
  View()


folds


chunk_id
rm(chunk_id)
dim(chunk.list[[1]])
2657*188

#############



chunk_id %>% as_tibble()

tib_4 <- chunk_id %>%
  as_tibble() %>%
  bind_cols(., tib_2)

tib_4 %>%
  head() %>%
  View()


sapply(x, add_chunk)

gsub("token.*", "", x[1])

tib_2 <- chunk.list[[1]]

vars.long.tib <- gather(chunk.list[[1]][, -c(2, 3, 4)], variable_name, variable_value, -chunk_id, na.rm = TRUE)

freq.tib <- gather(chunk.list[[1]][, -c(2, 3, 4)], variable_name, variable_value, -chunk_id, na.rm = TRUE) %>%
  mutate(combined = paste(paste(variable_name, variable_value, sep = "=="))) %>%
  select(chunk_id, combined) %>%
  group_by(chunk_id, combined) %>%
  summarize(n()) %>%
  arrange(desc(`n()`))

rm(combined_freq.tib)

seq_along(chunk.list)

combined_rel_freq.tib <- tibble(chunk_id="a", combined = "a")


start_time <- Sys.time()
for(i in seq_along(chunk.list)) {
  
  start_time2 <- Sys.time()
  freq.tib <- gather(chunk.list[[i]][, -c(2, 3, 4)], variable_name, variable_value, -chunk_id, na.rm = TRUE) %>%
    mutate(combined = paste(paste(variable_name, variable_value, sep = "=="))) %>%
    select(chunk_id, combined) %>%
    group_by(chunk_id, combined) %>%
    summarize(n())  %>%
    arrange(desc(`n()`))
    
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


rel_freq.tib <- freq.tib
rel_freq.tib$`n()` <- freq.tib$`n()` %>%
  divide_by(nrow(chunk.list[[i]])) %>%
  multiply_by(100)

check_var.tib <- freq.tib$`n()` %>%
  divide_by(nrow(chunk.list[[i]])) %>%
  multiply_by(100) %>%
  as_tibble()

which(check_var.tib$value >= 1)

nrow(chunk.list[[i]])

object.size(combined_freq.tib)

row_names.tib <- result %>%
  row.names() %>%
  as_tibble()

head(combined_freq.tib)
head(result)
dim(result)

result[1, 1]
row.names(result)

start_time <- Sys.time()
result <- xtabs(`n()` ~ chunk_id+combined, data = combined_rel_freq.tib, sparse = FALSE)  ## works maybe
end_time <- Sys.time()
end_time - start_time

object.size(result)
result <- apply(result, 2, as.numeric)
object.size(result)

result_2 <- as_tibble(result)

result_2 %>%
  head() %>%
  View()


summary(col_totals.tib$value)

sum(result_2[, 1:2 ])

culled_cols <-  which(col_totals.tib$value >= floor(346/5)) 

result_3 <- result_2 %>%
  select(culled_cols)

result_3 <- result_3 %>%
  select(ordered_cols)

result_3[1:10, 1:10] %>%
  View()
  
ordered_cols <-  result_3 %>%
  colSums() %>%
  order(decreasing = TRUE)

col_totals.tib <- apply(result_2, 2, sum) %>%
  as_tibble()

result_2[, 35:57] %>%
  sapply(., sum)

rm(col_totals)

result_2[1,1]


floor(346/5)




result_2[1:10,] %>% separate(col = combined, into = some_names) %>%
  View()

some_names <- result_2[1:10, 2] %>%
  unlist()

result_2 %>%
  head() %>%
  View()

col_totals.tib %>% 
  arrange(desc(value)) %>%
  View()

col_totals.tib$value %>%
  summary()

col_totals.tib %>%
  filter(value >= 15000) %>%
  nrow()

283930/185

culled_cols <- order(col_totals[1:10])

which(col_totals == max(col_totals)) %>%
  length()

result_2[, 1:10] %>%
  summarise_each(funs(sum)) %>%
  View()

result_2 %>%
  select(culled_cols)


chunk.list %>%
  sapply(nrow) %>%
  sum()

15135/657888
40.25*1709
1709 * .4025
 
head(result)

head(combined_freq.tib)

dim(result)
result[1:2,]

result_2 <- as_tibble(result)

object.size(freq.tab)

which(freq.tab[, 3] >= 100) %>%
  length()

quantile(unlist(freq.tab[, 3]), seq(0, 1, .01))


vars.long.tib %>%
  head() %>%
  View()

tib_3 <- set_colnames(tib_3[1], "chunk_id")
head(tib_3)


var_names <- colnames(tib_3)
var_names[1] <- "chunk_id"
tib_3 <- set_colnames(tib_3, var_names)
