# script for further variable selection and reduction

library(tidyverse)
library(caret)
library(magrittr)
library(stringr)

# list_of_tibbles <- readRDS(file = "2018-Feb-22_all_files_comb_vars.R")

input_dir <-"./output_3"

files.v <- files.v <- dir(path=input_dir, pattern=".*rds")




sampled_list_of_tibbles <- vector(mode = "list", length(files.v))


for(i in seq_along(files.v)) {
  start_time <- Sys.time()
  
  working.tib <- readRDS(file = paste(input_dir, files.v[i], sep = "/"))
  
  sampled_rows <- sample(nrow(working.tib), 1000)
  sampled_list_of_tibbles[[i]] <- working.tib[sampled_rows, ] 
  
  rm(working.tib)
 
  file_id <- paste0("file_", i) %>%
    rep(nrow(sampled_list_of_tibbles[[i]]))
  
  sampled_list_of_tibbles[[i]] <- file_id %>%
    as_tibble() %>%
    bind_cols(., sampled_list_of_tibbles[[i]])
  
  keepers.v <- c(1, 3:ncol(sampled_list_of_tibbles[[i]]))
  
  var_names <- colnames((sampled_list_of_tibbles[[i]]))
  var_names[1] <- "file_id"
  sampled_list_of_tibbles[[i]] <- set_colnames(sampled_list_of_tibbles[[i]], var_names)
  
  sampled_list_of_tibbles[[i]] <- sampled_list_of_tibbles[[i]] %>%
    select(keepers.v)
  
  end_time <-Sys.time()
  print(end_time - start_time)
  print(Sys.time())
  cat(files.v[i], "has been processed")
  
}

end_time <-Sys.time()
print(end_time - start_time)


combined_rel_freq.tib <- tibble(file_id="a", combined = "a")


start_time <- Sys.time()
for(i in seq_along(sampled_list_of_tibbles)) {
  
  start_time2 <- Sys.time()
  freq.tib <- gather(sampled_list_of_tibbles[[i]], variable_name, variable_value, -file_id, na.rm = TRUE) %>%
    mutate(combined = paste(paste(variable_name, variable_value, sep = "=="))) %>%
    select(file_id, combined) %>%
    group_by(file_id, combined) %>%
    summarize(n())  %>%
    arrange(desc(`n()`))
  
  rel_freq.tib <- freq.tib
  rel_freq.tib$`n()` <- freq.tib$`n()` %>%
    divide_by(nrow(sampled_list_of_tibbles[[i]])) %>%
    multiply_by(100)
  
  
  combined_rel_freq.tib <-bind_rows(combined_rel_freq.tib, rel_freq.tib)
  end_time2 <- Sys.time()
  print(paste("completed loop ", i))
  print(end_time2 - start_time2)
  
}

end_time <- Sys.time()
end_time - start_time
print(Sys.time())


##################






combined_rel_freq.tib %>%
  head() %>%
  View()

start_time <- Sys.time()
result <- xtabs(`n()` ~ file_id+combined, data = combined_rel_freq.tib, sparse = FALSE)  ## works maybe
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
result_2 <- apply(result, 2, as.numeric)
end_time <-Sys.time()
print(end_time - start_time)





start_time <- Sys.time()
result_2 <- as_tibble(result_2)
end_time <-Sys.time()
print(end_time - start_time)


result_2[1:15, 1:15] %>%
  View()


col_totals <- result_2 %>%
  colSums()

col_totals %>%
  summary()

culled_columns <- col_totals[which(col_totals >= 300) ]


culled_names <- names(culled_columns)

culled_names

baddies.v <- which( str_detect(culled_names, "g2-parent-sibCount") == TRUE) 

culled_names <- culled_names[-baddies.v]

culled_names


looksee <- readRDS(file = "culed_names.rds")
looksee

saveRDS(culled_names, file = "Feb-26-2018_culed_names.rds")

start_time <- Sys.time()
culled.tib <-  combined_rel_freq.tib %>%
  filter(combined %in% culled_names)
end_time <-Sys.time()
print(end_time - start_time)


start_time <- Sys.time()
result <- xtabs(`n()` ~ file_id+combined, data = culled.tib, sparse = FALSE)  ## works maybe
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
result_2 <- apply(result, 2, as.numeric)
end_time <-Sys.time()
print(end_time - start_time)





start_time <- Sys.time()
result_2 <- as_tibble(result_2)
end_time <-Sys.time()
print(end_time - start_time)

sorted_col_totals <- order(col_totals, decreasing = TRUE)


result_2 <- result_2 %>%
  select(as.numeric(sorted_col_totals))
