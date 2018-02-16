# script for further variable selection and reduction

library(tidyverse)
library(caret)
library(magrittr)

list_of_tibbles <- readRDS(file = "all_files_comb_vars.R")

y <- list_of_tibbles[[1]]

keepers.v <- c(1, 5:188)


length(list_of_tibbles)

start_time <- Sys.time()
for(i in seq_along(list_of_tibbles)) {
  file_id <- paste0("file_", i) %>%
    rep(nrow(list_of_tibbles[[i]]))
  
  list_of_tibbles[[i]] <- file_id %>%
    as_tibble() %>%
    bind_cols(., list_of_tibbles[[i]])
  
  var_names <- colnames((list_of_tibbles[[i]]))
  var_names[1] <- "file_id"
  list_of_tibbles[[i]] <- set_colnames(list_of_tibbles[[i]], var_names)
  
  list_of_tibbles[[i]] <- list_of_tibbles[[i]] %>%
    select(keepers.v)
  
}

end_time <-Sys.time()
print(end_time - start_time)


combined_rel_freq.tib <- tibble(file_id="a", combined = "a")


start_time <- Sys.time()
for(i in seq_along(list_of_tibbles)) {
  
  start_time2 <- Sys.time()
  freq.tib <- gather(list_of_tibbles[[i]][, -c(2, 3, 4)], variable_name, variable_value, -file_id, na.rm = TRUE) %>%
    mutate(combined = paste(paste(variable_name, variable_value, sep = "=="))) %>%
    select(file_id, combined) %>%
    group_by(file_id, combined) %>%
    summarize(n())  %>%
    arrange(desc(`n()`))
  
  rel_freq.tib <- freq.tib
  rel_freq.tib$`n()` <- freq.tib$`n()` %>%
    divide_by(nrow(list_of_tibbles[[i]])) %>%
    multiply_by(100)
  
  
  combined_rel_freq.tib <-bind_rows(combined_rel_freq.tib, rel_freq.tib)
  end_time2 <- Sys.time()
  print(paste("completed loop ", i))
  print(end_time2 - start_time2)
  
}

end_time <- Sys.time()
end_time - start_time


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





col_totals <- result_2 %>%
  colSums()

col_totals %>%
  summary()

culled_columns <- col_totals[which(col_totals >= 17) ]


culled_names <- names(culled_columns)


saveRDS(culled_names, file = "culed_names.rds")

start_time <- Sys.time()
combined_rel_freq.tib %>%
  filter(combined %in% culled_names)
end_time <-Sys.time()
print(end_time - start_time)

