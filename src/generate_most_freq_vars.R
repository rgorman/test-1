## This script generates a limited set of new variables for each file in the input directory.

library(XML)
library(tidyverse)
library(stringr)

most_frequent.list <- readRDS("most_freq_list.R")



var_names.v <- readRDS("variable_names.R")

input.dir <- "./output_2"

files.v <- dir(path=input.dir, pattern=".*xml")

list_of_working_tibbles <- vector(mode = "list", length(files.v))

for (i in seq_along(files.v)) {
  
  start_time <- Sys.time()
  print(start_time)
  
  # read xml structure from file to .R object
  doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]), useInternalNodes=TRUE)
  
  # extract all <word> elements and children into XmlNodeList object
  word.nodes <- getNodeSet(doc.object, "//word")
  
  
  word.list <- xmlApply(word.nodes, xmlToList)
  
  
  
  
  for (j in seq_along(var_names.v)) {
    
    if ( j == 1) {
      
      base.df <- word.list %>% map_chr(var_names.v[j]) %>%
        data.frame(check.names = FALSE, stringsAsFactors = FALSE)
      
    } else {
      
      base.df <- word.list %>% map_chr(var_names.v[j]) %>%
        cbind(base.df,  ., stringsAsFactors = FALSE)
      
    }
    
  } # end of j loop
  
  colnames(base.df) <- var_names.v
  
  ###
  
  new_vars.m <- matrix(nrow = nrow(base.df), ncol = 1)
  nomina <- NULL
  
  for (k in seq_along(most_frequent.list)) {
    var_atoms <- most_frequent.list[[k]]
    if (length(var_atoms) > 1) {
      combined_var <- base.df[, var_atoms] %>%
        apply(., 1, paste0, collapse = "/")
      
    } else {
      combined_var <- base.df[, var_atoms]
      
    }
    new_vars.m <- cbind(new_vars.m, combined_var)
    
    nomina <- append(nomina, paste0(var_atoms, collapse = "_&_"))
  } # end of loop k
  
  
  new_vars.m <- new_vars.m[, -1]
  
  
  new_vars.m[str_detect(new_vars.m, "NA")] <- NA # add logical NA to cells
  
  
  new_vars.m[str_detect(new_vars.m, "NULL")] <- NA # add logical NA to cells
  
  new_vars.m <-tolower(new_vars.m)
  
  nomina <-  c("token_id", "cite", "form", nomina)
  
  
  file_name <- files.v[i] %>%
    gsub(".xml","", .)
  
  token_id <- seq_len(nrow(base.df)) %>%
    paste0(file_name, "_token_", .) 
  
  
  new_vars.m <- cbind(token_id, base.df[, "cite"], base.df[, "form"], new_vars.m)
  
  colnames(new_vars.m) <- nomina
  
  vars.tib <- as_tibble(new_vars.m)
  
  list_of_working_tibbles[[i]] <- vars.tib
  
  end_time <- Sys.time()
  print(paste("end of loop", i, files.v[i]))
  print(end_time - start_time)
  
  start_time <- Sys.time()
  print(paste("saving variables for", files.v[i]))
  saveRDS(list_of_working_tibbles, file = "all_files_comb_vars.R")
  end_time <- Sys.time()
  print(end_time - start_time)
  
  
} #end of main loop 


