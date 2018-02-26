## This script generates a limited set of new variables for each file in the input directory.

library(XML)
library(tidyverse)
library(stringr)

most_frequent.list <- readRDS("reduced_var_list_Feb-14-18.rds")

most_frequent.list <- reduced_atomic_vars.list

var_names.v <- readRDS("var_names_Feb-24-18")

input.dir <- "./output_2"

files.v <- dir(path=input.dir, pattern=".*xml")

# list_of_working_tibbles <- vector(mode = "list", length(files.v))

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
    
    if (mod(k, 500) == 0) {
      cat("k loop", k, "\n")
      print(Sys.time())
    }
    
  } # end of loop k
  
  
  new_vars.m <- new_vars.m[, -1]
  
  
  new_vars.m[str_detect(new_vars.m, "NA")] <- NA # add logical NA to cells
  
  
  new_vars.m[str_detect(new_vars.m, "NULL")] <- NA # add logical NA to cells
  
  new_vars.m <-tolower(new_vars.m)
  
   nomina <-  c("token_id", nomina)
  
  
  file_name <- files.v[i] %>%
    gsub(".xml","", .)
  
  token_id <- seq_len(nrow(base.df)) %>%
    paste0(file_name, "_token_", .) 
  
  
  new_vars.m <- cbind(token_id, new_vars.m)
  
  colnames(new_vars.m) <- nomina
  
  vars.tib <- as_tibble(new_vars.m)
  
  end_time <- Sys.time()
  print(paste("end of loop", i, files.v[i]))
  print(end_time - start_time)
  
  file_name <- gsub(".xml", "", files.v[i])
  
  
  start_time <- Sys.time()
  print(paste("saving variables for", files.v[i]))
  saveRDS(vars.tib, file = paste0("./output_3/", file_name, ".rds"))
  end_time <- Sys.time()
  print(end_time - start_time)
  
  rm(vars.tib)
  
} #end of main loop 





