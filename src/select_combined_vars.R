## This script is used to select combined variables for further consideration.

require(XML)
require(tidyverse)
require(stringr)

input.dir <- "./output_2"

files.v <- dir(path=input.dir, pattern=".*xml")




generate_vars <- function(var.list, df) {
  a <- unlist(var.list)
  b <- df[, a]
  if (length(a) > 1) {
    c <- apply(b, 1, paste0, collapse = "/")
  } else {
    c <- b 
  }
  return(c)
}


generate_var_names <- function(var.list) {
  a <- unlist(var.list)
  b <- paste0(a, collapse = "_&_")
  return(b)
}




doc.object <- xmlTreeParse(file.path(input.dir, files.v[1]), useInternalNodes=TRUE)


word.nodes <- getNodeSet(doc.object, "//word")


word.list <- xmlApply(word.nodes, xmlToList)


var_names.v <- map(word.list, names) %>%
  unlist() %>%
  unique()


var_names.v <- var_names.v[-which(var_names.v == "insertion_id" | var_names.v == "artificial") ]


var_names.v <- var_names.v[19:length(var_names.v)] # drop non-expanded attributes as variables

saveRDS(var_names.v, file = "var_names_Feb-24-18")


holder.tib <- tibble(variable_name="blank")

selected_vars.list <- list() # make empty list object to store result of loop
nomina.v <- NULL # make empty vector to store names


for (k in 1:3) { 
  selected_vars.list[[k]] <- combn(var_names.v, k, simplify = FALSE) # make all possible combinations of variables
  nomina.v <- paste(length(var_names.v), "Choose",  k, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}

names(selected_vars.list) <- nomina.v  # assign names to list elements



for(i in seq_along(files.v)) {
  
  start_time_1 <- Sys.time()
  
  # read xml structure from file to .R object
  doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]), useInternalNodes=TRUE)
  
  # extract all <word> elements and children into XmlNodeList object
  word.nodes <- getNodeSet(doc.object, "//word")
  
  
  word.list <- xmlApply(word.nodes, xmlToList)
  
  
sample_words.v <-   sample(seq_len(length(word.list)), 1000)

sampled_word.list <- word.list[sample_words.v]
  
  
 
  for (j in seq_along(var_names.v)) {
    
    if ( j == 1) {
      
      base.df <- sampled_word.list %>% map_chr(var_names.v[j]) %>%
        data.frame(check.names = FALSE, stringsAsFactors = FALSE)
      
    } else {
      
      base.df <- sampled_word.list %>% map_chr(var_names.v[j]) %>%
        cbind(base.df,  ., stringsAsFactors = FALSE)
      
    }
    
  } # end of j loop
  
  colnames(base.df) <- var_names.v
  
 # sample_rows <- sample(nrow(base.df), 1000)
  
 #  sampled_base.df <- base.df[sample_rows, ]
  
  
  
 
  
  new_vars.m <- matrix(nrow = nrow(sampled_base.df), ncol = 1)
  
  
  
  for (m in seq_along(selected_vars.list)) {
    start_time_2 <- Sys.time()
 
    a <- sapply(selected_vars.list[[m]], generate_vars, df = sampled_base.df)
    
    nomina <- sapply(selected_vars.list[[m]], generate_var_names)
    
    colnames(a) <- nomina
    
    new_vars.m <- cbind(new_vars.m, a)
    
    end_time_2 <- Sys.time()
    print(paste("m loop has finished iteration ", m, "of 3 for file ", files.v[i]))
    print(end_time_2 - start_time_2)
    print(Sys.time())
    
    
  }
  
  print(Sys.time())
  
  
  new_vars.m <- new_vars.m[, -1]
  
  
  new_vars.m[str_detect(new_vars.m, "NA")] <- NA # add logical NA to cells
  print(Sys.time())
  
  
  new_vars.m[str_detect(new_vars.m, "NULL")] <- NA # add logical NA to cells
  print(Sys.time())
  
  new_vars.m <-tolower(new_vars.m)
  
  vars.tib <- as_tibble(new_vars.m)
  
  rm(new_vars.m)
  
  
  file_var_ct.tib <-   gather(vars.tib, variable_name, variable_value, na.rm = TRUE) %>%
    group_by(variable_name) %>%
    summarize(n())
  print(Sys.time())
 
  
  holder.tib <- bind_rows(holder.tib, file_var_ct.tib)
  print(Sys.time())
  
  end_time_1 <- Sys.time()
  print(paste(files.v[i], "has been processed"))
  print(end_time_1 - start_time_1)
  print(Sys.time())
  
}

start_time_1 <- Sys.time()
agg_var_ct.tib  <- xtabs(`n()` ~ variable_name, data = holder.tib) %>%
  as_tibble() # combine all frequencies in holder.tib to in one table
end_time_1 <- Sys.time()
print(end_time_1 - start_time_1)
print(Sys.time())


saveRDS(agg_var_ct.tib, file = "cross_tab_aggr_Feb-24-18.rds")

agg_var_ct.tib$n %>%
  quantile(., probs = (seq(0.9, 1, 0.001)))

shorter_var.tib <- agg_var_ct.tib %>% 
  filter(n >= 13545) %>% # select value that will give reasonable number of variables
  arrange(desc(n))

saveRDS(shorter_var.tib, file = "vars_top-5_Feb-24-18.rds")

keepers.v <- NULL

keepers.v <- append(keepers.v,   apply(shorter_var.tib[,1], 1, `[`))

reduced_atomic_vars.list <- sapply(keepers.v, strsplit, split = "_&_")

saveRDS(reduced_atomic_vars.list, file = "reduced_var_list_Feb-14-18.rds")



##############

