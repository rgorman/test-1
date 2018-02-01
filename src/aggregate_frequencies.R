## A script to calculate the corpus-wide frequencies of combined variables with values > 0. 
## These frequencies will be used to select a subset of combined variables for further processing.


require(XML)
require(tidyverse)
require(stringr)

input.dir <- "./output_2"
files.v <- dir(path=input.dir, pattern=".*xml")

y <- readRDS(file = "variable_names.R") #pre-generated names of variables in XML input.
group_1.list <- readRDS(file = "combined_var_names.R")

############## user-defined  functions
generate_vars <- function(x) {
 # a <- unlist(x)
  b <- z[, a]
  if (length(a) > 1) {
    c <- apply(b, 1, paste0, collapse = "-")
  } else {
    c <- b 
  }
  return(c)
}


generate_var_names <- function(x) {
#  a <- unlist(x)
  b <- paste0(a, collapse = "_&_")
  c <- NULL
  c <- append(c, b)
  return(c)
}

######################

output.m <- matrix(ncol = 1)
colnames(output.m) <- c("variable_name")

output.tib <- as_tibble(output.m)



for (i in 18:35)  {
  
  # read xml structure from file to .R object
  doc.object <- xmlTreeParse(file.path(input.dir, files.v[i]), useInternalNodes=TRUE)
  
  # extract all <word> elements and children into XmlNodeList object
  word.nodes <- getNodeSet(doc.object, "//word")
  
  
  word.list <- xmlApply(word.nodes, xmlToList)
  
  
  for (j in seq_along(y)) {
    
    if ( j == 1) {
      
      z <- word.list %>% map_chr(y[j]) %>%
        data.frame(check.names = FALSE, stringsAsFactors = FALSE)
      
    } else {
      
      z <- word.list %>% map_chr(y[j]) %>%
        cbind(z,  ., stringsAsFactors = FALSE)
      
    }
    
  }
  
  colnames(z) <- y # add column names to z
  
  new_vars.m <- z[,1:2]
  
  for (k in 1:3) {
    
    start_time <- Sys.time()
    a <- sapply(group_1.list[[k]], generate_vars)
    
    nomina <- sapply(group_1.list[[k]], generate_var_names)
    
    colnames(a) <- nomina
    
    new_vars.m <- cbind(new_vars.m, a) %>%
      as.matrix()
    
    end_time <- Sys.time()
    
    print(paste(files.v[i], "loop", k))
    print(end_time - start_time)
    
    
  }
  
  
  start_time <- Sys.time()
  new_vars.m[str_detect(new_vars.m, "NA")] <- NA # add logical NA to cells
  end_time <- Sys.time()
  print(files.v[i])
  print(end_time - start_time)
  
  
  start_time <- Sys.time()
  new_vars.m[str_detect(new_vars.m, "NULL")] <- NA # add logical NA to cells
  end_time <- Sys.time()
  print(files.v[i])
  print(end_time - start_time)
  
 
 
 vars.tib <- as_tibble(new_vars.m[, -c(1, 2)])
 
 

  
  vars_long.tib <- gather(vars.tib, variable_name, variable_value,  na.rm = TRUE) %>%
    group_by(variable_name) %>%
    summarize(n())
  
output.tib <- bind_rows(output.tib, vars_long.tib)
  
 
  

   



print(paste("end of loop", i)) 
  
} # end of main loop


result <- xtabs(`n()` ~ variable_name, data = output.tib[ -1, ])
final_result.tib <- as_tibble(result, n = "count")

######################## create a set of the most commonly occuring variable types


most_frequent.tib <- final_result.tib %>%
  filter(count >=
           final_result.tib[, 2] %>%
           map(`[`) %>%
           unlist() %>%
           quantile(.99)) # argument of quantile() should be frequency precentile representing lower limit of variables selected




final_result.tib[2, 1] %>%
  as.character() %>%
  strsplit("_&_")

most_frequent.list <- most_frequent.tib[, 1] %>%
  map(`[`) %>%
  unlist() %>%
  strsplit("_&_")
  
  

generate_vars <- function(x) {
  a <- unlist(x)
  b <- z[, a]
  if (length(a) > 1) {
    c <- apply(b, 1, paste0, collapse = "-")
  } else {
    c <- b 
  }
  return(c)
}


a <- most_frequent.list[[2]]
e <- z[, d]
apply(e, 1, paste0, collapse = "-")

seq_along(most_frequent.list)

new_vars.m <- z[,1:2]

for (k in seq_along(most_frequent.list)) {
  
  start_time <- Sys.time()
  a <- sapply(most_frequent.list[[k]], generate_vars)
  
  nomina <- sapply(most_frequent.list[[k]], generate_var_names)
  
  colnames(a) <- nomina
  
  new_vars.m <- cbind(new_vars.m, a) %>%
    as.matrix()
  
  end_time <- Sys.time()
  
  print(paste(files.v[i], "loop", k))
  print(end_time - start_time)
  
  
}

#############################
new_vars.m <- matrix(nrow = nrow(z), ncol = 1)
nomina <- NULL

for (k in seq_along(most_frequent.list)) {
  x <- most_frequent.list[[k]]
  if (length(x) > 1) {
    a <- z[, x] %>%
      apply(., 1, paste0, collapse = "/")
    
  } else {
    a <- z[, x]
    
  }
  new_vars.m <- cbind(new_vars.m, a)
  
  nomina <- append(nomina, paste0(x, collapse = "_&_"))
}


new_vars.m <- new_vars.m[, -1]

colnames(new_vars.m) <- nomina

vars.tib <-as_tibble(new_vars.m)

file_name <- files.v[35] %>%
  gsub(".xml","", .)

token_id <- seq_len(nrow(z)) %>%
  paste0(file_name, "_token_", .) 

meta_data <- cbind(token_id, z[, "cite"], z[, "form"])
colnames(meta_data) <- c("token_id", "cite", "form")

vars.tib <- cbind(meta_data, vars.tib)


start_time <- Sys.time()
vars.long.tib <- gather(vars.tib, variable_name, variable_value, -token_id, na.rm = TRUE)
end_time <- Sys.time()
print(end_time - start_time)


vars.long.tib %>%
  group_by(variable_name) %>%
  summarize(n())


start_time <- Sys.time()
var_value.tib <- vars.long.tib %>%
  transmute(combined = paste(variable_name, variable_value, sep = "=") ) 
end_time <- Sys.time()
end_time - start_time

var_value.tib %>%  # this works 
  group_by(combined) %>%
  summarize(n()) %>%
  arrange(desc(`n()`)) %>%
  filter(`n()` >= 850) %>%
  View()


######################################

names(new_vars.m) <- nomina
dim(new_vars.m)

names(new_vars.m[1])


list_of_working_tibbles <- vector(mode = "list", 35)
list_of_working_tibbles[[1]] <- vars.tib


v <- list_of_working_tibbles[[1]]

1193494 / 8551



colnames(new_vars.m[3:187])

new_vars.m <- new_vars.m[, -c(1, 2)]

names(new_vars.m) <- nomina

new_vars.m %>%
  class()

test.tib <- 

<- nomina

nomina

ncol(new_vars.m)
x <- most_frequent.list[[2]]
length(x)

z[, x] %>%
apply(., 1, paste0, collapse = "-")


sapply(most_frequent.list[[2]], generate_vars) 

sapply(most_frequent.list[[2]], generate_var_names)

class(x)


colnames(new_vars.m)

output.tib

saveRDS(output.tib, file = "variable_count_interrupted_2.R")

saveRDS(most_frequent.list, file = "most_freq_list.R")

arrange(final_result.tib, desc(count))
sum(final_result.tib[, 2])


final_result.tib[, 2] %>%
  summary()

count.v <- final_result.tib[, 2]

count.v <- as.numeric(sapply(final_result.tib[, 2], paste0))
quantile(count.v, seq(0.9, 1, 0.005))

which(count.v >= 102473)

final_result.tib[which(final_result.tib[,2] >= 269126), ] %>%
  arrange(desc(count)) %>%
  View()

final_result.tib %>%
  filter(count >= 269126 ) %>%
  arrange(desc(count))


final_result.tib[2, 1] %>%
  as.character() %>%
  strsplit("_&_")

output.tib_2 

result[1:5]
longer.tib
vars_long.tib

output.tib

final_result.tib

longer.tib <- gather(test.tib, variable_name, variable_value,  na.rm = TRUE) %>%
  group_by(variable_name) %>%
  summarize(n()) %>%
  bind_rows(vars_long.tib, .)

View(head(new_vars.m))
View(head(test.m))
View(head(test.m))

dim(new_vars.m)

test.m <- new_vars.m[, -1]

test.tib <- as_tibble(test.m)

result_2.tib <- gather(test.tib, variable_name, variable_value,  na.rm = TRUE) %>%
  group_by(variable_name) %>%
  summarize(n())

result_3.tib <- bind_rows(result_1.tib, result_2.tib)

as_tibble(result_3.tib, n = "count")
result_1.tib

files.v <- files.v[1:2]
