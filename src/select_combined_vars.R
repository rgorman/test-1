## This script is used to select combined variables for further consideration.

require(XML)
require(tidyverse)
require(stringr)

input.dir <- "./output_2"

files.v <- dir(path=input.dir, pattern=".*xml")

# read xml structure from file to .R object
doc.object <- xmlTreeParse(file.path(input.dir, files.v[1]), useInternalNodes=TRUE)

# extract all <word> elements and children into XmlNodeList object
word.nodes <- getNodeSet(doc.object, "//word")


word.list <- xmlApply(word.nodes, xmlToList)




var_names.v <- map(word.list, names) %>%
  unlist() %>%
  unique()

var_names.v <- var_names.v[-which(var_names.v == "insertion_id" | var_names.v == "artificial") ]


colnames(base.df) <- var_names.v

var_names.v <- var_names.v[12:length(var_names.v)] # drop non-expanded attributes as variables



choose(length(var_names.v), seq_along(var_names.v)) # calculate the number of variable combinaitons produced by combining one, two, etc. variables. The number quickly becomes too large to be used in analysis.

choose(length(var_names.v), seq_along(var_names.v)) %>%
  sum() # c. 281 Trillion variable combinations if all lengths of variable combinations are used



selected_vars.list <- list() # make empty list object to store result of loop
nomina.v <- NULL # make empty vector to store names


for (i in 1:3) { # iterate for each item in group1.v
  selected_vars.list[[i]] <- combn(var_names.v, i, simplify = FALSE) # make all possible combinations of variables
  nomina.v <- paste(length(var_names.v), "Choose",  i, collapse = " ") %>% # create names for elements in list
    append(nomina.v, .)
}



###################

rm(var_names)
