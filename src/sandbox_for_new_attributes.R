# script to generate new attriubtes in xml files

rm(list = ls())

source("src/DD_functions.R")

require(XML)
require(igraph)
require(magrittr)
require(tidyverse)
require(stringr)




# Identify directory with the input files.
input.dir <- "./input_1"

# Identify directory for the output files.
output.dir <- "./ouput_1"


files.v <- dir(path=input.dir, pattern=".*xml") # A vector with each file name from input directory.


for (i in seq_along(files.v)) {
  
  doc <- xmlParse(file.path(input.dir, files.v[i]))  # create object with full xml of tree file. 
  
  sentence.nodes <- getNodeSet(doc, "//sentence") # extract setences from full tree xml
  
  sentence.list <- xmlApply(sentence.nodes, xmlToList) # convert nodes to list object
  
  subtree.xml <- xmlNode("subTree_document") # create root node for new xml document
  
  
  token.number.l <- list()
  
  net.token.number.l <- list()
  
  for (j in seq_along(sentence.list)) {
    
    
    
    
    sent_working <- lapply(sentence.list[[j]][1:length(sentence.list[[j]])  -1], extract_words) # extract target sentence 
    # with tokens as items in list object.
    
    node.list <- vector("list", 10)
    names(node.list) <- c("Ellipsis", "Subtree_eligibility", "Subtree", 
                          "DepDist", "Neighborhood", "Degree", "Node_Type",
                          "parent_order", "has_sib", "node_depth")
    
    word_names.v <- NULL
    word_names.v <- sent_working %>%
      map(names) %>%
      unlist() %>%
      append(word_names.v, .)
    
    
    node.list$Ellipsis <- sapply(sent_working, ellipsis_identification) # logical vector with TRUE for each ellipsis node in target sentence
    # vector is necessary input for function DD_criteria().
    
    punct.index.v <- unlist(lapply(sent_working, find_punct_index))
    
    edge.graph  <- extract_edge_graph(sent_working) # create graph object (package = "igraph") from sent_working.
    
    
    subtree.l <- ego(edge.graph, 50, mode = "out") # List of elements; each element contains id values
    # for subtree of given node. A subtree is the given node
    # and its direct AND indirect dependents.
    
    neighborhood.l <- ego(edge.graph, 1, mode = "out") # List of elements; each element contains id values
    # for neighborhood of given node. A neighborhood is
    # a given node and its direct dependents ONLY.
    
    
    node.list$Degree <- degree(edge.graph, mode = "out")
    
    node.list$DepDist <- sapply(sent_working, DD_calculation) # produces a  vector; this mode is suitible for insertion as 
    # values of attributes in word elements of the XML output.
    
    node.list$Subtree_eligibility <-  sapply(sent_working, Subtree_criteria)
    
    node.list$Neighborhood <- sapply(sent_working, neighborhood_extraction)
    
    node.list$Subtree  <- sapply(sent_working, subtree_extraction)
    
    node.list$Node_Type <- sapply(node.list$Neighborhood, get_node_type)
    
    node.list$parent_order <- sapply(sent_working, get_parent_order)
    
    node.list$has_sib <- sapply(sent_working, check_for_siblings)
    
    node.list$node_depth <- get_node_depth(sent_working)
    
    
    
    sentence_DD <- round(mean(sapply(sent_working, abs_DD_calculation), na.rm = TRUE), digits = 4 )
    
    token_number <- length(sent_working)
    
    net_token_number <- token_number - length(punct.index.v)
    
    
    a <- unlist(sentence.list[[j]][length(sentence.list[[j]])]) # make vector of sentence attributes.
    # sentence attributes appear in last sublist of each list
    # in sentence.list. This list is accessed using 
    # length(sentence.list[[j]]).
    
    sent.xml <- xmlNode("sentence") # create sentence node
    
    sent.xml  <-  addAttributes(sent.xml, id = a[".attrs.id"], 
                                document_id = a[".attrs.document_id"], 
                                stand_ref = a[".attrs.stand_ref"],
                                subdoc = a[".attrs.subdoc"], span = a[".attrs.span"],
                                
                                Mean_DepDist = sentence_DD,
                                Sentence_length = token_number) 
    
    
    
    sent.xml <- append.xmlNode(sent.xml, lapply(sent_working, populate_word_element))
    
    subtree.xml <- append.xmlNode(subtree.xml, sent.xml) # Insert sentence into document xml.
    
    token.number.l[[j]] <-  token_number
    
    net.token.number.l[[j]] <- net_token_number
    
  } # end of loop j
  
  
  saveXML(subtree.xml, paste0("./output_1/", files.v[i]))
  
  
  
  
} # end of loop i




subtree.xml[[47]]


get_node_type <- function(x) {
  node <- strsplit(x, split = " ") %>%
    unlist() %>%
    length() %>%
    is_greater_than(1)
  
  if(node == TRUE) {
    return("internal")
  } else {
    return("leaf")
  }
  
}




get_parent_order <- function(sentence) {
  id <- sentence["id"] %>%
    as.numeric()
  head <- sentence["head"] %>%
    as.numeric()
  
  if (head == 0) {
    return("NA")
  } else {
    if (is_less_than(head, id)) {
      return("before")
    } else {
      return("after")
    }
  }
  
}

 


check_for_siblings <- function(sentence) {
  
  head <- sentence["head"] %>%
    as.numeric()
  
  has_sibs <- strsplit(node.list$Neighborhood[head], split = " ") %>%
    unlist() %>%
    as.numeric() %>%
    length() %>%
    is_greater_than(2)
  
  return(has_sibs)
}

node.list$Neighborhood[17]
sapply(sent_working, check_for_siblings)

sent_working[[17]]

get_sibling_order <- function(sentence, siblings) {
  id <- sentence["id"] %>%
    as.numeric()
  
  head <- sentence["head"] %>%
    as.numeric()
  
  inclusive_sibs <- strsplit(siblings[head], split = " ") %>%
    unlist() %>%
    setdiff(., head) %>% 
    as.numeric()
  
  exclusive_sibs <- setdiff(inclusive_sibs, id)
  
 
  if (is_greater_than(length(exclusive_sibs), 0)) {
    x <- is_less_than(exclusive_sibs, id)
    
    output <- get_position(x)
    output <- paste0(output, collapse = "-")
    return(output)
     
  } else {
    return("NA")
  }
  
}


get_position <- function(x) {
  output <- NULL
  for(i in seq_along(x)) {
    if (x[i] == TRUE) {
      output <- append(output, "before")
    } else {
      output <- append(output, "after")
    }
  }
  return(output)
}



get_sibling_order(sent_working[[4]], node.list$Neighborhood[4])

z <- sapply(sent_working, get_sibling_order, siblings = node.list$Neighborhood)

z

z <- 4:6

get_position(z)

inclusive_sibs <- node.list$Neighborhood[1] %>% strsplit( split = " ") %>%
  unlist() %>%
  as.numeric()

exclusive_sibs2 <- setdiff(id, inclusive_sibs)
  
setdiff(sibs, id) ##

id <- 1
sapply(sent_working, get_sibling_order, siblings = node.list$Neighborhood)


neigh_size <- node.list$Neighborhood %>%
  strsplit(split = " ") %>%
  lengths()



heads <- sent_working %>%
  map("head") %>%
  unlist()


root <- which(neigh_size > 1 & heads == 0)


  
distances(edge.graph, 5, root)

sapply(1:5, distances, v = V(edge.graph), to = root)

x <- seq_along(heads)
y <- map_chr(x, ~distances(edge.graph, v = ., to = root) )

as.numeric(y) %>%
  max()

y[y == Inf] <- NA
as.integer(y)

j <- 675

word_names.v <- NULL
word_names.v <- sent_working %>%
  map(names) %>%
  unlist() %>%
  append(word_names.v, .)
  



get_node_depth <- function(input) {
  
  neigh_size <- node.list$Neighborhood %>%
    strsplit(split = " ") %>%
    lengths()
  
  heads <- input %>%
    map("head") %>%
    unlist()
  
  root <- which(neigh_size > 1 & heads == 0)
  
  if (length(root) == 1) {
    
    if ("insertion_id" %in% word_names.v) {
      
      x <- seq_along(heads)
      node_depths <- map_chr(x, ~distances(edge.graph, v = ., to = root) )
      node_depths[node_depths == Inf] <- NA
      node_depths <- as.integer(node_depths)
      
    } else {
      x <- seq_along(heads)
      x <- x[1:length(x)-1]
      node_depths <- map_chr(x, ~distances(edge.graph, v = ., to = root) )
      node_depths[node_depths == Inf] <- NA
      node_depths <- as.integer(node_depths)
      # node_depths <- append(node_depths, NA)
      
      
    }
    
  }
}



input <- sent_working
node_depths
j <- 38

map_chr(1:30, ~distances(edge.graph, v = ., to = root) )
V(edge.graph) %>%
  seq_along()

sent_working %>%
  length()

x <- extract_edge_graph(sent_working)

is.null(edge.graph) %>%
  not()

most_frequent.list[[2]]

rm(list = ls())

holder.tib <- readRDS(file = "sample_1000_variables_all_files.rds")
dim(sample_1000_all_files.tib)

sample_1000_all_files.list %>%
  class()


start_time_1 <- Sys.time()
agg_var_ct.tib  <- xtabs(`n()` ~ variable_name, data = holder.tib) %>%
  as_tibble() # combine all frequencies in holder.tib to in one table
end_time_1 <- Sys.time()
print(end_time_1 - start_time_1)
print(Sys.time())


agg_var_ct.tib <- agg_var_ct.tib %>%
  arrange(desc(n))


agg_var_ct.tib$n %>%
  quantile(., probs = (seq(0.9, 1, 0.001)))

shorter_var.tib <- agg_var_ct.tib %>% 
  filter(n >= 26896) %>% # select value that will give reasonable number of variables
  arrange(desc(n))

object.size(holder.tib)


new_vars.m[str_detect(new_vars.m, "NA")]

agg_var_ct.tib %>% 
  filter(variable_name, str_detect(., "parent-morph-pos"))

str_detect(agg_var_ct.tib[, 1], "pos")  

findrow.v <-  apply(agg_var_ct.tib[, 1], 1, str_detect, pattern = "parent-morph-pos")

which(findrow.v == TRUE)

sapply(sent_working, check_sibling_count)

sentence <- sent_working[[1]]



check_sibling_before <- function(sentence) {
  
  head <- sentence["head"] %>%
    as.numeric()
  
  dups <- append(head, id)
  
  id <- sentence["id"] %>%
    as.numeric()
  
  if (node.list$has_sib[id] == TRUE) {
    sibs <- strsplit(node.list$Neighborhood[head], split = " ") %>%
      unlist() %>%
      as.numeric()
    
    befores <- setdiff(sibs, dups) %>%
      is_less_than(id)
    
    if (TRUE %in% befores) {
      sib_before <- TRUE
    } else {
      sib_before <- FALSE
    }
      
  } else {
    
    sib_before <- NA
  }
  
  
  
  return(sib_before)
}




check_sibling_after <- function(sentence) {
  
  head <- sentence["head"] %>%
    as.numeric()
  
  dups <- append(head, id)
  
  id <- sentence["id"] %>%
    as.numeric()
  
  if (node.list$has_sib[id] == TRUE) {
    sibs <- strsplit(node.list$Neighborhood[head], split = " ") %>%
      unlist() %>%
      as.numeric()
    
    afters <- setdiff(sibs, dups) %>%
      is_greater_than(id)
    
    if (TRUE %in% afters) {
      sib_after <- TRUE
    } else {
      sib_after <- FALSE
    }
    
  } else {
    
    sib_after <- NA
  }
  
  
  
  return(sib_after)
}


sapply(sent_working, check_sibling_after)





setdiff(sibs, dups)

sapply(sent_working, check_sibling_before)
