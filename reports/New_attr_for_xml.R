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
  
  start_time <- Sys.time()
  
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
    
    
    subtree.l <-  if (not(is.null(edge.graph))) {ego(edge.graph, 50, mode = "out")} # List of elements; each element contains id values
    # for subtree of given node. A subtree is the given node
    # and its direct AND indirect dependents.
    
    neighborhood.l <- if (not(is.null(edge.graph)))  {ego(edge.graph, 1, mode = "out")} # List of elements; each element contains id values
    # for neighborhood of given node. A neighborhood is
    # a given node and its direct dependents ONLY.
    
    
    node.list$Degree <- if (not(is.null(edge.graph)))  {degree(edge.graph, mode = "out")}
    
    node.list$DepDist <- sapply(sent_working, DD_calculation) # produces a  vector; this mode is suitible for insertion as 
    # values of attributes in word elements of the XML output.
    
    node.list$Subtree_eligibility <-  sapply(sent_working, Subtree_criteria)
    
    node.list$Neighborhood <- sapply(sent_working, neighborhood_extraction)
    
    node.list$Subtree  <- sapply(sent_working, subtree_extraction)
    
    node.list$Node_Type <-  if (not(is.null(edge.graph))) {sapply(node.list$Neighborhood, get_node_type)}
    
    node.list$parent_order <- if (not(is.null(edge.graph)))  {sapply(sent_working, get_parent_order)}
    
    node.list$has_sib <- if (not(is.null(edge.graph)))  {sapply(sent_working, check_for_siblings)}
    
    node.list$node_depth <- if (not(is.null(edge.graph)))  {get_node_depth(sent_working)}
    
    
    
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
  
  end_time <- Sys.time()
  print(end_time - start_time)
  print(files.v[i])
  
  
} # end of loop i



