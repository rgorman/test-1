# functions for xml processing


################
extract_words <- function(x) { #  function to extract data from each word element in sentence. 
                               
  words.v <-  unlist(x)
  return  (words.v)
}

##########

ellipsis_identification <- function(x) {
  ellipse_check <- "insertion_id" %in% names(x)
  return(ellipse_check)
}
###############


extract_edge_graph <- function(sentence) {
  
  if (length(sentence) >2) {
    
    a <- find_heads(sentence)
    b <- find_ids(sentence)
    m <- matrix(a, ncol = 1)
    
    if (colSums(m) > 0) {
      m <- cbind(m, b)
      index <- which(m[, 1] > 0)
      m <- m[index, ]
      if (length(m) == 2) {
        m <- matrix(m, nrow = 1)
      }
      g <- graph_from_edgelist(m)
      return(g)
      
    }
    
  }
 
}



###############

#####################

find_punct_index <- function(x) { # A function to return id values of each punctuation mark in sentence.
  word.v <- NULL
  word.v <- append(word.v, x["relation"] == "AuxX") # If node is comma, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxK") # If node is sentence final punctuation, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxG") # If node is bracketing punctuation, assign TRUE.
  # word.v <- append(word.v, substr(x["postag"], 1, 1) == "u") # If part of speech is "punctuation", mark TRUE.
  
  
  if (TRUE %in% word.v) { 
    return(as.numeric(x["id"]))
  }
  
}



##################



DD_calculation <- function(x) { # A function to calculate DD for each eligible node.
  
  word.v <- NULL
  word.v <- append(word.v, as.numeric(x["head"]) == 0) # Nodes dependent on 0 (sentence root, etc) have no DD.
  
  # a set of lines to catch punctuation; no DD should be figured for punctuation marks. This code is somewhat redundant,
  # since it checks for relation values commonly assigned to punctuation and also checks part of speech attribute
  # for value "punctuation."
  word.v <- append(word.v, x["relation"] == "AuxX") # If node is comma, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxK") # If node is sentence final punctuation, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxG") # If node is bracketing punctuation, assign TRUE.
  word.v <- append(word.v, substr(x["postag"], 1, 1) == "u") # If part of speech is "punctuation", mark TRUE.
  
  
  word.v <- append(word.v, "insertion_id" %in% names(x)) # Nodes that are ellipses have no DD.
  word.v <- append(word.v, node.list$Ellipsis[as.numeric(x["head"])]) # Nodes dependent on ellipses have no DD. This assessment
  # requires ellipsis.v as input. That vector is the output of
  # function ellip_1().
  
  # node_DD.v <- NULL
  
  
  if (TRUE %in% word.v) { # do not calculate DD
    
    node_DD.v <- NA
    
  } else {
    # p.holder.v <- length(seq(heads.v[k], ids.v[k])) - length(setdiff(seq(heads.v[k], ids.v[k]), punct.index.v))
    b <-  length(seq(as.numeric(x["head"]), as.numeric(x["id"]))) - length(setdiff(seq(as.numeric(x["head"]), as.numeric(x["id"])),
                                                                                   punct.index.v))
    
    if (as.numeric(x["head"]) > as.numeric(x["id"])) { # Selects for a head node that follows the child node.
      a <- as.numeric(x["head"]) - (as.numeric(x["id"]) + b)  # Effectively moves child node (the subtrahend) closer in value to the head.
      
      if (a > 6) {
        node_DD.v <-  "GT6"
        
      } else {
        
        node_DD.v <- a 
      }
      
    } else { # Selects for a head node that precedes the child node.
      # p.holder.v <- (heads.v[k] + p.holder.v) - ids.v[k] # Effectively moves head node (the minuend) closer in value to the child.
      
      a <- (as.numeric(x["head"]) + b) - as.numeric(x["id"]) # Effectively moves head node (the minuend) closer in value to the child.
      if ( a < (0 - 6 )) {
        
        node_DD.v <- "LT-6"
        
      } else {
        
        node_DD.v <- a
      }
      
    }
    
    # if (heads.v[k] > ids.v[k])  { # Tests whether head node follows child node.
    
    
    
  }
  
  return(node_DD.v)
  
} # end of DD_calculation() function.



##################

find_heads <- function(sentence) {
  a <- unlist(sentence)
  
  a <-  a[which(names(a) == "word.head")]
  a <- as.numeric(a)
  return(a)
}




find_ids <- function(sentence) {
  a <- unlist(sentence)
  
  a <-  a[which(names(a) == "word.id")]
  a <- as.numeric(a)
  return(a)
}



###################
neighborhood_extraction <- function(x) {
  
  if (as.numeric(x["id"]) <= length(neighborhood.l) 
      & as.numeric(x["id"]) <= length(node.list$Subtree_eligibility))  {
    
    if (node.list$Subtree_eligibility[[as.numeric(x["id"])]] == TRUE) {
      a <- neighborhood.l[[as.numeric(x["id"])]]
      a <- paste0(a, collapse = " ")
      
    } else {
      a <- NA
    }
    
    
  } else {
    
    a <- NA
    
    
  }
  

  return(a)
}

##########################

Subtree_criteria <- function(x) { # function to identify tokens for which no DD should be figured.
  
  word.v <- NULL 
  
  
  # a set of lines to catch punctuation; no DD should be figured for punctuation marks. 
  word.v <- append(word.v, x["relation"] == "AuxX") # If node is comma, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxK") # If node is sentence final punctuation, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxG") # If node is bracketing punctuation, assign TRUE.
  
  
  if (TRUE %in% word.v) { # Checks word.v for any TRUE value.  Such a value disqualifies node from DD calculation.
    return(FALSE) # Node does NOT qualify for DD calculation.
  } else
    return(TRUE) # Node may be used in DD calculation.
  
}

###################

subtree_extraction <- function(x) {
  
  if (as.numeric(x["id"]) <= length(subtree.l)
      & as.numeric(x["id"]) <= length(node.list$Subtree_eligibility)) {
    
    if (node.list$Subtree_eligibility[[as.numeric(x["id"])]] == TRUE) {
      a <- subtree.l[[as.numeric(x["id"])]]
      a <- paste0(a, collapse = " ")
      
    } else {
      a <- NA
    }
    
    
  } else {
    
    a <- NA
    
  }
 
  return(a)
}


abs_DD_calculation <- function(x) { # A function to calculate DD for each eligible node.
  a <- NULL
  word.v <- NULL
  word.v <- append(word.v, as.numeric(x["head"]) == 0) # Nodes dependent on 0 (sentence root, etc) have no DD.
  
  # a set of lines to catch punctuation; no DD should be figured for punctuation marks. This code is somewhat redundant,
  # since it checks for relation values commonly assigned to punctuation and also checks part of speech attribute
  # for value "punctuation."
  word.v <- append(word.v, x["relation"] == "AuxX") # If node is comma, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxK") # If node is sentence final punctuation, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxG") # If node is bracketing punctuation, assign TRUE.
  word.v <- append(word.v, substr(x["postag"], 1, 1) == "u") # If part of speech is "punctuation", mark TRUE.
  
  
  word.v <- append(word.v, "insertion_id" %in% names(x)) # Nodes that are ellipses have no DD.
  word.v <- append(word.v, node.list$Ellipsis[as.numeric(x["head"])]) # Nodes dependent on ellipses have no DD. This assessment
  # requires ellipsis.v as input. That vector is the output of
  # function ellip_1().
  
  node_DD.v <- NULL
  
  
  if (TRUE %in% word.v) { # do not calculate DD
    
    node_DD.v <- NA
    
  } else {
    
    
    b <-  length(seq(as.numeric(x["head"]), as.numeric(x["id"]))) - length(setdiff(seq(as.numeric(x["head"]), as.numeric(x["id"])),
                                                                                   punct.index.v))
    
    if (as.numeric(x["head"]) > as.numeric(x["id"])) { # Selects for a head node that follows the child node.
      
      a <- as.numeric(x["head"]) - (as.numeric(x["id"]) + b)  # Effectively moves child node (the subtrahend) closer in value to the head.
      node_DD.v <- append(node_DD.v, a) 
      
    } else { # Selects for a head node that precedes the child node.
      # p.holder.v <- (heads.v[k] + p.holder.v) - ids.v[k] # Effectively moves head node (the minuend) closer in value to the child.
      
      a <- (as.numeric(x["head"]) + b) - as.numeric(x["id"]) # Effectively moves head node (the minuend) closer in value to the child.
      
      
      node_DD.v <- append(node_DD.v, a)
      
      
    }
    
  }
  
  return(abs(node_DD.v))
  
} # end of DD_calculation() function.




populate_word_element <- function(x) {
  y <- as.numeric(x["id"])
  word.xml <- xmlNode("word") # create word xml node
  
  if (node.list$Ellipsis[y] == FALSE) {
    
    word.xml <- addAttributes(word.xml, id = x["id"], form = x["form"], lemma = x["lemma"], postag = x["postag"],
                              relation = x["relation"], head = x["head"], cite = x["cite"], 
                              Subtree = node.list$Subtree[y],
                              DepDist = node.list$DepDist[y],
                              Neighborhood = node.list$Neighborhood[y],
                              Degree = node.list$Degree[y],
                              Node_Type = node.list$Node_Type[y],
                              Parent_Order = node.list$parent_order[y],
                              Has_Sib = node.list$has_sib[y],
                              Sib_Count = node.list$sib_count[y],
                              Has_Sib_Before = node.list$has_sib_before[y],
                              Has_Sib_After = node.list$has_sib_after[y],
                              Node_Depth = node.list$node_depth[y])
    
  } else {
    
    word.xml <- addAttributes(word.xml, id = x["id"], form = x["form"], lemma = x["lemma"], postag = x["postag"],
                              relation = x["relation"], head = x["head"], cite = x["cite"], insertion_id = x["insertion_id"],
                              artificial = x["artificial"], 
                              Subtree = node.list$Subtree[y],
                              DepDist = node.list$DepDist[y],
                              Neighborhood = node.list$Neighborhood[y],
                              Degree = node.list$Degree[y],
                              Node_Type = node.list$Node_Type[y],
                              Parent_Order = node.list$parent_order[y],
                              Has_Sib = node.list$has_sib[y],
                              Sib_Count = node.list$sib_count[y],
                              Has_Sib_Before = node.list$has_sib_before[y],
                              Has_Sib_After = node.list$has_sib_after[y],
                              Node_Depth = node.list$node_depth[y])
    
  }
  
  return(word.xml)
}



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



get_node_depth <- function(input) {
  
  neigh_size <- node.list$Neighborhood %>%
    strsplit(split = " ") %>%
    lengths()
  
  heads <- input %>%
    map("head") %>%
    unlist()
  
  root <- which(neigh_size > 1 & heads == 0)
  
  if (length(root) == 1) {
    
      
      x <- seq_along(V(edge.graph))
      node_depths <- map_chr(x, ~distances(edge.graph, v = ., to = root) )
      node_depths[node_depths == Inf] <- NA
      node_depths <- as.integer(node_depths)
      
   
    
  }
}




check_sibling_count <- function(sentence) {
  
  head <- sentence["head"] %>%
    as.numeric()
  
  id <- sentence["id"] %>%
    as.numeric()
  
  if (node.list$has_sib[id] == TRUE) {
    sib_count <- strsplit(node.list$Neighborhood[head], split = " ") %>%
      unlist() %>%
      as.numeric() %>%
      length() %>%
      subtract(2)
  } else {
    
    sib_count <- NA
  }
  
 
  
  return(sib_count)
}


check_sibling_before <- function(sentence) {
  
  head <- sentence["head"] %>%
    as.numeric()
  
  id <- sentence["id"] %>%
    as.numeric()
  
  if (node.list$has_sib[id] == TRUE) {
    sib_count <- strsplit(node.list$Neighborhood[head], split = " ") %>%
      unlist() %>%
      as.numeric() %>%
      length() %>%
      subtract(2)
  } else {
    
    sib_count <- NA
  }
  
  
  
  return(sib_count)
}





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


