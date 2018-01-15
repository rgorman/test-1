
ellip_1 <- function(x) { # function to identify tokens as ellipsis or not.
  a <- NULL
  b <- unlist(x)
  c <- unlist(sent_working[15])
  a <- append(a, "insertion_id" %in% names(b)) 
  "insertion_id" %in% names(c)
  
  return(a)
}

DD_criteria <- function(x) { # function to identify tokens for which no DD should be figured.
  
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
  
  
  # return(as.logical(word.v))
  
  if (TRUE %in% word.v) { # Checks word.v for any TRUE value.  Such a value disqualifies node from DD calculation.
    return(FALSE) # Node does NOT qualify for DD calculation.
  } else
    return(TRUE) # Node may be used in DD calculation.
  
}

####################
extract_degree <- function(x) {
  a <- degree(edge.graph, mode = "out")
  word.v <- NULL 
  
  if (length(a) < length(sent_working)) {
    a <- append(a, 0)
  }
  
  word.v <- append(word.v, x["relation"] == "AuxX") # If node is comma, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxK") # If node is sentence final punctuation, assign TRUE.
  word.v <- append(word.v, x["relation"] == "AuxG") # If node is bracketing punctuation, assign TRUE.
  
  if (TRUE %in% word.v) {
    return(NA)
  } else {
    return(a[as.numeric(x["id"])])
  }
  
}




punc_in_gap <- function(x) {
  a <- NULL
  a <- append(a, x %in% punct.index.v)
  return(a)
  
}


ellipis_in_gap <- function(x) {
  a <- NULL
  a <- append(a, ! ("insertion_id"  %in% names(x)) )
  
}


projectivity_extraction <- function(x) {
  
  
  if (as.numeric(x["id"]) <= length(subtree.l) & node.list$Projectivity_eligibility[[as.numeric(x["id"])]] == TRUE & 
      (length(node.list$Subtree[as.numeric(x["id"])]) > 0)) {
    
    if (! ("insertion_id" %in% names(x)) & (length(node.list$Subtree[as.numeric(x["id"])]) = 1)) {
      
      a <- strsplit(node.list$Subtree[as.numeric(x["id"])], " ")
      a <- as.numeric(a)
      a <- unlist(a)
      b <- min(a, na.rm = TRUE) : max(a, na.rm = TRUE)
      c <- setdiff(b, a)
      d <- sapply(c, punc_in_gap)
      d <- append(d, sapply(x, ellipis_in_gap))
      
      # a <- unlist(strsplit(node.list$Subtree[as.numeric(x["id"])], " "))
      # a <- as.numeric(a)
      # b <- min(a, na.rm = TRUE) : max(a, na.rm = TRUE)
      # c <- setdiff(b, a)
      # d <- sapply(c, punc_in_gap)
      # d <- append(d, sapply(x, ellipis_in_gap))
      
      if (FALSE %in% d) {
        e <- "NonProjective"
      } else {
        
        e <- "Projective"
        
      }
      
    }
    
  } else {
    
    e <- NA
  }
  
}



projectivity_eligibility <- function(x) {
  a <- NULL
  a <- append(a, x["relation"] == "AuxX")
  a <- append(a, x["relation"] == "AuxK")
  a <- append(a, x["relation"] == "AuxG")
  a <- append(a, "insertion_id" %in% names(x))
  
  if (TRUE %in% a) {
    
    return(FALSE)
  } else {
    
    return(TRUE)
  }
  
}


rel_extraction <- function(x)  {
  a <-  sent_working[[x]]["relation"]
}

pos_extraction <- function(x)  {
  a <-  sent_working[[x]]["postag"]
}

rel_pos_extraction <- function(x) {
  a <- sent_working[[x]]["relation"]
  b <- substr(sent_working[[x]]["postag"], 1, 1)
  c <- paste(a, b, sep = "-")
  
}


#############


neighborhood_values_extraction <- function(x) {
  
  result <- NULL
  
  if (node.list$Subtree_eligibility[as.numeric(x["id"])] == TRUE)  {
    
    a <-  strsplit(node.list$Neighborhood[as.numeric(x["id"])], " ")
    a <- sapply(a, as.numeric)
    
    
    holder1 <- NULL
    holder2 <- NULL
    holder3 <- NULL
    
    
    if (length(a) == 1) {
      
      
      result <- "NA"
      
    } else { # neighborhood with more than 1 member
      
      self <- a[1]
      # a <- a[2:length(a)]
      a <- sort(a)
      
      e <-  sapply (a, rel_extraction)
      index <- which(a == self)
      e[index] <- paste("self", e[index], sep = "." )
      
      
      holder1 <- e
      
      holder1 <- paste(holder1, collapse = "-")
      
      
      
      h <- sapply (a, pos_extraction)
      i <- substr(h, 1,1)
      i[index] <- paste("self", i[index], sep = "." )
      
      holder2 <- i
      
      holder2 <- paste(holder2, collapse = "-")
      
      
      # result <-  paste(result, collapse = "-")
      
      k <- sapply(a, rel_pos_extraction)
      k[index] <- paste("self", k[index], sep = "." )
      
      holder3 <- k
      
      holder3 <- paste(holder3, collapse = "-")
      
      result <- append(holder1, holder2)
      result <- append(result, holder3)
      
      
      return(result)
      
    }
    
  } else {
    
    return("NA")
  }
  
}
################



ellipsis_identification <- function(x) {
  ellipse_check <- "insertion_id" %in% names(x)
  return(ellipse_check)
}
###########



check_gap_heads <- function(x, arg2, arg3) {
  result <- NULL
  a <- sent_working[[x]]["head"]
  b <- as.numeric(a)
  result <- append(result, ((b > 0) & (b < arg2)) | b > arg3)
  
}


gap_extraction <- function(x) {
  y <- as.numeric(x["id"])
  if (node.list$Projectivity_eligibility[[as.numeric(x["id"])]] == TRUE) {
    a <- as.numeric(unlist(strsplit(node.list$Neighborhood[y], " ")))
    gap_min <- min(a)
    gap_max <- max(a)
    c <- seq(gap_min, gap_max)
    d <- which(! c %in% a)
    e <- c[d]
    
    result <- sapply(e, check_gap_heads, arg2=gap_min, arg3=gap_max)
    if (TRUE %in% result) {
      return("NonPlanar")
    } else {
      return("Planar")
    }
    
  } else {
    return(NA)
  }
}


check_ancestors <- function(x, id) {
  a <-  as.numeric(unlist(strsplit(node.list$Subtree[x], " ")))
  b <- id %in% a
}



find_ill_nested <- function(x, arg2) {
  if (length(which(node.list$Planarity == "NonPlanar")) >= 2) {
    y <- as.numeric(x["id"])
    if (node.list$Planarity[y] == "NonPlanar") {
      a <- as.numeric(unlist(strsplit(node.list$Neighborhood[y], " ")))
      gap_min <- min(a)
      gap_max <- max(a)
      c <- seq(gap_min, gap_max)
      d <- which(! c %in% a)
      e <- c[d] 
      f <- as.numeric(arg2[e]["head"])
      g <- append(e, f)
      
      result <- sapply(g, check_ancestors, id = y)
      
      if (TRUE %in% result) {
        return("WellNested")
      } else {
        return("IllNested")
        
      }
      
    } else {
      return("WellNested")
    }
  } else {
    return("WellNested")
  }
}
