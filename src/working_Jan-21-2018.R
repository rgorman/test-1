library(caret)


fold.v <- seq_len(nrow(z))

foldouts <- createFolds(fold.v, floor(nrow(z)/2000) )  ## works to create data partitions

(nrow(z)/1000) %>%
  floor()

 log2(1:64)- log2(2:65) 
1:16 * 0.087463

log2(2:65)- log2(1:64) 


URL <- "http://static.lib.virginia.edu/statlab/materials/data/stocks.zip"
download.file(url = URL, destfile = basename(URL))
unzip(basename(URL))


files <- list.files(pattern = "csv$") 
dat2 <- map(files, read.csv)
names(dat2) <- gsub("\\.csv", "", files)

map_dbl(dat2, ~mean(.$Open))
files
files <- files[c(2,4,5)]


combn(1:12, 2)
choose(12, 2)
choose(36, 1:36) %>%
  sum()
word.list[[1]][y[i]]

word.list %>% 
  map_chr(y[1]) %>%
  data.frame(check.names = FALSE, stringsAsFactors = FALSE)


write.csv(head(z), file = "variable_names.csv")


group1.v <- c(12:59) # indices (from vector "y") for creation of  variables.
#####################

group_1.list <- list() # make empty list object to store result of loop
nomina.v <- NULL # make empty vector to store names

system.time(
  for (i in 1:3) { # iterate for each item in group1.v
    group_1.list[[i]] <- combn(y[group1.v], i, simplify = FALSE) # make all possible combinations of variables
    nomina.v <- paste(length(group1.v), "Choose",  i, collapse = " ") %>% # create names for elements in list
      append(nomina.v, .)
  }
)

names(group_1.list) <- nomina.v  # assign names to list elements

lengths(group_1.list)

saveRDS(group_1.list, file = "combined_var_names.R")

group_1.list %>%
  lengths()

paste0(group_1.list[[2]][,1], collapse = "_&_")

group_1.list[[2]][,1]

group_1.list[[2]] %>%  map_chr()

pfuc <- function(x) paste0(x[, 1], collapse = "_&_")

x <- sapply(group_1.list[[2]], pfuc)

group_1.list[[3]][2]

x <- z[, group_1.list[[3]][,1]] 


 apply(group_1.list[3], 2, apply( 1, paste0, collapse = "-") )

lapply(group_1.list, `[[`, 2)




paste0(x[1,], collapse = "-")
apply(x, 1, paste0, collapse = "-")  # this works to popluate with values
paste0(group_1.list[[3]][,1], collapse = "_&_")


files.v[1] %>%
  gsub(".xml","", .)


pas_funct <- function(x) {
  apply(x, 1, paste0, collapse = "-")
}


apply(group_1.list[[3]], 2, `[[`, 1)


group_1.list[[3]] %>%
  apply(., 2, `[[`, 1)

group_1.list[[2]]

x <- group_1.list[[2]][1] %>%
  unlist()

z[, x]


x <- lapply(group_1.list[[2]], unlist) %>%
  unlist()
 

group_1.list[[2]][1]  

###########################  

x <- group_1.list[[2]][1]

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

c
generate_var_names <- function(x) {
  a <- unlist(x)
  b <- paste0(a, collapse = "_&_")
  c <- NULL
  c <- append(c, b)
  return(c)
}
  
  
#######################

test.d <- sapply(group_1.list[[2]], generate_vars)
test.d <- NULL

View(head(test.d))
seq_along(group_1.list)

new_vars.df <- z[, 1:2] 
 



lengths(group_1.list) %>%
  sum()
new_vars.df <- matrix(nrow = nrow(z), ncol = 4097)
new_vars.df <- z[,1:2]

new_vars.df <- as.data.frame(new_vars.df)

class(new_vars.df)

storage_list <- vector(mode = "list", sum(lengths(group_1.list))+2)
storage_list[[2]] <- z[,2]

for (i in 1:3) {
  
  start_time <- Sys.time()
  a <- sapply(group_1.list[[i]], generate_vars)
  
  nomina <- sapply(group_1.list[[i]], generate_var_names)
  
  colnames(a) <- nomina
  
  new_vars.df <- cbind(new_vars.df, a) %>%
    as.matrix()
  
  end_time <- Sys.time()
  
  print(paste("loop", i))
  print(end_time - start_time)
  
  
}



View(head(new_vars.df))

i <- 1

for (i in 1:12) {
  
  start_time <- Sys.time()
  a <- lapply(group_1.list[[i]], generate_vars)
  
  nomina <- sapply(group_1.list[[i]], generate_var_names)
  
  names(a) <- nomina
  
  
  
  end_time <- Sys.time()
  
  print(paste("loop", i))
  print(end_time - start_time)
  
  
}


test.m <- new_vars.df[, -c(1,2)]

View(head(test.m))


df[,1] <- z[, 1]
df[,2] <- z[, 2]

colnames(df[1:2]) <- n
colnames(df[1:2])

n <- colnames(z[1:2])

df <- data.frame(matrix(ncol = 4097, nrow = 15948))

View(head(new_vars.df))


test.df <- new_vars.df[1:6,]

new_vars.df[1:6,] %>%
  object.size()


which(test.df == "NULL")

test.df[test.df == NULL] <- NA

length(test.df)

which(test.df[,6] == "NULL")

View(head(test.m))

null.v <- which(test.df == "NULL")
test.df[null.v] <- NA

##########
start_time <- Sys.time()
test.m[which(str_detect(test.m, "NA"))] <- NA
end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
test.m[which(str_detect(test.m, "NULL"))] <- NA
end_time <- Sys.time()
print(end_time - start_time)




View(head(new_vars.df))
colnames(new_vars.df)

#################

which (new_vars.df == "NULL")
which(str_detect(new_vars.df, "NA"))


lapply(group_1.list[[1]], generate_var_names)
seq_along(group_1.list)

write.csv(new_vars.df, file = "new_variables.test.csv")
saveRDS(group_1.list, file = "combination_sets_vars.R")

main.df2 <- readRDS("new_variables_test.R")

start_time <- Sys.time()
write.csv(vars.long.tib, file = "vars_long_test.csv")
end_time <- Sys.time()
print(end_time - start_time)



test.df <- main.df2[, 1:10]
test.df[[which(str_detect(test.df, "NA"))]]


which(str_detect(test.m, "NA"))


choose(24, 1:4) %>%
  sum()
choose(47, 3)

p <- z[6, 2]
Encoding(p) <- "UTF-8"
p

object.size(new_vars.df)

test.m <- as.matrix(test.df)
test.m[which(str_detect(test.m, "NULL"))] <- NA ###


start_time <- Sys.time()
new_vars.df[str_detect(new_vars.df, "NA")] <- NA # add logical NA to cells
end_time <- Sys.time()
print(end_time - start_time)

new_vars.df <- cbind(token_id, new_vars.df)
new_vars.df <- new_vars.df[, -c(2,3)]

object.size(new_vars.df)
View(head(new_vars.df))




is.na(new_vars.df) %>%
  length()

length(new_vars.df) * 15948


65338956 - 65338956

file_name <- files.v[1] %>%
  gsub(".xml","", .)

token_id <- seq_len(nrow(z)) %>%
  paste0(file_name, "_token_", .)

new_vars.df <- cbind.data.frame(token_id, new_vars.df, stringsAsFactors = FALSE)


dim(test.m)

test.m <- cbind(token_id, test.m)

new_vars.df <- new_vars.df[, -c(2,3)]

View(head(test.m))

#############
start_time <- Sys.time()
vars.tib <- as_tibble(test.m)
end_time <- Sys.time()
end_time - start_time

View(head(vars.tib))

start_time <- Sys.time()
vars.long.tib <- gather(vars.tib, variable_name, variable_value, -token_id, na.rm = TRUE)
end_time <- Sys.time()
print(end_time - start_time)

object.size(vars.long.tib)

vars.long.tib
########################

x <- vars.long.tib %>%
  group_by(variable_name) %>%
  summarize(n())

xx <- bind_cols(x, x[,2])

xx.long.tib <- bind_rows(x, x)

rm(xx)

which(x[, 1] == "g1-parent-depdist")
x[1,]

result <- xtabs(`n()` ~ variable_name, data = xx.long.tib)  # this works !!

dim(result)

head(result)

column_to_rownames(x, x[,"variable_name"])

15948*2

vars.long.tib <- vars.long.tib[2:47845,]
head(vars.long.tib)

final.m <- apply(result, 2, as.numeric)
head(result)

ressult.v <- result[1:10]

as_tibble(ressult.v, n = "count")  ## this works !!

names(ressult.v)

dim(new_vars.df)
format(15948*18474, scientific = TRUE)

unique(vars.long.tib[, 3])

test.tb <- table(vars.long.tib[1:33000,])
test.tb

vars.long.tib

spread(vars.long.tib[1:100,], key = variable_name, value = variable_value) %>%
  table()

xtabs(variable_value ~ token_id+variable_name, data=vars.long.tib[1:10,])

head(vars.tib)

table(vars.tib[,18000]) %>%
  sum()


x[2]

vars.tib[,18000]

paste(vars.long.tib[2,2], vars.long.tib[2,3], sep =  "=")

load("matrix_100-token_chunks.R")
View(head(final.m))


######################


start_time <- Sys.time()
var_value.tib <- shorter.tib %>%
  transmute(combined = paste(variable_name, variable_value, sep = "=") ) 
end_time <- Sys.time()
end_time - start_time

var_value.tib

fake_labels <- sample(1:10, 100000, replace = TRUE)

var_value.tib3 <- cbind(fake_labels, var_value.tib[1:100000,])

freq.tbl <- table(var_value.tib3)

freq.tbl

head(freq.tbl)

var_value.tib4 <- var_value.tib3 %>%  # this works 
  group_by(fake_labels, combined) %>%
  summarize(n())


dim(freq.tbl)
freq.tbl[, 1:2]

result <- xtabs(`n()` ~ fake_labels + combined, data = var_value.tib4) ## this also works
result[, 1:2]

start_time <- Sys.time()
var_value.tib2 <- shorter.tib %>%
  transmute(combined = paste(variable_name, variable_value, sep = "=") )  %>%
  group_by(combined) %>%
  summarize(n())
end_time <- Sys.time()
end_time - start_time

var_value.tib2


arrange(x, desc(`n()`))

quantile(var_value.tib2$`n()`, seq(0, 1, 0.1))

object.size(x)

dim(test.m)

x2 <- vars.long.tib %>%
  group_by(variable_name) %>%
  summarize(n())
  
 

x2$`n()` %>%
  quantile(., probs = (seq(0, 1, 0.1)))




which(x2[,2] <= 5531) %>%
  length()

which(x[,2] > 15) %>%
  length()


x2$`n()`

keepers <- filter(x2, `n()`>= 5531)

summary(keepers)
keepers.v <- as.character(keepers[,1])

which(x2$variable_name %in% keepers.v)

keepers.v <- NULL
keepers.v <- append(keepers.v,   apply(keepers[,1], 1, `[`))

keepers.v
keepers <- arrange(keepers, desc(`n()`))
keepers

arrange(keepers, desc(`n()`))

shorter.tib <- filter(vars.long.tib, variable_name %in% keepers.v)

shorter.tib %>%
  group_by(variable_name, variable_value) %>%
  summarize(n())

x

shorter.tib

nrow(shorter.tib)
chunk <- sample(1:10, nrow(x), replace = TRUE)
chunk <- paste0("chunk_", chunk)
chunk
colnames(shorter.tib)
chunky <- mutate(x, chunk_id =  chunk)

chunky[1:1000,] %>%
  group_by(chunk_id, combined)

start_time <- Sys.time()
result <- xtabs(`n()` ~ chunk_id+combined, data = chunky, sparse = TRUE)  ## works maybe
end_time <- Sys.time()
end_time - start_time

chunky
result 



output.m <- apply(result, 2, as.numeric)
dim(output.m)

View(output.m[1:10,1:10])
rownames(output.m) <- sort(unique(chunk))

sum(output.m)

as_tibble(output.m[, 1:100])

as.factor(paste0("chunk", 1:10))

object.size(result)

8851 * 185
