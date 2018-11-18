###########################################################
### Construnc positional binary lists of letter bigrams ###
###########################################################

### Authors: Hongru Liu
### Project 4 Group 6

### Construct letter bigrams for a singel word
### input: a single word
### output: a list with nested lists contains positional letter bigrams
bigramize_word <- function(word) {
  results <- list()
  n <- nchar(word)
  k <- 0
  list_names <- c()
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      k <- k + 1
      results[[k]] <- paste(substring(word,i,i),substring(word,j,j),sep='')
      list_names[k] <- paste('PD_',i,'_',j,sep='')
    }
  }
  names(results) <- list_names
  return(results)
}

### Construct letter bigrams for more than one words
### input: more than one words
### output: a list with nested lists contains positional letter bigrams
bigramize_words <- function(words) {
  results <- sapply(words,bigramize_word)
  results <- apply(results,1,unlist)
  return(t(results))
}

### Construct letter bigrams for one or more words
### input: one or more words
### output: a list with nested lists contains positional letter bigrams
bigramize <- function(words) {
  cat("new length \n")
  l <- nchar(words[1])
  if(length(words)==1) {
    results <- bigramize_word(words)
  } else {
    tmp_results <- bigramize_words(words)
    n <- dim(tmp_results)[1]
    results <- list()
    for (i in 1:n) {
      results[[i]] <- tmp_results[i,]
    }
    list_names <- c()
    for (i in 1:(l-1)) {
      for (j in (i+1):l) {
        list_names <- c(list_names, paste('PD_',i,'_',j,sep=''))
      }
    }
    names(results) <- list_names
  }
  return(results)
}

unique_bigram <- function(l_n) {
  return(lapply(l_n, unique))
}



