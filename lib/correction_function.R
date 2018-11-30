
strip.text <- function(txt) {
  # remove apostrophes (so "don't" -> "dont", "Jane's" -> "Janes", etc.)
  txt <- gsub("'","",txt)
  # convert to lowercase
  txt <- tolower(txt)
  # change other non-alphanumeric characters to spaces
  txt <- gsub("[^a-z0-9]"," ",txt)
  # change digits to spaces
  txt <- gsub("[0-9]+"," ",txt)
  # split and make one vector
  txt <- unlist(strsplit(txt," "))
  # remove empty words
  txt <- txt[txt != ""]
  return(txt)
}

### Words within one transposition
Transpositions <- function(word) {
  # word<-"ad"
  N <- nchar(word)
  if (N > 2) {
    out<-c()
    # out <- rep(word, N - 1)
    word <- unlist(strsplit(word, NULL))
    # Permutations of the letters
    perms <- matrix(c(1:(N - 1), 2:N), ncol = 2)
    reversed <- perms[, 2:1]
    trans.words <- matrix(rep(word, N - 1), byrow = TRUE, nrow = N - 1)
    for(i in 1:(N-1)) {
      trans.words[i, perms[i,]] <- trans.words[i, reversed[i, ]]
      out[paste0("tra_",i,i+1)] <- paste(trans.words[i, ], collapse = "")
    }
  }
  else if (N == 2) {
    word <- unlist(strsplit(word, NULL))
    #out[paste0("trans_",1,2)] <- paste(word[2:1], collapse = "")
    out <- paste(word[2:1], collapse = "")
  }
  else {
    out <- paste(word, collapse = "")
  }
  return(out)
}


### Single letter deletions
Deletes <- function(word = FALSE) {
  N <- nchar(word) 
  out <- c()
  word <- unlist(strsplit(word, NULL))
  for(i in 1:N) {
    out[paste0("del_",i)] <- paste(word[-i], collapse = "")
  }
  return(out)
}

### Single-letter insertions.
Insertions <- function(word = FALSE) {
  N <- nchar(word) 
  out <- list()
  namess <-c()
  for (let in letters) {
    letter <- paste0("ins_",let)
    out[[letter]] <- rep(word, N + 1)
    left <- mapply(substr,word,((1-N):1),(0:N))
    right <- mapply(substr,word,(1:(1+N)),N)
    out[[letter]] <- mapply(paste,left,let,right,sep="")
    namess <- c(namess,paste0(letter,(1:(N+1)),""))
    # for (i in 1:(N + 1)) {
    #   out[[letter]][i] <- paste(substr(word, i-N, i-1), let, substr(word, i, N), sep="")
    # }
  }
  out <- unlist(out)
  names(out) <- namess
  return(out)
}

### Single-letter replacements
Replaces <- function(word) {
  N <- nchar(word)
  out <- list()
  namess <-c()
  for (let in letters) {
    letter<-paste0("sub_",let)
    out[[letter]] <- rep(word,N)
    left <- mapply(substr,word,((1-N):0),(0:(N-1)))
    right <- mapply(substr,word,(2:(1+N)),(N+1))
    out[[letter]] <- mapply(paste,left,let,right,sep="")
    namess <- c(namess,paste0(letter,(1:N),""))
    # for (i in 1:N) {
    #   out[[letter]][i] <- paste(substr(word, i - N, i - 1), let,
    #                             substr(word, i + 1, N + 1), sep = "")
    # # names(out[[letter]][i])<-paste0(letter,word[3])
    # }
  }
  out <- unlist(out)
  names(out) <- namess
  return(out)
}

### All possible outcomes for a word in one operation
Neighbors <- function(word) {
  neighbors <- c(word, Insertions(word),Deletes(word),Replaces(word), Transpositions(word))
  return(neighbors)
}

### Neighbors_clean
Neighbors_clean <- function(word,dtm) {
  #word<-"acresss"
  neighbors<-Neighbors(word)
  #dtm<-ground_truth_table
  # If it is a word, just return it.
  if (word %in% names(dtm)) {
    print(paste("No need to correct '", word, "'...", sep = ""))
    return (word)
  }
  # Otherwise, check for neighbors.
  else {
    # Which of the neighbors are known words?
    known <- which(neighbors %in% names(dtm))
    N.known <- length(known)
    # If there are no known neighbors, including the word,
    # look farther away.
    if (N.known == 0) {
      #print(paste("Having a hard time matching '", word, "'...", sep = ""))
      #neighbors <- unlist(lapply(neighbors, Neighbors))
      return()
    }
    # Then out non-words.
    neighbors_clean <- neighbors[which(neighbors %in% names(dtm))]
    N <- length(neighbors_clean)
  }
  #return (N)
  return(neighbors_clean)
}

### Clean
Clean <- function(neighbors,dtm=ground_truth_table) {
  # Which of the neighbors are known words?
  known <- which(neighbors %in% names(dtm))
  N.known <- length(known)
  # If there are no known neighbors, including the word,
  # look farther away.
  if (N.known == 0) {
    #print(paste("Having a hard time matching ", "..."))
    #neighbors <- unlist(lapply(neighbors, Neighbors))
    return()
  }
  # Then out non-words.
  neighbors_clean <- neighbors[which(neighbors %in% names(dtm))]
  N <- length(neighbors_clean)
  #return (N)
  return(neighbors_clean)
}

###  Word frequency MLE as determined by our corpus
Probability_MLE <- function(word, corpus) {
  N <- length(corpus)
  table_corpus <- table(corpus)
  word.number <- which(names(table_corpus) == word)
  count <- table_corpus[word.number]
  pval <- count/N
  return(pval)
}

### Word frequency ELE as determined by our corpus
Probability_ELE <- function(word, corpus) {
  V <- 112964
  N <- length(corpus)
  table_corpus <- table(corpus)
  word.number <- which(names(table_corpus) == word)
  count <- table_corpus[word.number]
  pval <- (count + 0.5) /(N + V/2)
  return(pval)
}

### count function
coun <- function(regex,corpus){
  #cat(regex)
  return(sum(unlist(lapply(gregexpr(regex,corpus)[lapply(gregexpr(regex,corpus),"[[",1)>0],length))))
}

### numerator of four types of typos
p_tc_sub<-function(word){
  #word<-"yang"
  nn<-length(Clean(Replaces(word)))
  if(nn>0){
    nam<-c()
    fenzi<-c()
    fenmu<-c()
    for(i in 1:nn){
      #i<-3
      nam[i]<-str_split(names(Clean(Replaces(word))[i]),"")
      rep_word<-nam[[i]][5]
      word_1 <- unlist(strsplit(word, NULL))
      reped_word<-word_1[as.numeric(nam[[i]][6])]
      fenzi[i]<-sub[reped_word,rep_word]
      fenmu[i]<-letter_count[rep_word]
    }
    fenmu<-as.vector(unlist(fenmu))
    p<-fenzi/fenmu
    names(p)<-Clean(Replaces(word))
    #typeof(fenmu)
    # typeof(fenzi)
    return(p)
  }else{return()}
}

p_tc_ins<-function(word){
  #word<-"acress"
  nn<-length(Clean(Insertions(word)))
  if(nn>0){ 
    nam<-c()
    fenzi<-c()
    fenmu<-c()
    for(i in 1:nn){
      nam[i]<-str_split(names(Clean(Insertions(word))[i]),"")
      inser_word<-nam[[i]][5]
      word_1 <- unlist(strsplit(word, NULL))
      if(as.numeric(nam[[i]][6])>1)
      {forward_word<-word_1[as.numeric(nam[[i]][6])-1]
      fenmu[i]<-combine_count[forward_word,inser_word]}else
      {forward_word<-'#'
      fenmu[i]<-coun(paste0(" ",inser_word),ground_truth_onelines)}
      
      fenzi[i]<-del[forward_word,inser_word]
      
    }
    fenmu<-as.vector(unlist(fenmu))
    p<-fenzi/fenmu
    names(p)<-Clean(Insertions(word))
    return(p)
  }else{ return()}
}

p_tc_del<-function(word){
  #word<-"acccess"
  nn<-length(Clean(Deletes(word)))
  if(nn>0){
    nam<-c()
    fenzi<-c()
    fenmu<-c()
    for(i in 1:nn){
      nam[i]<-str_split(names(Clean(Deletes(word))[i]),"")
      
      word_1 <- unlist(strsplit(word, NULL))
      del_word<-word_1[as.numeric(nam[[i]][5])]
      if(as.numeric(nam[[i]][5])>1)
      {forward_word<-word_1[as.numeric(nam[[i]][5])-1]
      fenmu[i]<-letter_count[forward_word]}else
      {forward_word<-'#'
      fenmu[i]<-coun(" ",ground_truth_onelines)}
      
      fenzi[i]<-add[forward_word,del_word]
      
    }
    fenmu<-as.vector(unlist(fenmu))
    p<-fenzi/fenmu
    names(p)<-Clean(Deletes(word))
    return(p)
  }else{return()}
}

p_tc_tran<-function(word){
  #word<-"acecss"
  word<-"ob"
  nn<-length(Clean(Transpositions(word)))
  if(nn>0){
    nam<-c()
    fenzi<-c()
    fenmu<-c()
    p<-list()
    for(i in 1:nn){
      nam[i]<-str_split(names(Clean(Transpositions(word))[i]),"")
      word_1 <- unlist(strsplit(word, NULL))
      X<-word_1[as.numeric(nam[[i]][5])]
      Y<-word_1[as.numeric(nam[[i]][6])]
      
      fenzi[i]<-rev[Y,X]
      fenmu[i]<-combine_count[Y,X]
      
    }
    
    fenmu<-as.vector(unlist(fenmu))
    p<-fenzi/fenmu
    names(p)<-Clean(Transpositions(word))
    
    return(p)
  }else{return()}
}

### find candidates of correction
candidates <- function(word) {
  candi <- c(word, p_tc_ins(word),p_tc_del(word),
             p_tc_sub(word), p_tc_tran(word))
  return(candi)
}

### correct a single word without context
Correct <- function(word, corpus=ground_truth_words_cleaned) {
  #cat("new")
  #word="book"
  dtm <- table(corpus)
  cans<-candidates(word)
  N<-length(cans)
  # If we found some neighbors, find the one with the highest
  # p-value.
  if (N > 1) {
    P_MLE <- 0*(1:N)
    P_ELE <- 0*(1:N)
    P_MLE[1]<-0
    P_ELE[1]<-0
    for (i in 2:N) {
      P_MLE[i] <- Probability_MLE(names(cans[i]), corpus)*as.numeric(cans[i])
      P_ELE[i] <- Probability_ELE(names(cans[i]), corpus)*as.numeric(cans[i])
      
    }
    names(P_MLE)<-names(cans)
    names(P_ELE)<-names(cans)
    #out <- list(neighbors[which.max(P_MLE)],neighbors[which.max(P_ELE)])
    prob_MLE <-(P_MLE[-1])[order(P_MLE[-1],decreasing = TRUE)[1:3]]
    prob_ELE <-(P_ELE[-1])[order(P_ELE[-1],decreasing = TRUE)[1:3]]
  } else {
    prob_MLE <- NA  
    prob_ELE <- NA
  }
  return(list(MLE_candi=prob_MLE,ELE_candi=prob_ELE))
}

### P_LEFT
P_L<-function(i,error_word=errors,corpus=g){
  count_L<-c()
  if(length(candidates(error_word[i]))>1 & i>1){
    for(j in 1:length(candidates(error_word[i])[-1])){
      count_L[paste(Left[i],names(candidates(names(Left[i]))[-1][j]))]<-str_count(corpus,paste(Left[i],names(candidates(names(Left[i]))[-1][j]))) 
    }
    return(count_L)}else{ return()}
}

### P_RIGHT
P_R<-function(i,error_word=errors,corpus=g){
  count_R<-c()
  if(length(candidates(error_word[i]))>1){
    for(j in 1:length(candidates(error_word[i])[-1])){
      count_R[paste(names(candidates(names(Right[i]))[-1][j]),Right[i])]<-str_count(corpus,paste(names(candidates(names(Right[i]))[-1][j]),Right[i])) 
    }
    return(count_R)}else{return()}
}

### Correct a single word with context
Correct_c4 <- function(n, corpus=ground_truth_words_cleaned,errors_word_set=errors) {
  cat("new")
  #n<-3
  word<-errors_word_set[n]
  #word="book"
  dtm <- table(corpus)
  cans<-candidates(word)
  N<-length(cans)
  # If we found some neighbors, find the one with the highest
  # p-value.
  L<-P_L(n)
  
  R<-P_R(n)
  if((!is.null(L)) & (!is.null(R))&(N > 1)){
    
    P_MLE <- c()#0*(1:N)
    P_ELE <- c()#0*(1:N)
    P_MLE[1]<-0
    P_ELE[1]<-0
    for (i in 2:N) {
      #i<-2
      P_MLE[i] <- Probability_MLE(names(cans[i]),corpus)*as.numeric(cans[i])*L[i-1]*R[i-1]
      P_ELE[i] <- Probability_ELE(names(cans[i]),corpus)*as.numeric(cans[i])*L[i-1]*R[i-1]
    }
    names(P_MLE)<-names(cans)
    names(P_ELE)<-names(cans)
    #out <- list(neighbors[which.max(P_MLE)],neighbors[which.max(P_ELE)])
    # prob_MLE <-(P_MLE[-1])[order(P_MLE[-1],decreasing = TRUE)[1:3]]
    #prob_ELE <-(P_ELE[-1])[order(P_ELE[-1],decreasing = TRUE)[1:3]]
    prob_MLE <-(P_MLE[-1])[order(P_MLE[-1],decreasing = TRUE)[1]]
    prob_ELE <-(P_ELE[-1])[order(P_ELE[-1],decreasing = TRUE)[1]]
    return(list(errors_word_set[n],prob_MLE,prob_ELE))}else{return()}
}