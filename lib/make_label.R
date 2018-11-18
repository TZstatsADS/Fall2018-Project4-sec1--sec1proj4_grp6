#######################
### Error detection ###
#######################

### Authors: Hongru Liu
### Project 4 Group 6

### Construct a list that indicates whether a word in the given string is an error.
### input: one string, one positional bigram lists
### output: a label list that indicates whether a word in the given string is an error. 1 is an error and 0 is not.
make_label <-function(word_list,bigram_list) {
  final_labels <- list()
  for (i in 1:length(word_list)) {
    cat("current file number =",i,"\n")
    cur_file <- word_list[[i]]
    cur_labels <- c()
    for (j in 1:length(cur_file)) {
      cur_word <- cur_file[j]
      cur_length <- nchar(cur_word)
      if (cur_length>=3) {
        cur_bigram <- bigramize_word(cur_word)
      } else {
        cur_bigram <- list(cur_word)
      }
      cur_dic <- bigram_list[[paste("l_",cur_length,sep="")]]
      tmp_lable <- c()
      for (k in 1:length(cur_bigram)){
        tmp_lable[k] <- cur_bigram[[k]]%in%cur_dic[[k]]
      }
      cur_tmp_lable <- as.numeric(!sum(tmp_lable)==length(cur_bigram))
      cur_labels[j] <- cur_tmp_lable
    }
    final_labels[[i]] <- cur_labels
  }
  return(final_labels)
}
