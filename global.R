#global.R to load in the following to global environment
#1. cleanup 1gram, 2gram, 3gram, 4gram DF
#2. libraries and functions

library(dplyr)
library(shiny)
library(tm)
library(stylo)
library(RWeka)
library(stringr)

load("20170820_ngram_table.RData")

table1gram <- ngram_table %>% filter(gramtype == "gram1")
table2gram <- ngram_table %>% filter(gramtype == "gram2")
table3gram <- ngram_table %>% filter(gramtype == "gram3")
table4gram <- ngram_table %>% filter(gramtype == "gram4")

#functions to clean up corpus and get Ngrams

getNGrams <- function(words, n) {
  NGram <- NGramTokenizer(words, Weka_control(min = n, max = n))
  remove_c <- grep("\\bc \\b",NGram) #NGramTokenizer put "c" at the start of sentence
  
  if (length(remove_c) > 0) {
    NGram <- NGram[-c(remove_c)]
  }
  
  return(NGram)
}

cleanSample <- function(x) {
  
  # to lower case
  x <- tolower(x)
  
  # Remove twitter hash tag "#"
  x <- gsub("#\\S+", "", x)
  
  # Replace shortform
  x <- gsub("wanna","want to",x)
  x <- gsub("i'm", "i am", x)
  x <- gsub("can't", "cannot", x)
  x <- gsub("won't", "will not", x)
  x <- gsub("ain't", "am not", x)
  x <- gsub("what's", "what is", x)
  x <- gsub("'d", " would", x)
  x <- gsub("'re", " are", x)
  x <- gsub("n't", " not", x)
  x <- gsub("'ll", " will", x)
  x <- gsub("'ve", " have", x)
  
  # Remove numbers, foreign characters and only retained letters a-z type.
  x <- gsub("[^a-z ]", "", x)
  
  # Remove profanity
  #https://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-and-how-to-use-facebooks-moderation-tool/
  load("profane.RData")
  x <- removeWords(x, profane)
  
  # Remove stopwords
  x <- removeWords(x, stopwords("english"))
  
  # Replace truncated words, e.g. substitute "lil to "little"
  x <- gsub("\\blil\\b","little",x)
  x <- gsub("\\bppl\\b","people",x)
  x <- gsub("\\bb\\b", "be", x)
  x <- gsub("\\bc\\b", "see", x)
  x <- gsub("\\br\\b", "are", x)
  x <- gsub("\\bu\\b", "you", x)
  x <- gsub("\\by\\b", "why", x)
  x <- gsub("\\bo\\b", "oh", x)
  
  # Eyeball the cleaned texts for further cleaning
  x <- gsub("\\bwhoa\\b", "", x)
  x <- gsub("\\brt\\b", "", x)
  
  # Remove unnecessary blank spaces
  x <- stripWhitespace(x)
  
  return(x)
}

theNextWord <- function(sentence, predict_word_no = 5) {
  
  Words <- txt.to.words(cleanSample(sentence))
  Words_len <- length(Words)
  
  # If user input is blank space or it has been filtered out, return comments
  if (Words_len == 0){
    predictedNextWord <- data.frame(error = "Input is either blank / filtered out. Try another sentence.")
    return(predictedNextWord)
  }
  
  # Only use the last 3 words
  if (Words_len > 3) {
    Words_len <- 3L
    Words <- tail(Words, Words_len)
  }
  
  # get exact matching next word from 4gram, 3gram and 2gram DF
  match_len <- c()
  match_list <- c()
  
  for (i in rev(1:Words_len)) {
    tmp <- c()
    tmp <- matchingWord(tail(Words, i))
    
    if (is.na(tmp[1])) {
      match_len <- c(match_len, 0)
    } else {
      selc <- tmp %in% match_list
      match_list <- c(match_list, tmp[!selc])
      match_len <- c(match_len, length(tmp[!selc]))
      #rm(selc, tmp)
    }
    if (sum(match_len) > predict_word_no) break
  }
  
  # if none of the 4gram, 3gram and 3gram exact next word is found
  # give the five most frequent 1gram
  
  if (sum(match_len) == 0) {
    predictedNextWord <- as.data.frame(table1gram$word[1:predict_word_no]) 
    colnames(predictedNextWord) <- "unigram"
    return(predictedNextWord)
  }
  
  # revInd <- rev(1:Words_len)
  # revInd <- revInd[!(match_len %in% 0)]
  # match_len <- match_len[!(match_len %in% 0)]
  
  # find where there are matches only
  match_len_tmp <- c()
  for (j in 1:length(match_len)){
    if (match_len[j] != 0){
      match_len_tmp <- c(match_len_tmp,match_len[j])
    }
  }
  match_len <- match_len_tmp
  
  # Calculate "stupid" backoff 
  # Proceedings of the 2007 Joint Conference on Empirical Methods in Natural Language Processing 
  # and Computational Natural Language Learning
  # Thorsten Brants Ashok C. Popat Peng Xu Franz J. Och Jeffrey Dean
  # eqn 5
  # the recursion is supposed to end at unigrams but below code doesn't include unigram
  backoff <- c()
  for (i in 1:length(match_len)) {
    if (i==1){
      backoff <- c(backoff, rep(1, match_len[i]))
    } else {
      backoff <- c(backoff, rep(0.4, match_len[i]))
    }
  }
  
  # Truncate matches to max 10
  if (length(match_list) > 10) {
    match_list <- match_list[1:10]
    backoff <- backoff[1:10]
  }
  
  # Get probability
  P <- c()
  for (i in 1:length(unique(backoff))) {
    tmp2 <- c()
    tmp2 <- backoff %in% unique(backoff)[i]
    
    P <- c(P, scoring(match_list[tmp2], tail(Words, rev(1:Words_len)[i])))
  }
  
  # Output dataframe result
  prediction <- data.frame(word = match_list, score=as.numeric(exp(P)*backoff), stringsAsFactors = FALSE )
  prediction <- arrange(prediction, desc(score))
  #prediction[1:5,]
  return(prediction[1:predict_word_no,])
}

matchingWord <- function(Words) {
  last_word <- NA
  string <- paste("^", paste(Words, collapse = " "), "\\b", sep = "")
  
  if (length(Words) == 1) last_word <- word(table2gram$word[grep(string, table2gram$word)],-1)
  if (length(Words) == 2) last_word <- word(table3gram$word[grep(string, table3gram$word)],-1)
  if (length(Words) == 3) last_word <- word(table4gram$word[grep(string, table4gram$word)],-1)
  return(last_word)
}


scoring <- function(suffix_word, prefix_words) {
  
  # Create one sentence with spacing
  one_sentence <- paste(prefix_words, collapse=" ")
  
  # 1gram prob
  tmp <- table1gram$word %in% prefix_words
  gram1_prob <- table1gram$freq[tmp] / sum(table1gram$freq)
  rm(tmp)
  
  # Generate 2gram from prefix words collapsed as a sentence
  gram2 <- getNGrams(one_sentence,2)
  
  # loop through gram2 and append each bigram term
  if (length(gram2) != 0) {
    terms <- c()
    for (i in 1:length(gram2)){
      string <- paste("^", paste(gram2[i], collapse = " "), "\\b", sep = "")
      #P(A intersect B)
      tmp2 <- table2gram$freq[grep(string, table2gram$word)] / sum(table2gram$freq)
      #log10(P(B intersect A)/P(A)) = log10(P(B | A)) 
      #= probability of last word given immediate prefix word
      tmp2 <- log10(tmp2 / gram1_prob[i])
      terms <- c(terms, tmp2)
    }
  } else terms <- c()
  
  final_score <- c()
  for (i in 1:length(suffix_word)) {
    string <- c()
    string <- paste("^", paste(tail(prefix_words, 1), suffix_word[i], collapse = " "), 
                    "\\b", sep = "")
    final_score <- c(final_score, 
                     as.numeric(sum(terms) + log10((table2gram$freq[grep(string, table2gram$word)] / sum(table2gram$freq) ) / tail(gram1_prob, 1))))
  }
  return(final_score)
}

