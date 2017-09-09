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
