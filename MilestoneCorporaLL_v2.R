#MilestoneCorporaLL_v2.R builds from MilestoneCorporaLL.R
#It adds final cleaning to the corpus
#It adds language modelling component (see NLP_notes folder, 4_languagemodeling.pdf)

#Installation of RWeka can be tricky on MacOS Sierra
#sudo R CMD javareconf #On terminal 
#install.packages("rJava",type='source')
#install.packages("RWeka")
#sudo ln -f -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib

library(tm)
library(stylo)
library(RWeka)
library(dplyr)
library(stringr)

# Read in data and save total line per file
con <- file("data/final/en_US/en_US.blogs.txt")
blogs <- readLines(con)
close(con)

con <- file("data/final/en_US/en_US.news.txt")
news <- readLines(con)
close(con)

con <- file("data/final/en_US/en_US.twitter.txt")
twitter <- readLines(con)
close(con)
rm(con)

#Counting number of lines per file
con_blog <- file("data/final/en_US/en_US.blogs.txt", "r") 
totalLineBlog <- length(readLines(con_blog))
con_news <- file("data/final/en_US/en_US.news.txt", "r") 
totalLineNews <- length(readLines(con_news))
con_twit <- file("data/final/en_US/en_US.twitter.txt", "r") 
totalLineTwit <- length(readLines(con_twit))
rm(con_blog,con_news,con_twit)


#Use rbinom to select 2%, 2%, 1% of blog, news, 
#and twit lines, respectively because the number of twitter lines
#about double blog and news lines.
set.seed(101)
selcLinesBlog <- rbinom(totalLineBlog,1,0.02)
selcLinesBlog <- as.logical(selcLinesBlog)
blogs_Sample <- blogs[selcLinesBlog]

set.seed(102)
selcLinesNews <- rbinom(totalLineNews,1,0.02)
selcLinesNews <- as.logical(selcLinesNews)
news_Sample <- news[selcLinesNews]

set.seed(103)
selcLinesTwit <- rbinom(totalLineTwit,1,0.01)
selcLinesTwit <- as.logical(selcLinesTwit)
twitter_Sample <- twitter[selcLinesTwit]

save(blogs_Sample, news_Sample, twitter_Sample, file = "20170820_Sample_blogs_new_twitter.RData")
rm(list=ls())

load("20170820_Sample_blogs_new_twitter.RData")

#profanity RData creation
#https://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-and-how-to-use-facebooks-moderation-tool/
con <- file("data/profane.txt")
profane <- readLines(con)
close(con)
save(profane,file="profane.RData")
rm(profane,con)

#source in all the functions to clean up corpus and get Ngrams
source("functions/corpusCleanToNgrams.R")

# Clean data, save file
blogs_Clean <- cleanSample(blogs_Sample)
news_Clean <- cleanSample(news_Sample)
twitter_Clean <- cleanSample(twitter_Sample)

save(blogs_Clean, news_Clean, twitter_Clean, file = "20170820_clean_blogs_new_twitter.RData")

rm(blogs_Sample, news_Sample, twitter_Sample)

# convert cleaned texts to corpus
blogsCorpus <- VCorpus(VectorSource(blogs_Clean))
newsCorpus <- VCorpus(VectorSource(news_Clean))
twitterCorpus <- VCorpus(VectorSource(twitter_Clean))

save(blogsCorpus, newsCorpus, twitterCorpus, file = "20170820_corpus_ngram.RData")

rm(blogs_Clean, news_Clean, twitter_Clean)

# Create word list from corpus variables
dataBlogWords <- txt.to.words(blogsCorpus)
dataNewsWords <- txt.to.words(newsCorpus)
dataTwitterWords <- txt.to.words(twitterCorpus)

save(dataBlogWords, dataNewsWords, dataTwitterWords, file = "20170820_txt2wordsCorpus.RData")

rm(blogsCorpus, newsCorpus, twitterCorpus)

#create unigram
raw1gramB <- getNGrams(dataBlogWords, 1)
raw1gramN <- getNGrams(dataNewsWords, 1)
raw1gramT <- getNGrams(dataTwitterWords, 1)

save(raw1gramB, raw1gramN, raw1gramT, file = "20170820_raw1gram.RData")

# Create raw 2gram variables
raw2gramB <- getNGrams(dataBlogWords, 2)
raw2gramN <- getNGrams(dataNewsWords, 2)
raw2gramT <- getNGrams(dataTwitterWords, 2)

save(raw2gramT, raw2gramN, raw2gramB, file = "20170820_raw2gram.RData")

# Create raw 3gram variables
raw3gramB <- getNGrams(dataBlogWords, 3)
raw3gramN <- getNGrams(dataNewsWords, 3)
raw3gramT <- getNGrams(dataTwitterWords, 3)

save(raw3gramT, raw3gramN, raw3gramB, file = "20170820_raw3gram.RData")

# Create raw 4gram variables
raw4gramB <- getNGrams(dataBlogWords, 4)
raw4gramN <- getNGrams(dataNewsWords, 4)
raw4gramT <- getNGrams(dataTwitterWords, 4)

save(raw4gramB, raw4gramN, raw4gramT, file = "20170820_raw4gram.RData")

rm(list=ls())

### generated filtered table for each ngram
load("20170820_raw1gram.RData")
raw1gram <- table(c(raw1gramB, raw1gramN, raw1gramT))
wf1gram <- data.frame(word=names(raw1gram), freq=as.numeric(raw1gram), stringsAsFactors = FALSE)
# the "c" is extra character introduced by NGramTokenizer, needs to be removed
wf1gram <- wf1gram %>% filter(word != "c",freq != 1) %>% arrange(desc(freq))
wf1gram <- arrange(wf1gram, desc(freq))
save(wf1gram, file = "20170820_raw1gram_table.RData")

load("20170820_raw2gram.RData")
table2gram <- table(c(raw2gramB,raw2gramN,raw2gramT))
wf2gram <- data.frame(word=names(table2gram), freq=as.numeric(table2gram), stringsAsFactors = FALSE)
wf2gram <- wf2gram %>% filter(freq != 1) %>% arrange(desc(freq)) 
save(wf2gram, file = "20170820_raw2gram_table.RData")

load("20170820_raw3gram.RData")
table3gram <- table(c(raw3gramB,raw3gramN,raw3gramT))
wf3gram <- data.frame(word=names(table3gram), freq=as.numeric(table3gram), stringsAsFactors = FALSE)
wf3gram <- wf3gram %>% filter(freq != 1) %>% arrange(desc(freq)) 
save(wf3gram, file = "20170820_raw3gram_table.RData")

load("20170820_raw4gram.RData")
table4gram <- table(c(raw4gramB,raw4gramN,raw4gramT))
wf4gram <- data.frame(word=names(table4gram), freq=as.numeric(table4gram), stringsAsFactors = FALSE)
wf4gram <- wf4gram %>% filter(freq != 1) %>% arrange(desc(freq)) 
save(wf4gram, file = "20170820_raw4gram_table.RData")

# Generate indicator of where each type of ngram type begins and ends after concatenation
wf1gram$gramtype <- rep("gram1",nrow(wf1gram))
wf2gram$gramtype <- rep("gram2",nrow(wf2gram))
wf3gram$gramtype <- rep("gram3",nrow(wf3gram))
wf4gram$gramtype <- rep("gram4",nrow(wf4gram))

# Concatenate tables, write .txt file
ngram_table <- rbind(wf1gram, wf2gram, wf3gram, wf4gram)

save(ngram_table, file = "20170820_ngram_table.RData")

write.table(ngram_table, file = "final_ngram_table.txt", sep = "\t", row.names = FALSE)

#deploy shiny app
