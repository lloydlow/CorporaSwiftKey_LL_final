# NextWordPrediction

### Introduction
Prediction of the next word that a person will type such as on a blog, news and twitter is a useful feature because it saves users from typing too much and this feature has commercial value. For example, SwiftKey is a company interested in building a smart keyboard that helps minimise typing on mobile devices because as most of us are aware, typing on smartphones is more difficult due to its size. However, human languages are complex and there are other factors that make interpretation of sentences difficult such as users mixing languages, unusual short forms and spelling errors. A good next word prediction tool must take all these factors into account. This repository holds the source code to a simple yet efficient implementation of N-gram language model with "stupid" backoff scoring method to predict next word.

Sources of the datasets are prepared by SwiftKey, which are corpus of texts in four languages sourced from blogs, news and twitter (see URL below). Only the datasets in English language is used.
Data source:  https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

### Data Science Capstone Project - Next Word Prediction Shiny App
Here you will find descriptions of algorithm and associated code and data file used to create a Shiny App that predicts the next word that a user will type.

20170820_ngram_table.RData - RData of the cleaned and processed table of unigram, bigram, trigram and quadrigram dataset, which can be loaded into R

functions/corpusCleanToNgrams.R - Contains getNGrams and cleanSample functions that are the main workhorse functions to clean and create the appropriate Ngram from the corpus

global.R - load the four Ngram tables, required libraries and functions created for this project into global environment for use in Shiny App

MilestoneCorporaLL_v2.R - contains the R code that processed the corpus to Ngram table

profane.RData - RData of the collection of profanity words collected from an online source

README.md - readme on github

server.R - server R code for shiny app

ui.R - User interface R code for shiny app

### Shiny App URL
 https://lloydlow.shinyapps.io/CorporaSwiftKey_LL/
 
### R presentation on this App
http://rpubs.com/lloyd/305887

