library(ggplot2)
library(ggthemes)
library(quanteda)
library(stringi)
library(dplyr)
library(RColorBrewer)
if(!file.exists("./swearWords.txt")){
        download.file(url = "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt",
                      destfile= "./swearWords.txt",
                      method = "curl")
}
profanity <- scan("./swearWords.txt", what = "character", sep = "\n", encoding = "UTF-8")
# make tokens for all n-grams.
tokensAll = dataSample %>% tokens(what="word", remove_numbers = TRUE,
                                  remove_punct = TRUE,
                                  remove_separators = TRUE,
                                  remove_symbols = TRUE,
                                  remove_twitter = TRUE,
                                  verbose = TRUE) %>% tokens_tolower() %>% tokens_remove(profanity)
       
save(tokensAll,file="tokensAll.RData")
rm(dataSample)
# ngram <- tokens_ngrams(tokensAll, n = c(2:5),concatenator = " ")
# create different grams 
unigrams <- dfm(tokensAll, ngrams = 1, concatenator = " ", ignoredFeatures = profanity)
bigrams <- dfm(tokensAll, ngrams = 2, concatenator = " ", ignoredFeatures = profanity)
trigrams <- dfm(tokensAll, ngrams = 3, concatenator = " ", ignoredFeatures = profanity)

# make a function of plot
createPlot <- function(data) {
        ggplot(data, aes(x = reorder(word,freq), y = freq)) + geom_bar(stat="identity", fill='darkred') +coord_flip()+theme_gdocs()+
                geom_text(aes(label=freq), colour="white",hjust=1.25, size=5.0)
}

# slect top 20 frequece words
top20unigrams <- topfeatures(unigrams, 20)
top20bigrams <- topfeatures(bigrams, 20)
top20trigrams <- topfeatures(trigrams, 20)
unigramsDf_20 <- data.frame(word=names(top20unigrams), freq=top20unigrams)
bigramsDf_20 <- data.frame(word=names(top20bigrams), freq=top20bigrams)
trigramsDf_20 <- data.frame(word=names(top20trigrams), freq=top20trigrams)

# make a plot of the selected n-grams and their frequences.
createPlot(unigramsDf_20)
createPlot(bigramsDf_20)
createPlot(trigramsDf_20)
