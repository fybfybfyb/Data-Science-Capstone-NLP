library(stringi)
library(ggplot2)
library(ggthemes)
library(tm)
setwd("final\\en_US")

# Read the file from the working directory
blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE) 

# summary the basic information of the given data set
blogs_length <- length(blogs)
news_length <- length(news)
twitter_length <- length(twitter)
blogs_wordNumber <- stri_count_words(blogs)
news_wordNumber <- stri_count_words(news)
twitter_wordNumber <- stri_count_words(twitter)
blogs_size_MB <- fi=le.size("en_US.blogs.txt")/10^6
news_size_MB <- file.size("en_US.news.txt")/10^6
twitter_size_MB <- file.size("en_US.twitter.txt")/10^6
summary_table <- data.frame(file = c("blogs","news","twitter"),
                            file_size_MB = c(blogs_size_MB,news_size_MB,twitter_size_MB),
                            line_number = c(blogs_length,news_length,twitter_length),
                            word_number = c(sum(blogs_wordNumber,news_wordNumber,twitter_wordNumber)))

# Data sampling
set.seed(2018)
dataSample <- c(sample(blogs, length(blogs)*0.05, replace = FALSE),
                sample(news, length(news)*0.05, replace = FALSE) ,
                sample(twitter, length(twitter)*0.05,replace = FALSE))
length(dataSample)
save(dataSample,file="dataSample.RData")
rm(blogs,news,twitter)

# Data Cleaning
# Do a set of processing for the corpus. By checking getTransformations(), 
# you can see different transformations.
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
clean.corpus<-function(corpus){
        corpus <- tm_map(corpus, toSpace, "#|/|@|\\|")
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, stripWhitespace)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removeWords, stopwords('english'))
        corpus <- tm_map(corpus, removeWords, profanity)
        corpus <- tm_map(corpus, stemDocument)
        return(corpus)
}

# get frequences of different terms in the processed termdocument matrix
getFreqence <- function(x){
        x <- removeSparseTerms(x, sparse=0.9999)
        term.freq<-sort(rowSums(as.matrix(x)),decreasing=TRUE)
        freq.df<-data.frame(word=names(term.freq),frequency=term.freq)
        return(freq.df)
}

#bigram token maker
bigram.tokenizer <-function(x){
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}
        
#trigram token maker
trigram.tokenizer <-function(x){
        unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
}
        

# based on the parameter, we can make different plots of them
createPlot <- function(data) {
        ggplot(data, aes(x = words, y = frequency)) + geom_bar(stat="identity", fill='darkred') +coord_flip()+theme_gdocs()+
                geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)
}
   
# Corpus is a large collection of texts.
corpus <- VCorpus(VectorSource(dataSample))
clean.corpus(corpus)

# get the termDocument matrix
tdm_2 <- TermDocumentMatrix(corpus, control=list(tokenize=bigram.tokenizer))
tdm_3 <- TermDocumentMatrix(corpus, control=list(tokenize=trigram.tokenizer))
freq.df_2 <- getFreqence(tdm_2)
freq.df_3 <- getFreqence(tdm_3)

#Make a barplot of the top terms in 2-grams
freq.df_2 <- subset(freq.df_2, frequency >= 90)
freq.df_3 <- subset(freq.df_3, frequency >= 90)
freq.df_2 <- freq.df_2[order(freq.df_2$frequency, decreasing = FALSE),]
freq.df_3 <- freq.df_3[order(freq.df_3$frequency, decreasing = FALSE),]
freq.df_2$word <- factor(freq.df_2$word, levels = unique(as.character(freq.df_2$word)))
freq.df_3$word <- factor(freq.df_3$word, levels = unique(as.character(freq.df_3$word)))
createPlot(freq.df_2)
createPlot(freq.df_3)

#choose one color
display.brewer.all()
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

#create a simple word cloud
set.seed(1234)
wordcloud(freq.df_2$word,freq.df_2$freq,max.words=500, random.order=FALSE, colors=pal)
wordcloud(freq.df_3$word,freq.df_3$freq,max.words=500, random.order=FALSE, colors=pal)

