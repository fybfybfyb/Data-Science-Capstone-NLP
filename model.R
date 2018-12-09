library(data.table)
library(qdap)
library(tm)
library(quanteda)
library(stringi)

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
# We build a modelsList which contain 1-5grams
#unigrams <- dfm(tokensAll, ngrams = 1, concatenator = " ", ignoredFeatures = profanity)
bigrams <- dfm(tokensAll, ngrams = 2, concatenator = " ", ignoredFeatures = profanity)
trigrams <- dfm(tokensAll, ngrams = 3, concatenator = " ", ignoredFeatures = profanity)
fourgrams <- dfm(tokensAll, ngrams = 4, concatenator = " ", ignoredFeatures = profanity)
fivegrams <- dfm(tokensAll, ngrams = 5, concatenator = " ", ignoredFeatures = profanity)

# convert the n-grams to a data table with two columns: word, freq 
# and then sort according to freq. Finally gather all to create a modelsList.
# dt1 <- data.table(word = featnames(unigrams), freq = colSums(unigrams))
# dt1 <- dt1[order(-dt1$freq),]

dt2 <- data.table(word = featnames(bigrams), freq = colSums(bigrams))
rm(bigrams)
dt2 <- dt2[,.(w1 = unlist(strsplit(word,split = " "))[1],w2 = unlist(strsplit(word,split = " "))[2],
              freq = freq),by = word]
dt2$word = NULL
saveRDS(dt2, "./final/bigram.rds")
fwrite(dt2, "./final/bigram.csv")


dt3 <- data.table(word = featnames(trigrams), freq = colSums(trigrams))
rm(trigrams)
dt3 <- dt3[,.(w1 = unlist(strsplit(word,split = " "))[1],w2 = unlist(strsplit(word,split = " "))[2],
              w3 = unlist(strsplit(word,split = " "))[3],freq = freq),by = word]
dt3$word = NULL
saveRDS(dt3,"./final/trigram.rds")
fwrite(dt3, "./final/trigram.csv")


dt4 <- data.table(word = featnames(fourgrams), freq = colSums(fourgrams))
rm(fourgrams)
dt4 <- dt4[,.(w1 = unlist(strsplit(word,split = " "))[1],w2 = unlist(strsplit(word,split = " "))[2],
              w3 = unlist(strsplit(word,split = " "))[3],w4 = unlist(strsplit(word,split = " "))[4],
           freq = freq),by = word]
dt4$word = NULL
saveRDS(dt4,"./final/fourgram.rds")
fwrite(dt4, "./final/fourgram.csv")


dt5 <- data.table(word = featnames(fivegrams), freq = colSums(fivegrams))
rm(fivegrams)
dt5 <- dt5[,.(w1 = unlist(strsplit(word,split = " "))[1],w2 = unlist(strsplit(word,split = " "))[2],
              w3 = unlist(strsplit(word,split = " "))[3],w4 = unlist(strsplit(word,split = " "))[4],
              w5 = unlist(strsplit(word,split = " "))[5], freq = freq),by = word]
dt5$word = NULL
saveRDS(dt5,"./final/fivegram.rds")
fwrite(dt5, "./final/fivegram.csv")
rm(tokensAll)
# > dt <- data.table(word = c("I am boy", "She is girl"), freq = c(15,25))
#modelsList <- list(as.data.frame(dt5),as.data.frame(dt4),as.data.frame(dt3),as.data.frame(dt2),as.data.frame(dt1))
#save(modelsList,file="modelsList.RData")
# give an example of how to predict using the function "predict_Backoff" .
# testline <- "Hey sunshine, can you follow me and make me the"
# predict_Backoff(testline, modelsList, isDebugMode = T)

# define a predict function based on Katz_Backoff and n-grams. 
# predict_Backoff <- function(testline,modelsList,isDebugMode = F){
#         # Max number of ngrams supported
#         maxNGramIndex = length(modelsList)
#       #  Clean the test string
#       line = iconv(testline,"latin1","ASCII",sub="")
#       line = line %>% replace_abbreviation %>% replace_contraction %>% removeNumbers %>%
#              removePunctuation %>% tolower  %>% stripWhitespace
# 
#         if(isDebugMode)
#                 print(line)
#         
#         # Tokenize the test string
#         words <- unlist(strsplit(line, split=" "))
#         len <- length(words)
#         
#         if(isDebugMode)
#                 print(paste("Length of the string is: ",len))
#         
#         # If test string is lower than the max number of ngrams then we do not need to go through all the ngram model
#         # Instead we will look into N-gram models having N less than the length of test string
#         if(len < maxNGramIndex){
#                 nGramIndex = len + 1
#                 
#                 localModelsList = modelsList[(maxNGramIndex-len):maxNGramIndex]
#                 
#         }else{
#                 nGramIndex = maxNGramIndex
#                 localModelsList = modelsList
#         }
#         
#         if(isDebugMode)
#                 print(paste("Number of models will be used: ",length(localModelsList)))
#         
#         for(model in localModelsList){
#                 
#                 # +2 offest to match number of words with nGram model
#                 #if(nGramIndex != maxNGramIndex)
#                 pattern = paste0("^",paste(words[(len - nGramIndex + 2):len],collapse = " "))
#                 
#                 if(isDebugMode)
#                         print(pattern)
#                 
#                 # Find the pattern in the respective n-gram model
#                 nextWords = model[grep(pattern,model$word)[1:5],1]
#                 nextWords = nextWords[!is.na(nextWords)]
#                 if(length(nextWords) != 0){
#                         nextWordIndex = sample(1:length(nextWords),1)
#                         nextWord = nextWords[nextWordIndex]
#                 }else{
#                         nextWord = NA
#                 }
#                 
#                 # Print top 5 match
#                 if(isDebugMode)
#                         print(nextWords)
#                 
#                 if(isDebugMode)
#                         print(paste("Predicated word: ",nextWord))
#                 
#                 nGramIndex = nGramIndex - 1
#                 
#                 # If the next word is predicted then return the answer
#                 # Else backoff to check the word with n-1 gram models
#                 if(!is.na(nextWord)){
#                         
#                         # The returned word will have have queried word as it was used to match
#                         # Just remove the queried word from the predicted word for better user experience
#                         tempNextWord = unlist(strsplit(as.character(nextWord)," "))
#                         
#                         if(isDebugMode)
#                                 print(paste("Splitted text: ",tempNextWord))
#                         
#                         nextWord = paste(tempNextWord[length(tempNextWord)])
#                         
#                         break
#                 }
#                 
#                 
#         }
#         
#         if(is.na(nextWord)){
#                 if(isDebugMode)
#                         print(paste("No match found in ",paste(1:maxNGramIndex,collapse = ","),"Gram models so returning the most frequent word"))
#                 nextWord = modelsList[[maxNGramIndex]][1,1]
#         }
#         
#         if(isDebugMode)
#                 print(paste("The next predicated word using",nGramIndex+1,"gram model:-", nextWord))
#         return(nextWord)
#         
# }

