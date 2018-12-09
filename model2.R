library(data.table)
library(qdap)
library(tm)
library(quanteda)
library(stringi)

#dt2 <- fread("bigram.csv")
#dt3<- fread("trigram.csv")
#dt4 <- fread("fourgram.csv")
#dt5 <- fread("fivegram.csv")

dt2 <- readRDS("bigram.rds")
dt3 <- readRDS("trigram.rds")
dt4 <- readRDS("fourgram.rds")
dt5 <- readRDS("fivegram.rds")

dataCleaner<-function(text){
        
        cleanText <- tolower(text)
        cleanText <- removePunctuation(cleanText)
        cleanText <- removeNumbers(cleanText)
        #cleanText <- str_replace_all(cleanText, "[^[:alnum:]]", " ")
        cleanText <- stripWhitespace(cleanText)
        
        return(cleanText)
}

cleanInput <- function(text){
        
        textInput <- dataCleaner(text)
        textInput <- iconv(textInput,"latin1","ASCII",sub="")
        textInput <- unlist(strsplit(textInput, split=" "))
        
        return(textInput)
}


predictWords <- function(line, isDebugMode = F){
        
        # calculate the length of the word and decide the n-grams to use.
       # line <- unlist(strsplit(line, split=" "))
        len <- length(line)
        
        if(isDebugMode)
                print(paste("Length of the string is: ",len))
        
        if (len>=4) {
                line <- line[(len-3):len]
                }
        
        else if(len==3) {
                line <- c(NA,line)
                }
        
        else if(len==2){
                line <- c(NA,NA,line)
                }
        
        else{
                line <- c(NA,NA,NA,line)
                }
        
        if(isDebugMode)
                print(paste("New pattern is: ",paste(line, collapse = " ")))
        
        pred <- head(dt4[w1 == line[1] & w2 == line[2] & w3 == line[3] & w4 == line[4]][order(-freq)]$w5)
        
        if(isDebugMode)
                print(paste("5-gram prediction : ",pred))
        
        if(length(pred) == 0){
                if(isDebugMode)
                        print(paste("New pattern is: ",paste(line[2:4], collapse = " ")))
                
                pred <- head(dt4[w1 == line[2] & w2 == line[3] & w3 == line[4]][order(-freq)]$w4)
                
                if(isDebugMode)
                        print(paste("4-gram prediction : ",pred))
                
                if(length(pred) == 0){
                        if(isDebugMode)
                                print(paste("New pattern is: ",paste(line[3:4], collapse = " ")))
                        
                        pred <- head(dt3[w1 == line[3] & w2 == line[4]][order(-freq)]$w3)
                        
                        if(isDebugMode)
                                print(paste("3-gram prediction : ",pred))
                        
                        if(length(pred) == 0){
                                if(isDebugMode)
                                        print(paste("New pattern is: ",paste(line[4], collapse = " ")))
                                
                                pred <- head(dt2[w1 == line[4]][order(-freq)]$w2)
                                
                                if(isDebugMode)
                                        print(paste("2-gram prediction : ",pred))
                        }
                }
        }
        print(paste("Predicated word: ",paste(pred, collapse = " ")))
        return(pred)
}
# line <- "Every inch of you is perfect from the bottom to the"
# predictWords(line, isDebugMode = F)
