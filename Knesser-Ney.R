rm(list=ls())
if(length(dev.list()!=0)) { dev.off() }
library(tidyverse)
library(tm)
library(slam)
library(wordcloud)
library(SemNetCleaner)

#########
readkey <- function() {
        line <- readline(prompt="Press [enter] to continue")
}

#########
getDataSet <- function() {
        temp <- tempfile()
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",temp)
        unzip(temp, exdir=getwd())
        unlink(temp)
        file.rename("final","textSource")
}

#########
readTextFile <- function(filename, ...){
        con <- file(filename, "r")
        text <-readLines(con, encoding = "UTF-8", skipNul = TRUE, ...)
        close(con)
        return(iconv(text, "latin1", "ASCII", sub="")) # only ASCII characters, remove emoticons
}

#########
writeTextFile <- function(text,filename) {
        con <- file(filename, "w")
        writeLines(text, con, sep="\n")
        close(con)
}

#########
getData <- function(sampleDirectory,sampleFile,nSamples,forceNew=FALSE) {
        
        nSamplesFile <- file.path(sampleDirectory,paste0("../",sampleFile,".nSamples.RData"))

        
        #Check if the sample file has been created
        if(!dir.exists(sampleDirectory)) {
                dir.create(sampleDirectory)
        }
        fullSampleFile <- file.path(sampleDirectory,sampleFile)
        
        sampleTestDirectory <- paste0(sampleDirectory,"TEST")
        if(!dir.exists(sampleTestDirectory)) {
                dir.create(sampleTestDirectory)
        }
        testSampleFile <- file.path(sampleTestDirectory,sampleFile)
        
        
        if(file.exists(nSamplesFile)) {
                load(nSamplesFile) #reads nSamplesOld
        } else {
                nSamplesOld <- 0
        }
        
        if(forceNew || (nSamples != nSamplesOld)) {
                if(file.exists(fullSampleFile)) file.remove(fullSampleFile)
        }
        
        if(!file.exists(fullSampleFile)) {
                print("Building new samples file")
                
                twitterFile <- "./textSource/en_US/en_US.twitter.txt"
                blogsFile <- "./textSource/en_US/en_US.blogs.txt"
                newsFile <- "./textSource/en_US/en_US.news.txt"
                bonusFile <- "./textSource/en_US/en_US.bonus.txt"
                
                # Lines, words, bytes
                print("Lines, words, bytes:")
                system(paste("wc",twitterFile))
                system(paste("wc",blogsFile))
                system(paste("wc",newsFile))
                system(paste("wc",bonusFile))
                
                # Read all the data in
                
                twitterText <- readTextFile(twitterFile,-1)
                blogsText <- readTextFile(blogsFile,-1)
                newsText <- readTextFile(newsFile,-1)
                bonusText <- readTextFile(bonusFile,-1)
                
                print("=============Twitter head and tail:")
                print(head(twitterText,n=2))
                print(tail(twitterText,n=2))
                
                print("=============Blogs head and tail")
                print(head(blogsText,n=2))
                print(tail(blogsText,n=2))
                
                print("=============News head and tail")
                print(head(newsText,n=2))
                print(tail(newsText,n=2))
                
                print("Memory use:")
                print(paste("Twiter text:",format(object.size(twitterText), units = "MB")))
                print(paste("Blogs text",format(object.size(blogsText), units = "MB")))
                print(paste("News text",format(object.size(newsText), units = "MB")))
                
                # Combine the given files
                allText <- c(twitterText, blogsText, newsText)
                rm(twitterText, blogsText, newsText)
                print("Length of all text (lines):")
                print(length(allText))
                
                # Sample
                fullSampleNumber <- round(nSamples*1.2,0) # add 20% for the test set
                workingText <- sample(allText, fullSampleNumber, replace=FALSE) 
                rm(allText)
                
                # Append the bonus text
                
                workingText <- c(bonusText,workingText)
                bonusLength <- length(bonusText)
                fullSampleNumber <- fullSampleNumber+bonusLength
                if(fullSampleNumber != length(workingText)) stop("Error!")
                
                # Write it
                writeTextFile(workingText[1:(nSamples+bonusLength)],fullSampleFile) # this is the training set
                writeTextFile(workingText[(nSamples+bonusLength+1):(fullSampleNumber)],testSampleFile) # this is the test set
                nSamplesOld <- nSamples
                save(nSamplesOld, file = nSamplesFile)
                #readkey()
        }
}

#########
# Create the Corpus
createCorpus <- function(nSamples, forceNew = FALSE) {
        set.seed(1234)
        sampleDirectory <- "./textSample"
        sampleFile <- "textSample.txt"
        
        print("Creating Corpus")
        getData(sampleDirectory,sampleFile, nSamples, forceNew = FALSE)
        
        # Reading the Corpus as a directory source appears to be more memory efficient than
        # reading it as a vector of lines
        myCorpus <- VCorpus(DirSource(sampleDirectory),
                            readerControl=list(readPlain, language="en", load=TRUE))
        return(myCorpus)
}

#########
# Transform symbols/punctuations etc. into a space (adapted from: https://rpubs.com/elisb/EDA_nword)
cleanStuff = function(x) {
        x <- str_replace_all(x, fixed(" i'm "), " i am ")
        x <- str_replace_all(x, fixed(" im "), " i am ")
        x <- str_replace_all(x, fixed(" youre "), " you are ")
        x <- str_replace_all(x, fixed(" youve "), " you have ")
        x <- str_replace_all(x, fixed(" you've "), " you have ")
        x <- str_replace_all(x, fixed(" he's "), " he is ")
        x <- str_replace_all(x, fixed(" she's"), " she is ")
        x <- str_replace_all(x, fixed(" it's "), " it is ")
        x <- str_replace_all(x, fixed(" we're "), " we are ")
        x <- str_replace_all(x, fixed(" they're "), " they are ")
        x <- str_replace_all(x, fixed(" i'll "), " i will ")
        x <- str_replace_all(x, fixed(" you'll "), " you will ")
        x <- str_replace_all(x, fixed(" he'll "), " he will ")
        x <- str_replace_all(x, fixed(" she'll "), " she will ")
        x <- str_replace_all(x, fixed(" we'll "), " we will ")
        x <- str_replace_all(x, fixed(" they'll "), " they will ")
        x <- str_replace_all(x, fixed(" can't "), " can not ")
        x <- str_replace_all(x, fixed("n't "), " not ")
        x <- str_replace_all(x, fixed(" dont "), " do not ")
        x <- str_replace_all(x, fixed("'d "), " would ")
        x <- str_replace_all(x, fixed("'ll "), " will ")
        x <- str_replace_all(x, fixed("'re "), " are ")
        x <- str_replace_all(x, fixed("'ve "), " have ")
        x <- str_replace_all(x, fixed("ca not"), "can not")
        x <- str_replace_all(x, fixed("pls"), "please")
        x <- str_replace_all(x, fixed("'s"), " ")
        x <- str_replace_all(x, fixed(" u "), "you")
        x <- paste0(x,".") #Add a period to every line
        x <- str_replace_all(x, "[!?.:;]\\.$", ".") # Remove duplicate periods
        x <- str_replace_all(x, "[^\x01-\x7F]", " ")  # Replace all non-ASCII characters - should be redundant as we excluded in readTextFile
        return(x)
}

#########
# Identify end of sentence periods with the character sntcfs
# Adapted from https://regex101.com/r/lS5tT3/15
identifyFullStops = function(x) {
        x <- str_replace_all(x, "[!?.:;]+(?=$|\\s)", " sntcfs ")
}

#########
# Replace end of sentence tokens "sntcfs" with shorter token ">"
nicerFullStops = function(x) {

        #It would be great to figure out better way to do this ...
        x <- str_replace_all(x, "sntcfs\\s+sntcfs\\s+sntcfs\\s+sntcfs\\s+sntcfs\\s+sntcfs", " > ")   
        x <- str_replace_all(x, "sntcfs\\s+sntcfs\\s+sntcfs\\s+sntcfs\\s+sntcfs", " > ")   
        x <- str_replace_all(x, "sntcfs\\s+sntcfs\\s+sntcfs\\s+sntcfs", " > ")   
        x <- str_replace_all(x, "sntcfs\\s+sntcfs\\s+sntcfs", " > ")   
        x <- str_replace_all(x, "sntcfs\\s+sntcfs", " > ")   
        x <- str_replace_all(x, fixed("sntcfs"), " > ")
}

#########
# Transform symbols/punctuations etc. into a space (borrowed from: https://rpubs.com/elisb/EDA_nword)
cleanToSpace <- content_transformer(
        function(x, pattern) {
        return (str_replace_all(x, pattern, " "))
                }
        )

#########
# Cleanup the corpus to just "nice" words
cleanupCorpus <- function(myCorpus, keepStopWords) {
        #print("Cleaning ...")
        # Read profanity list
        profanityList <- suppressWarnings(readLines("https://gist.githubusercontent.com/ryanlewis/a37739d710ccdb4b406d/raw/3b70dd644cec678ddc43da88d30034add22897ef/google_twunter_lol"))
        profanityList <- profanityList[order(str_length(profanityList),decreasing=TRUE)]
        
        #Cleanup. Don't remove stop words nor stem 
        myCorpus <- myCorpus %>% 
                tm_map(content_transformer(tolower)) %>% 
                tm_map(removeWords, profanityList)  %>% 
                tm_map(content_transformer(cleanStuff)) %>% 
                tm_map(removeWords, c("b", "c", "d", "e",
                                      "f", "g", "h", "j",
                                      "k", "l", "m", "n", 
                                      "o", "p", "q", "r", 
                                      "s", "t", "u", "v", 
                                      "w", "x", "y", "z", 
                                      "rt","ya")) %>%      #remove some common abbreviations and random letters
                tm_map(content_transformer(identifyFullStops)) %>%
                tm_map(cleanToSpace, "(ftp|http|ht
                       tps|www)[^[:space:]]+") %>%  #remove URL's
                tm_map(cleanToSpace, "(ftp|http|https|www)[^[:space:]]+") %>%  #remove emails
                tm_map(cleanToSpace, "(#[[:alnum:]_]*)") %>%                   #remove Twitter hashtags
                tm_map(cleanToSpace, "(@[[:alnum:]_]*)") %>%                   #remove Twitter @user
                tm_map(content_transformer(removeNumbers)) %>% 
                tm_map(content_transformer(removePunctuation)) %>% 
                tm_map(content_transformer(nicerFullStops)) %>%
                tm_map(content_transformer(stripWhitespace))
        if(!keepStopWords) myCorpus <- myCorpus %>% tm_map(removeWords, stopwords("english"))
        
        return(myCorpus)
}

#####
# Create n-grams from text
nGramTokenizer <- function(x,n)
        unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

#########
# Build the Document Term Matrices from unigrams to ngrams
# Returns a list of dtm's
buildDTM <-function(myCorpus,maxNgram, minWordCount=1, minCharWordLength=1) {
        print("Building Document Term Matrices")
        dtm <- vector(mode = "list", length = maxNgram)
        for(iGram in 1:maxNgram) {
                print(paste("iGram=",iGram))
                
                if(iGram == 1) mwc <- 1 else {
                        mwc <- 2*round(minWordCount/iGram)  # reduce the number of repeats as iGram increases
                        if(mwc < round(minWordCount/2)) mwc <- round(minWordCount/2)
                        print(paste("mwc =",mwc))
                }
                
                dtm [[iGram]] <- myCorpus %>% 
                        DocumentTermMatrix(
                                control=list(
                                        tokenize=function(x) nGramTokenizer(x,iGram),
                                        bounds=list(local=c(mwc,Inf)),
                                        wordLengths=c(minCharWordLength, Inf)
                                )
                        )
        }
        return(dtm)
}

#########
# Build Bag of Words dataframe from DTM
buildBagOfWords <- function(dtm) {
        print("Building Bags of Words")
        maxNgram <- length(dtm)
        bagOfWords <- vector(mode = "list", length = maxNgram)
        for(iGram in 1:maxNgram) {
                wordVector <- as.data.frame(sort(col_sums(dtm[[iGram]]), decreasing = TRUE))
                if(nrow(wordVector)>0) {
                        bagOfWords[[iGram]] <-  wordVector
                        bagOfWords[[iGram]][,2] <- cumsum(bagOfWords[[iGram]][,1])
                        names(bagOfWords[[iGram]]) <- c("count", "cumCount")
                } else {
                        bagOfWords[[iGram]] <- NULL   
                }
                # Remove rows with names containing a full-stop (>) character
                # This effectively breaks ngrams at the full-stop.
                rowsWithPeriods <- which(str_detect(row.names(bagOfWords[[iGram]]),">"))
                bagOfWords[[iGram]] <- bagOfWords[[iGram]][-rowsWithPeriods,]
        }
        # Reorder in descending order
        for(iGram in 1:maxNgram) {
                bagOfWords[[iGram]] <- bagOfWords[[iGram]][order(bagOfWords[[iGram]][,"count"],decreasing = TRUE),]
                bagOfWords[[iGram]][,2] <- cumsum(bagOfWords[[iGram]][,1])
        }
        return(bagOfWords)
}

######
# Plot pareto, wordcloud and cumulative count
plotBagOfWords<- function (bagOfWords,numBins=40) {
        print("Plotting Bags of Words")
        plot(1:2,1:2)  # cleanup graphics device ...??
        maxNgram <- length(bagOfWords)
        for(iGram in 1:maxNgram) {
                # Pareto plot
                ngrams <- row.names(bagOfWords[[iGram]])
                count <- bagOfWords[[iGram]][,"count"]
                dfplot <- data.frame(ngram= factor(head(ngrams, numBins),
                                                  levels=head(row.names(bagOfWords[[iGram]]), numBins)),
                                     count= head(count, numBins))
                fig <- ggplot(dfplot, aes(x=ngram, y=count)) + 
                        geom_bar(stat="identity") + 
                        xlab(paste0(iGram,"-Grams")) + 
                        ylab("count") +
                        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                print(fig)
                
                # Wordcloud
                # wordcloud(names(freq1), freq1, min.freq=1000,colors=brewer.pal(5, "Dark2"))
                
                if(iGram < 4) suppressWarnings(wordcloud(ngrams, count, max.words = 200, random.order = FALSE,rot.per=0.35, use.r.layout=FALSE,colors=brewer.pal(8, "Dark2")))
                
                # Cumulative count plot
                fig <- ggplot(bagOfWords[[iGram]], aes(x=seq_along(cumCount),y=cumCount)) + 
                        geom_line() + 
                        xlab(paste0("Number of ", iGram, "-Grams")) + 
                        ylab("Cummulative Count") +
                        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                print(fig)
        }
}

#########
viewBagOfWords <- function(bagOfWords, name="BoW") {
        print("Viewing Bags of Words")
        maxNgram <- length(bagOfWords)
        print(paste("List has",maxNgram,"ngrams"))
        for(iGram in 1:maxNgram) {
                View(bagOfWords[[iGram]], paste0(iGram,"-grams", name))
        }
}

#########
findNThresholdBagOfWords <- function(bagOfWords, threshold) {
        print(paste("Finding Threshold=", threshold, "in Bags of Words"))
        maxNgram <- length(bagOfWords)
        
                
        nThreshold <- vector(mode="numeric", length=maxNgram)
        for(iGram in 1:maxNgram) {
                nRows <- nrow(bagOfWords[[iGram]])
                totalCount = sum(bagOfWords[[iGram]][nRows,"cumCount"])
                if(nRows > 0) for(i in 1:nRows) {
                        if(bagOfWords[[iGram]][i,"cumCount"]/totalCount >= threshold) break
                }
                nThreshold[iGram] <- i
        }
        return(nThreshold)
}

###############################################################################
#
buildCleanBoW <- function (nSamples, maxNgram, minWordCount, threshold, keepStopWords) {
        # Create Bag of Words from Corpus via Document Term Matrix
        myCorpus <- createCorpus(nSamples, forceNew=FALSE) %>% 
                cleanupCorpus(keepStopWords)
        
        # Limit the dictionary
        initBagOfWords <- myCorpus %>%       
                buildDTM(maxNgram=1, minWordCount=1, minCharWordLength=1) %>%
                buildBagOfWords()
        
        #viewBagOfWords(initBagOfWords)
        
        nThreshold <- findNThresholdBagOfWords(initBagOfWords, 0.5)
        print("50% Thresholds:")
        print(nThreshold)
        
        nThreshold <- findNThresholdBagOfWords(initBagOfWords, 0.9)
        print("90% Thresholds:")
        print(nThreshold)
        
        nThreshold <- findNThresholdBagOfWords(initBagOfWords, threshold)
        print(paste0(threshold*100,"% Thresholds:"))
        print(nThreshold)
        
        dictionary <- row.names(initBagOfWords[[1]])
        notInDictionary <- dictionary[(nThreshold+1):length(dictionary)]
        dictionary <- dictionary[1:nThreshold]
        
        notInDictionary <- notInDictionary[order(str_length(notInDictionary),decreasing = TRUE)]
        tran <- "%"
        
        # Replace words notInDictionary with unktkn (indicated by %) in Corpus
        
         myCorpus <- tm_map(myCorpus, content_transformer(
                function(x) textclean::replace_tokens(x, notInDictionary,tran)
        ))
        
        rm(dictionary,notInDictionary,initBagOfWords)  # save space
        
        bagOfWords <- myCorpus %>%       
                buildDTM(maxNgram, minWordCount=minWordCount, minCharWordLength=1) %>% 
                buildBagOfWords()
        
        #plotBagOfWords(bagOfWords)
        #viewBagOfWords(bagOfWords)
        
        for(i in 1:length(bagOfWords)) {
                bagOfWords[[i]] <- bagOfWords[[i]] %>% select(count)
        }
        
        return(bagOfWords)
}

###############################################################################
#
buildContCounts <- function(bow) {
        print("Calculating contCounts")
        
        maxNgram <- length(bow)
        for(i in 2:maxNgram) {
                print(paste("i=",i))
                strings <- row.names(bow[[i]])
                splitWords <- sapply(strings,function(x) {nGramTokenizer(x,1)})
                beginning <- splitWords[1,]
                ending <- splitWords[i,]
                if(i > 2) for(j in 2:(i-1)) {
                        beginning <- paste(beginning,splitWords[j,])
                        ending <- paste(splitWords[i-(j-1),],ending)
                }
                beginningCount <- table(beginning)
                endingCount <- table(ending)
                bow[[i-1]]$preceeds <- 0
                bow[[i-1]]$follows <- 0
                
                bow[[i-1]][names(beginningCount),"preceeds"] <- beginningCount
                bow[[i-1]][names(endingCount),"follows"] <- endingCount
                
        }
        # set "preceeds" and "follows" to the count for the maxNgram-gram
        bow[[maxNgram]]$preceeds <- bow[[maxNgram]]$count
        bow[[maxNgram]]$follows <- bow[[maxNgram]]$count
        
        return(bow)
}

###############################################################################
#
calcProbs <- function(bow) {
        print("Calculating probabilities")
        maxNgram <- length(bow)
        for(i in 1:maxNgram) {
                sums <- apply(bow[[i]],2,sum, na.rm = TRUE)
                probs <- sweep(bow[[i]],2,sums,FUN='/')
                bow[[i]]$countProb <- probs$count
                bow[[i]]$preceedsProb <- probs$preceeds
                bow[[i]]$followsProb <- probs$follows
                #Sort
                bow[[i]] <- bow[[i]][order(bow[[i]]$followsProb,decreasing=TRUE),]
        }
        return(bow)
}

###############################################################################
#
createTheModel <- function () {
        
        gc()
        ptm <- proc.time()
        
        nSamples <- 2500000
        maxNgram <- 5 # min 2
        threshold <- 0.98  #!! SET BACK TO 0.98
        minWordCount <- 8
        keepStopWords <- TRUE
        
        bow <- buildCleanBoW(nSamples, maxNgram, minWordCount, threshold, keepStopWords) %>% buildContCounts() %>% calcProbs()
        
        modelFile <- "nGramModel.RData"
        save(nSamples, maxNgram, threshold, minWordCount, keepStopWords,bow,modelFile,file=modelFile)
        
        print(proc.time() - ptm)
        gc()
}

###############################################################################
#
knesserNeySearch <- function(phrase, bow, keepStopWords) {
        maxNgram <- length(bow)
        
        #plug the phrase into a Corpus to apply the same filters as we did for the training set
        miniCorpus <- VCorpus(VectorSource(phrase)) %>% cleanupCorpus(keepStopWords)
        
        tokenPhrase <- nGramTokenizer(miniCorpus[[1]]$content,1)
        tokenPhrase <- tokenPhrase[1:(length(tokenPhrase)-1)]  # CleanupCorpus adds a period to the end of every line, need to remove
        
        phraseLength <- length(tokenPhrase)
        
        #Remove words not in our "dictionary"
        for(i in 1:phraseLength) {
                found <- bow[[1]][tokenPhrase[i],"count"]
                if(is.na(found)) tokenPhrase[i]<-"%"
        }
        
        # Break up into search strings
        
        searchList <- vector(mode="character",length=phraseLength)
        n <- phraseLength
        searchList[1] <- tokenPhrase[n]
        
        if(phraseLength>1) {
                for(i in 2:phraseLength) {
                        n <- n-1
                        searchList[i] <- paste(tokenPhrase[n],searchList[i-1])
                }
        }
        searchListCaret <- paste0("^",searchList, " ")  #pattern starts with ...
        
        
        searchResults <- vector(mode="list",length=phraseLength)
        names(searchResults) <- searchList
        
        searchLength <- min(phraseLength,maxNgram-1)
        
        for(i in 1:searchLength) {
                if(length(aSearchResult <- which(str_detect(row.names(bow[[i+1]]),searchListCaret[i])))==0) {
                        searchLength<-i-1; 
                        break
                }
                searchResults[[i]] <- bow[[i+1]][aSearchResult,,drop=FALSE]
                # Extract the last word of each result
                row.names(searchResults[[i]]) <-sapply(row.names(searchResults[[i]]), function(x){(str_split(x," ")[[1]][i+1])})
                
                #Remove "%" from the results
                removeIndex <- which(row.names(searchResults[[i]])=="%")
                if(length(removeIndex)>0) searchResults[[i]] <- searchResults[[i]][-removeIndex,]
                if(nrow(searchResults[[i]])==0) {
                        searchResults[[i]] <- list(NULL)
                        searchLength<-i-1; 
                        break
                }
                
        }
        
        if( searchLength > 0 ) {
                d <- 0.75  #Backoff factor
                # Calculate Kneser-Ney probabilities
                
                for(i in 1:searchLength) {
                        #First term
                        numerator <- pmax((searchResults[[i]]$count - d),0)
                        denominator <- bow[[i]][searchList[i],]$count
                        searchResults[[i]]$term1 <- numerator/denominator
                        
                        #lambda
                        lambda <- d/denominator*bow[[i]][searchList[i],]$preceeds
                        
                        if(i == 1) {
                                recurTerm <- bow[[1]][row.names(searchResults[[i]]),]$followsProb
                        } else {
                                recurTerm <- searchResults[[i-1]][row.names(searchResults[[i]]),]$prob        
                        }
                        
                        #Second term
                        searchResults[[i]]$term2 <- lambda*recurTerm
                        searchResults[[i]]$prob <- rowSums(cbind(searchResults[[i]]$term1,searchResults[[i]]$term2),na.rm=TRUE)
                        searchResults[[i]]$prob <- searchResults[[i]]$prob/sum(searchResults[[i]]$prob)
                        
                        #Sort
                        searchResults[[i]] <- searchResults[[i]][order(searchResults[[i]]$prob,decreasing=TRUE),]
                }
        } else {
                #print("Found nothing!")
                searchResults[[1]] <- bow[[1]][1,,drop=FALSE]
                searchLength <- 1
        }
        
        returnSR <- searchResults[1:searchLength]
        return(returnSR)
}

###############################################################################
#
runTheModel <- function (phrase) {
        #gc()
        ptm <- proc.time()
        
        #phrase <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
        numSuggestions <- 3
        discountFactor<-0.4
        
        searchResults <- knesserNeySearch(phrase, bow, keepStopWords)
        
        #Merge into a single list
        
        searchLength <- length(searchResults)
        if(searchLength == 1) {
                finalResults <- searchResults[[1]]  
        }
        else {
                discount <- vector(mode="numeric",length=searchLength)
                discount[searchLength]<-1
                for(i in (searchLength-1):1) {
                        discount[i] <- discount[i+1]*discountFactor
                }
                
                finalResults <- discount[1]*searchResults[[1]]
                for(i in 2:searchLength) {
                        finalResults[rownames(searchResults[[i]]),"prob"] <- finalResults[rownames(searchResults[[i]]),"prob"] + discount[i]*searchResults[[i]]$prob
                }
                finalResults <- finalResults[order(finalResults$prob,decreasing=TRUE),]
        }
        
        #print(proc.time() - ptm)       
        returnWords <- rownames(finalResults[1:min(5,nrow(finalResults)),])
        #gc()
        return(returnWords)
}
