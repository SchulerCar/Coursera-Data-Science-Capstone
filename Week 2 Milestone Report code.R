rm(list=ls())
if(length(dev.list()!=0)) { dev.off() }
library(tidyverse)
library(tm)
library(stringr)
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
                
                # Lines, words, bytes
                print("Lines, words, bytes:")
                system(paste("wc",twitterFile))
                system(paste("wc",blogsFile))
                system(paste("wc",newsFile))
                
                # Read all the data in
                
                twitterText <- readTextFile(twitterFile,-1)
                blogsText <- readTextFile(blogsFile,-1)
                newsText <- readTextFile(newsFile,-1)
                
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
                
                # Combine them
                allText <- c(twitterText, blogsText, newsText)
                rm(twitterText, blogsText, newsText)
                print("Length of all text (lines):")
                print(length(allText))
                
                # Sample
                fullSampleNumber <- round(nSamples*1.2,0) # add 20% for the test set
                workingText <- sample(allText, fullSampleNumber, replace=FALSE) 
                rm(allText)
                # Write it
                writeTextFile(workingText[1:nSamples],fullSampleFile) # this is the training set
                writeTextFile(workingText[(nSamples+1):fullSampleNumber],testSampleFile) # this is the test set
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
        x <- str_replace_all(x, fixed(" it's "), " it is ")
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
        x <- str_replace_all(x, "[^\x01-\x7F]", " ")  # Replace all non-ASCII characters
        
        return(x)
}

#########
# Identify end of sentence periods with the character sntcfs
# Adapted from https://regex101.com/r/lS5tT3/15
identifyFullStops = function(x) {
        x <- str_replace_all(x, "[!?.]+(?=$|\\s)", " sntcfs ")
}

#########
# Replacee end of sentence tokens "sntcfs" with shorter token ">"
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
cleanupCorpus <- function(myCorpus) {
        print("Cleaning ...")
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
                                      "rt","ya")) %>%      #remove some common abbreviations an random letters
                tm_map(content_transformer(identifyFullStops)) %>%
                tm_map(cleanToSpace, "(ftp|http|https|www)[^[:space:]]+") %>%  #remove URL's
                tm_map(cleanToSpace, "(ftp|http|https|www)[^[:space:]]+") %>%  #remove emails
                tm_map(cleanToSpace, "(#[[:alnum:]_]*)") %>%                   #remove Twitter hashtags
                tm_map(cleanToSpace, "(@[[:alnum:]_]*)") %>%                   #remove Twitter @user
                tm_map(content_transformer(removeNumbers)) %>% 
                tm_map(content_transformer(removePunctuation)) %>% 
                tm_map(content_transformer(nicerFullStops)) %>%
                tm_map(content_transformer(stripWhitespace)) #%>% 
                #tm_map(removeWords, stopwords("english"))
        return(myCorpus)
}

#####
# Create n-grams from text
nGramTokenizer <- function(x,n)
        unlist(lapply(ngrams(words(x), n), paste, collapse = " "), use.names = FALSE)

#########
# Build the Document Term Matrices from unigrams to ngrams
# Returns a list of dtm's
buildDTM <-function(myCorpus,maxNgram, minWordCount=2, minCharWordLength=1) {
        print("Building Document Term Matrices")
        dtm <- vector(mode = "list", length = maxNgram)
        for(iGram in 1:maxNgram) {
                print(paste("iGram=",iGram))
                dtm [[iGram]] <- myCorpus %>% 
                        DocumentTermMatrix(
                                control=list(
                                        tokenize=function(x) nGramTokenizer(x,iGram),
                                        bounds=list(local=c(minWordCount,Inf)),
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
                        names(bagOfWords[[iGram]]) <- c("<count>", "cumCount")
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
                bagOfWords[[iGram]] <- bagOfWords[[iGram]][order(bagOfWords[[iGram]][,"<count>"],decreasing = TRUE),]
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
                count <- bagOfWords[[iGram]][,"<count>"]
                dfplot <- data.frame(ngram= factor(head(ngrams, numBins),
                                                  levels=head(row.names(bagOfWords[[iGram]]), numBins)),
                                     count= head(count, numBins))
                fig <- ggplot(dfplot, aes(x=ngram, y=count)) + 
                        geom_bar(stat="identity") + 
                        xlab(paste0(iGram,"-Grams")) + 
                        ylab("<count>") +
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


#########
populateModel <- function(bagOfWords, threshold) {
        print(paste("Populating model"))
        maxNgram <- length(bagOfWords)
        
        #Create the continuation count list for Kneser-Ney
        contCount <- vector(mode="list", length=maxNgram-1)
        
        #Populate "follow" back to front
        for(iGram in maxNgram:2) { 
                if (nrow(bagOfWords[[iGram]]) > 0) {
                        #Split the ngrams into (n-1)grams (pregram) and a word
                        words <- str_split(row.names(bagOfWords[[iGram]]),fixed(" "), simplify=TRUE)
                        
                        if(iGram>2) {
                                if(nrow(words)>1) {
                                        preGram <- apply(words[,1:(iGram-1)],1,paste,collapse=" ")
                                        postGram <- apply(words[,2:(iGram)],1,paste,collapse=" ")
                                } else {

                                        preGram <- paste(words[1,1:(iGram-1)],collapse=" ")
                                        postGram <- paste(words[1,2:(iGram)],collapse=" ")
                                }
                        }
                        else {
                                preGram <- words[,1]
                                postGram <- words[,iGram]
                        }
                        
                        #Populate counts in the (n-1)gram dataframe
                        numGrams <- length(preGram)
                        print(paste("Populating iGram=",iGram-1,"with numGrams=",numGrams))
                        if(numGrams>1) { 
                                for(i in 1:numGrams) {
                                        bagOfWords[[iGram-1]][preGram[i],words[i,iGram]] <- bagOfWords[[iGram]][i,"<count>"]
                                } 
                        } else {
                                # Create the new column manually
                                newColumn <- vector(mode="numeric",length=nrow(bagOfWords[[iGram-1]]))
                                whichRow <- which(row.names(bagOfWords[[iGram-1]])==preGram)
                                newColumn[whichRow] <- bagOfWords[[iGram]][1,"<count>"]
                                bagOfWords[[iGram-1]] <- cbind(bagOfWords[[iGram-1]],newColumn)
                                names( bagOfWords[[iGram-1]]) <- c("<count>","cumCount",words[1,iGram])
                        }
                        
                        #Create and populate contCount dataframe
                        ccRowNames <- unique(words[,1])
                        ccColNames <- unique(postGram)
                        contCount[[iGram-1]] <- data.frame(matrix(NA,nrow=length(ccRowNames),ncol=length(ccColNames)))
                        row.names(contCount[[iGram-1]]) <- ccRowNames
                        colnames(contCount[[iGram-1]]) <- ccColNames
                        #Populate counts in the (n-1)gram dataframe
                        for(i in 1:numGrams) {
                                contCount[[iGram-1]][words[i,1],postGram[i]] <- 1
                        }
                        #Collapse the contCount dataframe to just the column sums
                        contCount[[iGram-1]] <- colSums(contCount[[iGram-1]],na.rm = TRUE)
                } else {
                        print ("No high order n-grams, reducing model order")
                        bagOfWords[[iGram]] <- NULL
                        maxNgram <- maxNgram - 1
                }
        }
        
        
        #Now we can drop the numGrams iGram
        bagOfWords[[maxNgram]] <- NULL
        maxNgram <- maxNgram-1
        
        # Create new unigram to prepend later on ...
        df<- data.frame(NULL)
        uniGram <- rbind(df,bagOfWords[[1]][,1])
        colnames(uniGram) <- row.names(bagOfWords[[1]])
        
        #Remove <count> and cumCount from all
        for(iGram in 1:maxNgram) {
                bagOfWords[[iGram]] <- bagOfWords[[iGram]][,-c(1,2),drop=FALSE]

        }
        
        #Prepend the new unigram
        newBagOfWords <- vector(mode = "list", length = maxNgram+1)
        newBagOfWords[[1]] <- uniGram
        for(iGram in 1:maxNgram) {
                newBagOfWords[[iGram+1]] <- bagOfWords[[iGram]]
        }
        
        rm(bagOfWords) # Free space
        maxNgram <- length(newBagOfWords)
        
        #Remove empty lines
        print("Removing empty lines")
        for(iGram in 1:maxNgram) {
                print(paste("iGram=", iGram))
                #Remove empty lines
                if(nrow(newBagOfWords[[iGram]])>0) {
                        indices <- which(rowSums(is.na(newBagOfWords[[iGram]])) == ncol(newBagOfWords[[iGram]]))
                        rowsDeleted <- length(indices)
                        print(paste("Removing", rowsDeleted, "empty rows"))
                        if(rowsDeleted>0) newBagOfWords[[iGram]] <- newBagOfWords[[iGram]][-indices,]
                }
        }
                               

#        bagOfWords <- c(uniGram,bagOfWords)  THIS DOESN'T WORK!!
        
        returnList <- vector(mode="list", length=2)
        returnList[[1]] <- newBagOfWords
        returnList[[2]] <- contCount
        
        return(returnList)
}

###############################################################################
#
buildModel <- function (nSamples, maxNgram, minWordCount, threshold) {
        # Create Bag of Words from Corpus via Document Term Matrix
        myCorpus <- createCorpus(nSamples, forceNew=FALSE) %>% 
                cleanupCorpus()
        
        # Limit the dictionary
        initBagOfWords <- myCorpus %>%       
                buildDTM(maxNgram=1, minWordCount=2, minCharWordLength=1) %>%   #!! reset minWordCount to 2
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
        
        populate<- populateModel(bagOfWords, threshold)
        
        bagOfWords <- populate[[1]]
        contCount <- populate [[2]]
        
        maxNgram <- length(bagOfWords)
        
        #viewBagOfWords(bagOfWords)
        #viewBagOfWords(contCount, name="CC")
        
        # Kneser-Nay normalization, implemented following
        # https://medium.com/@dennyc/a-simple-numerical-example-for-kneser-ney-smoothing-nlp-4600addf38b8
        # http://www.foldl.me/2014/kneser-ney-smoothing/
        # https://www.coursera.org/lecture/language-processing/count-n-gram-language-models-IdJFl
        # https://nlp.stanford.edu/~wcmac/papers/20050421-smoothing-tutorial.pdf
        # http://www2.denizyuret.com/ref/goodman/chen-goodman-99.pdf
        # https://web.stanford.edu/~jurafsky/slp3/
        
        # Version implemented: http://www2.denizyuret.com/ref/goodman/chen-goodman-99.pdf
        
        print("Kneser-Ney")
        model <- bagOfWords # to get the structure
        # Let the count assigned to each unigram be the number of different words that it follows
        model[[1]][1,] <- NA # erase the counts
        model[[1]][1,] <- contCount[[1]][names(model[[1]])]/sum(contCount[[1]])
        
        for(iGram in 2:maxNgram) {
                print(iGram)
                rowCountBoW <- rowSums(bagOfWords[[iGram]],na.rm=TRUE)
                #columnCountBoW <- colSums(bagOfWords[[iGram]],na.rm=TRUE)
                bagOfWordsTF <- bagOfWords[[iGram]]>0
                #numEntriesBoW <- sum(bagOfWordsTF, na.rm=TRUE)
                rowTFCountBoW <- rowSums(bagOfWordsTF,na.rm=TRUE)
                #columnTFCountBoW <- colSums(bagOfWordsTF,na.rm=TRUE)
                
                if(iGram < maxNgram) d <- 0.75 # Change back to 0.75
                else d <- 0
                
                print("Calculating first term")

                #calcOption - 1. uses the counts
                #           - 2. uses the continuation counts
                
                calcOption <- 1
                
                if(calcOption ==1) {
                        # This option uses the counts
                        if(ncol(model[[iGram]])>1) {
                                numRows <- nrow(model[[iGram]])
                                for(i in 1:numRows) {
                                        print(paste("iGram:", iGram,"Row:",i,"of",numRows))
                                        model[[iGram]][i,] <- pmax((bagOfWords[[iGram]][i,] - d),0)/rowCountBoW[i]
                                }
                        } else {
                                model[[iGram]] <- bagOfWords[[iGram]]
                                model[[iGram]][model[[iGram]]>0]<-1
                        }
                        
                } else if(calcOption == 2) {
                        #This option uses the continuation counts
                        if(iGram < maxNgram) {
                                preGram <- row.names(model[[iGram]])
                                word <- names(model[[iGram]])
                                
                                numRows <- length(preGram)
                                for(i in 1:numRows) {
                                        print(paste("iGram:", iGram,"Row:",i,"of",numRows))
                                        model[[iGram]][preGram[i],word] <- contCount[[iGram]][paste(preGram[i],word)]/contCount[[iGram-1]][preGram[i]]    
                                }
                        } else {
                                numRows <- nrow(model[[iGram]])
                                for(i in 1:numRows) {
                                        print(paste("iGram:", iGram,"Row:",i,"of",numRows))
                                        model[[iGram]][i,] <- bagOfWords[[iGram]][i,]/rowCountBoW[i]
                                }
                                
                        }
                        
                        #Eliminate blank lines
                        print("Removing empty lines")
                        print(paste("iGram=", iGram))
                        #Remove empty lines
                        if(nrow(model[[iGram]])>0) {
                                indices <- which(rowSums(is.na(model[[iGram]])) == ncol(model[[iGram]]))
                                rowsDeleted <- length(indices)
                                print(paste("Removing", rowsDeleted, "empty rows"))
                                if(rowsDeleted>0) model[[iGram]] <- model[[iGram]][-indices,]
                        }
                        
                } else stop("unsupported calcOption")
                

                
                print("Calculating adjustments")
                if(iGram < maxNgram) {
                        numRowsBW <- nrow(model[[iGram]])
                        for(i in 1:numRowsBW ) {
                                print(paste("iGram:", iGram,"Row:",i,"of",numRowsBW))
                                
                                preGram <- row.names(model[[iGram]])[i]
                                
                                if(iGram > 2) shorterPreGram <- paste(nGramTokenizer(preGram,1)[2:(iGram-1)],collapse=" ")
                                else shorterPreGram <- 1
                                
                                words <- names(model[[iGram]])
                                lambda <- d/rowCountBoW[i]*rowTFCountBoW[i]*model[[iGram-1]][shorterPreGram,words]
                                model[[iGram]][i,] <- colSums(rbind(model[[iGram]][i,], lambda),na.rm=TRUE)
                                sumModel <- sum(model[[iGram]][i,],na.rm=TRUE)
                                model[[iGram]][i,] <- model[[iGram]][i,]/sumModel 
                        }
                }

                # clear "bagOfWords" to free memory
                bagOfWords[[iGram]] <- "Released"
                gc()  # Garbage collector
        }
        
        return(model)
}

###############################################################################
#
fetchTopWords <- function(found) {
        foundVector <- as.vector(t(found))                           #coerce to a vector
        names(foundVector) <- names(found)                           #copy the names
        foundVector <- sort(foundVector,decreasing=TRUE)             #sort in descending order
        foundVector <- foundVector[!is.na(foundVector)]              #keep only the hits
        return(foundVector)
}

#########
getNext<- function(tokenPhrase, nGram) {
        searchFor <- paste(tokenPhrase,collapse=" ")
        found <- nGram[searchFor,]
        rowsFound <- nrow(found)
        if(rowsFound > 1) stop("Found more than one match in getNext")
        if(rowsFound > 0) foundVector <- fetchTopWords(found)        #extract the top words and cond. prob
        return(foundVector)
}

suggestWords<- function(phrase, model, numSuggestions) {
        maxNgram <- length(model)
        
        #plug the phrase into a Corpus to apply the same filters as we did for the training set
        miniCorpus <- VCorpus(VectorSource(phrase)) %>% cleanupCorpus()
        
        tokenPhrase <- nGramTokenizer(miniCorpus[[1]]$content,1)
        
        phraseLength <- length(tokenPhrase)
        
        #Remove words not in our "dictionary"
        for(n in 1:phraseLength) {
                found <- model[[1]][1,tokenPhrase[n]]
                if(is.null(found)) tokenPhrase[n]<-"%"
        }
        
        print("Looking for:")
        print(tokenPhrase)
        
        concatVectors <- as.vector(NULL)
        discount <-0.01
        discountFactor <- vector(mode="numeric",length=maxNgram)
        discountFactor[maxNgram] <- 1 # for cheap Katz backoff
        for(iGram in (maxNgram-1):1) discountFactor[iGram]<-discount*discountFactor[iGram+1]
        discountFactor <- discountFactor/sum(discountFactor)
        
        maxWords <- min(phraseLength, maxNgram-1)
        if(maxWords>0) for(n in (maxWords-1):0) {
                foundVector <- getNext(tokenPhrase[(phraseLength-n):phraseLength],model[[n+2]])
                print("Token Phrase")
                print(paste(n+2,":",paste(tokenPhrase[(phraseLength-n):phraseLength],collapse=" ")))
                foundVector <- discountFactor[n+2]*foundVector
                print("Found Vector")
                print(foundVector[1:min(5,length(foundVector))])
                concatVectors <- c(concatVectors,foundVector)
        }
        
        #Collect same and add statistics
        uniqueWords <- unique(names(concatVectors))
        numberUniqueWords <- length(uniqueWords)
        
        if(numberUniqueWords>0) {
                collectedWords <- (sapply(uniqueWords, function (x) concatVectors[names(concatVectors)==x]))
                
                finalList <- vector(mode = "numeric", length = numberUniqueWords)
                
                for(n in 1:numberUniqueWords) {
                        finalList[n] <- sum(collectedWords[[n]])  #Implementing interpolation smoothing 2-gram through nGram
                }
                names(finalList) <- uniqueWords
                print("Sums:")
                print(finalList[1:min(5,numberUniqueWords)])
                
                # Remove full stop and unk
                finalList <- sort(finalList[names(finalList)!=">" & names(finalList)!="%"],decreasing=TRUE)
                
                print("Suggestions:")
                print(paste(phrase, "..."))
                print(names(finalList)[1:10])
        } else {
                print(names(model[[1]])[1:10])
        }
}

nSamples <- 30000
maxNgram <- 3 # min 2
threshold <- 0.98  #!! SET BACK TO 0.98
minWordCount <- 2   #!! SET BACK TO 2

model <- buildModel(nSamples, maxNgram, minWordCount, threshold)
viewBagOfWords(model,name="Model")

modelFile <- "nGramModel.RData"
save(model,file=modelFile)




phrase <- "A paragraph"
numSuggestions <- 3
suggestions <- suggestWords(phrase, model, numSuggestions)

