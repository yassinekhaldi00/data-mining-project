library(tm)
library(SnowballC)
library(NLP)
library(twitteR)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)


load("C:/Users/yassine/Documents/Data Mining TP/rdmTweets.RData")

# Transforming Text

tweetsFrame <- do.call("rbind", lapply(tweets, as.data.frame))

tweetsCorpus <- VCorpus(VectorSource(tweetsFrame$text))
tweetsCorpus <- tm_map(tweetsCorpus, content_transformer(tolower))

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
tweetsCorpus <- tm_map(tweetsCorpus, content_transformer(removeNumPunct))

tweetsCorpus <- tm_map(tweetsCorpus, removePunctuation)

tweetsCorpus <- tm_map(tweetsCorpus, removeNumbers)

myStopwords <- c(stopwords('english'), "available", "via")
myStopwords <- setdiff(myStopwords, c("r", "big"))
tweetsCorpus <- tm_map(tweetsCorpus, removeWords, myStopwords)

tweetsCorpus <- tm_map(tweetsCorpus, stripWhitespace)

#Stemming Words

tweetsCorpusCopy <- tweetsCorpus

tweetsCorpus <- tm_map(tweetsCorpus, stemDocument)

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
 # a word in dictionary. Remove empty string to avoid above issue.
    x <- x[x != ""]
    x <- stemCompletion(x, dictionary=dictionary)
    x <- paste(x, sep="", collapse=" ")
    PlainTextDocument(stripWhitespace(x))
    }
tweetsCorpus <- lapply(tweetsCorpus, stemCompletion2, dictionary=tweetsCorpusCopy)
tweetsCorpus <- Corpus(VectorSource(tweetsCorpus))

inspect(tweetsCorpus[11:15])

#Building a Term-Document Matrix
tdm <- TermDocumentMatrix(tweetsCorpus, control=list(wordLengths=c(1,Inf)))

idx <- which(dimnames(tdm)$Terms == "r")
inspect(tdm[idx+(0:5),101:110])

#Frequent Terms and Associations
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=10)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +xlab("Terms") + ylab("Count") + coord_flip()
#barplot(termFrequency, las=2)
# which words are associated with "mining"?
print(findAssocs(tdm, "airport", 0.25))

#Word Cloud
m <- as.matrix(tdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]

# word cloud
set.seed(375) # to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,colors=pal)

#Clustering Words
tdm2 <- removeSparseTerms(tdm, sparse=0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
rect.hclust(fit, k=10)
groups <- cutree(fit, k=10)

