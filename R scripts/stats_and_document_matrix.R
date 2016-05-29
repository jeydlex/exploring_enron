library(dplyr) 
library(tm) 
library(SnowballC) 
library(ggplot2)

data <- read.csv(file="data/clean_emails.csv", head=TRUE, sep=",")

#Trying with 10 emails only
docs <-select(data, processed_text) #I had to filter it again because there is some trash in the data
emails<-Corpus(VectorSource(docs$processed_text))

#Building bag of words from the corpus
DTM <- DocumentTermMatrix(emails) #This builds the document terms matrix
DTM

sparse_DTM <- removeSparseTerms(DTM, 0.97) #if I set it to 0.90 then nothing changes, under that it does

inspect(sparse_DTM) 

sparse_DTM #93% sparsity

#EXPLORING DATA BY FREQUENCY OF TERMS
freq <- sort(colSums(as.matrix(sparse_DTM)), decreasing=TRUE)   
head(freq, 14) 
tail(freq, 14) 
wf <- data.frame(word=names(freq), freq=freq) #A data frame with the frequency of each term

#Plotting the term frequency using ggplot
p <- ggplot(subset(wf, freq>18000), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p

write.table(DTM, file="data/DTM.csv", row.names=FALSE, sep=",")