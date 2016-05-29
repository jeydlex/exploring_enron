library(RTextTools)
library(e1071)

enron.emails <- read.csv(file="data/enron_emails_top_users.csv", head=TRUE, sep=",")
data.sent <- read.csv(file="data/sentiment/enron_sent_may.csv", head=FALSE, sep=",")

#creating a term matrix from all the data
matrix= create_matrix(data.sent[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE, tm::weightTfIdf) 

#CREATE A container that is split in training and testing set - if I erase "as.numeric" then 
#label names appear, but it will produce a mistake for testing results... easy to fix :) 
#container = create_container(matrix, as.numeric((data.sent[, 2])), trainSize = 1:640, testSize=641:800, virgin = FALSE)  #removeSparseTerms
container = create_container(matrix, (data.sent[, 2]), trainSize = 1:640, testSize=641:800, virgin = FALSE)  #removeSparseTerms

#Training and classifying
svm.model <- train_model(container,"SVM")
svm.results <- classify_model(container,svm.model) #classifying to see accuracy

#ACCURACY
recall_accuracy(as.numeric(as.factor(data.sent[641:800, 2])), svm.results[, "SVM_LABEL"]) # 0.7672956
table(as.factor(data.sent[641:800,2]), svm.results[, "SVM_LABEL"])
#Very interesting about results http://stats.stackexchange.com/questions/62621/recall-and-precision-in-classification

# VIEW THE RESULTS BY CREATING ANALYTICS
analytics <- create_analytics(container, svm.results)
head(analytics@algorithm_summary)
head(analytics@label_summary)
head(analytics@document_summary)
head(analytics@ensemble_summary)

#TESTING THE CLASSIFIER ON THE INITIAL 100 TUPLES IN THE TRAINING SET, WITHOUT LABELS
data.test <- read.csv(file="data/sentiment/test.csv", head=TRUE, sep=",")
new_matrix <- create_matrix(data.test[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE, tm::weightTfIdf, originalMatrix=matrix) 

predSize <-length(data.test$text);
predictionContainer <- create_container(new_matrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

    #Classification results
    results <- classify_model(predictionContainer, svm.model) 

#TESTING THE CLASSIFIER ON TOTALLY NEW DATA - FROM ROW 800 TO 900
enron.test <- as.data.frame(enron.emails$processed_text[801:10733])
colnames(enron.test) <- c('text')

#measuring time
start.time <- Sys.time()
#beginning classfication
    enron.test.matrix <- create_matrix(enron.test[,1], language="english", 
                                removeStopwords=FALSE, removeNumbers=TRUE, 
                                stemWords=FALSE, tm::weightTfIdf, originalMatrix=matrix)
    enron.test.size <- length(enron.test$text)
    predict.enron.test <- create_container(enron.test.matrix, labels=rep(0,enron.test.size), testSize=1:enron.test.size, virgin=FALSE)
    results.enron.test <- classify_model(predict.enron.test, svm.model) 

#end classifcation
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken #2.780741 mins for almost 10 thousand elements

#exporting sentiment
write.table(results.enron.test, file="data/sentiment/sentiment_prediction_svm.csv", row.names=FALSE, sep=",")

sentiment <- as.data.frame(c(as.factor(data.sent$V2),results.enron.test$SVM_LABEL))
colnames(sentiment) <- c('sentiment')
write.table(sentiment, file="data/sentiment/sentiment_prediction_10733_emails.csv", row.names=FALSE, sep=",")

#STATS: Checking the sentiment distribution in the end

