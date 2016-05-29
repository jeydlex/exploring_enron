library(RTextTools)
library(e1071)

data.sent <- read.csv(file="data/sentiment/enron_sent_may.csv", head=FALSE, sep=",")

matrix= create_matrix(data.sent[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE, tm::weightTfIdf) 

#NAIVE BAYES CLASSIFIER
mat = as.matrix(matrix)
classifier1 = naiveBayes(mat[1:640,], as.factor(data.sent[1:640,2]) )

predicted = predict(classifier, mat[641:799,]); 
predicted
table(data.sent[641:799, 2], predicted)
recall_accuracy(data.sent[641:800, 2], predicted)

# the other methods
container = create_container(matrix, as.numeric(as.factor(data.sent[, 2])), trainSize = 1:640, 
                             testSize = 641:800, virgin = FALSE)  #removeSparseTerms

models = train_models(container, algorithms = c("MAXENT", "SVM"))


#SUPPORT VECTOR MACHINES
results = classify_models(container, models)
recall_accuracy(as.numeric(as.factor(data.sent[641:800, 2])), results[, "SVM_LABEL"])
    # accuracy: 0.7672956
    table(as.factor(data.sent[641:800,2]), results[, "SVM_LABEL"])

#the specific classifier for SVM - checked help to find example
svm.model <- train_model(container,"SVM")
svm.results <- classify_model(container,svm.model)
precision_recall_f1 <- create_precisionRecallSummary(container, results)
    
#trying the classifier on new data
new_strings <- c("attached the report", "one final note   deeply sorry live uncertain troubled period company  saddened")
new_matrix <- create_matrix(new_strings, language="english",
                            removeNumbers=TRUE, stemWords=TRUE, weighting=tm::weightTfIdf, originalMatrix = NULL)



results <- classify_models(new_corpus, model1) 
    
#MAXENT 
recall_accuracy(as.numeric(as.factor(data.sent[641:799, 2])), results[, "MAXENTROPY_LABEL"])
    #0.6477987
    table(as.factor(data.sent[641:799,2]), results[, "MAXENTROPY_LABEL"])

# model summary
analytics = create_analytics(container, results)
summary(analytics)
