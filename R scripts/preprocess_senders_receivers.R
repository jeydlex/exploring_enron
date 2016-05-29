#So here we begin again
library(dplyr) 

data <- read.csv(file="data/users_pairs.csv",head=TRUE,sep=",")

#Because of the massive messages from one person to another, I'd like to see only the people who send the messages
filter.From <- filter(data, grepl('@enron', From))
filter.To <- filter(filter.From, grepl('@enron', To))

#Counting the number of messages
summarize(filter.To , from = n_distinct(From), to = n_distinct(To), messages = n_distinct(Cod))
#from    to messages
#3069 10867    23701

#Grouping users by num
senders <- group_by(filter.To, From)
senders.sum <- as.data.frame(summarise(senders, numMsg = n_distinct(Cod)))

#Data that should be cleaned includes announcements and those with regular expressions
trash.data <- filter(senders.sum, grepl("2|40|mbx_iscinfra|ecn3125b|announcement|notifications|no.address|infrastructure", From))
clean <- anti_join(senders.sum,trash.data, by = "From")

summary(senders.sum$numMsg)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.000   1.000   2.000   7.723   7.000 722.000 

#Note: I can set the threshold to be the mean for now, but later I must look for a way to justify
a <-boxplot(clean$numMsg, horizontal = TRUE, col = "light green")
outliers <- as.data.frame(a$out)
summary(outliers) #17 is the lowest outlier

#Since this would be anomaly detection, I would work only with people who sent over 17 messages
# Min.   : 14.00       Max.   :722.00 
most.active <- filter(clean, numMsg>=14)
senders.list <- inner_join(filter.To,most.active, by = "From") #list of most active people with all data

#Finding the outliers that received the most emails
complete.list <- filter(senders.list, To %in% most.active$From)
s <- filter(senders.list, To %in% c("adam.johnson@enron.com")) #just a test for one

summarize(complete.list , from = n_distinct(From), to = n_distinct(To), topics = n_distinct(Cod))
# from  to topics
# 371 361  10787

#WRAPPING UP: Grouping users by num
group.complete <- group_by(complete.list, From)
complete.sum <- as.data.frame(summarise(group.complete, Sent_mail = n_distinct(Cod)))
complete <- inner_join(complete.list,complete.sum, by = "From")
complete <- select(complete, Cod, From, To, Sent_mail) #selecting only the ones I am interested on
boxplot(complete.sum$Sent_mail, horizontal = TRUE, col = "light green")

#exporting data
write.table(complete, file="data/top_users.csv", row.names=FALSE, sep=",")