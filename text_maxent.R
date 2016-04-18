library(xlsx)
library(tm)
library(RTextTools)
library(e1071)
library(caret)
library(ROCR)


cdr<- getwd()
setwd("C:/Users/Kr_Suraj_Baranwal/Desktop/Academics/DACS/DA/Project/Project Data")

#Loading and Preprocessing the data

text<- read.xlsx("Data2013.xlsx", 1)#reading the text file
text<-text[,1:2]
text$Incident.Category. = as.character(text$Incident.Category.)
text$Incident.Category.[text$Incident.Category.=="LTI"|text$Incident.Category.=="Other Injury"|text$Incident.Category.=="Medical Case"]<- "Injury"
text$Incident.Category. = as.factor(text$Incident.Category.)
text$Brief.Description.of.Incident = as.character(text$Brief.Description.of.Incident)
text<-na.omit(text)

#Random Shuffling of the text data
set.seed(1)
index<- sample(2, dim(text)[1], replace = T, prob = c(0.8,0.2))
text = rbind(text[index==1,], text[index==2,])
#Creating container with the text data
input <- text$ Brief.Description.of.Incident
class <- as.factor(text$Incident.Category.)
text_mat<-create_matrix(input, language = "english",removeNumbers = TRUE, 
                        removePunctuation = T, removeStopwords =T, 
                        removeSparseTerms=0.998, stemWords = T, weighting = weightTfIdf)
dim(text_mat)


train_data<- create_container(text_mat, as.numeric(class), trainSize = 1:650,
                              testSize = 651:904, virgin = F)
#training with MAXENT
set.seed(25)
model<-train_model(train_data, "MAXENT")
result<- classify_model(train_data, model)

cross_val <- cross_validate(train_data, 10, "MAXENT" )
cross_val
performance = create_analytics(train_data, result)
summary(performance)

#one vs all classification
class <- text$Incident.Category.[651:904]
x<-NULL
y<-NULL
y[as.integer(class)== 1] <- 0
y[as.integer(class)== 2] <- 1
z<-result[1]
x[z == 1] <- 0
x[z == 2] <- 1

#Confusion Matrix
cf_mat <- confusionMatrix(x, y)

#ROC Curve
pred<- prediction(x,y)
perf<-performance(pred, "tpr","fpr")
plot(perf, lty = 1, lwd = 2, col = 'blue')

#Calculating AUC
auc <- performance(pred,"auc")
auc <- unlist(slot(auc, "y.values"))
print("AUC is:")
print(auc)
setwd(cdr)