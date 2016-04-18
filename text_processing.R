library(xlsx)
library(tm)
library(RTextTools)
library(e1071)
library(caret)
library(ROCR)


cdr<- getwd()
setwd("C:/Users/Kr_Suraj_Baranwal/Desktop/Academics/DACS/DA/Project/Text processing")

#Rndom Shuffling of the text data

text<- read.xlsx("text_data.xlsx", 1) #reading the text file
index<- sample(2, dim(text)[1], replace = T, prob = c(0.8,0.2))
text = rbind(text[index==1,], text[index==2,])

#Creating container with the text data
input <- text$ Brief.Description.of.Incident
class <- as.factor(text$Primary.Cause)
text_mat<-create_matrix(input, language = "english",removeNumbers = TRUE, 
                        removePunctuation = T, removeStopwords =T, 
                        removeSparseTerms=0.998, stemWords = T)
dim(text_mat)
set.seed(33)

train_data<- create_container(text_mat, as.numeric(class), trainSize = 1:200,
                              testSize = 201:300, virgin = F)
#training with SVM
model<-train_model(train_data, "SVM")
result<- classify_model(train_data, model)

cross_val <- cross_validate(train_data, 10, "SVM" )
cross_val
performance = create_analytics(train_data, result)
summary(performance)
z <- result[1]
#one vs all classification
class <- text$Primary.Cause[201:300]


for (j in 1:length(levels(class)))
  { 
        print("class")
        print(j)
        if(sum(z==j)!=0 & sum(as.integer(class)==j)!=0){
              x <-NULL
              y <-NULL
              y[as.integer(class) != j] <- 0
              y[as.integer(class) == j] <- 1
              x[z == j] <- 1
              x[z != j] <- 0
            
          #Confusion Matrix
              cf_mat <- confusionMatrix(y, x)
              
          #ROC Curve
              pred<- prediction(x,y)
              perf<-performance(pred, "tpr","fpr")
              plot(perf, lty = 1, lwd = 2, col = 'blue')
          
          #Calculating AUC
              auc <- performance(pred,"auc")
              auc <- unlist(slot(auc, "y.values"))
              print("AUC:")
              print(auc)
              print("Confusion Matrix")
              print(cf_mat)
              
  }
  else print("No variables with this class found, repeating for the next class")
        setwd(cdr)
}
