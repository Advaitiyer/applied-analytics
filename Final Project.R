#load packages and csv file
library(ggplot2)
library(dplyr)
library(gridExtra)
library(corrplot)
library(caret)
library(ggthemes)
library(RColorBrewer)
library(fmsb)
library(rpart.plot)
library(ROCR)
library(rpart)
library(rattle)
library(cluster)
library(factoextra)
library(pROC)
library(klaR)
library(e1071)
library(ElemStatLearn)
library(mlbench)
library(caretEnsemble)
library(arulesViz)


# load("workspace.RData") 
#################################################################################
adult_data<-read.csv("adult.data.csv",sep=",",stringsAsFactors=T)
adult_data <- na.omit(adult_data)
adult_data <- adult_data[!adult_data$native.country =='?',]
sum(!complete.cases(adult_data))
sum(sapply(adult_data, is.na))
sum(sapply(adult_data, is.infinite))
sum(sapply(adult_data, is.nan))
summary(adult_data)
################################################################################

######### #####Unsupervised Learning
##
###############################################################################################################
## Association Rules
RulesData <- data.frame(adult_data$workclass,adult_data$education,adult_data$marital.status,adult_data$occupation,adult_data$relationship,adult_data$race,adult_data$sex,adult_data$native.country,adult_data$salary)
RulesData$age <- cut(adult_data$age, breaks = c(10,20,30,40,50,60,Inf),labels=c("teens","twenties","thirties","forties","fifties","senior"))
RulesData$education.num <- cut(adult_data$education.num, breaks = seq(min(adult_data$education.num), max(adult_data$education.num), (max(adult_data$education.num)-min(adult_data$education.num))/4),labels=c("basic","normal","advanced","preminum"))
RulesData$capital.gain <- cut(adult_data$capital.gain, breaks = c(-Inf,1000,5000,Inf),labels=c("low","normal","high"))
RulesData$capital.loss<- cut(adult_data$capital.loss, breaks = c(-Inf,1000,5000,Inf),labels=c("low","normal","high"))
RulesData$hours.per.week<- cut(adult_data$hours.per.week, breaks = c(-Inf,20,40,80,Inf),labels=c("low","normal","high","super"))
RulesData <- na.omit(RulesData)
RulesTran <- as(RulesData,"transactions")
summary(RulesTran)
itemLabels(RulesTran)
### item frequency
itemFreq <-itemFrequency(RulesTran)
#
plot(itemFreq)
#
itemFrequencyPlot(RulesTran,topN=20)
### Rules 
Rule <- apriori(RulesTran , parameter = list(support =0.03, confidence = 0.9,minlen=2), appearance = list(rhs='adult_data.salary<50k', default='lhs'))
rules <- sort(Rule,decreasing = TRUE, by ='confidence')
# we have 305 rules here
# Termination is the core reason for attrition = 1
inspect(rules[1:10])
summary(rules)

plot(rules[1:10],method = "graph", main="Association Rules for Salary >= 50k", cex=0.8)
###############################################################################################################
adult_data1 <- adult_data
adult_data1$age <- as.numeric(paste(adult_data1$age))
adult_data1$fnlwgt <- as.numeric(paste(adult_data1$fnlwgt))
adult_data1$education.num <- as.numeric(paste(adult_data1$education.num))
adult_data1$capital.gain <- as.numeric(paste(adult_data1$capital.gain))
adult_data1$capital.loss <- as.numeric(paste(adult_data1$capital.loss))
adult_data1$hours.per.week <- as.numeric(paste(adult_data1$hours.per.week))

adult_data1$workclass <- NULL
adult_data1$education <- NULL
adult_data1$marital.status <- NULL
adult_data1$occupation <- NULL
adult_data1$relationship <- NULL
adult_data1$race <- NULL
adult_data1$sex <- NULL
adult_data1$native.country <- NULL
adult_data1$salary <- NULL
#### Correlation matrix
library(MASS)
library(corrplot)
scaledf <- scale(adult_data1, center = T, scale = T)
Z <- as.matrix(scaledf) 
res <- cor(Z, use="pairwise.complete.obs")
corrplot(res)
#### K-means (kmode)
# Two clusters:
par(mfrow=c(2,2))
set.seed(111)
df_km <- kmeans(adult_data1, 2)
df_km
cluster_assignment <- data.frame(adult_data1,df_km$cluster)
cluster_assignment
table(cluster_assignment$df_km.cluster,adult_data$salary)
clusplot(adult_data,df_km$cluster,color = TRUE,shade = TRUE, lines = 0)

set.seed(123)
df_km <- kmeans(adult_data1, 2)
df_km
cluster_assignment <- data.frame(adult_data1,df_km$cluster)
cluster_assignment
table(cluster_assignment$df_km.cluster,adult_data$salary)
clusplot(adult_data,df_km$cluster,color = TRUE,shade = TRUE, lines = 0)

set.seed(1331)
df_km <- kmeans(adult_data1, 2)
df_km
cluster_assignment <- data.frame(adult_data1,df_km$cluster)
cluster_assignment
table(cluster_assignment$df_km.cluster,adult_data$salary)
clusplot(adult_data,df_km$cluster,color = TRUE,shade = TRUE, lines = 0)

set.seed(12345)
df_km <- kmeans(adult_data1, 2)
df_km
cluster_assignment <- data.frame(adult_data1,df_km$cluster)
cluster_assignment
table(cluster_assignment$df_km.cluster,adult_data$salary)
clusplot(adult_data,df_km$cluster,color = TRUE,shade = TRUE, lines = 0)

# Three clusters:
par(mfrow=c(2,2))
set.seed(111)
df_km <- kmeans(adult_data1, 3)
df_km
cluster_assignment <- data.frame(adult_data1,df_km$cluster)
cluster_assignment
table(cluster_assignment$df_km.cluster,adult_data$salary)
clusplot(adult_data,df_km$cluster,color = TRUE,shade = TRUE, lines = 0)

set.seed(123)
df_km <- kmeans(adult_data1, 3)
df_km
cluster_assignment <- data.frame(adult_data1,df_km$cluster)
cluster_assignment
table(cluster_assignment$df_km.cluster,adult_data$salary)
clusplot(adult_data,df_km$cluster,color = TRUE,shade = TRUE, lines = 0)

set.seed(1331)
df_km <- kmeans(adult_data1, 3)
df_km
cluster_assignment <- data.frame(adult_data1,df_km$cluster)
cluster_assignment
table(cluster_assignment$df_km.cluster,adult_data$salary)
clusplot(adult_data,df_km$cluster,color = TRUE,shade = TRUE, lines = 0)

set.seed(12345)
df_km <- kmeans(adult_data1, 3)
df_km
cluster_assignment <- data.frame(adult_data1,df_km$cluster)
cluster_assignment
table(cluster_assignment$df_km.cluster,adult_data$salary)
clusplot(adult_data,df_km$cluster,color = TRUE,shade = TRUE, lines = 0)

###############################################################################################################
### Supervised Learning
###############################################################################################################
# split data
table(RulesData$adult_data.salary)
trainindex <- createDataPartition(RulesData$adult_data.salary, p = 0.6,list =FALSE)
traindata <- RulesData[trainindex,]
testdata <- RulesData[-trainindex,]
table(traindata$adult_data.salary)
table(testdata$adult_data.salary)
###############################################################################################################


## decision tree
dt_model <- train(adult_data.salary ~.,data = traindata, metric = 'Accuracy',method = 'rpart')
print(dt_model)
print(dt_model$finalModel)
par(mfrow=c(1,1))
rpart.plot(dt_model$finalModel)
dt_predict <- predict(dt_model, newdata = testdata, na.acction = na.omit, type = 'prob')
dt_predict
dt_predict <- predict(dt_model, newdata = testdata, na.acction = na.omit, type = 'raw')
dt_predict
# CM
df <-as.data.frame(cbind(testdata$adult_data.salary,dt_predict))
table(df)
confusionMatrix(dt_predict, testdata$adult_data.salary)
###############################################################################################################


### Naive Bayes
#model_nb1 <- train(adult_data.salary ~ ., data = testdata, method = "nb",laplace = 1)
library(e1071)
library(caret)
model_nb1 <- naiveBayes(adult_data.salary ~ ., data = traindata)
model_nb1$tables
predict_nb1 <- predict(model_nb1, newdata=testdata, type=c("raw","class"))
###############################################################################################################



### KNN
model_knn <- train(adult_data.salary ~ ., data = traindata, method = "knn")
model_knn
predict_knn <- predict(model_knn, newdata = testdata)
## Plot
plot(model_knn)
## Hold Out Method
confusionMatrix(predict_knn, testdata$adult_data.salary)
## Bootstrap
print(model_knn)
###############################################################################################################



### Random Forest
model_rf <- train(adult_data.salary ~ ., data = traindata, method = "rf")
model_rf
plot(model_rf)
## Predict
predict_rf <- predict(model_rf, newdata = testdata)
## Hold Out Method
confusionMatrix(predict_rf, testdata$adult_data.salary)
## Bootstrap
print(model_rf)
## Important Variable
varimp_rf <- varImp(model_rf)
varimp_rf
plot(varimp_rf, main = "Variable Importance with Random Forest")
###############################################################################################################



# SVM
svm <- train(adult_data.salary ~ ., data = traindata,scale = FALSE ,method = "svmLinear")
svm
#plot(svm)  
## Predict
predict_svm <- predict(svm, newdata = testdata)
### Hold Out Method
confusionMatrix(predict_svm, testdata$adult_data.salary)
### Bootstrap
print(svm)
###############################################################################################################


### Logistic
model_lg <- train(adult_data.salary ~ ., data = traindata, method="glm", family="binomial")
print(model_lg)
predict_lg <- predict(model_lg, newdata = testdata)
confusionMatrix(predict_lg, testdata$adult_data.salary)
###############################################################################################################



# Model Compare
model_comparison <- resamples(list(RF = model_rf, SVM = svm, KNN = model_knn,logistic = model_lg))
summary(model_comparison)

scales <- list(x = list(relation = "free"),
               y = list(relation = "free"))
bwplot(model_comparison, scales = scales)
