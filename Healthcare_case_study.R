#Loading the data
library("readxl")
healthcare_data <- read.csv("C:\\Users\\admin\\Desktop\\Healthcare - Diabetes\\health care diabetes.csv")
View(healthcare_data)
str(healthcare_data)

#Checking the variables which can be done as factor
table(healthcare_data$Pregnancies)
table(healthcare_data$Outcome)


healthcare_data$Pregnancies[healthcare_data$Pregnancies>10] <- 10

#Converting continous variable into categorical variable
healthcare_data$Outcome <- as.factor(healthcare_data$Outcome)
healthcare_data$Pregnancies <- as.factor(healthcare_data$Pregnancies)
str(healthcare_data)


#Checking for any NA
sum(is.na(healthcare_data))


#Counting the number of zeroes in dataframe
library(plyr)
nonzero <- function(x) sum(x == 0)
numcolwise(nonzero)(healthcare_data)


df<- healthcare_data

# deleting the rows whcih contains 0's in Glucose,BP and BMI as the count is very small
library(dplyr)
df <- filter(df,df$Glucose > 0)
df <- filter(df,df$BloodPressure>0)
df <- filter(df,df$BMI>0)

#counting the number of zeroes again
numcolwise(nonzero)(df)

#converting all zeroes to NA
df1 <- df[, 4:5][df[, 4:5] == 0] <- NA


#Now counting the number of NA's
sum(is.na(df))
sum(is.na(df$SkinThickness))
sum(is.na(df$Insulin))


#Checking the %of data whcih is missing
sum(is.na(df$SkinThickness))/nrow(df)*100
sum(is.na(df$Insulin))/nrow(df)*100


#Replacing those NA's by the method of prediction
#Checking which variable is correlated to Insulin and SkinThickness
summary(lm(Insulin~.,data=df))
summary(lm(SkinThickness~.,data=df))


#Predicting the Insulin Values with the help of correlated variable
library(rpart)
anova_mod <- rpart(Insulin ~ . - Glucose, 
                   data=df[!is.na(df$Insulin), ], 
                   method="anova", na.action=na.omit)
Insulin_pred <- predict(anova_mod, df[is.na(df$Insulin), ])
Insulin_pred <- data.frame(Insulin_pred)


#Replacing those predicted insulin values with NA's
s <- 1
for (val in 1:nrow(df)){
  if (is.na(df[val,5])) {
    df[val,5] <- Insulin_pred[s,1]
    s <- s+1
  } 
}

#Now we can check that NA count is 0
sum(is.na(df$Insulin))


#Predicting SkinThcikness with the help of correlated variable
anova_mod2 <- rpart(SkinThickness ~ . - BMI, 
                    data=df[!is.na(df$SkinThickness), ], 
                    method="anova", na.action=na.omit)
Skin_pred <- predict(anova_mod2, df[is.na(df$SkinThickness), ])
Skin_pred <- data.frame(Skin_pred)


#Replacing NA's with predicted values
s <- 1
for (val in 1:nrow(df)){
  if (is.na(df[val,4])) {
    df[val,4] <- Skin_pred[s,1]
    s <- s+1
  } 
}


#Now the count of NA is 0
sum(is.na(df$SkinThickness))

df_new <- df

#Boxplot to detect the outliers
library(ggplot2)
ggplot(stack(df), aes(x = ind, y = values)) +
  geom_boxplot()


#to find the outliers
#Defining the quantiles
summary(df$BloodPressure)
quantile(df$BloodPressure,probs = c(0,.25,.50,.75,.90,1))
summary(df$BMI)
quantile(df$BMI,probs = c(0,.25,.50,.5,.75,.90,1))
summary(df$DiabetesPedigreeFunction)
quantile(df$DiabetesPedigreeFunction,probs = c(0,.25,.50,.75,.85,.90,.95,1))
summary(df$Age)
quantile(df$Age,probs = c(0,.25,.50,.75,.90,1))
summary(df$SkinThickness)
quantile(df$SkinThickness,probs = c(0,.25,.50,.60,.75,.90,1))
summary(df$Insulin)
quantile(df$Insulin,probs = c(0,.25,.50,.60,.75,.90,1))



#to remove the outliers
outliers <- boxplot(df$BloodPressure,plot=FALSE)$out
df<-df[-which(df$BloodPressure %in% outliers), ]
#boxplot(df$BloodPressure)

outliers <- boxplot(df$SkinThickness,plot=FALSE)$out
df<-df[-which(df$SkinThickness %in% outliers), ]
#boxplot(df$SkinThickness)

outliers <- boxplot(df$Insulin,plot=FALSE)$out
df<-df[-which(df$Insulin %in% outliers), ]
#boxplot(df$Insulin)

outliers <- boxplot(df$BMI,plot=FALSE)$out
df<-df[-which(df$BMI %in% outliers), ]
#boxplot(df$BMI)

outliers <- boxplot(df$DiabetesPedigreeFunction,plot=FALSE)$out
df<-df[-which(df$DiabetesPedigreeFunction %in% outliers), ]
#boxplot(df$DiabetesPedigreeFunction)

outliers <- boxplot(df$Age,plot=FALSE)$out
df<-df[-which(df$Age %in% outliers), ]
#boxplot(df$Age)

ggplot(stack(df), aes(x = ind, y = values)) +
  geom_boxplot()


#Writing the final data to local disc after data processing
df_new <- df
write.csv(df_new,"C:\\Users\\admin\\Desktop\\Healthcare - Diabetes\\data.csv", row.names = FALSE)

#Histogram to check the data distribution
#install.packages("Hmisc")
library(Hmisc)
hist.data.frame(df)

#Plot with the count of outcomes on all variables
ggplot(data=df) +
  geom_histogram(mapping=aes(x=Glucose,fill=Outcome),color="black",binwidth = 5)

ggplot(data=df) +
  geom_histogram(mapping=aes(x=BMI,fill=Outcome), color = "black",binwidth =3)

ggplot(data=df) +
  geom_histogram(mapping=aes(x=Age,fill=Outcome),color = "black",binwidth =3)

ggplot(data=df) +
  geom_histogram(mapping=aes(x=BloodPressure,fill=Outcome), color = "black",binwidth =3) 

ggplot(data=df) +
  geom_histogram(mapping=aes(x=Insulin,fill=Outcome), color = "black",binwidth =3)

ggplot(data=df) +
  geom_histogram(mapping=aes(x=SkinThickness,fill=Outcome), color = "black",binwidth =3)

#To know the data type of all the varibales 
str(df)


#SCATTER PLOT

lower.panel<-function(x, y){
  points(x,y, pch=19, col=c("blue", "green", "yellow"))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(df[2:8], lower.panel = lower.panel, 
      upper.panel = NULL)




#HEATMAP to analyse the correlation

df_graph <- data.matrix(df)
#install.packages("corrplot")
library(corrplot)
df.cor<-cor(df_graph)
df.cor
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = df.cor, col = palette, symm = TRUE)



############################


#Model building

#Defining train and test data
set.seed(124)
train_indices <- sample(1:nrow(df), 0.70 * nrow(df))
train <- df[train_indices, ]
test <- df[-train_indices,]
#View(test)

#Having a look at the data distribution in the dataframe along with train and test data
prop.table(table(df$Outcome))
prop.table(table(train$Outcome))
prop.table(table(test$Outcome))


###Logistic regression

# Fit the model
library(class)
log_reg <- glm(Outcome ~ . , data = train,family = 'binomial')
summary(log_reg)

#To improvise the model and prevent the loss of data
library(MASS)
new_mod=stepAIC(log_reg)
summary(new_mod)
new_mod$formula

#We use the variables which ever is having more impact on the outcome
new_mod <- glm(new_mod$formula,data = train,family = 'binomial')
summary(new_mod)

#Predicting the probability of having the outcome here
pred_prob <- predict(new_mod, newdata = test,type = 'response')
#View(pred_prob)

#Factorising the predictions depending on the threshhold 0.5
pred_class <- as.factor(ifelse(pred_prob >= 0.5, 1, 0))
#if the threshhold/probability is greater than 0.5 then it will be taken as 1 or else 0
#Confusion matrix
table(test$Outcome,pred_class)

#Classification report


t<-table(test$Outcome,pred_class)

#Defining the functions for finding the accuracy, error rate, prescion and f1_score
prediction_accuracy <- function(new_df){
  accuracy = (t[2,2] + t[1,1]) / (t[2,2] + t[1,2] + t[1,1] + t[2,1])
  accuracy = round(accuracy, 2)
  paste("The prediction accuracy is: ", accuracy)
}

classification_error_rate <- function(new_df){
  classification_error_rate = (t[1,2] + t[2,1]) / (t[2,2] + t[1,2] + t[1,1] +         t[2,1])
  classification_error_rate = round(classification_error_rate, 2)
  paste0('The classification error rate is: ',classification_error_rate)
}

precision <- function(new_df){
  p       = (t[2,2] / (t[2,2] + t[1,2]))
  p       = round(p, 2)
  paste0('The precision is: ', p)
}


f1.score <- function(new_df){
  p = (t[2,2] / (t[2,2] + t[1,2]))
  s = t[2,2] / (t[2,2] + t[2,1])
  f1 = (2*p*s) / (p + s)
  f1 = round(f1, 2)
  paste0("The F1 score is: ", f1)
}


#Detailed classification report 
caret::confusionMatrix(pred_class, test$Outcome,positive='0')
prediction_accuracy(test)
classification_error_rate(test)
precision(test)
f1.score(test)
#ROC curve
#install.packages("precrec")
library(precrec)
precrec_obj <- evalmod(scores = pred_prob, labels = test$Outcome)
autoplot(precrec_obj)
#All possible plots of various model
precrec_obj <- evalmod(scores = pred_prob, labels = test$Outcome,mode="basic")
autoplot(precrec_obj)


#########################


###N B


# set.seed(100)
# 
# train_indices <- sample(1:nrow(df), 0.70 * nrow(df))
# train <- df[train_indices, ]
# test <- df[-train_indices,]

library(e1071)
pred_3<-naiveBayes(Outcome~.,data=train)
pred_3_prob<-predict(pred_3,type="raw",newdata=test)
pred_3_class<-as.factor(ifelse(pred_3_prob[,2]>.5,1,0))

#Confusion matrix
t<-table(test$Outcome,pred_3_class)
print(t)

#Classification Report
caret::confusionMatrix(pred_3_class, test$Outcome,positive='0')
prediction_accuracy(test)
classification_error_rate(test)
precision(test)
f1.score(test)
#ROC curve
precrec_obj <- evalmod(scores = pred_3_prob, labels = test$Outcome)
autoplot(precrec_obj)
precrec_obj <- evalmod(scores = pred_3_prob, labels = test$Outcome,mode="basic")
autoplot(precrec_obj)



###########################

#SVM


# set.seed(100)
# train_indices <- sample(1:nrow(df), 0.70 * nrow(df))
# train <- df[train_indices, ]
# test <- df[-train_indices,]


library(e1071)
classifier = svm(formula = Outcome ~ .,
                 data = train,
                 type = 'C-classification',
                 kernel = 'linear')
pred_4 <- predict(classifier,newdata=test,type="response")
#Confusion matrix
t <- table(test$Outcome,test_pred)
print(t)

#Classification report
caret::confusionMatrix(pred_4, test$Outcome,positive='0')
prediction_accuracy(test)
classification_error_rate(test)
precision(test)
f1.score(test)


#install.packages("pROC")
library(pROC)

#ROC curve
pred_4 <- as.vector(pred_4, mode = "numeric")
lrROC <- roc(test$Outcome ~ 
               pred_4,plot=TRUE,print.auc=TRUE,col="green",
             lwd =4,legacy.axes=TRUE,main="ROC Curves")
             
             
             
##################             

#KNN

# set.seed(100)
# 
# train_indices <- sample(1:nrow(df), 0.70 * nrow(df))
# train <- df[train_indices, ]
# test <- df[-train_indices,]
# nrow(train)
# nrow(test)
library(class)
pred_2<- knn(train,test,train$Outcome,k=5)
#Confusion matrix
table(test$Outcome,pred_2)

#Classification Report
t<-table(test$Outcome,pred_2)
caret::confusionMatrix(pred_2, test$Outcome,positive='0')
prediction_accuracy(test)
classification_error_rate(test)
precision(test)
f1.score(test)
#ROC curve
pred_2 <- as.vector(pred_2, mode = "numeric")

lrROC <- roc(test$Outcome ~ 
               pred_2,plot=TRUE,print.auc=TRUE,col="red",
             lwd =4,legacy.axes=TRUE,main="ROC Curves")




