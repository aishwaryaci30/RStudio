#Load package mlbench

setwd('~/Desktop')

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)
View(PimaIndiansDiabetes)

mydata <-PimaIndiansDiabetes

#Data understanding (Variable description)
summary(mydata)
str(mydata)
dim(mydata)
diabatic <- split(mydata,mydata$diabetes)
sd_pos<-sd(diabatic$pos$glucose)
sd_neg<-sd(diabatic$neg$glucose)
summary(diabatic$pos$glucose)
summary(diabatic$neg$glucose)
sd_pos
sd_neg
View(mydata)

### Exploratory Data Analysis (Shape of Distribution)
attach(mydata)
install.packages("ggplot2")
library(ggplot2)

# For continuous variables

ggplot(mydata, aes(x = glucose)) + 
  geom_density(aes(fill = diabetes), alpha = 0.3) + 
  scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
  scale_fill_manual(values = c("darkturquoise","lightcoral")) + xlim(-30,250)


hist(diabatic$pos$glucose, 10,main="Distribution of glucose with diabetes ",
     xlab="Glucose level",ylab="No. of people")
hist(diabatic$neg$glucose, 10,main="Distribution of glucose without diabetes ",
     xlab="Glucose level",ylab="No. of people")

#Difference of mean in glucose with and without diabetes
posmean<-diabatic$pos$glucose
negmean<-diabatic$neg$glucose
t.test(posmean, negmean)
#Box Plot to compare mean in glucose with and without diabetes
boxplot(glucose~diabetes)

# Checking null data
sapply(mydata,function(x) sum(is.na(x)))

# Checking # of unique values in each column
sapply(mydata,function(x) length(unique(x)))

# Convert independent variable into numeric
mydata$diabetes = as.numeric(mydata$diabetes)
mydata$pregnant = as.numeric(mydata$pregnant)
mydata$glucose = as.numeric(mydata$glucose)
mydata$pressure = as.numeric(mydata$pressure)
mydata$triceps = as.numeric(mydata$triceps)
mydata$insulin = as.numeric(mydata$insulin)
mydata$age = as.numeric(mydata$age)

# Correlation check
install.packages("corrplot")
library(corrplot)
correlations=cor(mydata[sapply(mydata, function(x) !is.factor(x))])
corrplot(correlations, diag = FALSE)

# Binary variable needs to be converted into factor variable
mydata$diabetes = as.factor(mydata$diabetes)

### Data Split 
install.packages("lattice")
install.packages("caret")
library(caret)
set.seed(1234)
trainIndex = createDataPartition(diabetes, p=0.7, list = FALSE, times = 1)
train.data = mydata[trainIndex, ]
test.data  = mydata[-trainIndex,]

dim(train.data)
dim(test.data)

prop.table(table(mydata$diabetes))
prop.table(table(train.data$diabetes))
prop.table(table(test.data$diabetes))

### Model refining - Logistic Regression
logit_model1  =  glm(diabetes ~ age+mass+glucose+pregnant, 
                     data = train.data, 
                     family = binomial(link="logit"))

summary(logit_model1)

# Check for multicollinearity
install.packages("carData")
install.packages("car")
library(car)
vif(logit_model1)


# Performance metrics (in-sample)
pred = predict(logit_model1, data=train.data, type="response")
y_pred_num = ifelse(pred>0.5,2,1)
y_pred = factor(y_pred_num, levels=c(1,2))
y_actual = train.data$diabetes
install.packages("e1071")
confusionMatrix(y_pred,y_actual,positive="2")


# ROC plot
install.packages("ROCR")
library(ROCR)
train.roc <- prediction(pred, train.data$diabetes)
plot(performance(train.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for train data")
abline(0, 1, lty = 8, col = "blue")

# AUC
train.auc = performance(train.roc, "auc")
train.area = as.numeric(slot(train.auc, "y.values"))
train.area