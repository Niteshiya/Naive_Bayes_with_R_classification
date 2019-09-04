#Load libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
#Data
data <- read.csv("binary.csv")
str(data)
#crosstabulation for admit and rank
xtabs(~admit+rank,data=data)
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)
str(data)

#Visualtization
pairs.panels(data[,-1])
#Only numeric variable are gre/gpa 
#the corelation coeff are not that strong
data %>%
  ggplot(aes(x=admit,y=gre,fill=admit)) +
  geom_boxplot() +
  ggtitle("Box Plot")
#avg gre is higher of people getting admission so their is some potential over classification
#if we do the same thing for gpa
data %>%
  ggplot(aes(x=admit,y=gpa,fill=admit)) +
  geom_boxplot() +
  ggtitle("Box Plot")
#avg gpa is higer for admited people
data %>%
  ggplot(aes(x=gre,fill=admit)) +
  geom_density(alpha=0.8,color="black") +
  ggtitle("Density Plot")
#we can see the overlap

#Data partition
set.seed(1234)
ind <- sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind==2,]

#NAIVE BAYES
#P(A/B)=(P(A)*P(B/A))/P(B)
#{usekernel=T}if varriable are not normally distributed so that it performs better 
model <- naive_bayes(admit~.,data=train)
model
#cross cheacking mean of gre if admit==0
train %>%
  filter(admit=="0") %>%
  summarize(mean(gre),sd(gre))
#Plot model we will get 3 plots
plot(model)

#Predict
p <- predict(model,train,type="prob")
cbind(p,train) %>% head()

#Confusion Matrix
tab <- table(predict(model,train),train$admit)
tab
#accuracy
acc <- sum(diag(tab))/sum(tab)
acc

#Testing data
tab1 <- table(predict(model,test),test$admit)
acc1 <- sum(diag(tab1))/sum(tab1)
tab1
acc1
