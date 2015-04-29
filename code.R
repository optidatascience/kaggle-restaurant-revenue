### code.R --- 
## 
## Filename: code.R
## Description: 
## Author: Liang Zhou
## Maintainer: Liang Zhou
## Created: Tue Apr 28 14:14:22 2015 (-0500)
## Version: 
## Package-Requires: ()
## Last-Updated: Wed Apr 29 16:26:48 2015 (-0500)
##           By: lzhou10
##     Update #: 6
######################################################################
## 
### Commentary: 
## 
## 
## 
######################################################################
## 
### Change Log:
## 
## 
######################################################################
## 
### Code:


library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Metrics)

setwd('H:/Projects/Kaggle/Restaurant/')

train <- data.table(read.csv('./train.csv'))
test <- data.table(read.csv('./test.csv'))

train$source = 'Train'
test$revenue = NA
test$source = 'Test'
alldata = rbind(train,test)

## plot some distributions
alldata.1 <- alldata %>% gather("variable","value",6:42)
g <- ggplot(alldata.1,aes(x=value,group=source,color=source))
g <- g + geom_density() + facet_wrap(~variable)
(g)

## formatting data
str(train)
train$Open.Date <- as.Date(train$Open.Date,"%m/%d/%Y")
test$Open.Date <- as.Date(test$Open.Date,"%m/%d/%Y")
range(train$Open.Date)
range(test$Open.Date)

## dummy variables
## train$Open.Weeks = as.numeric(difftime('2014-12-31',train$Open.Date,units='weeks'))
train$Open.Yrs  = 2015-year(train$Open.Date)
train$Open.Month = month(train$Open.Date)

## try gbm
library(gbm)
GBM_NTREES = 400
## 400 trees in the model; can scale back later for predictions
##, if desired or overfitting is suspected
GBM_SHRINKAGE = 0.001
## shrinkage is a regularization parameter dictating how
## fast/aggressive the algorithm moves across the loss gradient
## 0.05 is somewhat aggressive; default is 0.001,
## values below 0.1 tend to produce good results
## decreasing shrinkage generally improves results,
## but requires more trees, so the two should be adjusted in tandem
GBM_DEPTH = 4
## depth 4 means each tree will evaluate four decisions; 
## will always yield [3*depth + 1] nodes and [2*depth + 1]
## terminal nodes (depth 4 = 9) 
## because each decision yields 3 nodes, but each decision will come from a prior node
GBM_MINOBS = 5
## regularization parameter to dictate how many observations
## must be present to yield a terminal node
## higher number means more conservative fit; 30 is fairly high,
## but good for exploratory fits; default is 10


train.id = sample(train$Id,120)

train.data = train[Type!="DT" & Id %in% train.id,-c(1,2,3,44),with=F]
gfit <- gbm(revenue~.,data=train.data,distribution='gaussian', n.tree= GBM_NTREES
           ,shrinkage= GBM_SHRINKAGE, interaction.depth= GBM_DEPTH
           ,n.minobsinnode= GBM_MINOBS)

tP1 <- predict.gbm(object = gfit, newdata = train[Id%in%train.id], GBM_NTREES)
hP1 <- predict.gbm(object = gfit, newdata = train[!Id%in%train.id], GBM_NTREES)

rmse(tP1,train[Id%in%train.id]$revenue)
rmse(hP1,train[!Id%in%train.id]$revenue)

## produce results
str(test)
test$Open.Yrs  = 2015-year(test$Open.Date)
test$Open.Month = month(test$Open.Date)

summary(test$Type)
test.1 <- test[ Type%in%c("FC","IL")]
test.2 <- test[!Type%in%c("FC","IL")]

set.seed(213123)
test.2.rand <- runif(nrow(test.2))
test.2$Type <- ifelse(test.2.rand>0.5,"FC","IL")

test.data <- rbind(test.1,test.2)[order(Id)]

test.pred = predict.gbm(object=gfit,newdata=test.data,GBM_NTREES)

output <- data.frame(Id=test.data$Id,Prediction=test.pred)
write.csv(output,file="output.csv",row.names=F)
######################################################################
### code.R ends here
