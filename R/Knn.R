library(ggplot2)
library(gridExtra)
library(caret)
library(class)
library(datasets)

data(iris)
summary(iris)

p1 <- ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=Species)) +
  geom_point(alpha=0.8)

p2 <- ggplot(iris, aes(x=Petal.Length, y=Petal.Width, col=Species)) +
  geom_point(alpha=0.8)

grid.arrange(p1, p2, ncol=2)

iris_tr_feat <- iris[,1:4]

#Creating Training and Test data set. Training data will be used to build model whereas test data will be used for validation and optimisation of model by tuning k value.

set.seed(123)  # To get the same random sample
dat.d <- sample(1:nrow(iris_tr_feat),size=nrow(iris_tr_feat)*0.7,replace = FALSE) #random selection of 70% data.

train.iris <- iris_tr_feat[dat.d,] # 70% training data
test.iris <- iris_tr_feat[-dat.d,] # remaining 30% test data

#Now creating seperate dataframe for 'Species' feature which is our target.
train.gc_labels <- iris[dat.d,5]
test.gc_labels  <- iris[-dat.d,5]   

NROW(train.gc_labels)

#To identify optimum value of k, generally square root of total no of observations (105) 
# which is 10.24 is taken, so will try with 10, 11 then will check for optimal value of k.

knn.6 <-  knn(train=train.iris, test=test.iris, cl=train.gc_labels, k=6)

ACC.6 <- 100 * sum(test.gc_labels == knn.6)/NROW(test.gc_labels)  # For knn = 6

table(knn.6 ,test.gc_labels)  # to check prediction against actual value in tabular form

confusionMatrix(knn.6 ,test.gc_labels)


