library(dplyr)
library(psych)

iris=read.csv(file.choose())
iris=iris%>%
  rename(Sepal.Length=SepalLengthCm,
         Sepal.Width=SepalWidthCm,
         Petal.Length=PetalLengthCm,
         Petal.Width=PetalWidthCm)
iris=iris[-1]
class(iris)
str(iris)
iris$Species=as.character(iris$Species)
iris$Species[iris$Species=='Iris-setosa']='setosa'
iris$Species[iris$Species=='Iris-versicolor']='Versicolor'
iris$Species[iris$Species=='Iris-virginica']='Virginica'
iris$Species=as.factor(iris$Species)
levels(iris$Species)

#pairs.panels
pairs.panels(iris[,1:4],
             bg=c('red','green','blue')[iris$Species],
             pch = 21,
             gap=0)

#Data partition
set.seed(555)
ind=sample(2,nrow(iris),replace=T,prob = c(0.6,0.4))
training=iris[ind=='1',]
testing=iris[ind=='2',]

#Linear Discriminant Analysis
library(MASS)
linear=lda(Species~.,data=training)
linear
summary(linear)
linear$prior
linear$counts
linear$scaling

#Prediction
p=predict(linear,training)
ldahist(p$x[,1],g=training$Species)
ldahist(p$x[,2],g=training$Species)

#Bi-Plot
library(devtools)
install_github('fawda123/ggord')
library(ggord)

ggord(linear,training$Species,ylim=c(-10,10))
linear
summary(linear)

#Patition Plot
library(klaR)
partimat(Species~.,data=training,method='lda')
partimat(Species~.,data=training,method='qda')

#Confusion Matrix and Accuracy - Training data
p1=predict(linear,training)$class
tab=table(Predicted=p,Actual=training$Species)
tab
sum(diag(tab))/sum(tab)
1-sum(diag(tab))/sum(tab)

#Confusion Matrix and Accuract - Testing data
p2=predict(linear,testing)$class
tab2=table(Predicted=p2,Actual=testing$Species)
tab2
sum(diag(tab2))/sum(tab2)
1-sum(diag(tab2))/sum(tab2)
