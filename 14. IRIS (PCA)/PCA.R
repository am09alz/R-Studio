library(dplyr)
library(psych)
iris=read.csv(file.choose())
iris=iris%>%
  rename(Sepal.Length=SepalLengthCm,
         Sepal.Width=SepalWidthCm,
         Petal.Length=PetalLengthCm,
         Petal.Width=PetalWidthCm)
iris=iris[-1]
str(iris)
iris$Species=as.character(iris$Species)
iris$Species[iris$Species=='Iris-setosa']='setosa'
iris$Species[iris$Species=='Iris-versicolor']='Versicolor'
iris$Species[iris$Species=='Iris-virginica']='Virginica'
iris$Species=as.factor(iris$Species)
levels(iris$Species)

#Partition Data
set.seed(111)
ind=sample(2,nrow(iris),replace=T,prob = c(0.8,0.2))
training=iris[ind=='1',]
testing=iris[ind=='2',]

#Pairs.panels
pairs.panels(training[-5],
             bg=c('red','green','blue')[training$Species],
             pch = 21,
             gap=0)

#Principal Component Analysis
pca=prcomp(training[-5],
           center = TRUE, #center is the mean of each variable eg mean(training$Sepal.Length)
           scale. =TRUE) #scale. is the sd of each variable eg sd(training$Sepal.Length)
pca
attributes(pca)
pca$sdev
pca$rotation
pca$center
pca$scale
pca$x

#Orthogonaility of PCs
pairs.panels(pca$x,
             bg=c('red','green','blue')[training$Species],
             pch = 21,
             gap=0)

#Bi-Plot
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g=ggbiplot(pca,groups = training$Species,obs.scale=1,var.scale=1,
           ellipse=TRUE, circle = TRUE,ellipse.prob = 0.68)
g=g+theme(legend.direction = 'horizontal',legend.position = 'top')+
  scale_color_discrete(name='Species')

#Prediction with Pincipal Components
trg=predict(pca,training)
trg=data.frame(trg,training[5])

tst=predict(pca,testing)
tst=data.frame(tst,testing[5])
              
#Multinominal Logistic Regression with the first two PCs
library(nnet)
mymodel=multinom(Species~PC1+PC2,data = trg)
summary(mymodel)

#Confusion Matrix and Misclassification Error - Training
p=predict(mymodel,trg)
tab=table(predicted=p,Actual=trg$Species)
1-sum(diag(tab))/sum(tab)

#Confusion Matrix and Misclassification Error- Testing
p1=predict(mymodel,tst)
tab1=table(Predicted=p1, Actual=tst$Species)
1-sum(diag(tab1))/sum(tab1)
