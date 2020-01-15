setwd("C:\\Users\\guest-pc\\Desktop\\Data Science\\R\\2. Titanic Kaggle")
table1 <- matrix(rnorm(25),5)
table2 <- matrix(rnorm(25),5)
res <- rcorr(table1, table2, type="pearson")
res
table1

getwd()
setwd('C:\\Users\\guest-pc\\Desktop\\Data Science\\R Studio\\R\\3. Lung Capacity')
getwd()
myData=read.csv(file.choose())
attach(myData)

myData=myData %>%
  rename(
    Gender=X1,
    Height=X2,
    Smoker=X3,
    Exercise=X4,
    Age=X5,
    Lung_Capacity=Y
  )

myData=myData %>%
  rename(LungCap = Lung_Capacity)
  
myData
head(myData)
colnames(myData)
myData=myData[-1,-7]
class(myData$Age)
class(myData$LungCap)
myData$Age=as.integer(as.character(myData$Age))
myData$LungCap=as.numeric(as.character(myData$LungCap))

cor(Age,LungCap)
model=lm(LungCap~Age)
summary(model)
rm(model)
attributes(model)
coef(model)
confint(model)
confint(model, level=0.99)
par(mfrow=c(2,2))
par(mfrow=c(1,1))
plot(model)
plot(myData$Age,myData$LungCap, main='Scatterplot')
abline(model,col=2,lwd=3) #For abline to work, it requires the lm model


str(myData)
levels(myData$Gender)
class(myData$Gender)
myData[is.na(myData)]=0 #changes all all na values to 0
myData$Gender=as.character(myData$Gender) #must change to character when renaming factors
myData[myData$Gender==0,]$Gender='F'
myData[myData$Gender==1,]$Gender='M'
myData$Gender=as.factor(myData$Gender)

myData$Smoker=as.character(myData$Smoker)
myData[myData$Smoker==0,]$Smoker='No'
myData[myData$Smoker==1,]$Smoker='Yes'
myData$Smoker=as.factor(myData$Smoker)
class(myData$Smoker)

str(myData)
myData$Exercise=as.integer(as.character(myData$Exercise))
myData$Height=as.numeric(as.character(myData$Height))

model1=lm(myData$LungCap~myData$Age+myData$Height)
summary(model1)
confint(model1, conf.level=0.95)

cor(myData$Age,myData$Height, method='pearson')

model2=lm(myData$LungCap~myData$Age+myData$Height
          +myData$Smoker+myData$Exercise+myData$Age)
summary(model2)
plot(model2)

myData$Height[1:10]
catheight=cut(myData$Height[1:10],breaks=c(0,68,69,70,71,72), 
              labels=c('A','B','C','D','E'),right=FALSE)
catheight

myData$Height
catheight=cut(myData$Height,breaks=c(0,60,65,70,75,80), 
              labels=c('A','B','C','D','E'),right=FALSE)
catheight
levels(catheight)

mean(myData$LungCap[catheight=='A'])
mean(myData$LungCap[catheight=='B'])
mean(myData$LungCap[catheight=='C'])
mean(myData$LungCap[catheight=='D'])
mean(myData$LungCap[catheight=='E'])

mod=lm(myData$LungCap~catheight)
summary(mod)

mod2=lm(myData$LungCap~myData$Age+myData$Smoker)
summary(mod2)

table(myData$Smoker)
myData$Smoker=relevel(myData$Smoker,ref='Yes')
table(myData$Smoker)

mod3=lm(myData$LungCap~myData$Age+myData$Smoker)
summary(mod3)

plot(myData$Age[myData$Smoker=='No'],myData$LungCap[myData$Smoker=='No'],
     col='blue', xlab='Age',ylab='LungCap',main='LungCap vs Age,Smoker',
     ylim=c(4000,6500),xlim=c(15,85))
points(myData$Age[myData$Smoker=='Yes'],myData$LungCap[myData$Smoker=='Yes'],
       col='red',pch=16)
legend(12,6700,legend=c('NonSmoker','Smoker'),
       col=c('blue','red'),pch=c(1,16),bty='n')


model1=lm(myData$LungCap~myData$Age+myData$Smoker+myData$Age:myData$Smoker)
summary(model1)

#Model 1 - Not smoking
abline(a=5425.989,b=2.548,col='blue',lwd=2)
#Model 1 - smoking
abline(a=5035.337,b=-1.245,,col='red',lwd=2)

model2=lm(myData$LungCap~myData$Age+myData$Smoker)
summary(model2)

#Model 2 - Not smoking
abline(a=5481.232,b=1.384,col='blue',lwd=2)
#Model 2 - smoking
abline(a=4917.427,b=1.384,col='red',lwd=2)

#Another way to do the abline
abline(lm(myData$LungCap[myData$Smoker=='No']~myData$Age[myData$Smoker=='No']),
       col='blue',lwd=2)
abline(lm(myData$LungCap[myData$Smoker=='Yes']~myData$Age[myData$Smoker=='Yes']),
       col='red',lwd=2)

myData$catheight=catheight
mod4=lm(myData$LungCap~myData$Age+myData$catheight)
summary(mod4)

plot(myData$Age[myData$catheight=='A'],myData$LungCap[myData$catheight=='A'],
     ylim=c(4000,6500),xlim=c(18,85), col=2,pch=1,xlab='Age',ylab='LungCap',main='LungCap vs Age,catheight')
points(myData$Age[myData$catheight=='B'],myData$LungCap[myData$catheight=='B'],
       col=3, pch=2)
points(myData$Age[myData$catheight=='C'],myData$LungCap[myData$catheight=='C'],
       col=4,pch=3)
points(myData$Age[myData$catheight=='D'],myData$LungCap[myData$catheight=='D'],
       col=5,pch=4)
points(myData$Age[myData$catheight=='E'],myData$LungCap[myData$catheight=='E'],
       col=9,pch=5)

abline(lm(myData$LungCap[myData$catheight=='A']~myData$Age[myData$catheight=='A']),
       col=2,lwd=2)
abline(lm(myData$LungCap[myData$catheight=='B']~myData$Age[myData$catheight=='B']),
       col=3,lwd=2)
abline(lm(myData$LungCap[myData$catheight=='C']~myData$Age[myData$catheight=='C']),
       col=4,lwd=2)
abline(lm(myData$LungCap[myData$catheight=='D']~myData$Age[myData$catheight=='D']),
       col=5,lwd=2)
abline(lm(myData$LungCap[myData$catheight=='E']~myData$Age[myData$catheight=='E']),
       col=9,lwd=2)

full.model=lm(myData$LungCap~myData$Age+I(myData$Age^2))
reduced.model=lm(myData$LungCap~myData$Age)
summary(full.model)
summary(reduced.model)
anova(reduced.model,full.model)

model3=lm(myData$LungCap~myData$Age+myData$Gender+myData$Smoker+myData$Height)
model4=lm(myData$LungCap~myData$Age+myData$Gender+myData$Smoker)
anova(model4,model3)

plot(myData$Height,myData$LungCap,main='polynomial regression',las=1)
pred=predict(model4,data.frame(),interval = 'confidence')
pred

model5=lm(myData$LungCap~myData$Height)
summary(model5)
abline(model5,lwd=2,col='red')

model6=lm(myData$LungCap~myData$Height+I(myData$Height^2))
summary(model6)
lines(smooth.spline(myData$Height,predict(model6)),lwd=2,col='blue')
lines(smooth.spline(model6,lwd=2,col='blue')


model7=lm(myData$LungCap~myData$Height+I(myData$Height^2)+I(myData$Height^3))
summary(model7)
lines(smooth.spline(myData$Height,predict(model7)),lwd=2,col='green')

anova(model5,model6)
setwd('C:\\Users\\guest-pc\\Desktop\\Data Science\\R Studio\\R\\3. Lung Capacity')
getwd()
