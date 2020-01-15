library('naivebayes')
library('dplyr')
library('ggplot2')
library('psych')

#Data
setwd('C:\\Users\\guest-pc\\Desktop\\Data Science\\R Studio\\R\\10. Student Grades (Naive Bayes)')
data=read.csv(file.choose(),header = T)

str(data)

xtabs(~admit+rank,data=data)
data$rank=as.factor(data$rank)
data$admit=as.factor(data$admit)

#Visualisation
pairs.panels(data[-1])
pairs.panels(data[,c(-1,-4)])
pairs.panels(data[,c(2:3)])
pairs.panels(data[(1:50),c(2:3)])

pairs.panels(data[data$admit == "0",2:3], lm=TRUE)
pairs.panels(data[data$admit == "1",2:3], lm=TRUE)

data%>%
  ggplot(aes(x=admit,y=gpa,fill=admit))+
  geom_boxplot()+
  ggtitle('Box Plot')

x=data%>%
  ggplot(aes(x=gre,fill=admit))+
  geom_density(alpha=0.8,color='black')+
  ggtitle('Density Plot')
x + scale_fill_manual( values = c("green","red"))
x+theme(legend.background = element_rect(colour = 'green',fill='grey'),
        legend.position='top')

#Data Partition
set.seed(1234)
ind=sample(2,nrow(data),replace=T,prob=c(0.8,0.2))
train=data[ind==1,]
test=data[ind==2,]

#Naive Bayes Model
model=naive_bayes(admit~.,data=train,usekernel = T)
model
 
train %>%
  filter(admit=='1') %>%
  summarise(mean(gre),sd(gre))

plot(model)

#Predict
p=predict(model,train,type='prob')
head(cbind(p,train))

pred_train=predict(model,train)
pred_test=predict(model,test)
  
#Confusion Matrix - train data
table_train=table(pred_train,train$admit)
table_train
1-sum(diag(table_train))/sum(table_train)

#Confusion Matrix - test data
table_test=table(pred_test,test$admit)
table_test
1-sum(diag(table_test))/sum(table_test)
