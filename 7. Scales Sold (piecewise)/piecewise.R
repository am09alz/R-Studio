mydata=data.frame('Months.Employed'=c(41,106,76,100,22,12,85,111,40,51,0,12,6,56,19),
                  'Scales.Sold'=c(275,296,317,376,162,150,367,308,189,235,83,112,67,325,189),
                  'X'=('NA'))
attach(mydata)
dummyknot=rep(0,length(Months.Employed))
dummyknot[Months.Employed>90]=1

mydata$Xdif=Months.Employed-90
mydata$DN=dummyknot
mydata$X=mydata$Xdif*mydata$DN

regoutput=lm(Scales.Sold~Months.Employed+X,data=mydata)
summary(regoutput)

plot(Months.Employed,Scales.Sold,ylim = c(0,500),xlim = c(0,120))
lines(smooth.spline(Months.Employed,predict(regoutput)))

x=lm(Scales.Sold~Months.Employed+I(Months.Employed^2))
summary(x)

plot(Months.Employed,Scales.Sold,ylim = c(0,500),xlim = c(0,120))
lines(smooth.spline(Months.Employed,predict(x)))

Y=87.2172+3.4094*(10)-7.8726*(10-90)*0
Y=87.2172+3.4094*(100)-7.8726*(100-90)*1