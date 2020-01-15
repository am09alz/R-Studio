
utilities=read.csv(file.choose())
str(utilities)

#Scatter Plot
plot(Fuel_Cost~Sales,utilities)
with(utilities,text(Fuel_Cost~Sales,labels=Company))

plot(Fuel_Cost~Sales,utilities,ylab = 'Fuel Cost')
with(utilities,text(Fuel_Cost~Sales,labels=Company,cex=0.5,pos=4))

#Normalization
z=utilities[,-1]
m=apply(z,2,mean)
s=apply(z,2,sd)
z=scale(z,m,s)
z=scale(z,center = TRUE,scale = TRUE) #Same thing with this one line of code

#Calculating Euclidian Distance
distance=dist(z)
print(distance,digits = 3)

#Hierarchical Cluster Dendrogram with complete linkage
hc.c=hclust(distance)
plot(hc.c,labels = utilities$Company,hang=-1)

#Hierarchical Cluster Dendrogram with average linkage
hc.a=hclust(distance,method = 'average')
plot(hc.a,labels = utilities$Company,hang=-1)

#Cluster Membership
member.c=cutree(hc.c,3)
member.a=cutree(hc.a,3)
table(member.c,member.a)

#Cluster Means
aggregate(z,list(member.c),mean)
#Cluster Means - original units
aggregate(utilities[,-1],list(member.c),mean)

#Silhouette Plot
library(cluster)
plot(silhouette(cutree(hc.c,3),distance))

#Scree Plot
wss <- (nrow(z)-1)*sum(apply(z,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(z, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

#K-Means Clustering
kc=kmeans(z,3)
attributes(kc)
plot(Sales~D.Demand,utilities,col=kc$cluster)




