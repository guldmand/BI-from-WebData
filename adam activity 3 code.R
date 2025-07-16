#test comment 

#read data
setwd("C:/Users/adam/OneDrive/School/AU Business Intelligence/RStudio/")
data = read.csv("tldata2.csv")

#make some things factors
data$Country           = as.factor(data$Country)
data$Active.This.Week  = as.factor(data$Active.This.Week)
data$Supporter.Status. = as.factor(data$Supporter.Status.)
#rank is an ordered factor
data$Rank = factor(data$Rank, levels=c("D","D+","C-","C","C+","B-","B","B+","A-","A","A+","S-","S","S+","SS","U","X","X+"))

#remove index (standing does this)
data$X = NULL 

attach(data)

#-------------------- Neural Net --------------------
#min max normalization





#-------------------- clustering --------------------
#normalize
onlynums = data[,c(1,4,5,6,7,8,9,10,11,12)]
for (i in 1:10) {
  onlynums[,i] = scale(onlynums[,i], 
                       center=min(onlynums[,i]),
                       scale=max(onlynums[,i]) - min(onlynums[,i]))
}

library("cluster")
km = kmeans(onlynums, 3)
clusplot(onlynums, km$cluster, color=T, shade=T, labels=0, lines=0)


km = kmeans(a,10)
clusplot(a, km$cluster, color=T, shade=T, labels=0, lines=0)

data$clusternum = km$cluster

k = 10
ksplits = sample( rep(1:k, ceiling(nrow(data)/k) ), nrow(data) )




