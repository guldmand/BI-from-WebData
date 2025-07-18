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

data$Active.This.Week = ifelse(data$Active.This.Week == "Yes", 1, 0)
data$Supporter.Status. = ifelse(data$Supporter.Status. == "Yes", 1, 0)

#remove index (standing does this)
data$X = NULL 

attach(data)

#-------------------- Neural Net --------------------
#min max normalization
data_normalized = data

for(i in c(1,4,5,6,7,8,9,10,11,12)) {
  data_normalized[,i] = scale(
    data_normalized[,i], 
    center=min(data_normalized[,i]), 
    scale=max(data_normalized[,i])-min(data_normalized[,i]))
}

str(data_normalized, give.attr=F)

#turn rank factor into flags
for (i in 1:length(levels(data_normalized$Rank))) {
  data_normalized[,ncol(data_normalized)+1] = 0 #make new column
  #set appropriate ones to 1
  data_normalized[
    data_normalized$Rank == levels(data_normalized$Rank)[i], #select rows matching rank
    ncol(data_normalized)] = 1 #select last column (just added)
  
  varname = sprintf( "flag%sRank", levels(data_normalized$Rank)[i] )
  varname = sub("+", "Plus", varname, fixed=T) #fixed=T treats '+' literal
  varname = sub("-", "Minus", varname, fixed=T)
  names(data_normalized)[ ncol(data_normalized) ] = varname
}

# remove orig rank var and remove one flag
data_normalized$Rank = NULL
data_normalized$flagDRank = NULL

#remove unneeded variables
data_normalized$Username   = NULL
data_normalized$Country    = NULL
data_normalized$RankColour = NULL

#remove correlated variables
data_normalized$Standing     = NULL
data_normalized$Wins         = NULL
data_normalized$PPS          = NULL
data_normalized$VS           = NULL
data_normalized$Tetra.Rating = NULL

#make target a factor again
data_normalized$Supporter.Status. = as.factor(data_normalized$Supporter.Status.)
levels(data_normalized$Supporter.Status.) = c("No", "Yes")

str(data_normalized, give.attr=F)

#make training set with higher proportion of supporters (50/50 split)
allsupporters = data_normalized[data_normalized$Supporter.Status. == "Yes",]
allnonsupporters = data_normalized[data_normalized$Supporter.Status. == "No",]
train = rbind( allsupporters, 
               allnonsupporters[sample(1:nrow(allnonsupporters), nrow(allsupporters)),] )
#shuffle it
train = train[sample(1:nrow(train), nrow(train)),]


library("nnet")
for (i in c(1,2,3,5,10,15,20,30)) {
  neuralnet1 = nnet(Supporter.Status. ~ ., data=train, size=i, trace=F)
  
  trainPredict1 = predict(neuralnet1, train, type="class")
  trainPredictTable1 = table(train$Supporter.Status., trainPredict1)
  trainProportionTable1 = proportions(trainPredictTable1, 1)
  
  testPredict1 = predict(neuralnet1, data_normalized, type="class")
  testPredictTable1 = table(data_normalized$Supporter.Status., testPredict1)
  testProportionTable1 = proportions(testPredictTable1, 1)
  
  cat(i, "Nodes, Train Correct %:", 
        proportions(trainPredictTable1)[1] + proportions(trainPredictTable1)[4],
      ", Test Correct %:", 
      proportions(testProportionTable1)[1] + proportions(testProportionTable1)[4], "\n")  
}


#bigger net
library("neuralnet")
neuralnet2 = neuralnet(Supporter.Status. ~ ., data=train, hidden=3, thresh=0.01, rep=1, lifesign='full')
#plot(neuralnet2)
plot(neuralnet2, show.weights=F)
estimateSupport2 = predict(neuralnet2, data_normalized, type="class")

#predict() returns both Y/N output nodes, take the max
dichotPredict = 0
for (i in 1:nrow(estimateSupport2)) {
  dichotPredict[i] = ifelse(estimateSupport2[i,2] >= estimateSupport2[i,1],
    "Yes", "No")
}

estimateSupportTable2 = table(data_normalized$Supporter.Status., dichotPredict)
proportionTable2 = proportions(estimateSupportTable2, 1)
estimateSupportTable2
proportionTable2
print(proportions(estimateSupportTable2)[1] + proportions(estimateSupportTable2)[4])



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




