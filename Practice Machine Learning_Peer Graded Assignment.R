#Set the working directory and load the datsets
setwd("D:/Documents and Settings/tnjunge/Documents/R")
pmltesting <- read.csv(file="D:/Documents and Settings/tnjunge/Documents/R/pml-testing.csv", header=TRUE, sep=",",na.strings = c("NA","#DIV/0!",""))
pmltraining <- read.csv(file="D:/Documents and Settings/tnjunge/Documents/R/pml-training.csv", header=TRUE, sep=",",na.strings = c("NA","#DIV/0!",""))

dim(pmltesting);dim(pmltraining)

library(caret);library(lattice);library(ggplot2);library(randomForest)
NA_Count = sapply(1:dim(pmltraining)[2],function(x)sum(is.na(pmltraining[,x])))
NA_list = which(NA_Count>0)

pmltraining = pmltraining[,-NA_list]
pmltraining = pmltraining[,-c(1:7)]
pmltraining$classe = factor(pmltraining$classe)

##Conduct the exact cleaning on the testing datasets
NA_Count1 = sapply(1:dim(pmltesting)[2],function(x)sum(is.na(pmltesting[,x])))
NA_list1 = which(NA_Count1>0)
pmltesting = pmltesting[,-NA_list]
pmltesting = pmltesting[,-c(1:7)]
dim(pmltraining);dim(pmltesting)

#Create the test and Training datasets
InTrainPml <- createDataPartition(y =pmltraining$classe,p=0.6,list=FALSE)
trainingpml<- pmltraining[InTrainPml,]
testingpml <- pmltraining[-InTrainPml,]


#MODELLING USING THE TREE METHOD
modfitt1 <- train(classe~.,method="rpart",data=trainingpml)

pred=predict(modfitt1,newdata=testingpml)
z=confusionMatrix(pred,testingpml$classe)
z$table
##Accuracy
z$overall[1]

##From the consufion matrix the accuracy of 0.4933724 shows no purity hence the model is rejected

##RANDOM FOREST METHOD

modfitt2=randomForest(classe~., data=trainingpml, method='class')
pred2 = predict(modfitt2,testingpml,type='class') 
qplot(roll_belt, magnet_dumbbell_y, colour=classe, data=trainingpml) 

##Check the accuracy of the model
z2=confusionMatrix(pred2,testingpml$classe)
z2$table
z2$overall[1]

##Accuracy of 99% hence chosen as a model to use on the test datasets(pmltesting)

pred3 =  predict(modfitt2,pmltesting,type='class')
nofiles = length(pred3)
for (i in 1:nofiles){
  filename =  paste0("problem_id",i,".txt")
  write.table(pred3[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
pred3





