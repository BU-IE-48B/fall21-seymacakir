

# import libraries 
library(data.table)
library(ggplot2)
library(tidyr)
require(dplyr)
require(rpart)
require(caret)
library(tidyverse)
require(TSrepr)
require(TSdist)
require(dtw)
require(TunePareto)

# set directory and set seed to prevent randomness each calculation.  
setwd('D:\\Users\\seyma\\Documents\\GitHub\\fall21-seymacakir\\midterm')
set.seed(448)

# read data
main_path <- getwd()

data_path=sprintf('%s/imbalance_series.csv',main_path)
data <-as.data.table( fread(data_path))

# assign id for each observation 
data[,id:= 1:.N]


# check if there is equality 

data[net_imb==0]

#assign classes

data[net_imb>0, class:= 1 ] 
data[net_imb<0]$class = -1 

data$class = as.factor(data$class)

data[,diff := net_imb -shift(net_imb,1)]



# create a prediction tabse 
prediction_hours = data.table(hour = 0:23, class =0)

# split data as test and train
train = data[date < '2021-09-25']
train[, baseline_1:= 0]


test = tail(data,7*24)
test[,baseline_1 := 0]

# assign the most common classe as prediction for particular hour. 

predictions <-  as.data.table(train %>% group_by(hour) %>%
                                summarise(class_1 = sum(class==1),
                                          class_2  = sum(class== -1) ))

predictions[  class_1 > class_2 , predicted := 1]
predictions[  class_1 < class_2]$predicted = -1

for ( i in (0:23)){
  train[hour == i]$baseline_1 = predictions[hour == i]$predicted
  test[hour == i]$baseline_1 = predictions[hour == i]$predicted
  
}
# create confusion matrix for calculate accuracy
print("train data")
confusionMatrix(data= as.factor(train$baseline_1), reference = as.factor(train$class))

print("test data")
confusionMatrix(data= as.factor(test$baseline_1), reference = as.factor(test$class))


# create a column for predicitons of baseline 1 

data[,baseline_1 := c(train$baseline_1, test$baseline_1)]




# add the class of previous hours but extract last three hours since it will be unkown at prediction time.

for( i in c(4:8) ){
  data[, sprintf("hour_%s",i) := shift(class,i)]
}

# divide data as test and train 
train = data[date < '2021-09-25']
train[, baseline_2:= 0]


test = tail(data,7*24)
test[,baseline_2 := 0]

# based on the most frequent imbalance type for the last five hours obseved the predictions assigned. 

for ( i in 1:1320) {
  count_class1 = sum(train[i, c(10:14)] == 1,na.rm = TRUE)
  count_class2 = sum(train[i, c(10:14)] == -1, na.rm = TRUE)
  if(count_class1 > count_class2){
    train[i]$baseline_2 = 1
  }
  else {
    train[i]$baseline_2 = -1
  }
  
}

for ( i in 1:nrow(test)) {
  count_class1 = sum(test[i, c(10:14)] == 1,na.rm = TRUE)
  count_class2 = sum(test[i, c(10:14)] == -1, na.rm = TRUE)
  if(count_class1 > count_class2){
    test[i]$baseline_2 = 1
  }
  else {
    test[i]$baseline_2 = -1
  }
  
}

# create confusion matrix for calculate accuracy

print("train data")
confusionMatrix(data= as.factor(train$baseline_2), reference = as.factor(train$class))
print("test data")
confusionMatrix(data= as.factor(test$baseline_2), reference = as.factor(test$class))


# read data again
main_path <- getwd()

data_path=sprintf('%s/imbalance_series.csv',main_path)
data <-as.data.table( fread(data_path))

# assign id for each observation 
data[,id:= 1:.N]

# check if there is equality 

data[net_imb==0]

# no equality two classes is observed 
#assign classes

data[net_imb>0, class:= 1 ] 
data[net_imb<0]$class = -1 

data$class = as.factor(data$class)

data[,diff := net_imb -shift(net_imb,1)]


# get time series by previous hours for each time. 

for( i in c(1:24) ){
  
  
  data[, sprintf("hour_%s",i) := shift(net_imb,i)]
  
}

for( i in c(1:24) ){
  
  
  data[, sprintf("diff_%s",i) := shift(diff,i)]
  
}



# function of k-nn classifier for k 1,5,10

k_levels=c(1,5,10)

nn_classify_cv=function(dist_matrix,train_class,test_indices,k=1){
  
  test_distances_to_train=dist_matrix[test_indices,]
  test_distances_to_train=test_distances_to_train[,-test_indices]
  train_class=train_class[-test_indices]
  #print(str(test_distances_to_train))
  ordered_indices=apply(test_distances_to_train,1,order)
  if(k==1){
    nearest_class=as.numeric(trainclass[as.numeric(ordered_indices[1,])])
    nearest_class=data.table(id=test_indices,nearest_class)
  } else {
    nearest_class=apply(ordered_indices[1:k,],2,function(x) {trainclass[x]})
    nearest_class=data.table(id=test_indices,t(nearest_class))
  }
  
  long_nn_class=melt(nearest_class,'id')
  
  class_counts=long_nn_class[,.N,list(id,value)]
  class_counts[,predicted_prob:=N/k]
  wide_class_prob_predictions=dcast(class_counts,id~value,value.var='predicted_prob')
  wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
  class_predictions=class_counts[,list(predicted=value[which.max(N)]),by=list(id)]
  
  
  return(list(prediction=class_predictions,prob_estimates=wide_class_prob_predictions))
  
}

# the cross validation paramters set. 
nof_rep=3
n_fold=5


for(i in 0:23){
  
  # data divided by hours and the time-series are selected. 
  traindata <- data[hour == i ,12:32][2:55]
  
  trainclass <- data[hour == i ,"class"][2:55]$class
  diff_train <- data[hour == i ,37:56][2:55]
  testdata <- data[hour == i ,12:32][56:62]
  testclass <- data[hour == i ,"class"][56:62]$class
  
  
  cv_indices=generateCVRuns(trainclass, ntimes =nof_rep, nfold = n_fold, 
                            leaveOneOut = FALSE, stratified = TRUE)
  
  # calculate distances and store them to save time
  
  large_number=1000000000
  dist_euc=as.matrix(dist(traindata))
  diag(dist_euc)=large_number
  fwrite(dist_euc,sprintf('%s/distances/24/euc_raw_dist-%s.csv',main_path,i),col.names=F)
  
  dist_diff_euc=as.matrix(dist(diff_train))
  diag(dist_diff_euc)=large_number
  fwrite(dist_diff_euc,sprintf('%s/distances/24/euc_diff_dist-%s.csv',main_path,i),col.names=F)
  
  dist_dtw=as.matrix(dtwDist(traindata))
  diag(dist_dtw)=large_number
  fwrite(dist_dtw,sprintf('%s/distances/24/dtw_raw_dist-%s.csv',main_path,i),col.names=F)
  
  dist_lcss=TSDatabaseDistances(traindata,distance='lcss', epsilon = 0.05)
  dist_lcss=as.matrix(dist_lcss)
  diag(dist_lcss)=large_number
  fwrite(dist_lcss,sprintf('%s/distances/24/lcss_raw-%s.csv',main_path,i),col.names=F)  
  
  dist_folder=sprintf('%s/distances/24/',main_path)
  dist_files=list.files(dist_folder, full.names=T)
}

# the accuracy values are calculated vy cross validation

result=vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))
iter=1
for(m in 1:length(dist_files)){ 
  #print(dist_files[m])
  dist_mat=as.matrix(fread(dist_files[m],header=FALSE))
  for(i in 1:nof_rep){
    this_fold=cv_indices[[i]]
    for(j in 1:n_fold){
      test_indices=this_fold[[j]]
      for(k in 1:length(k_levels)){
        current_k=k_levels[k]
        current_fold=nn_classify_cv(dist_mat,trainclass,test_indices,k=current_k)
        accuracy=sum(trainclass[test_indices]==current_fold$prediction$predicted)/length(test_indices)
        approach= str_split(str_split(dist_files[m],"/")[[1]][10], "-")[[1]][1]
        tmp=data.table(time_interval= 24, approach = approach, hour=str_split(dist_files[m],"-")[[1]][3],repid=i,foldid=j,
                       k=current_k,acc=accuracy)
        result[[iter]]=tmp
        iter=iter+1
        
      }
      
    }
    
  }   
  
}

# results recorded as list 

list_of_24=rbindlist(result)

for(i in 0:23){
  
  # data divided by hours and the time-series are selected.
  traindata <- data[hour == i ,12:20][2:55]
  trainclass <- data[hour == i ,"class"][2:55]$class
  traindata <- data[hour == i ,12:20][2:55]
  diff_train <- data[hour == i ,37:44][2:55]
  testclass <- data[hour == i ,"class"][56:62]$class
  
  
  cv_indices=generateCVRuns(trainclass, ntimes =nof_rep, nfold = n_fold, 
                            leaveOneOut = FALSE, stratified = TRUE)
  
  # calculate distances store them to save time
  
  large_number=1000000000
  dist_euc=as.matrix(dist(traindata))
  diag(dist_euc)=large_number
  fwrite(dist_euc,sprintf('%s/distances/12/euc_raw_dist-%s.csv',main_path,i),col.names=F)
  
  dist_diff_euc=as.matrix(dist(diff_train))
  diag(dist_diff_euc)=large_number
  fwrite(dist_diff_euc,sprintf('%s/distances/12/euc_diff_dist-%s.csv',main_path,i),col.names=F)
  
  dist_dtw=as.matrix(dtwDist(traindata))
  diag(dist_dtw)=large_number
  fwrite(dist_dtw,sprintf('%s/distances/12/dtw_raw_dist-%s.csv',main_path,i),col.names=F)
  
  dist_lcss=TSDatabaseDistances(traindata,distance='lcss', epsilon = 0.05)
  dist_lcss=as.matrix(dist_lcss)
  diag(dist_lcss)=large_number
  fwrite(dist_lcss,sprintf('%s/distances/12/lcss_raw-%s.csv',main_path,i),col.names=F)  
  
  dist_folder=sprintf('%s/distances/12/',main_path)
  dist_files=list.files(dist_folder, full.names=T)
}

# the accuracy values are calculated 

result=vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))
iter=1
for(m in 1:length(dist_files)){ 
  #print(dist_files[m])
  dist_mat=as.matrix(fread(dist_files[m],header=FALSE))
  for(i in 1:nof_rep){
    this_fold=cv_indices[[i]]
    for(j in 1:n_fold){
      test_indices=this_fold[[j]]
      for(k in 1:length(k_levels)){
        current_k=k_levels[k]
        current_fold=nn_classify_cv(dist_mat,trainclass,test_indices,k=current_k)
        accuracy=sum(trainclass[test_indices]==current_fold$prediction$predicted)/length(test_indices)
        approach= str_split(str_split(dist_files[m],"/")[[1]][10], "-")[[1]][1]
        tmp=data.table(time_interval = 12, approach = approach, hour=str_split(dist_files[m],"-")[[1]][3],repid=i,foldid=j,
                       k=current_k,acc=accuracy)
        result[[iter]]=tmp
        iter=iter+1
        
      }
      
    }
    
  }   
  
}
# the results are recorded as list 
list_of_12=rbindlist(result)


for(i in 0:23){
  traindata <- data[hour == i ,12:17][2:55]
  diff_train <- data[hour == i ,37:41][2:55]
  trainclass <- data[hour == i ,"class"][2:55]$class
  testdata <- data[hour == i ,12:17][56:62]
  testclass <- data[hour == i ,"class"][56:62]$class
  
  
  cv_indices=generateCVRuns(trainclass, ntimes =nof_rep, nfold = n_fold, 
                            leaveOneOut = FALSE, stratified = TRUE)
  
  # calculate distances and store them to save time
  
  large_number=1000000000
  dist_euc=as.matrix(dist(traindata))
  diag(dist_euc)=large_number
  fwrite(dist_euc,sprintf('%s/distances/9/euc_raw_dist-%s.csv',main_path,i),col.names=F)
  
  dist_diff_euc=as.matrix(dist(diff_train))
  diag(dist_diff_euc)=large_number
  fwrite(dist_diff_euc,sprintf('%s/distances/9/euc_diff_dist-%s.csv',main_path,i),col.names=F)
  
  dist_dtw=as.matrix(dtwDist(traindata))
  diag(dist_dtw)=large_number
  fwrite(dist_dtw,sprintf('%s/distances/9/dtw_raw_dist-%s.csv',main_path,i),col.names=F)
  
  dist_lcss=TSDatabaseDistances(traindata,distance='lcss', epsilon = 0.05)
  dist_lcss=as.matrix(dist_lcss)
  diag(dist_lcss)=large_number
  fwrite(dist_lcss,sprintf('%s/distances/9/lcss_raw-%s.csv',main_path,i),col.names=F)  
  
  dist_folder=sprintf('%s/distances/9/',main_path)
  dist_files=list.files(dist_folder, full.names=T)
}

# accuracy values are calculated 

result=vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))
iter=1
for(m in 1:length(dist_files)){ 
  #print(dist_files[m])
  dist_mat=as.matrix(fread(dist_files[m],header=FALSE))
  for(i in 1:nof_rep){
    this_fold=cv_indices[[i]]
    for(j in 1:n_fold){
      test_indices=this_fold[[j]]
      for(k in 1:length(k_levels)){
        current_k=k_levels[k]
        current_fold=nn_classify_cv(dist_mat,trainclass,test_indices,k=current_k)
        accuracy=sum(trainclass[test_indices]==current_fold$prediction$predicted)/length(test_indices)
        approach= str_split(str_split(dist_files[m],"/")[[1]][10], "-")[[1]][1]
        tmp=data.table(time_interval = 9, approach = approach, hour=str_split(dist_files[m],"-")[[1]][3],repid=i,foldid=j,
                       k=current_k,acc=accuracy)
        result[[iter]]=tmp
        iter=iter+1
        
      }
      
    }
    
  }   
  
}

# results are recorded as list 

list_of_9=rbindlist(result)



# results merged and grouped to calculate avarage accuracy 

overall_results = rbind(list_of_24,list_of_12,list_of_9)
overall_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k,time_interval)]

# read data again
main_path <- getwd()

data_path=sprintf('%s/imbalance_series.csv',main_path)
data <-as.data.table( fread(data_path))

# assign id for each observation 
data[,id:= 1:.N]

# check if there is equality 

data[net_imb==0]

# no equality two classes is observed 
#assign classes

data[net_imb>0, class:= 1 ] 
data[net_imb<0]$class = -1 

data$class = as.factor(data$class)

data[,diff := net_imb -shift(net_imb,1)]


# get time series by previous hours for each time. 

for( i in c(1:7) ){
  
  
  data[, sprintf("day_%s",i) := shift(net_imb,i*24)]
  
}


# the paramter is set. 
nof_rep=3
n_fold=5
k_levels=c(1,5,10)

# data split as test and train. 
traindata <- data[73:(1488-7*24), 9:11]
trainclass <- data[73:(1488-7*24)]$class

cv_indices=generateCVRuns(trainclass, ntimes =nof_rep, nfold = n_fold, 
                          leaveOneOut = FALSE, stratified = TRUE)

# calculate distances and store them to save time

large_number=1000000000
dist_euc=as.matrix(dist(traindata))
diag(dist_euc)=large_number
fwrite(dist_euc,sprintf('%s/distances/3/euc_raw_dist.csv',main_path),col.names=F)

# This calculations is needed too much effort so it is not calculated. 
#dist_dtw=as.matrix(dtwDist(traindata))
#diag(dist_dtw)=large_number
#fwrite(dist_dtw,sprintf('%s/distances/3/dtw_raw_dist.csv',main_path),col.names=F)

#dist_lcss=TSDatabaseDistances(traindata,distance='lcss', epsilon = 0.05)
#dist_lcss=as.matrix(dist_lcss)
#diag(dist_lcss)=large_number
#fwrite(dist_lcss,sprintf('%s/distances/3/lcss_raw.csv',main_path,i),col.names=F)  

dist_folder=sprintf('%s/distances/3/',main_path)
dist_files=list.files(dist_folder, full.names=T)


# the accuracy values calculated

result=vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))
iter=1
for(m in 1:length(dist_files)){ 
  #print(dist_files[m])
  dist_mat=as.matrix(fread(dist_files[m],header=FALSE))
  for(i in 1:nof_rep){
    this_fold=cv_indices[[i]]
    for(j in 1:n_fold){
      test_indices=this_fold[[j]]
      for(k in 1:length(k_levels)){
        current_k=k_levels[k]
        current_fold=nn_classify_cv(dist_mat,trainclass,test_indices,k=current_k)
        accuracy=sum(trainclass[test_indices]==current_fold$prediction$predicted)/length(test_indices)
        
        tmp=data.table(time_interval= "3 days", approach = "dist_euc_raw",repid=i,foldid=j,
                       k=current_k,acc=accuracy)
        result[[iter]]=tmp
        iter=iter+1
        
      }
      
    }
    
  }   
  
}
list_of_3days=rbindlist(result)


# create train and test data 
traindata <- data[(7*24 +1):(1488-7*24), 9:15]
trainclass <- data[(7*24 +1 ):(1488-7*24)]$class
cv_indices=generateCVRuns(trainclass, ntimes =nof_rep, nfold = n_fold, 
                          leaveOneOut = FALSE, stratified = TRUE)

# calculate distances and store them to save time

large_number=1000000000
dist_euc=as.matrix(dist(traindata))
diag(dist_euc)=large_number
fwrite(dist_euc,sprintf('%s/distances/7/euc_raw_dist.csv',main_path),col.names=F)

#dist_dtw=as.matrix(dtwDist(traindata))
#diag(dist_dtw)=large_number
#fwrite(dist_dtw,sprintf('%s/distances/7/dtw_raw_dist.csv',main_path),col.names=F)

#dist_lcss=TSDatabaseDistances(traindata,distance='lcss', epsilon = 0.05)
#dist_lcss=as.matrix(dist_lcss)
#diag(dist_lcss)=large_number
#fwrite(dist_lcss,sprintf('%s/distances/7/lcss_raw.csv',main_path,i),col.names=F)  

dist_folder=sprintf('%s/distances/7/',main_path)
dist_files=list.files(dist_folder, full.names=T)


# the accuracy values calculated

result=vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))
iter=1
for(m in 1:length(dist_files)){ 
  #print(dist_files[m])
  dist_mat=as.matrix(fread(dist_files[m],header=FALSE))
  for(i in 1:nof_rep){
    this_fold=cv_indices[[i]]
    for(j in 1:n_fold){
      test_indices=this_fold[[j]]
      for(k in 1:length(k_levels)){
        current_k=k_levels[k]
        current_fold=nn_classify_cv(dist_mat,trainclass,test_indices,k=current_k)
        accuracy=sum(trainclass[test_indices]==current_fold$prediction$predicted)/length(test_indices)
        
        tmp=data.table(time_interval= "7 days", approach = "dist_euc_raw",repid=i,foldid=j,
                       k=current_k,acc=accuracy)
        result[[iter]]=tmp
        iter=iter+1
        
      }
      
    }
    
  }   
  
}

list_of_7days=rbindlist(result)

overall_results = rbind(list_of_3days,list_of_7days)
overall_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k,time_interval)]

overall_results = rbind(list_of_3days,list_of_7days)
x <- overall_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k,time_interval)]
overall_results = rbind(list_of_24,list_of_12,list_of_9)
y<- overall_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k,time_interval)]


ggplot(overall_results,aes(x=paste0(approach,'+',k, '+', time_interval ), y=acc)) +
  geom_boxplot() + labs(
    title = "Resulst of Accuracy",
    x = "Methods",
    y = "Accuracy"
  ) + 
  coord_flip()

results <- rbind(x,y)
results[avg_acc == max(results$avg_acc)]


# read data again
main_path <- getwd()

data_path=sprintf('%s/imbalance_series.csv',main_path)
data <-as.data.table( fread(data_path))

# assign id for each observation 
data[,id:= 1:.N]

# check if there is equality 

data[net_imb==0]

# no equality two classes is observed 
#assign classes

data[net_imb>0, class:= 1 ] 
data[net_imb<0]$class = -1 

data$class = as.factor(data$class)

data[,diff := net_imb -shift(net_imb,1)]


# get time series by previous hours for each time. 

for( i in c(1:24) ){
  
  
  data[, sprintf("hour_%s",i) := shift(net_imb,i)]
  
}

# predictions 

for(i in 0:23){
  
  testdata <- data[hour == i ,12:17][56:62]
  testclass <- data[hour == i ,"class"][56:62]$class
  
  
  #cv_indices=generateCVRuns(trainclass, ntimes =nof_rep, nfold = n_fold, 
  #                          leaveOneOut = FALSE, stratified = TRUE)
  
  # calculate distances and store them to save time
  
  large_number=1000000000
  dist_euc=as.matrix(dist(testdata))
  diag(dist_euc)=large_number
  fwrite(dist_euc,sprintf('%s/distances/test/euc_raw_dist-%s.csv',main_path,i),col.names=F)
  
  dist_folder=sprintf('%s/distances/test/',main_path)
  dist_files=list.files(dist_folder, full.names=T)
}

# accuracy values are calculated 
# I xould not calculate since time is not enough for me. 










