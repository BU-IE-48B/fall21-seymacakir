library(data.table)
library(ggplot2)
library(tidyr)
require(dplyr)
require(rpart)
require(caret)
require(TSrepr)
require(TSdist)
require(dtw)
require(TunePareto)
library(zoo)
require("logging")
require(stringr)
require(GGally)
require(rpart)
require(rattle)

# To avoid randomness in each run of code the randomness standardized. 
setwd('C:\\Users\\seycakir\\Downloads\\project')
set.seed(488)

# Since data needs long process, the logging used for keep track of process. 
basicConfig(level='FINEST')
addHandler(writeToFile, file="C:\\Users\\seycakir\\Downloads\\project\\Logger.log", level='DEBUG')

raw_data = fread('bulk_imbalance.csv')
#raw_data[c(1:5,(nrow(raw_data)-5):nrow(raw_data)),]
str(raw_data)
summary(as.factor(raw_data$system_direction))
tail(raw_data,30)

data = copy(raw_data[,c("date","hour","net","system_direction")])
data[,id:= 1:.N]
for( i in 1:72){
    data[, sprintf("hour_%s",i) := shift(net, i)]
}
data[system_direction ==""]$system_direction = "Positive"
data$system_direction = as.factor(data$system_direction)
tail(data)
summary(data$system_direction)

plot_data = copy(data)
plot_data[,value:= 1]
plot_data[,w_day:=as.character(wday(date))]
plot_data[,mon:=as.character(month(date))]
plot_data[,trnd:= 1:.N]

ggplot(plot_data, aes(fill= system_direction,y=value, x=hour)) + 
    geom_bar(position="stack", stat="identity") + 
    labs(
    title = "distribution of class by hour"
    )


ggplot(plot_data, aes(fill= system_direction,y=value, x=w_day)) + 
    geom_bar(position="stack", stat="identity") + 
    labs(
    title = "distribution of class by day"
    )

ggplot(plot_data, aes(fill= system_direction,y=value, x=mon)) + 
    geom_bar(position="stack", stat="identity") + 
    labs(
    title = "distribution of class by Month"
    )

# to obtain different time series length the function below is recorded to avoid to much code. And representations is recorded to use different part of the project. 

raw_time_series= function(rep,data_length,data){
tmp = copy(data)
for (j in c(12:23)){
if (data_length > 72){
for( i in 73:data_length){
    tmp[, sprintf("hour_%s",i) := shift(net, i)]
}
matrix = tmp[(id > data_length) & hour == j, 5:ncol(tmp)]
}

matrix = tmp[hour == j & id > data_length, 5:ncol(tmp)]

file_name = sprintf('%s_time%s_%s', rep, data_length,j)
fwrite(matrix,sprintf('%s/representations/%s.csv',getwd(),file_name),col.names=F)
loginfo("%s is created.",file_name)
}
}


raw_time_series("raw",72,data)
raw_time_series("raw",24*7,data)
raw_time_series("raw",24*7*2,data)

# The function below can represent data based on different maxdepth paramaters and record them. 
rpart_representation = function(rep,maxdepth,data){
for (j in c(12:23)){

traindata = data[hour == j, 4:ncol(data)]
traindata = traindata[id>72]
long_train=melt(traindata,id.vars=c('id','system_direction'))
long_train$variable = as.character(long_train$variable)
long_train[,time:=as.numeric(gsub("hour_", "", variable))]
long_train=long_train[,list(id,system_direction,time,value)]
long_train=long_train[order(id,time)]


representation = as.data.table(t((j-6):72))

for ( i in unique(traindata$id)){     
df = long_train[time > (j-7) & id==i]
# set parameters and represent data
tree_fit=rpart(value~time,df,control=rpart.control(maxdepth=maxdepth))
df [,tree_rep:=predict(tree_fit,df)]
# change actual variables with representation
representation = rbind(representation,t(df$tree_rep))
}

file_name = sprintf('%s_maxdepth%s_%s', rep, maxdepth,j)
fwrite(representation[-1,],sprintf('%s/representations/%s.csv',getwd(),file_name),col.names=F)
loginfo("%s is created.",file_name)
}
}

rpart_representation("rpart",3,data)
rpart_representation("rpart",5,data)
rpart_representation("rpart",10,data)

# The function below can represent data based on different number of important points paramaters and record them.
pla_representation = function(rep,nimportant,data){
for (j in c(12:23)){

traindata = data[hour == j, 4:ncol(data)]
traindata = traindata[id > 72]
long_train=melt(traindata,id.vars=c('id','system_direction'))
long_train$variable = as.character(long_train$variable)
long_train[,time:=as.numeric(gsub("hour_", "", variable))]
long_train=long_train[,list(id,system_direction,time,value)]
long_train=long_train[order(id,time)]
representation = as.data.table(t((j-6):72))


for ( i in unique(traindata$id)){     
 df = long_train[time > (j-7) & id==i]
 data_ts = df$value
 pla_rep=repr_pla(data_ts, times = nimportant, return = "both")
 setnames(pla_rep,c('places','points'),c('time','pla_rep'))
 pla_rep$time=pla_rep$time+(j-7 +1)

 df=merge(df,pla_rep,by='time',all.x=T)
 df[, pla_rep := na.approx(pla_rep)]

representation = rbind(representation,t(df$pla_rep))
}

 file_name = sprintf('%s_nimportant%s_%s', rep, nimportant,j)
 print(file_name)
 fwrite(representation[-1,],sprintf('%s/representations/%s.csv',getwd(),file_name),col.names=F)
 loginfo("%s is created.",file_name)
}
}

pla_representation("pla",3,data)
pla_representation("pla",5,data)
pla_representation("pla",10,data)

# The function below can represent data based on different alpahabet size and segment length paramaters and record them.
sax_representation = function(rep, sax_segment_length=4,sax_alphabet_size=5,data){
for (j in c(12:23)){

traindata = data[hour == j, 4:ncol(data)]
traindata = traindata[id > 72]
long_train=melt(traindata,id.vars=c('id','system_direction'))
long_train$variable = as.character(long_train$variable)
long_train[,time:=as.numeric(gsub("hour_", "", variable))]
long_train=long_train[,list(id,system_direction,time,value)]
long_train=long_train[order(id,time)]
representation = as.data.table(t((j-6):72))


for ( i in unique(traindata$id)){     
 df = long_train[time > (j-7) & id==i]
 data_ts = df$value
sax_rep=repr_sax(data_ts, q = sax_segment_length, a = sax_alphabet_size)
sax_rep
dummy_time=c(1:(length(sax_rep)-1))*sax_segment_length 
dummy_time=c(dummy_time,length(data_ts))
dummy_time = dummy_time + (j-7)
 
dt_sax=data.table(time=dummy_time,sax_rep_char=sax_rep)
df=merge(df,dt_sax,by='time',all.x=T)
dt_sax=data.table(time=dummy_time,sax_rep_char=sax_rep)
df[,sax_rep_char_num:=nafill(as.numeric(as.factor(sax_rep_char)),'nocb')] # from data.table  
df[,sax_rep:=mean(value),by=list(sax_rep_char_num)] 
  
representation = rbind(representation,t(df$sax_rep))
}

 file_name = sprintf('%s_segmenthlength%sapphabetsize%s_%s', rep,sax_segment_length,sax_alphabet_size ,j)
 print(file_name)
 fwrite(representation[-1,],sprintf('%s/representations/%s.csv',getwd(),file_name),col.names=F)
 loginfo("%s is created.",file_name)
}
}


sax_representation("sax",6,5,data)
sax_representation("sax",12,5,data)
sax_representation("sax",6,3,data)
sax_representation("sax",12,3,data)

df_alternative = copy(data[,c("date","hour","net","system_direction")])
df_alternative[,id:= 1:.N]
df_alternative[system_direction == "Positive"]$net = 1
df_alternative[system_direction == "Negative"]$net = -1
df_alternative[system_direction == "Neutral"]$net = 0


for( i in 1:72){
    df_alternative[, sprintf("hour_%s",i) := shift(net, i)]
}
tail(df_alternative)


for (j in c(12:23)){

matrix = df_alternative[hour == j & id > 72, 6:ncol(df_alternative)]

file_name = sprintf('classrep_time72_%s',j)
fwrite(matrix,sprintf('%s/representations/%s.csv',getwd(),file_name),col.names=F)
loginfo("%s is created.",file_name)
}


# The raw time series representations readed from representations file.
file_path = sprintf('%s\\representations\\', getwd())
file_names = list.files(file_path, full.names=T)
approaches = list.files(file_path, full.names=F)
raw_rep = as.logical(c(1:length(file_names)))
for ( i in 1:length(file_names)){
 raw_rep[i]= grepl('raw_time72', file_names[i])

}
file_names= file_names[raw_rep]
approaches = approaches[raw_rep]



for (j in c(1:length(file_names))){

df_alternative_2 = fread(file_names[j])
approach = approaches[j]
file_name = sprintf("standard%s",strsplit(approach,".c")[[1]][1])
matrix = as.matrix(df_alternative_2[,-1])
scaled_matrix = t(apply(matrix,1, function(x) (x - mean(x))/ sd(x)))

fwrite(scaled_matrix,sprintf('%s/representations/%s.csv',getwd(),file_name),col.names=F)
loginfo("%s is created.",file_name)
}



# k-nn classification function
nn_classify_cv=function(dist_matrix,train_class,test_indices,k=1){
    
    test_distances_to_train=dist_matrix[test_indices,]
    test_distances_to_train=test_distances_to_train[,-test_indices]
    train_class=train_class[-test_indices]
    
    ordered_indices=apply(test_distances_to_train,1,order)
    if(k==1){
        nearest_class=train_class[as.numeric(ordered_indices[1,])]
        nearest_class=data.table(id=test_indices,nearest_class)
    } else {
        nearest_class=apply(ordered_indices[1:k,],2,function(x) {train_class[x]})
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

# function for calculate average accuracy of each method

get_acc = function(dist_mat,nof_rep = 5,n_fold = 10, k_levels, trainclass){
  
cv_indices=generateCVRuns(trainclass, ntimes =nof_rep, nfold = n_fold, 
                          leaveOneOut = FALSE, stratified = TRUE)

result=vector('list',nof_rep*n_fold*length(k_levels))
iter=1
  
      
    for(i in 1:nof_rep){
        this_fold=cv_indices[[i]]
        for(j in 1:n_fold){
            test_indices=this_fold[[j]]
            for(k in 1:length(k_levels)){
                current_k=k_levels[k]
                current_fold=nn_classify_cv(dist_mat,trainclass,test_indices,k=current_k)   
                accuracy=sum(trainclass[test_indices]==current_fold$prediction$predicted)/length(test_indices)
                tmp=data.table(dist_approach = dist_approach,
                               hour = h ,
                               representation= representation, 
                               paramater= paramater,
                               repid=i,
                               foldid=j,
                               k=current_k,
                               acc=accuracy)
                result[[iter]]=tmp
                iter=iter+1
                
            }
            
        }
    
    }   
    

return (list(result))
}


# The distance matrix are getted from representations recorded before.
file_path = sprintf('%s\\representations\\', getwd())
file_names = list.files(file_path, full.names=T)
approaches = list.files(file_path, full.names=F)


for (m in (1:length(approaches))){
dist_approach = "euc"
approach = approaches[m]
representation = strsplit(approach,"_")[[1]][1]
paramater = strsplit(approach,"_")[[1]][2]
h = strsplit(strsplit(approach,"_")[[1]][3],".c")[[1]][1]
df = fread(file_names[m])



if (representation == "raw"){
    
    if (paramater == "time72"){
    tmp = data[id > 72 & hour == h ]
    dist_matrix = as.matrix(dist(df[1:nrow(tmp),-1]))
    diag(dist_matrix) = 2*max(dist_matrix)
    
        
    }
    if (paramater == "time168"){
    tmp = data[id > 168 & hour == h ]
    dist_matrix = as.matrix(dist(df[1:nrow(tmp),-1]))
    diag(dist_matrix) = 2*max(dist_matrix)
        
    }
    if (paramater =="time336"){
    tmp = data[id > 336 & hour == h ]
    dist_matrix = as.matrix(dist(df[1:nrow(tmp),-1]))
    diag(dist_matrix) = 2*max(dist_matrix)
        
    }
}
if (representation != "raw"){
    tmp = data[id > 72 & hour == h ]
    dist_matrix = as.matrix(dist(df[1:nrow(tmp)]))
    diag(dist_matrix) = 2*max(dist_matrix)
    
}


file_name = sprintf('%s_%s_%s_%s', dist_approach,representation,paramater,h)
fwrite(dist_matrix,sprintf('%s/distances/%s.csv',getwd(),file_name),col.names=F)
loginfo("%s is calculated.",file_name)

}

for (m in (1:length(approaches))){
dist_approach = "lcss-epsilon0.5"
approach = approaches[m]
representation = strsplit(approach,"_")[[1]][1]
paramater = strsplit(approach,"_")[[1]][2]
h = strsplit(strsplit(approach,"_")[[1]][3],".c")[[1]][1]
df = fread(file_names[m])

tmp = data[hour == h & date >= "2021-11-01"]

if (representation == "raw"){
    
    dist_matrix = as.matrix(TSDatabaseDistances(df[(nrow(df)-nrow(tmp)+1):nrow(df),-1],distance='lcss',epsilon=0.05))
    diag(dist_matrix) = 2*max(dist_matrix)
    
}
if (representation != "raw"){
    dist_matrix = as.matrix(TSDatabaseDistances(df[(nrow(df)-nrow(tmp)+1):nrow(df)],distance='lcss',epsilon=0.05))
    diag(dist_matrix) = 2*max(dist_matrix)
    
}


file_name = sprintf('%s_%s_%s_%s', dist_approach,representation,paramater,h)
fwrite(dist_matrix,sprintf('%s/distances/%s.csv',getwd(),file_name),col.names=F)
loginfo("%s is calculated.",file_name)

}


for (m in (1:length(approaches))){
dist_approach = "erp-g0.5"
approach = approaches[m]
representation = strsplit(approach,"_")[[1]][1]
paramater = strsplit(approach,"_")[[1]][2]
h = strsplit(strsplit(approach,"_")[[1]][3],".c")[[1]][1]
df = fread(file_names[m])

 tmp = data[hour == h & date >= "2021-11-01"]

if (representation == "raw"){
    
    dist_matrix = as.matrix(TSDatabaseDistances(df[(nrow(df)-nrow(tmp)+1):nrow(df),-1],distance='erp',g=0.5))
    diag(dist_matrix) = 2*max(dist_matrix)
    
}
if (representation != "raw"){
   
    dist_matrix = as.matrix(TSDatabaseDistances(df[(nrow(df)-nrow(tmp)+1):nrow(df)],distance='erp',g=0.5))
    diag(dist_matrix) = 2*max(dist_matrix)
    
}


file_name = sprintf('%s_%s_%s_%s', dist_approach,representation,paramater,h)
fwrite(dist_matrix,sprintf('%s/distances/%s.csv',getwd(),file_name),col.names=F)
loginfo("%s is calculated.",file_name)

}

for (m in (1:length(file_names))){
dist_approach = "dtw"
approach = approaches[m]
representation = strsplit(approach,"_")[[1]][1]
paramater = strsplit(approach,"_")[[1]][2]
h = strsplit(strsplit(approach,"_")[[1]][3],".c")[[1]][1]
df = fread(file_names[m])

tmp = data[ hour == h & date >= "2021-11-01"]

if (representation == "raw"){
     

    dist_matrix = as.matrix(dtwDist(df[(nrow(df)-nrow(tmp)+1):nrow(df),-1]))
    
}
if (representation != "raw"){
    dist_matrix = as.matrix(dtwDist(df[(nrow(df)-nrow(tmp)+1):nrow(df)]))
    
}

diag(dist_matrix) = 2*max(dist_matrix)


file_name = sprintf('%s_%s_%s_%s', dist_approach,representation,paramater,h)
fwrite(dist_matrix,sprintf('%s/distances/%s.csv',getwd(),file_name),col.names=F)
loginfo("%s is calculated.",file_name)

}

# recorded distances readed to avoid repeat calculations
file_path = sprintf('%s\\distances\\', getwd())
file_names = list.files(file_path, full.names=T)
approaches = list.files(file_path, full.names=F)

results = vector('list',length(file_names))
length(results)

for (m in (1:length(approaches))){

approach = approaches[m]
dist_approach = strsplit(approach,"_")[[1]][1]
representation = strsplit(approach,"_")[[1]][2]
paramater = strsplit(approach,"_")[[1]][3]
h = strsplit(strsplit(approach,"_")[[1]][4],".c")[[1]][1]
dist_matrix = as.matrix(fread(file_names[m]))

if(dist_approach == "euc"){
if (representation == "raw"){
    
    if (paramater == "time72"){
    tmp = data[id > 72 & hour == h & date < "2022-01-09"]
    
    
        
    }
    if (paramater == "time168"){
    tmp = data[id > 168 & hour == h &date < "2022-01-09"]
    
    }
    if (paramater =="time336"){
    tmp = data[id > 336 & hour == h &date < "2022-01-09"]
    
    }
}
if (representation != "raw"){
    tmp = data[id > 72 & hour == h &date < "2022-01-09"]
    
}
}

if(dist_approach != "euc"){
tmp = data[ hour == h & date >= "2021-11-01" & date < "2022-01-09"]
}





dist_matrix = dist_matrix[1:nrow(tmp),1:nrow(tmp)]
tmp_train_class = tmp$system_direction
k_levels = c(1,3,5)
tmp_train = get_acc(dist_mat =dist_matrix,k_levels= k_levels,trainclass = tmp_train_class)
tmp_train  = rbindlist(tmp_train[[1]])
results[[m]] = tmp_train[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(hour,dist_approach,representation,paramater,k)]
loginfo("%s by k-nn classification accuracies are calculated.",approach)

}


overall_results = rbindlist(results)
overall_results[order(hour,avg_acc,-sdev_acc)]


best_results = data.table(t(1:8))
for (i in 12:23){
best_results = rbind(best_results,(head(overall_results[order(-avg_acc,sdev_acc)][hour ==i],1)),use.names = FALSE)
}

mean(best_results$avg_acc)

pred_dates = unique( data[date >= "2022-01-09"]$date)

colnames(best_results) = colnames(overall_results)
best_results = best_results[-1]
best_results

predict_by_best_result = function(h,pred_date,approach,k){
tmp = data[id > 72 &  hour == h &date <= pred_date]
tmp_train_class = tmp$system_direction
file_path = sprintf("%s\\distances\\%s",getwd(),approach)
dist_matrix = fread(file_path)
dist_matrix = as.matrix(dist_matrix[1:nrow(tmp)])
prediction_indices = c(as.numeric(which.max(dist_matrix[-nrow(dist_matrix),ncol(dist_matrix)])),nrow(dist_matrix))
return(nn_classify_cv(dist_matrix, tmp_train_class, prediction_indices,k=k)$prediction[2, predicted])
}
predictions = data.table(pred_dates,hour_12 = "not_predicted",
hour_13 = "not_predicted",hour_14 = "not_predicted",hour_15 = "not_predicted",hour_16 = "not_predicted",
hour_17 = "not_predicted",hour_18 = "not_predicted",
hour_19 = "not_predicted",hour_20 = "not_predicted",hour_21 = "not_predicted",hour_22 = "not_predicted",hour_23 = "not_predicted")


for( i in 1:length(pred_dates)){
   predictions[i,2:13]= data.table( predict_by_best_result(12,pred_dates[i], "dtw_rpart_maxdepth10_12.csv",5),
                      predict_by_best_result(13,pred_dates[i], "dtw_classrep_time72_13.csv",3),
                      predict_by_best_result(14,pred_dates[i], "dtw_rpart_maxdepth3_14.csv",5),
                      predict_by_best_result(15,pred_dates[i], "euc_classrep_time72_15.csv",5),
                      predict_by_best_result(16,pred_dates[i], "erp-g0.5_standardraw_time72_16.csv",5),
                      predict_by_best_result(17,pred_dates[i], "dtw_pla_nimportant10_17.csv",5),
                      predict_by_best_result(18,pred_dates[i], "dtw_sax_segmenthlength6apphabetsize3_18.csv",5),
                      predict_by_best_result(19,pred_dates[i], "erp-g0.5_raw_time336_19.csv",5),
                      predict_by_best_result(20,pred_dates[i], "dtw_classrep_time72_20.csv",5),
                      predict_by_best_result(21,pred_dates[i], "erp-g0.5_classrep_time72_21.csv",3),
                      predict_by_best_result(22,pred_dates[i], "euc_classrep_time72_22.csv",5),
                      predict_by_best_result(23,pred_dates[i], "euc_classrep_time72_23.csv",5))
                      
}


raw_data[hour >= 12 & date == pred_dates[14]]$system_direction = "Positive"

correct = c(1:14)
for( i in 1:14){
correct[i]= sum(raw_data[hour >= 12 & date == pred_dates[i]]$system_direction == predictions[pred_dates == pred_dates[i],2:13])
}
predictions[, acc:= correct/12]
predictions

mean(predictions$acc)

data = fread('bulk_imbalance.csv')
data_w = fread('2022-01-22_weather.csv')
head(data_w)

unified = str_c(data_w$variable,"_",data_w$lon,"_",data_w$lat )
data_w[,location := unified]
data_w= data_w[,c("date","hour","location","value")]
data_w = dcast(data_w,date+hour~location )
head(data_w)


lm_data = merge(data[,c("date","hour","system_direction","net")],data_w, by = c("date","hour"))
head(lm_data)


lm_data[,w_day:=as.character(wday(date))]
lm_data[,mon:=as.character(month(date))]
lm_data[,trnd:= 1:.N]

for (i in c(36,48,60,72,24*7)){
  lm_data[,sprintf("lag_%s",i):= shift(net,i)]
  
}


lm_data = lm_data[169:nrow(lm_data)]

ggpairs(lm_data[,c("net","lag_36","lag_48","lag_60","lag_72","lag_168")])

test_data = lm_data[date >= "2022-01-09"]
lm_data = lm_data[date < "2022-01-09"]


lm_base=lm(net~trnd+as.factor(w_day)+as.factor(mon)+as.factor(hour),lm_data)
summary(lm_base)
tmp=copy(lm_data)
tmp[, pred:= predict(lm_base, tmp)]
tmp[pred>= 50,c_pred:= "Positive"]
tmp[pred<=-50,c_pred:= "Negative"]
tmp[pred<50 & pred>-50,c_pred:= "Neutral"]
print("accuracy")
sum(tmp$system_direction==tmp$c_pred) / length(tmp$system_direction)


tmp[,residuals:= net-pred]
df = tmp[,-c("net","date","pred","c_pred","system_direction")]
model_1 = rpart(residuals ~. ,df, control = rpart.control(maxdepth = 3,cp = 0) )
fancyRpartPlot(model_1)


tmp[,A:= as.numeric(TMP_2.m.above.ground_28.75_41   >= 30.451)]
tmp[, B:= as.numeric(trnd >= 22669.5)]
tmp[,C:= as.numeric(trnd   < 22817.5)]
lm_base_2 = lm(net~ trnd + factor(hour) + factor(mon) + factor(w_day) + A:B:C,tmp)
summary(lm_base_2)
tmp[,pred := predict(lm_base_2,tmp)]
tmp[pred>= 50,c_pred:= "Positive"]
tmp[pred<=-50,c_pred:= "Negative"]
tmp[pred<50 & pred>-50,c_pred:= "Neutral"]
tmp[,residuals:= net-pred]
print("Accuracy")
sum(tmp$system_direction==tmp$c_pred) / length(tmp$system_direction)


df_2  = tmp[,-c("net","date","pred","c_pred","system_direction")]
model_2 = rpart(residuals ~.-A-B-C ,df_2, control = rpart.control(maxdepth = 3,cp = 0) )
fancyRpartPlot(model_2)

tmp[,D:= as.numeric(TMP_2.m.above.ground_28.75_41   >= 27.6025 ) ]
tmp[,E:=  as.numeric( trnd    >= 14607.5) ]
tmp[,F:=as.numeric(w_day %in% c(4,5,6)) ]
lm_base_3 = lm(net~ trnd + factor(hour) + factor(mon) + factor(w_day) + A:B:C + D:E:F,tmp)
summary(lm_base_3)
tmp[,pred := predict(lm_base_3,tmp)]
tmp[pred>= 50,c_pred:= "Positive"]
tmp[pred<=-50,c_pred:= "Negative"]
tmp[pred<50 & pred>-50,c_pred:= "Neutral"]
tmp[,residuals:= net-pred]
print("Accuracy")
sum(tmp$system_direction==tmp$c_pred) / length(tmp$system_direction)


df_3 = tmp[,-c("net","date","pred","c_pred","system_direction","A","B","C","D","E","F")]
model_3 = rpart(residuals ~.,df_3, control = rpart.control(maxdepth = 3,cp = 0) )
fancyRpartPlot(model_3)


tmp[, H:= as.numeric(TMP_2.m.above.ground_30.5_39.75 >= 26.989) ]
tmp[,G:= as.numeric(lag_36       >= 4433.206 )  ]   
lm_base_4 = lm(net~ trnd + factor(hour) + factor(mon) + factor(w_day) + A:B:C + D:E:F + H:G,tmp)
summary(lm_base_4)
tmp[,pred := predict(lm_base_4,tmp)]
tmp[pred>= 50,c_pred:= "Positive"]
tmp[pred<=-50,c_pred:= "Negative"]
tmp[pred<50 & pred>-50,c_pred:= "Neutral"]
tmp[,residuals:= net-pred]
print("Accuracy")
sum(tmp$system_direction==tmp$c_pred) / length(tmp$system_direction)



test_data[system_direction == ""]$system_direction = "Positive"
test_data[, pred := predict(lm_base, test_data)]




test_data[pred>= 50,c_pred:= "Positive"]
test_data[pred<=-50,c_pred:= "Negative"]
test_data[pred<50 & pred>-50,c_pred:= "Neutral"]


test_data = test_data[hour %in% 12:23]
print("Accuracy")
sum(test_data$system_direction==test_data$c_pred) / length(test_data$system_direction)

test_data[,c("date","hour","system_direction","c_pred")]

mean(best_results$avg_acc)

mean(predictions$acc)

sum(test_data$system_direction==test_data$c_pred) / length(test_data$system_direction)

data = fread("bulk_imbalance.csv")
df= copy(data[,c("date","hour","net","system_direction")])
df[system_direction == ""]$system_direction = "Positive"

for( i in c(48,168)){
    df[, sprintf("lag_%s",i) := shift(system_direction, i)]
}
head(df[169:nrow(df)])

pred_baseline1 = df[hour >= 12 & date >= "2022-01-09",-6]
pred_baseline1

sum(pred_baseline1$system_direction==pred_baseline1$lag_48) / length(pred_baseline1$system_direction)

pred_baseline2 = df[hour >= 12 & date >= "2022-01-09",-5]
pred_baseline2

sum(pred_baseline2$system_direction==pred_baseline2$lag_168) / length(pred_baseline2$system_direction)

data[system_direction == ""]$system_direction = "Positive"
data$system_direction = as.factor(data$system_direction)
summary(data[hour > 11 & date > "2022-01-09"]$system_direction)

