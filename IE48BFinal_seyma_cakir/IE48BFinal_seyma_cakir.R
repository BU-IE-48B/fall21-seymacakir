## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(include = TRUE, echo = TRUE, warning = FALSE, message = FALSE)


## ---- echo=FALSE-------------------------------------------------------------------------------------
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
require(lubridate)
require(forecast)
require(rattle)


## ----------------------------------------------------------------------------------------------------
setwd('D:\\Users\\seyma\\Documents\\GitHub\\fall21-seymacakir\\IE48BFinal_seyma_cakir')
set.seed(448)


## ----------------------------------------------------------------------------------------------------
raw_data = fread("production_with_weather_data.csv")
tail(raw_data)
str(raw_data)
summary(raw_data)
data = raw_data[date < "2021-11-01" ]
tail(raw_data)
test_data = raw_data[date >= "2021-11-01" ]
head(test_data)


## ----------------------------------------------------------------------------------------------------
ggplot(data, aes(x= date, y=production)) + geom_line() + labs( title = "Production During Time", xlab = "time", y= "Production") + theme_minimal()


## ----------------------------------------------------------------------------------------------------
data[,w_day:=as.character(wday(date,label=T))]
data[,mon:=as.character(month(date,label=T))]
data[,trnd:= 1:.N]

head(data[,c("date","hour","w_day","mon","hour","trnd","production")])

lm_base=lm(production~trnd+w_day+mon+as.factor(hour),data)
summary(lm_base)



## ----------------------------------------------------------------------------------------------------
tmp=copy(data)
tmp[,actual:=production]
tmp[,predicted:=predict(lm_base,tmp)]
tmp[,residual:= actual-predicted]
#head(tmp)
ggplot(tmp ,aes(x=date)) +
        geom_line(aes(y=actual,color='real')) + 
        geom_line(aes(y=predicted,color='predicted'))



## ----------------------------------------------------------------------------------------------------
#checkresiduals(lm_base)


## ----------------------------------------------------------------------------------------------------

library(GGally)
lag_data = data[,"production"]

for( i in c(24,36,48,72) ){
 lag_data[, sprintf("lag_%s",i) := shift(production,i)]

}
ggpairs(lag_data)

acf(data$production, 72)



## ----------------------------------------------------------------------------------------------------
for( i in c(24,36,48,72)  ){
 tmp[, sprintf("lag_%s",i) := shift(production,i)]

}


## ---- out.width= '%100'------------------------------------------------------------------------------
head(tmp)
library(rattle)

tmp = tmp[73:nrow(tmp)]

fit_res_tree=rpart(residual~ .-production-actual-date-predicted,tmp,
                   control=rpart.control(cp=0,maxdepth=4))
fancyRpartPlot(fit_res_tree)
summary(fit_res_tree)


## ----------------------------------------------------------------------------------------------------
tmp[,is_DSWRF_surface_39_35.25 := DSWRF_surface_39_35.25 <589.358]
tmp[,is_lag_36:= lag_36 < 0.000950]
tmp[,is_TCDC_TCDC_entire.atmosphere_38.75_35:= TCDC_entire.atmosphere_38.75_35 >= 63]
tmp[,is_TCDC_low.cloud.layer_38_35.75:= TCDC_low.cloud.layer_38_35.75 >= 0.1]



## ----------------------------------------------------------------------------------------------------

lm_base_2=lm(production~trnd+w_day+mon+as.factor(hour)+ is_DSWRF_surface_39_35.25:is_TCDC_TCDC_entire.atmosphere_38.75_35:is_lag_36:is_TCDC_low.cloud.layer_38_35.75,tmp)

summary(lm_base_2)


## ----------------------------------------------------------------------------------------------------

tmp_v2 = copy(tmp)

tmp_v2$predicted =predict(lm_base_2,tmp_v2)
tmp_v2$residual= tmp_v2$actual-tmp_v2$predicted

fit_res_tree_2=rpart(residual~ .-production-actual-date-predicted -is_DSWRF_surface_39_35.25-is_TCDC_TCDC_entire.atmosphere_38.75_35-is_lag_36- is_TCDC_low.cloud.layer_38_35.75
                   ,tmp_v2,
                   control=rpart.control(cp=0,maxdepth=4))
fancyRpartPlot(fit_res_tree_2)


## ----------------------------------------------------------------------------------------------------
tmp_v2[,is_lag24 := lag_24 <146]
tmp_v2[,is_TCDC_middle.cloud.layer_38.75_35.75 := TCDC_middle.cloud.layer_38.75_35.75 < 5.7 ]
tmp_v2[,is_TCDC_low.cloud.layer_38.5_35.25 := TCDC_low.cloud.layer_38.5_35.25 < 93 ]
tmp_v2[,is_DSWRF_surface_38.75_35.25 := DSWRF_surface_38.75_35.25 < 435 ]



## ----------------------------------------------------------------------------------------------------
lm_base_3=lm(production~trnd+w_day+mon+as.factor(hour)+ is_DSWRF_surface_39_35.25:is_TCDC_TCDC_entire.atmosphere_38.75_35:is_lag_36:is_TCDC_low.cloud.layer_38_35.75 +is_lag24:is_TCDC_middle.cloud.layer_38.75_35.75:is_TCDC_low.cloud.layer_38.5_35.25:is_DSWRF_surface_38.75_35.25
               ,tmp_v2)
summary(lm_base_3)


## ----------------------------------------------------------------------------------------------------
tmp_v3 = copy(tmp_v2)

tmp_v3$predicted =predict(lm_base_3,tmp_v3)
tmp_v3$residual= tmp_v3$actual-tmp_v3$predicted

fit_res_tree_3=rpart(residual~ .-production-actual-date-predicted -is_DSWRF_surface_39_35.25-is_TCDC_TCDC_entire.atmosphere_38.75_35-is_lag_36- is_TCDC_low.cloud.layer_38_35.75 - is_lag24-is_TCDC_middle.cloud.layer_38.75_35.75-is_TCDC_low.cloud.layer_38.5_35.25-is_DSWRF_surface_38.75_35.25
                   ,tmp_v3,
                   control=rpart.control(cp=0,maxdepth=4))
fancyRpartPlot(fit_res_tree_3)


## ----------------------------------------------------------------------------------------------------
#summary(fit_res_tree_3)
tmp_v3[, is_lag_24_2 := lag_24 >  65.37035]
tmp_v3[, is_TCDC_entire.atmosphere_38_35.25 := TCDC_entire.atmosphere_38_35.25 >= 76.9]
tmp_v3[, is_lag_24_3 := lag_24 <146]
tmp_v3[, is_lag_48_2 :=lag_48 >= 138]


## ----------------------------------------------------------------------------------------------------
lm_base_4=lm(production~trnd+w_day+mon+as.factor(hour)+ is_DSWRF_surface_39_35.25:is_TCDC_TCDC_entire.atmosphere_38.75_35:is_lag_36:is_TCDC_low.cloud.layer_38_35.75 +is_lag24:is_TCDC_middle.cloud.layer_38.75_35.75:is_TCDC_low.cloud.layer_38.5_35.25:is_DSWRF_surface_38.75_35.25 + is_lag_24_2:is_TCDC_entire.atmosphere_38_35.25:is_lag_24_3:is_lag_48_2
               ,tmp_v3)
summary(lm_base_4)


## ----------------------------------------------------------------------------------------------------
#checkresiduals(lm_base_4)


## ----------------------------------------------------------------------------------------------------
raw_data = fread("production_with_weather_data.csv")
raw_data[,w_day:=as.character(wday(date,label=T))]
raw_data[,mon:=as.character(month(date,label=T))]
raw_data[,trnd:= 1:.N]
for( i in c(24,36,48,72)  ){
 raw_data[, sprintf("lag_%s",i) := shift(production,i)]

}




raw_data[,is_DSWRF_surface_39_35.25 := DSWRF_surface_39_35.25 <589.358]
raw_data[,is_lag_36:= lag_36 < 0.000950]
raw_data[,is_TCDC_TCDC_entire.atmosphere_38.75_35:= TCDC_entire.atmosphere_38.75_35 >= 63]
raw_data[,is_TCDC_low.cloud.layer_38_35.75:= TCDC_low.cloud.layer_38_35.75 >= 0.1]
raw_data[,is_lag24 := lag_24 <146]
raw_data[,is_TCDC_middle.cloud.layer_38.75_35.75 := TCDC_middle.cloud.layer_38.75_35.75 < 5.7 ]
raw_data[,is_TCDC_low.cloud.layer_38.5_35.25 := TCDC_low.cloud.layer_38.5_35.25 < 93 ]
raw_data[,is_DSWRF_surface_38.75_35.25 := DSWRF_surface_38.75_35.25 < 435 ]
raw_data[, is_lag_24_2 := lag_24 >  65.37035]
raw_data[, is_TCDC_entire.atmosphere_38_35.25 := TCDC_entire.atmosphere_38_35.25 >= 76.9]
raw_data[, is_lag_24_3 := lag_24 <146]
raw_data[, is_lag_48_2 :=lag_48 >= 138]
raw_data[is.na(is_TCDC_low.cloud.layer_38.5_35.25)]$is_TCDC_low.cloud.layer_38.5_35.25 = TRUE
raw_data[is.na(is_DSWRF_surface_38.75_35.25)]$is_DSWRF_surface_38.75_35.25 = TRUE 
raw_data[is.na(is_TCDC_low.cloud.layer_38_35.75)]$is_TCDC_low.cloud.layer_38_35.75 = TRUE
raw_data[is.na(is_DSWRF_surface_39_35.25)]$is_DSWRF_surface_39_35.25 = TRUE

test_data = raw_data[date>=  "2021-11-01"  ]
head(test_data)

summary(test_data)
model_predictions = as.numeric(predict(lm_base_4,test_data))
model_predictions[model_predictions<0] = 0
actual = test_data$production

residuals = as.numeric(abs(actual -model_predictions))
model_wampe = sum(residuals)/sum(actual)
base_line_predictions = test_data$lag_48
base_line_wampe = sum(abs(base_line_predictions- actual)) / sum(actual)

print("model based wampe")
print(model_wampe)
print("base line  wampe")
print(base_line_wampe)

