rm(list=ls(all=TRUE))
setwd("C:\\Users\\Anoop\\Desktop\\INSOFE\\Project")

## IMPORTING THE DATA AND REPLACING '99999' WITH NA's
agency_data=read.csv(file="agency_final.csv", header=TRUE, sep=",",na.strings =99999)

## REMOVING THE AGENCY_ID
agency_data = agency_data[,-2]

## TAKING ONLY THE RELEVANT DATA
agency_data_relevant= agency_data[,1:18]
write.csv(agency_data_relevant,"C:/Users/Anoop/Desktop/INSOFE/Project/data_relevant.csv", row.names=F)

## IMPORTING THE RELEVANT DATA SET
agency_data_partial=read.csv(file="C:\\Users\\Anoop\\Desktop\\INSOFE\\Project\\data_relevant.csv", header=TRUE, sep=",")

## CHECKING THE NUMBER OF NA's
na_count <-sapply(agency_data, function(y) sum(is.na(y)))
data.frame(na_count)

## OVERVIEW OF THE DATA
str(agency_data)
summary(agency_data)

## SUBSETTING THE DATA WHICH IS TO BE IMPUTED
agency_data_impute=agency_data_relevant[,c(1:5,8,11)]

## IMPUTING THE SUBSETTED DATA
library(DMwR)
imputed_data<-knnImputation(agency_data_impute,scale=T,k=5) #KNN Imputation
sum(is.na(imputed_data))

## STORING THE IMPUTED DATA SET 
write.csv(imputed_data,"C:/Users/Anoop/Desktop/INSOFE/Project/data_IMPUTED.csv", row.names=F)

## IMPORTING THE IMPUTED DATA SET
imputed_data=read.csv(file="C:\\Users\\Anoop\\Desktop\\INSOFE\\Project\\data_IMPUTED.csv", header=TRUE, sep=",")

## REMOVE THE FEATURES WHICH WERE SUBSETTED FOR IMPUTATION FROM THE ORIGINAL DATA SET
agency_data_partial = agency_data_relevant[,-c(1:5,8,11)]
write.csv(agency_data_partial,"C:/Users/Anoop/Desktop/INSOFE/Project/agency_data_partial.csv", row.names=F)
agency_data_partial=read.csv(file="C:\\Users\\Anoop\\Desktop\\INSOFE\\Project\\agency_data_partial.csv", header=TRUE, sep=",")

## ATTACHING THE IMPUTED DATA SET TO THE ORIGINAL DATA SET
final_data1=data.frame(agency_data_partial,imputed_data)
write.csv(final_data,"C:/Users/Anoop/Desktop/INSOFE/Project/final_data.csv", row.names=F)

## IMPORTING THE FINAL DATA
final_data=read.csv(file="C:\\Users\\Anoop\\Desktop\\INSOFE\\Project\\final_data.csv", header=TRUE, sep=",")
final_data = final_data[,-1]

na_count <-sapply(final_data, function(y) sum(is.na(y)))
data.frame(na_count)

summary(final_data1)
str(final_data1)

rm(agency_data)
rm(agency_data_partial)
rm(imputed_data)
rm(agency_data_relevant)

final_data$RETENTION_POLY_QTY<- as.numeric(final_data$RETENTION_POLY_QTY)

# RETENTION_POLY_QTY
for (i in 1:nrow(final_data)){
  final_data$RETENTION_POLY_QTY[i] <- final_data$RETENTION_POLY_QTY[i]*(12/final_data$MONTHS[i])}
write.csv(final_data,"final_data.csv")

## PREV_POLY_INFORCE_QTY
for (i in 1:nrow(final_data)){
  final_data$PREV_POLY_INFORCE_QTY[i] <- final_data$PREV_POLY_INFORCE_QTY[i]*(12/final_data$MONTHS[i])}
write.csv(final_data,"final_data.csv")

###########################################################################################################

## COMPUTATION OF RETENTION_RATIO ( RETENTION_POLY_QTY / PREV_POLY_INFORCE_QTY )
for (i in 1:nrow(final_data)){
  if (final_data$RETENTION_POLY_QTY[i]==0 )
    {
    final_data$RETENTION_RATIO[i]=0
    }
  else
    if(final_data$RETENTION_POLY_QTY[i]!=0 && final_data$PREV_POLY_INFORCE_QTY[i]==0)
    {
      final_data$RETENTION_RATIO[i]=0.78
    }
  else
    {
  final_data$RETENTION_RATIO[i] <- (final_data$RETENTION_POLY_QTY[i]/final_data$PREV_POLY_INFORCE_QTY[i])
    }
}

##OUTLIER REMOVAL
summary(final_data)

IQR_RETENTION_RATIO=0.8327-0
OUTLIER1=0.8327+1.5*IQR_RETENTION_RATIO
OUTLIER2=0-1.5*IQR_RETENTION_RATIO
MEAN_RETENTION_RATIO=0.3342

for (i in 1:nrow(final_data))
{
  if(final_data$RETENTION_RATIO[i]>OUTLIER1)
  {
    final_data$RETENTION_RATIO[i]=MEAN_RETENTION_RATIO
  }
  else
    if(final_data$RETENTION_RATIO[i]<OUTLIER2)
    {
      final_data$RETENTION_RATIO[i]=MEAN_RETENTION_RATIO
    }
}

save.image()
#####################################################################################################


#SCALING UPTO 12 MONTHS
for(i in 1:nrow(final_data))
{
#final_data$RETENTION_POLY_QTY[i] <- final_data$RETENTION_POLY_QTY[i]*(12/final_data$MONTHS[i])
final_data$POLY_INFORCE_QTY[i] <- final_data$POLY_INFORCE_QTY[i]*(12/final_data$MONTHS[i])
#final_data$PREV_POLY_INFORCE_QTY[i] <- final_data$PREV_POLY_INFORCE_QTY[i]*(12/final_data$MONTHS[i])
final_data$NB_WRTN_PREM_AMT[i] <- final_data$NB_WRTN_PREM_AMT[i]*(12/final_data$MONTHS[i])
final_data$WRTN_PREM_AMT[i] <- final_data$WRTN_PREM_AMT[i]*(12/final_data$MONTHS[i])
final_data$PREV_WRTN_PREM_AMT[i] <- final_data$PREV_WRTN_PREM_AMT[i]*(12/final_data$MONTHS[i])
final_data$PRD_ERND_PREM_AMT[i] <- final_data$PRD_ERND_PREM_AMT[i]*(12/final_data$MONTHS[i])
final_data$PRD_INCRD_LOSSES_AMT[i] <- final_data$PRD_INCRD_LOSSES_AMT[i]*(12/final_data$MONTHS[i])
}


##########################################################################################################

summary(final_data)

## COMPUTATION OF LOSS_RATIO ( PRD_INCRD_LOSSES_AMT / WRTN_PREM_AMT ) 

for (i in 1:nrow(final_data)){
  if (final_data$PRD_INCRD_LOSSES_AMT[i]==0 )
  {
    final_data$LOSS_RATIO[i]=0
  }
  else
    if(final_data$PRD_INCRD_LOSSES_AMT[i]!=0 && final_data$WRTN_PREM_AMT[i]==0)
    {
      final_data$LOSS_RATIO[i]=1200
    }
  else
  {
    final_data$LOSS_RATIO[i] <- (final_data$PRD_INCRD_LOSSES_AMT[i] /final_data$WRTN_PREM_AMT[i])  
  }
}

##OUTLIER REMOVAL FOR LOSS_RATIO
summary(final_data)

IQR_LOSS_RATIO=0.14-0
OUTLIER_LOSS_RATIO_TOP=0.14+1.5*IQR_LOSS_RATIO
OUTLIER_LOSS_RATIO_BOTTOM=0-1.5*IQR_LOSS_RATIO
MEAN_LOSS_RATIO=1200

for (i in 1:nrow(final_data))
{
  if(final_data$LOSS_RATIO[i]>OUTLIER_LOSS_RATIO_TOP)
  {
    final_data$LOSS_RATIO[i]=MEAN_LOSS_RATIO
  }
  else
    if(final_data$LOSS_RATIO[i]<OUTLIER_LOSS_RATIO_BOTTOM)
    {
      final_data$LOSS_RATIO[i]=MEAN_LOSS_RATIO
    }
}

save.image()

#####################################################################################################

## COMPUTATION OF LOSS__RATIO_3YR
for (i in 1:nrow(final_data))
{
  if(final_data$STAT_PROFILE_DATE_YEAR[i] == 2005){
    final_data$LOSS_RATIO_3YR[i] = final_data$LOSS_RATIO[i]
  } else
  {if(final_data$STAT_PROFILE_DATE_YEAR[i] == 2006){
    final_data$LOSS_RATIO_3YR[i] = (final_data$LOSS_RATIO[i] + final_data$LOSS_RATIO[i-1])/2 }
    else 
    {if(final_data$STAT_PROFILE_DATE_YEAR[i] == 2007 |final_data$STAT_PROFILE_DATE_YEAR[i] == 2008 
        |final_data$STAT_PROFILE_DATE_YEAR[i] == 2009 | final_data$STAT_PROFILE_DATE_YEAR[i] == 2010
        |final_data$STAT_PROFILE_DATE_YEAR[i] == 2011 | final_data$STAT_PROFILE_DATE_YEAR[i] == 2012
        |final_data$STAT_PROFILE_DATE_YEAR[i] == 2013 | final_data$STAT_PROFILE_DATE_YEAR[i] == 2014
        |final_data$STAT_PROFILE_DATE_YEAR[i] == 2015){
      final_data$LOSS_RATIO_3YR[i] = (final_data$LOSS_RATIO[i] + final_data$LOSS_RATIO[i-1] + final_data$LOSS_RATIO[i-2])/3 }
    }
  }
}

write.csv(final_data,"final_data.csv")
save.image()

#####################################################################################################

## COMPUTATION OF GROWTH_RATE(USED TO FURTHER CALCULATE GROWTH_RATE_3YR)

summary(final_data)

for (i in 1:nrow(final_data)){
  if(final_data$PREV_WRTN_PREM_AMT[i] == 0 && final_data$WRTN_PREM_AMT[i] != 0)
  {final_data$GROWTH_RATE[i] <- 11.58} 
  if (final_data$PREV_WRTN_PREM_AMT[i] < 0 && final_data$WRTN_PREM_AMT[i] > 0)
  {final_data$GROWTH_RATE[i] <- (final_data$WRTN_PREM_AMT[i]-final_data$PREV_WRTN_PREM_AMT[i])/(-(final_data$PREV_WRTN_PREM_AMT[i]))}
  if(final_data$PREV_WRTN_PREM_AMT[i] == 0)
  {final_data$GROWTH_RATE[i] <- 0}
  else{
    final_data$GROWTH_RATE[i] <- (final_data$WRTN_PREM_AMT[i]-final_data$PREV_WRTN_PREM_AMT[i])/final_data$PREV_WRTN_PREM_AMT[i]}}

sum(final_data$GROWTH_RATE > 11.58)
sum(final_data$GROWTH_RATE < -5)

## OUTLIER REMOVAL FOR GROWTH_RATE

for (i in 1:nrow(final_data)){
  if(final_data$GROWTH_RATE[i] > 11.58)
  {final_data$GROWTH_RATE[i] = 11.58} else 
    if(final_data$GROWTH_RATE[i] < -5)
    {final_data$GROWTH_RATE[i] = -5}
}
save.image()


##########################################################################################################################


## COMPUTATION OF GROWTH_RATE_3YR

for (i in 1:nrow(final_data))
{
  if(final_data$STAT_PROFILE_DATE_YEAR[i] == 2005){
    final_data$GROWTH_RATE_3YR[i] = final_data$GROWTH_RATE[i]
  } else
  {if(final_data$STAT_PROFILE_DATE_YEAR[i] == 2006){
    final_data$GROWTH_RATE_3YR[i] = (final_data$GROWTH_RATE[i] + final_data$GROWTH_RATE[i-1])/2 }
    else 
    {if(final_data$STAT_PROFILE_DATE_YEAR[i] == 2007 |final_data$STAT_PROFILE_DATE_YEAR[i] == 2008 
        |final_data$STAT_PROFILE_DATE_YEAR[i] == 2009 | final_data$STAT_PROFILE_DATE_YEAR[i] == 2010
        |final_data$STAT_PROFILE_DATE_YEAR[i] == 2011 | final_data$STAT_PROFILE_DATE_YEAR[i] == 2012
        |final_data$STAT_PROFILE_DATE_YEAR[i] == 2013 | final_data$STAT_PROFILE_DATE_YEAR[i] == 2014
        |final_data$STAT_PROFILE_DATE_YEAR[i] == 2015){
      final_data$GROWTH_RATE_3YR[i] = (final_data$GROWTH_RATE[i] + final_data$GROWTH_RATE[i-1] + final_data$GROWTH_RATE[i-2])/3 }
    }
  }
}

write.csv(final_data,"final_data.csv")
save.image()


final_data_2 <- final_data[,-c(1,2,4,7,17,18)]
final_data_3 <- final_data_2[,-c(1,12)]

###########################################################################################################################

# AGGREGATING ALL THE VALUES(MEAN)

library(plyr)

vector_agency = names(final_data_3)[c(7:10)]


Agency_Retention_ratio_agg <- ddply(final_data_3, vector_agency, summarize, value=mean(RETENTION_RATIO))
Agency_Growth_Rate_agg <- ddply(final_data_3, vector_agency, summarize, value=mean(GROWTH_RATE))
Agency_Loss_Ratio_agg <- ddply(final_data_3, vector_agency, summarize, value=mean(LOSS_RATIO))
Agency_Loss_agg <- ddply(final_data_3, vector_agency, summarize, value=mean(PRD_INCRD_LOSSES_AMT))
Agency_Profit_agg <- ddply(final_data_3, vector_agency, summarize, value=mean(PRD_ERND_PREM_AMT))

Agency_overall_agg <- data.frame(Agency_Retention_ratio_agg,Agency_Growth_Rate_agg[,5],Agency_Loss_Ratio_agg[,5],
                               Agency_Loss_agg[,5],Agency_Profit_agg[,5])

colnames(Agency_overall_agg)[5:9] <- c("RETENTION_RATIO","GROWTH_RATE","LOSS_RATIO","LOSS","PROFIT")
str(Agency_overall_agg)
summary(Agency_overall_agg)
write.csv(Agency_overall_agg,"Agency_Overall_agg.csv")
save.image()

################################################################################################


### Creating the necessary preprocessing data for decision trees ##############################
Agency_Performance <- Agency_overall_agg

#Categorizing RETENTION_RATIO
Agency_Performance$RETENTION_RATIO_PERFORMANCE <- 
  ifelse(Agency_Performance$RETENTION_RATIO < 0.2780,"Bad","Good")


#Categorizing LOSS_RATIO
Agency_Performance$LOSS_RATIO_PERFORMANCE <-
  ifelse(Agency_Performance$LOSS_RATIO < 160.4306 ,"Good","Bad")


#Categorizing GROWTH_RATE
Agency_Performance$GROWTH_RATE_PERFORMANCE <- 
  ifelse(Agency_Performance$GROWTH_RATE < 0.005578,"Bad","Good")


Agency_Performance$RETENTION_RATIO_PERFORMANCE <- as.factor(Agency_Performance$RETENTION_RATIO_PERFORMANCE)
Agency_Performance$LOSS_RATIO_PERFORMANCE <- as.factor(Agency_Performance$LOSS_RATIO_PERFORMANCE)
Agency_Performance$GROWTH_RATE_PERFORMANCE <- as.factor(Agency_Performance$GROWTH_RATE_PERFORMANCE)

Agency_Performance_1 <- Agency_Performance[,-c(5:9)]
str(Agency_Performance_1)

write.csv(Agency_Performance_1,"Performance1.csv")
save.image()

### Applying decision trees ###################################################################
#Applying trees
#Rule Model

#1. RETENTION_RATIO
library(C50)
Agency_Retention_Perf2 <- Agency_Performance_1[,-c(1,6,7)]
Retention_Ratio_tree <- C5.0(RETENTION_RATIO_PERFORMANCE~.,data=Agency_Retention_Perf2)
summary(Retention_Ratio_tree)

#Splitting into Train & Test data
set.seed(456)
Rows_data <- sample(1:nrow(Agency_Retention_Perf2),nrow(Agency_Retention_Perf2)*0.7)
trainData <- Agency_Retention_Perf2[Rows_data,]
testData <- Agency_Retention_Perf2[-Rows_data,]

#Evaluating training set_DT
pred_train_DT21 = predict(Retention_Ratio_D2,trainData,type = "class")
sort(as.numeric(unique(pred_train_DT21)))
conf_Matrix3 = table(pred_train_DT21,trainData$RETENTION_RATIO_PERFORMANCE)
conf_Matrix3 = conf_Matrix3[order(as.numeric(rownames(conf_Matrix3))), ] 
Accuracy_train_Retention_Ratio <- sum(diag(conf_Matrix3))/sum(conf_Matrix3)*100

#Validating on Test data set_DT
pred_test_DT22 = predict(Retention_Ratio_D2,testData, type = "class")
sort(as.numeric(unique(pred_test_DT22)))
conf_Matrix4 = table(pred_test_DT22,testData$RETENTION_RATIO_PERFORMANCE)
conf_Matrix4 = conf_Matrix4[order(as.numeric(rownames(conf_Matrix4))), ] 
Accuracy_test_Retention_Ratio <- sum(diag(conf_Matrix4))/sum(conf_Matrix4)*100

#2. GROWTH_RATE
library(C50)
Agency_Growth_Perf2 <- Agency_Performance_1[,-c(1,5,6)]
Growth_Rate_D2 <- C5.0(GROWTH_RATE_PERFORMANCE~.,data=Agency_Growth_Perf2)
summary(Growth_Rate_D2)

#Splitting into Train & Test data
set.seed(100)
Rows_data <- sample(1:nrow(Agency_Growth_Perf2),nrow(Agency_Growth_Perf2)*0.7)
trainData <- Agency_Growth_Perf2[Rows_data,]
testData <- Agency_Growth_Perf2[-Rows_data,]

#Evaluating training set_DT
pred_train_DT23 = predict(Growth_Rate_D2,trainData,type = "class")
sort(as.numeric(unique(pred_train_DT23)))
conf_Matrix5 = table(pred_train_DT23,trainData$GROWTH_RATE_PERFORMANCE)
conf_Matrix5 = conf_Matrix5[order(as.numeric(rownames(conf_Matrix5))), ] 
Accuracy_train_Growth_Rate <- sum(diag(conf_Matrix5))/sum(conf_Matrix5)*100

#Validating on Test data set_DT
pred_test_DT24 = predict(Growth_Rate_D2,testData, type = "class")
sort(as.numeric(unique(pred_test_DT24)))
conf_Matrix6 = table(pred_test_DT24,testData$GROWTH_RATE_PERFORMANCE)
conf_Matrix6 = conf_Matrix6[order(as.numeric(rownames(conf_Matrix6))), ] 
Accuracy_test_Growth_Rate <- sum(diag(conf_Matrix6))/sum(conf_Matrix6)*100

#3. LOSS_RATIO
library(C50)
Agency_Loss_Perf2 <- Agency_Performance_1[,-c(1,5,7)]
Loss_Ratio_D2 <- C5.0(LOSS_RATIO_PERFORMANCE~.,data=Agency_Loss_Perf2)
summary(Loss_Ratio_D2)

#Splitting into Train & Test data
set.seed(100)
Rows_data <- sample(1:nrow(Agency_Loss_Perf2),nrow(Agency_Loss_Perf2)*0.7)
trainData <- Agency_Loss_Perf2[Rows_data,]
testData <- Agency_Loss_Perf2[-Rows_data,]

#Evaluating training set_DT
pred_train_DT25 = predict(Loss_Ratio_D2,trainData,type = "class")
sort(as.numeric(unique(pred_train_DT25)))
conf_Matrix7 = table(pred_train_DT25,trainData$LOSS_RATIO_PERFORMANCE)
conf_Matrix7 = conf_Matrix7[order(as.numeric(rownames(conf_Matrix7))), ] 
Accuracy_train_Loss_Ratio <- sum(diag(conf_Matrix7))/sum(conf_Matrix7)*100

#Validating on Test data set_DT
pred_test_DT26 = predict(Loss_Ratio_D2,testData, type = "class")
sort(as.numeric(unique(pred_test_DT26)))
conf_Matrix8 = table(pred_test_DT26,testData$LOSS_RATIO_PERFORMANCE)
conf_Matrix8 = conf_Matrix8[order(as.numeric(rownames(conf_Matrix8))), ] 
Accuracy_test_Loss_Ratio <- sum(diag(conf_Matrix8))/sum(conf_Matrix8)*100

#OUTPUTS
#SUMMARY & ACCURACY - RETENTION_RATIO
summary(Retention_Ratio_D2)
Accuracy_train_Retention_Ratio
Accuracy_test_Retention_Ratio

#SUMMARY & ACCURACY - GROWTH_RATE
summary(Growth_Rate_D2)
Accuracy_train_Growth_Rate2
Accuracy_test_Growth_Rate2

#SUMMARY & ACCURACY - LOSS_RATIO
summary(Loss_Ratio_D2)
Accuracy_train_Loss_Ratio
Accuracy_test_Loss_Ratio

save.image()








