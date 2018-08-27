rm(list=ls())
library(caret)
library(dplyr)
library(e1071)
library(ggplot2)
library(lubridate)
library(stringr)
library(Amelia)
library(DMwR)
library(MASS)
library(dummies)

#reading train data
data1<-read.csv("train (3).csv",header = T)

#datapreprocessing and feature engineering of train data

#checking for missing values by column
sort(colSums(is.na(data1)),decreasing = TRUE)
missmap(data1)
str(data1)

#removing useless attributes
omitted<-c("NameOfTheVehicle","SellerType","OfferType","VehicleID","ZipCode","DataCollectedDate","NumberOfPictures")
data2<-data1[,!(colnames(data1))%in%omitted]

#creating new attribute of time period
start<-as.Date(data2$DateOfAdLastSeen,"%d-%m-%Y %H:%M")
finish<-as.Date(data2$DateOfAdCreation,format="%d-%m-%Y %H:%M")
data2$timediff<-start-finish


#removing useless attributes
omit2<-c("DateOfAdLastSeen","DateOfAdCreation","ModelOfTheVehicle")
data3<-data2[,!(colnames(data2))%in%omit2]


#imputing missing values
sort(colSums(is.na(data3)),decreasing = TRUE)
data4<-data3[!is.na(data3$TypeOfTheFuelUsed),]
data4<-data4[!is.na(data4$GearBoxType),]
data4<-data4[!is.na(data4$VehicleType),]
data4[is.na(data4$IsDamageRepaired),"IsDamageRepaired"]<-"No"
omit3<-c("MonthOfVehicleRegistration","YearOfVehicleRegistration")
data4$usage<-year(now())-data4$YearOfVehicleRegistration
data4<-data4[,!(colnames(data4))%in%omit3]

#coverting to factors and numeric values
data4$VehicleType<-factor(data4$VehicleType)
data4$BrandOfTheVehicle<-factor(data4$BrandOfTheVehicle)
data4$TypeOfTheFuelUsed<-factor(data4$TypeOfTheFuelUsed)
y1<-data4$Price
data4$Price<-NULL
data4$timediff<-as.numeric(data4$timediff)
data4$usage<-as.numeric(data4$usage)

#scaling and dummifying variables
col1<-sapply(data4,is.numeric)
numdata<-data4[,col1]
rp<-preProcess(numdata,method=c("center","scale"))
numdata<-predict(rp,numdata)
catdata<-data4[,!colnames(data4)%in%colnames(numdata)]
catdata1<-dummyVars(~.,catdata)
catdata<-predict(catdata1,catdata)
catdata<-as.data.frame(catdata)
finaltrain_data<-as.data.frame(cbind(numdata,catdata))
Price<-y1
finaltrain_data$Price<-Price


#writing to csv file
write.csv(finaltrain_data,file="mithtrain.csv",row.names = FALSE)
#str(data4)



#datapreprocessing and feature engineering of test data
tdata1<-read.csv("test (1).csv",header = T)



#checking for missing values by column
sort(colSums(is.na(tdata1)),decreasing = TRUE)
missmap(tdata1)

#removing useless attributes
omitted<-c("NameOfTheVehicle","SellerType","OfferType","VehicleID","ZipCode","DataCollectedDate","NumberOfPictures")
tdata2<-tdata1[,!(colnames(tdata1))%in%omitted]
tstart<-as.Date(tdata2$DateOfAdLastSeen,"%d-%m-%Y %H:%M")
tfinish<-as.Date(tdata2$DateOfAdCreation,format="%d-%m-%Y %H:%M")
tdata2$timediff<-tstart-tfinish

#same as previously done for test
omit2<-c("DateOfAdLastSeen","DateOfAdCreation","ModelOfTheVehicle")
tdata3<-tdata2[,!(colnames(tdata2))%in%omit2]
tdata3$usage<-year(now())-tdata3$YearOfVehicleRegistration
omit3<-c("MonthOfVehicleRegistration","YearOfVehicleRegistration")
tdata4<-tdata3[,!(colnames(tdata3))%in%omit3]

tdata4$IsDamageRepaired[is.na(tdata4$IsDamageRepaired)]<-"No"
sort(colSums(is.na(tdata4)),decreasing = TRUE)
tdata4$TypeOfTheFuelUsed[is.na(tdata4$TypeOfTheFuelUsed)]<-"petrol"
sort(colSums(is.na(tdata4)),decreasing = TRUE)
tdata4$GearBoxType[is.na(tdata4$GearBoxType)]<-"manual"
tdata4$VehicleType[is.na(tdata4$VehicleType)]<-"limousine"
tdata4$timediff<-as.numeric(tdata4$timediff)
tdata4$usage<-as.numeric(tdata4$usage)


tdata4$VehicleType<-factor(tdata4$VehicleType)
tdata4$BrandOfTheVehicle<-factor(tdata4$BrandOfTheVehicle)
tdata4$TypeOfTheFuelUsed<-factor(tdata4$TypeOfTheFuelUsed)




col1<-sapply(tdata4,is.numeric)
tnumdata<-tdata4[,col1]
tnumdata<-predict(rp,tnumdata)
tcatdata<-tdata4[,!colnames(tdata4)%in%colnames(tnumdata)]
tcatdata<-predict(catdata1,tcatdata)
tcatdata<-as.data.frame(tcatdata)
finaltest_data<-as.data.frame(cbind(tnumdata,tcatdata))
sort(colSums(is.na(finaltest_data)),decreasing = TRUE)
VehicleID<-tdata1$VehicleID
finaltest_data$VehicleID<-VehicleID



#writing to csv file
write.csv(finaltest_data,file="mithtest.csv",row.names = FALSE)






