

#install.packages("metafor")
#install.packages("meta")
#install.packages("xlsx")
#install.packages("rJava")
#install.packages("xlsxjars")
#install.packages("netmeta")
#install.packages("tidyverse")
#install.packages("swirl")
#install.packages("readxl")
#install.packages("WriteXLS")

library(metafor)
library(meta)
library(xlsx)
library(readxl)
library(netmeta)
library(tidyverse)
#library(swirl)
library(WriteXLS)


rm(list = ls())


data= read_excel("C:/Users/kc19o338/Desktop/Analysis schizofrenia/Datasets/dataset_relapse.xlsx", na="99999")

####Placebo should be used as one node irrespective if oral or depot. Therefore we need to change "medication application" for placebo
#data<-mutate(data, Medicationapplication=ifelse(Drug_name=="Placebo", "oral or depot", Medicationapplication))


#### Oral and depot applications should be used as separate interventions. Therefore we need to add the type of medication application to drug_name
data$Drug_name<-paste(data$Drug_name,data$Medicationapplication)


#### We extracted the number of relapses from study start up to 3 months (=< 12 wks), from more than 3 months (>12 wks) up to 6 months (=<26 wks), from more than 6 months (>26 wks) up to 9 month (=< 39 wks), from more than 9 months (>39 wks) up to 12 months (<52 wks) and for more than 12 wks (>52 wks).We also enter data of the latest time point in each study in the outcome "N_Relapse_AnyTime".
#### As described in my specific questions, we may pool some of these intervals. One possibility is to use relapses from more than 6 months up to 12 month (>26 wks and =< 52 wks)
#### Therefore we need to use 9 month relapse data when 12 month data is not available
data<-mutate(data, Relapse_N_12m=ifelse(is.na(Relapse_N_12m), Relapse_N_9m, Relapse_N_12m))


#### Here we create new columns in the dataset which contain the information for the analysis of dichotomous data, i.e. we add columns at the end of the table. 
####CAVE: HERE WE NEED TO ADAPT DEPENDING ON THE OUTCOME WE WANT TO ANALYSE

dataset_dichotomous1<-
  data%>%
  mutate(
    IndivArm_n=data$N_arm_total_stapf,
    IndivArm_events=data$Relapse_N_AnyTime
  )

#### Some studies have several arms with the same drug but with different doses. We need to pool these arms, i.e. to calculate the sum of participants and sum of events

dataset_dichotomous2<-
  select(dataset_dichotomous1, Final_ID_all, Study_name, Drug_name, IndivArm_n, IndivArm_events)%>%
  drop_na%>%
  group_by(Final_ID_all, Study_name, Drug_name)%>%
  summarise(
    pooled_n=sum(IndivArm_n, na.rm = FALSE), 
    pooled_events=sum(IndivArm_events, na.rm = FALSE)
  )


##When we want additional columns in the dataset(e.g. for subgroup analysis), then we need to first type it into the select function and then either type it in the group_by function (when study-based information) or type it in the summarize function (when arm-based information, here probably most of the time as weighted mean)


####Continue with NMA

## Somehow "dataset_dichotomous2" cannot be used directly by pairwise function. We need to create a new dataframe first

dataset_dichotomous3=data.frame(
  Final_ID_all=dataset_dichotomous2$Final_ID_all,
  Study_name=dataset_dichotomous2$Study_name, 
  Drug_name=dataset_dichotomous2$Drug_name, 
  pooled_n=dataset_dichotomous2$pooled_n, 
  pooled_events=dataset_dichotomous2$pooled_events, 
  stringsAsFactors = FALSE)


## Bring longformat in pairwise

TestPair <- pairwise(treat=Drug_name, event=N_relapsed, n=N_randomized, data=data, sm="OR", studlab=Final_ID_all, allstudies = TRUE)


### Delete the study "Dencker 19.." because its comparison is disconnected to the network

TestPair=TestPair[TestPair$studlab!=4376,]


### Run NMA ###

net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = TestPair, sm = "OR", comb.random=TRUE, comb.fixed=FALSE, prediction=TRUE)

#### When there is a error message because there are separate networks, type:
####netconnection(treat1, treat2, studlab, data = TestPair)

summary(net1, digits = 2)

print(net1, digits = 2)

decomp.design(net1)

netgraph(net1)
forest(net1,ref="Placebo oral")
