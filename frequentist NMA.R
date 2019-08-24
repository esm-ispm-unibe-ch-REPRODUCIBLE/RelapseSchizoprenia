
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

##load data
DATA= read_excel("C:/Users/kc19o338/Desktop/Analysis schizofrenia/Datasets/dataset_relapse_final.xlsx", na="99999")

#### check for studies without info for relapses
K<-DATA[which(is.na(DATA$N_relapsed)),]
table(K$Final_ID_all) 
# studies with NA relapses :985, 1226, 2997, 3131, 3305, 4114, 4362, 4398, 4410, 4412, 4447, 4519, 4584, 4650
#delete those studies
DATA=DATA[DATA$Final_ID_all!=985 & DATA$Final_ID_all!=1226  & DATA$Final_ID_all!=2997 & DATA$Final_ID_all!=3131 & DATA$Final_ID_all!=3305 & DATA$Final_ID_all!=4114 & DATA$Final_ID_all!=4362 & DATA$Final_ID_all!=4398 & DATA$Final_ID_all!=4410 & DATA$Final_ID_all!=4412 & DATA$Final_ID_all!=4447 & DATA$Final_ID_all!=4519 & DATA$Final_ID_all!=4584 & DATA$Final_ID_all!=4650,]

### pairwise data
TestPair <- pairwise(treat=Drug_name, event=N_relapsed, n=N_randomized, data=DATA, sm="OR", studlab=Final_ID_all, allstudies = TRUE)

### Run NMA ###

net1 <- netmeta(TE, seTE, treat1, treat2, studlab, data = TestPair, sm = "OR", comb.random=TRUE, comb.fixed=FALSE, prediction=TRUE)
#results
summary(net1, digits = 2)

print(net1, digits = 2)

decomp.design(net1)
#plots
netgraph(net1)
forest(net1,ref="Placebo oral")
