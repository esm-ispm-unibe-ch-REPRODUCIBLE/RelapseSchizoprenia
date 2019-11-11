#*********************************************************************************
#             Load the libraries needed             
#*********************************************************************************
library(meta)
library(metafor)
library(netmeta)
library(readxl)
library(devtools)
library(NMAJags)
library(R2jags)

#**********************************************************************************************
#             Network Meta-Analysis           
#**********************************************************************************************

################################################################################
# 1. Data
################################################################################

DATA= read_excel("C:/Users/kc19o338/Desktop/Analysis schizofrenia/Datasets/dataset_relapse_final.xlsx", na="99999")
DATA=DATA[DATA$Final_ID_all!=985 & DATA$Final_ID_all!=1226  & DATA$Final_ID_all!=2997 & DATA$Final_ID_all!=3131 & DATA$Final_ID_all!=3305 & DATA$Final_ID_all!=4114 & DATA$Final_ID_all!=4362 & DATA$Final_ID_all!=4398 & DATA$Final_ID_all!=4410 & DATA$Final_ID_all!=4412 & DATA$Final_ID_all!=4447 & DATA$Final_ID_all!=4519 & DATA$Final_ID_all!=4584 & DATA$Final_ID_all!=4650,]

##network meta-analysis
NMAdataBinary=make.jagsNMA.data(studyid=Final_ID_all,t=Drug_name,r=N_relapsed,n=N_randomized,data=DATA,type="binary",reference = "Placebo oral or depot")
NMAinJAGSBinP <- jags.parallel(data = NMAdataBinary, inits = NULL, 
                               parameters.to.save = c("ORref","tau",'b'), n.chains = 2, n.iter = 120000,
                               n.burnin = 10000,DIC=F,n.thin=100,
                               model.file = modelNMABinary2)
print(NMAinJAGSBinP)
traceplot(NMAinJAGSBinP)
#network meta-regression
NMRdataBinary=make.jagsNMA.data(studyid=Final_ID_all,t=Drug_name,r=N_relapsed,n=N_randomized,data=DATA,othervar = Duration - 26,type="binary",reference = "Placebo oral or depot")



NMRinJAGSBinP <- jags.parallel(data = NMRdataBinary, inits = NULL, 
                               parameters.to.save = c("ORref","tau",'b'), n.chains = 2, n.iter = 120000,
                               n.burnin = 10000,DIC=F,n.thin=100,
                               model.file = modelNMRBinary)
# These are our results
print(NMRinJAGSBinP)
traceplot(NMRinJAGSBinP)
save(NMRinJAGSBinP,file="NMRinJAGSBinP.RData",envir = .GlobalEnv)
load("NMRinJAGSBinP.RData")

################################################################################
# 4. CODA: Convergence Diagnosis analysis
################################################################################

# Check the chains convergence for all parameters using the trace plot
traceplot(NMRinJAGSBinP,varname="tau" ) 
traceplot(NMRinJAGSBinP,varname="LORref" )
traceplot(NMRinJAGSBinP,varname="b" )
# They look good, they converge and have good mixing.

# Interpretation of the regression coeffecients
# b indicates the change (increase/decrease) in the relative treatment effect (logOR) per one year change in the relative randomisation year 

################################################################################
# 5. More insights about the results ...
################################################################################

# a. Make the forestplot for the different relative treatments (not studies)
#LORref <- as.vector(NMRinJAGSBinP$BUGSoutput$mean$LORref)
#seLORref <- as.vector(NMRinJAGSBinP$BUGSoutput$sd$LORref)
#m1 <- metagen(LORref,seLORref,sm='OR') 
#forest(m1,overall = F)
