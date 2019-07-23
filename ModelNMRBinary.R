modelNMRBinary=function(){
for(i in 1:ns) { 
    w[i,1]<- 0
    theta[i,t[i,1]]<- 0
    u[i] ~ dnorm(0,.01)
##binomial likelihood of number of events for each arm k of study i
    for (k in 1:na[i]) {r[i,t[i,k]] ~ dbin(p[i, t[i, k]],n[i,t[i,k]])
         logit(p[i,t[i,k]])<- u[i] + theta1[i,t[i,k]]
         theta1[i, t[i, k]]<- theta[i,t[i,k]]+beta[t[i,1],t[i,k]]*variab[i]
         }                                                   
    ##parameterization of the 'true' effect of each comparison 
    ##of arm k vs. baseline arm (1) of study i                
    for (k in 2:na[i]) {
       ##distribution of random effects
      theta[i,t[i,k]] ~ dnorm(md[i,t[i,k]],precd[i,t[i,k]])
      ## accounting for correlation between effect sizes estimated in multi-arm trials
      md[i,t[i,k]]<- d[t[i,k]] - d[t[i,1]]+ sw[i,k]                                   
      w[i,k]<- (theta[i,t[i,k]]  - d[t[i,k]] + d[t[i,1]])      
      sw[i,k]<- sum(w[i,1:(k-1)])/(k-1)
      precd[i,t[i,k]]<- prec*2*(k-1)/k
    }
  }
  ##prior distribution for heterogeneity
  tau ~dnorm(0,10)%_%T(0,)                                  
  prec<- 1/pow(tau,2)
  tau.sq<- pow(tau,2)
  ##prior distribution for basic parameters
  d[ref] <- 0
  for (k in 1:(ref - 1)) {
    d[k] ~ dnorm(0, 1e-04)
  }
  for (k in (ref + 1):nt) {
    d[k] ~ dnorm(0, 1e-04)
  }
  b[ref] <- 0
  for (k in 1:(ref - 1)) {
    b[k] ~ dnorm(B, precB)
  }
  for (k in (ref + 1):nt) {
    b[k] ~ dnorm(B,precB)
  }
  B~dnorm(0,0.0001)
  precB<-1/(tauB*tauB)
  tauB~dnorm(0,10)%_%T(0,)
  #Ranking of treatments#
  #regression coefficients
  for(i in 1:nt) {
    for (j in 1:nt) {
      beta[i,j]<-b[j]-b[i]
    }
  }
  ##OR for each comparison 
  for(i in 1:(nt-1)) {
    for (j in (i+1):nt) {
      OR[j,i]<- exp(d[j] - d[i])
      LOR[j,i]<- d[j] - d[i]}
  }
  for (c in 1:nt) {
    ORref[c] <- exp(d[c] - d[ref])
    LORref[c]<- d[c] - d[ref]
  }
#  thetad~dnorm(0,0.001)
 # precisiond<-1/varianced
  #varianced<-taud*taud
  #taud~ dnorm(0,1)%_%T(0,)
  
}

