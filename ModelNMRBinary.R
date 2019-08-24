
modelNMRBinary=function(){
  for(i in 1:ns) { 
    w[i,1]<- 0
    theta[i,t[i,1]]<- 0                                             
    
    ##binomial likelihood of number of events for each arm k of study i
    
    for (k in 1:na[i]) {r[i,t[i,k]] ~ dbin(p[i, t[i, k]],n[i,t[i,k]])}                                                   
    ##parameterization of the 'true' effect of each comparison 
    ##of arm k vs. baseline arm (1) of study i                
    
    logit(p[i,t[i,1]])<- u[i]
    for (k in 2:na[i]) {
      
      logit(p[i,t[i,k]])<- u[i] + theta1[i,t[i,k]]
      theta1[i, t[i, k]]<- theta[i,t[i,k]]+beta[t[i,1],t[i,k]]*variab[i]
      
      ##distribution of random effects
      theta[i,t[i,k]] ~ dnorm(md[i,t[i,k]],precd[i,t[i,k]])
      
      ## accounting for correlation between effect sizes estimated in multi-arm trials
      
      md[i,t[i,k]]<- mean[i,k] + sw[i,k]                                   
      w[i,k]<- (theta[i,t[i,k]]  - mean[i,k])          
      sw[i,k]<- sum(w[i,1:(k-1)])/(k-1)
      precd[i,t[i,k]]<- prec *2*(k-1)/k  
      
      ##consistency equations
      mean[i,k] <-d[t[i,k]] - d[t[i,1]] 
      
    }}
  
  ##prior distribution for log-odds in baseline arm of study i
  for (i in 1:ns) {u[i] ~ dnorm(0,.01)}
  
  ##prior distribution for heterogeneity
  tau ~ dnorm(0,10)%_%T(0,)                                   
  prec<- 1/pow(tau,2)
  tau.sq<- pow(tau,2)
  ##prior distribution for basic parameters
  d[ref] <- 0
  for(k in 1:(ref-1)) {d[k] ~ dnorm(0,.01)}
  for(k in (ref+1):nt) {d[k] ~ dnorm(0,.01)}
  
  ##OR for each comparison 
  for(i in 1:(nt-1)) {
    for (j in (i+1):nt) {
      OR[j,i]<- exp(d[j] - d[i])
      LOR[j,i]<- d[j] - d[i]}
    
  }
  
  for(j in 1:(ref-1)){ORref[j]<- exp(d[j] - d[ref])}
  for(j in (ref+1):nt) {ORref[j]<- exp(d[j] - d[ref])}
  for(j in 1:(ref-1)){LORref[j]<- d[j] - d[ref]}
  for(j in (ref+1):nt) {LORref[j]<- d[j] - d[ref]}
  #Ranking of treatments#
  #regression coefficients
  for (i in 1:nt) {
    for (j in 1:nt) {
      beta[i, j] <- b[j] - b[i]
    }
  }
  b[ref] <- 0
  for (k in 1:(ref - 1)) {
    b[k] ~ dnorm(B, precB)
  }
  for (k in (ref + 1):nt) {
    b[k] ~ dnorm(B,precB)
  }
  B~dnorm(0,0.01)
  precB<-1/(tauB*tauB)
  tauB~ dnorm(0,10)%_%T(0,)
}



