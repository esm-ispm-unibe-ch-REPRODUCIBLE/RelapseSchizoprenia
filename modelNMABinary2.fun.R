#### function similar to modelNMABinary that also returns LORref ######

modelNMABinary2=function () 
{
  for (i in 1:ns) {
    w[i, 1] <- 0
    theta[i, t[i, 1]] <- 0
    for (k in 1:na[i]) {
      r[i, t[i, k]] ~ dbin(p[i, t[i, k]], n[i, t[i, k]])
    }
    logit(p[i, t[i, 1]]) <- u[i]
    for (k in 2:na[i]) {
      logit(p[i, t[i, k]]) <- u[i] + theta[i, t[i, k]]
      theta[i, t[i, k]] ~ dnorm(md[i, t[i, k]], precd[i, 
                                                      t[i, k]])
      md[i, t[i, k]] <- mean[i, k] + sw[i, k]
      w[i, k] <- (theta[i, t[i, k]] - mean[i, k])
      sw[i, k] <- sum(w[i, 1:(k - 1)])/(k - 1)
      precd[i, t[i, k]] <- prec * 2 * (k - 1)/k
      mean[i, k] <- d[t[i, k]] - d[t[i, 1]]
    }
  }
  for (i in 1:ns) {
    u[i] ~ dnorm(0, 0.01)
  }
  tau ~ dnorm(0, 1) %_% T(0, )
  prec <- 1/pow(tau, 2)
  tau.sq <- pow(tau, 2)
  d[ref] <- 0
  for (k in 1:(ref - 1)) {
    d[k] ~ dnorm(0, 0.01)
  }
  for (k in (ref + 1):nt) {
    d[k] ~ dnorm(0, 0.01)
  }
  for (i in 1:(nt - 1)) {
    for (j in (i + 1):nt) {
      OR[j, i] <- exp(d[j] - d[i])
      LOR[j, i] <- d[j] - d[i]
    }
  }
  for (j in 1:(ref - 1)) {
    ORref[j] <- exp(d[j] - d[ref])
    LORref[j]<-d[j]-d[ref]
  }
  for (j in (ref + 1):nt) {
    ORref[j] <- exp(d[j] - d[ref])
    LORref[j]<-d[j]-d[ref]
  }
}

