```{r}
param = function (data, size=0.35, cluster=5, method = c("spherical","diagonal"), seed = 243) {
  set.seed(seed)
  # get number of rows and columns of data
  n = nrow(data)
  d = ncol(data)
  
  sample.id = sort(sample(1:n, n*size))
  label = train_sub$y[sample.id]
  
  mu = matrix(0, cluster, d)
  for(i in 1:cluster) { mu[i,] <- apply(data[label==(i-1),], 2, mean) }
  
  if(method == "spherical") { sigma = runif(5,1,1) }
  
  if(method == "diagonal") {
    sigma = matrix(0, cluster, d)
    for(t in 1:cluster) {
      sigma[t,] = runif(d,1,1)
    }
  }

  Fij = runif(5,0,1)
  pi = Fij/sum(Fij)
  return(list(mu = mu, sigma = sigma, pi = pi))
}

parameter = param(train_compress, method = "spherical")
```

```{r}
condition_prob = function(data, parameter, method = c("spherical","diagonal")) {
  
  n = nrow(data)
  d = ncol(data)
  cluster = length(parameter$pi)
  
  mu = parameter$mu 
  pi = parameter$pi
  sigma = parameter$sigma
  
  if(method=="spherical") {
    a = matrix(0, n, cluster)
    for(i in 1:n) {
      xi = x[i,]
      for(j in 1:cluster) {
        uj = mu[j,]
        pi_j = pi[j]
        a[i,j] = -d/2*log(sigma[j])-0.5/sigma[j]*sum((x[i,]-mu[j,])^2)+log(pi[j])
      }
    }
  }
  else if (method=="diagonal") {
    a = matrix(0,n,cluster)
    for(j in 1:n) {
      xi = x[j,]
      for(i in 1:cluster)
      {
        uj = mu[i,]
        sigma2_j = sigma[i,]
        pi_j = pi[i]
        a[j,i] = -0.5*sum(log(sigma2_j))-0.5*t((xi-uj)^2)%*%(1/sigma2_j)+log(pi_j)
      }
    }
  }
 
  F_matrix = matrix(0,n,cluster)
  logmargin = rep(0,n)
  for (j in 1:n) {
    A = max(a[j,])
    logmargin[j] = A+log(sum(exp(a[j,]-A)))
    F_matrix[j,] = a[j,]-logmargin[j]
  }
  return(list('F' = F_matrix, pij = a, logmargin = logmargin))
}

loglikelihood = function(data, parameter, method = c("spherical","diagonal")) {
  marg = condition_prob(data, parameter, method = method)$logmargin
  return(sum(marg))
}

LL = condition_prob(train_compress, parameter, method = "spherical")
loglikelihood(train_compress, parameter, method = "spherical")
```

```{r}
EM = function(data, size, epsilon = 0.0001,method = c("spherical","diagonal"), seed = 243) {
  
  parameter = initial(data,size = 0.5, method = method, seed = seed)
  
  n = nrow(data)
  d = ncol(data)
  cluster = length(parameter$pi)
  
  mu = parameter$mu 
  pi = parameter$pi
  sigma = parameter$sigma
  
  loglike_b = loglikelihood(data, parameter, method = method)
  
  Q = c(loglike_b)
  while(T) {
    TT = condition_prob(data, parameter,method = method)
    F_matrix = TT$`F` #(i,j) element is the log conditional probability of Z_i=j given x_i
    
    if(method=="diagonal") {
      mf = max(F_matrix)
      col_sum = apply(exp(F_matrix-mf), 2, sum)
      p_af = col_sum/sum(exp(F_matrix-mf)) 
      u_af = matrix(0,nrow = cluster,ncol = d )
      sigma2_af = matrix(0,nrow = cluster,ncol = d )
      
      for(j in 1:cluster) {
        Fj = F_matrix[,j] # log(F1j,...,Fnj) ^T
        u_af[j,] = (t(x)%*%exp(Fj-max(Fj))) /sum(exp(Fj-max(Fj))) # muj = sum(xiFij)/sum(Fij)
        sigma2_af[j,] = t(sweep(x,2,u_af[j,])^2)%*%exp(Fj-max(Fj))/sum(exp(Fj-max(Fj)))
      }
      sigma2_af = 0.05 + sigma2_af
    }
    
    else if(method=="spherical") {
      p_af = rep(0, cluster)
      u_af = matrix(0, cluster, d)
      sigma2_af = rep(0,cluster)
      for(j in 1:cluster) {
        Fj = F_matrix[,j]
        p_af[j] = sum(exp(Fj-max(Fj)))*exp(max(Fj))/n
        u_af[j,] = t(x)%*%exp(Fj-max(Fj))/sum(exp(Fj-max(Fj)))
        S = apply(M,1,FUN = function(x){sum(x^2)})
        sigma2_af[j] = t(S)%*%exp(Fj-max(Fj))/(d*sum(exp(Fj-max(Fj))))
      }
    }
    parameter_new = list(mu = u_af, pi = p_af, sigma = sigma2_af)
    loglike_af = loglikelihood(data, parameter_new, method = method)
    
    Q = c(Q,loglike_af)
    if(abs((loglike_af-loglike_b)/loglike_b)<epsilon) {
      break
    }
   
    parameter = parameter_new
    loglike_b = loglike_af
  }
  return(list(parameter = parameter, Q = Q, 
              'F' =exp(F_matrix-max(F_matrix))*exp(max(F_matrix)) ))
}
```

```{r}
error_rate = function(ID_cluster, label) {
  digit = unique(label)
  label_hat = rep(0, length(label))
  
  for(i in 1:length(digit)) {
    Ind = which(label==digit[i])
    same_num = rep(0,length(digit))
    for(j in 1:length(digit)) {
      same_num[j] = length(intersect(Ind,which(ID_cluster==j)))
    }
    label_hat[ID_cluster==which.max(same_num)] = digit[i]
  }
  error = sum(label_hat!=label)/length(label)
  return(error)
}

errorrate_s = c()
for(seed in c(1000,10000,100000)) {
  estimate = EM(train_compress, size = 0.02, epsilon = 1e-5, method = "spherical",seed = seed)
  plot(estimate$Q,type = "l",caption = paste("Likelihood of spherical, seed =",seed))
  ID_cluster = rep(0,nrow(train_compress))
  
  for(i in 1: nrow(train_compress)) {
    ID_cluster[i] = which.max(estimate$`F`[i,])
  }
  errorrate_s = c(errorrate_s,error_rate(ID_cluster,label))
}

names(errorrate_s) = c(1000,10000,100000)
library(knitr)
kable(errorrate_s,format = "latex", caption = "Error rate of spherical seed")
```

```{r}
errorrate_d = c()
for(seed in c(1000,10000,100000)) {
  estimate = EM(train_compress, size = 0.02,epsilon = 1e-5,method = "diagonal",seed = seed)
  plot(estimate$Q,type = "l",caption = paste("Likelihood of diagonal, seed =",seed))
  ID_cluster = rep(0,nrow(train_compress))
  
  for(i in 1: nrow(train_compress)) {
    ID_cluster[i] = which.max(estimate$`F`[i,])
  }
  errorrate_d = c(errorrate_d,error_rate(ID_cluster,label))
}

names(errorrate_d) = c(1000,10000,100000)
kable(errorrate_d,format = "latex", caption = "Error rate of diagonal seed")
```
