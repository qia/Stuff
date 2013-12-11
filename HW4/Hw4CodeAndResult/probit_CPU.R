library("mvtnorm");library("BayesBridge")
dat=read.table("/Users/Qian/Documents/STA_250/Stuff/HW4/data_02.txt",header=T)
y = as.matrix(dat[,1])
x = as.matrix(dat[,-1])
probit_mcmc_cpu = function(y,x,beta_0=rep(0,p),Sigma_0_inv=diag(rep(0,p)),niter=2000,burnin=500)
  {
    beta.sample = mat.or.vec( niter, p) 
    p = ncol(x); n = nrow(x)
    for ( j in 1:( niter+burnin ) ) {
        Sigma.t.inv = Sigma.0.inv + crossprod(x)
        Sigma.t = solve(Sigma.t.inv)
        z = rtnorm (n,mu=x%*%beta.0, sig=rep(1,n),left=-Inf, right=0)*(1-y)+
            rtnorm (n,mu=x%*%beta.0, sig=rep(1,n),left= 0, right= Inf)*(y)
        mu.t = Sigma.t %*%(crossprod(Sigma.0.inv, beta.0) + crossprod(x, z))
        Sigma.0.inv = Sigma.t.inv
        beta.0=mu.t
  if (j > burnin) {
    beta.sample[j-burnin,] = rmvnorm (1, beta.0 ,Sigma.t)
  }
}
return(tail(beta.sample))
}

probit_mcmc_cpu(y,x,beta_0=rep(0,p),Sigma_0_inv=diag(rep(0,p)),niter=2000,burnin=500)
glm(y ~ x[,-1], family = binomial(link = "probit"))