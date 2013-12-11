library("mvtnorm");library("BayesBridge");library("RCUDA");library("gdata")

probit_mcmc_GPU = function(y,x,beta_0=rep(0,p),Sigma_0_inv=diag(rep(0,p)),niter=2000,burnin=500)
{
  gpu_smapling = function(n,mu,sigma,a,b,maxtries)
  {
    cuGetContext(TRUE)
    m = loadModule("rtruncnorm.ptx")
    k = m$rtruncnorm_kernel
    xx = rep (0.0,n)
    numbtries = 0
    
    if (n <= 512L) {
      threads_per_block  <- 512L
      block_dims <- c(threads_per_block, 1L, 1L)
      grid_d1 <- 1
      grid_d2 <- 1
      grid_dims <- c(grid_d1, grid_d2, 1L)
    }else{
      threads_per_block <- 512L
      block_dims <- c(threads_per_block, 1L, 1L)
      grid_d1 <- floor(sqrt(n/threads_per_block))
      grid_d2 <- ceiling(n/(grid_d1*threads_per_block))
      grid_dims <- c(grid_d1, grid_d2, 1L)
    }
    
    nthreads <- prod(grid_dims)*prod(block_dims) 
    mem = copyToDevice(xx)
    .cuda(k, mem, n, mu, sigma, a, b, numbtries, maxtries,
          inplace = TRUE, gridDim = grid_dims, blockDim = block_dims)
    cu_ret = copyFromDevice(obj=mem,nels=mem@nels,type="float")
    return(cu_ret)
  }
  
  p = ncol(x); n = as.integer(nrow(x))
  niter = 2000
  burnin = 500
  beta.sample = mat.or.vec( niter, p) 
  beta.0 = rep(0,p)
  Sigma.0.inv = diag(rep(0,p))
  
  print("Begin MCMC")
  
  j = 1
  while (j<=niter+burnin)
  {
    Sigma.t.inv = Sigma.0.inv + crossprod(x)
    Sigma.t = solve(Sigma.t.inv) 
    z1 = 
      gpu_smapling(n,mu=unmatrix(x%*%beta.0), sigma=rep(1,n), a=rep(-Inf,n), b=rep(0,n),maxtries=n*3)
    z2 = 
      gpu_smapling(n,mu=unmatrix(x%*%beta.0), sigma=rep(1,n), a=rep(-Inf,n), b=rep(0,n),maxtries=n*3)
    z2=-z2
    z= z1*(1-y)+z2*y
    mu.t = Sigma.t %*%(crossprod(Sigma.0.inv, beta.0) + crossprod(x, z))
    Sigma.0.inv = Sigma.t.inv
    beta.0=mu.t
    if (j > burnin) {
      beta.sample[j-burnin,] = rmvnorm (1, beta.0 ,Sigma.t)
    }
    j=j+1
  }
  
  print("MCMC done")
  
}

dat=read.table("mini_data.txt",header=T)
y = as.matrix(dat[,1])
x = as.matrix(dat[,-1])

probit_mcmc_GPU(y,x,beta_0=rep(0,p),Sigma_0_inv=diag(rep(0,p)),niter=2000,burnin=500)

colMeans(beta.sample)
tail(beta.sample)