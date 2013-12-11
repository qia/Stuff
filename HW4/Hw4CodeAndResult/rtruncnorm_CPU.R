rtruncnorm = function(N,mu=rep(0,N),sigma=rep(1,N),a=rep(-3,N),b=rep(3,N))
{ maxtries= N*10
  i=0
  x=NULL
  while (length(x)<N & i<=maxtries/N) {
    temp = mu + sigma*rnorm (N)
    x = c(x,temp[which(temp>=a & temp<=b)])
    i=i+1
  }
  if (length(x)>=N){
    return (x[1:N])
  }else{ 
    print ("use 2nd method")
    idx=1;x = NULL;u_bar = NULL
    while (idx<=N){
      if (is.infinite(b[idx])) {
        u_bar[idx]  = a[idx]
      }
      else {
        u_bar[idx]  = -b[idx]
      }
      alpha = NULL;z = NULL;psi = NULL
      alpha[idx]  = (u_bar[idx]  + sqrt((u_bar[idx]^2+4)))/2
      z[idx]  = u_bar[idx]  - log(runif(1,0,1))/alpha[idx] 
      if (u_bar[idx]  < alpha[idx] )
      {
        psi[idx]  = exp (-(alpha[idx] -z[idx] )^2/2)
      }
      else
      {
        psi[idx]  = exp(-(u_bar[idx] -alpha[idx])^2/2)*exp(-(alpha[idx]-z[idx])^2/2)
      }
      
      if (runif(1,0,1) < psi[idx]  ) 
      { 
        if (is.infinite(b[idx])) {
          x[idx] = mu[idx] + sigma[idx]*z[idx]
          idx = idx + 1
        }
        else {
          x[idx] = mu[idx] - sigma[idx]*z[idx]
          idx = idx + 1
        }
      }
      
    }
  }
  return(x)
}

r_time=NULL
for (i in 1:8){
  N=10^i
  r_time[[i]]=system.time(
  rtruncnorm (N,mu=rep(2,N),sigma=rep(1,N),a=rep(0,N),b=rep(1.5,N)))}

N=10^4
x=rtruncnorm (N,mu=rep(2,N),sigma=rep(1,N),a=rep(0,N),b=rep(1.5,N))
x=rtruncnorm (N,mu=rep(3,N),sigma=rep(1,N),a=rep(-Inf,N),b=rep(4,N))
x=rtruncnorm (N,mu=rep(2,N),sigma=rep(1,N),a=rep(3,N),b=rep(Inf,N))
x=rtruncnorm (N,mu=rep(0,N),sigma=rep(1,N),a=rep(-Inf,N),b=rep(-10,N))