read.table("/Users/Qian/Documents/STA_250my/blr_pars_1103.csv")
rawdata=read.table("/Users/Qian/Documents/STA_250my/blr_data_1001.csv",head=T)
vec= mat.or.vec(100, 1)
for (i in 2:101){
vec[i-1]=as.character(rawdata[i,])
}
data=mat.or.vec(100, 4)
for (i in 1:100){
data[i,]=as.numeric(unlist(strsplit(vec[i],",")))
}
colnames(data)=unlist(strsplit(as.character(rawdata[1,]),","))

library("mnormt")
library("mcmcplots")


beta.0 = mat.or.vec(2, 1)  
Sigma.0.inv = matrix(c(1,0,0,1),ncol=2)
beta = mat.or.vec(11000, 2)
accept=NULL
###########################
logpibeta = function ( m, y , X, beta, beta.0, Sigma.0.inv)
{
  logp_ybeta = log(dmnorm ( t ( beta ), mean = beta.0 , varcov = Sigma.0.inv))
  for ( i in 1: 100){
    xbeta = t(X[i,]) %*% beta
    logit = exp (xbeta) / (1+ exp (xbeta) )
    logp_ybeta = logp_ybeta + log(dbinom(y[i], size=m[i], prob=logit ) )
  }  
  return(logp_ybeta)
}

############

bayes.logreg <- function(m,y,X,beta.0,Sigma.0.inv,
                           niter=10000,burnin=1000,
                           print.every=1000,retune=100,
                           verbose=TRUE){
 beta[1,]=beta.0
  for (t in 1: (niter+burnin-1) ){
   beta_star = t(rmnorm(1, beta[t,], Sigma.0.inv))
   u = runif(1, min=0, max=1)
   logalpha= logpibeta(m, y , X, beta_star, beta.0, Sigma.0.inv)-
     logpibeta(m, y , X, beta[t,], beta.0, Sigma.0.inv)
    if ( log(u) < logalpha )  beta[t+1,]=beta_star  
    else beta[t+1,]=beta[t,]
    if ( t%%retune==0 & t<=burnin ) 
    {
      accept = length(which(beta[(t-retune+2):(t+1),1]-beta[(t-retune+1):t,1]!=0))/retune
      if (accept < 0.3 ) Sigma.0.inv=Sigma.0.inv/3
      if (accept > 0.7 ) Sigma.0.inv=Sigma.0.inv*3
      print(accept)
      print(Sigma.0.inv)
     
    }    
  }
 return(beta)
}
beta_sample = NULL 
beta_sample = bayes.logreg(data[,2],data[,1],data[,3:4],beta.0,Sigma.0.inv)

###########################################

outpicfile = paste("graph",11111, ".png", sep ="")
png(filename=outpicfile )
par( mfrow = c( 2, 1 ) )
plot(beta_sample[,1],type="l",col="blue",ylab="Beta0",main="The plot of beta sample from MCMC")
plot(beta_sample[,2],type="l",col="blue",ylab="Beta1")
dev.off()

plot(beta_sample[,1],beta_sample[,2],xlab="Beta0",ylab="Beta1",main="The numerical coverage properties")
plot(beta_sample[10001:11000,1],beta_sample[10001:11000,2],xlab="Beta0",ylab="Beta1",main="The numerical coverage properties, drop burnin")

q=t(rbind(quantile(beta_sample[,1], probs = seq(0.01,0.99,by=0.01)),
quantile(beta_sample[,2], probs = seq(0.01,0.99,by=0.01))))
write.csv(q, file = "quantile.csv")