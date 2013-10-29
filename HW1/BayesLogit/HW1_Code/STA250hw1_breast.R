breast_cancer=read.table("/Users/Qian/Documents/STA_250/Stuff/HW1/BayesLogit/breast_cancer.txt")
breastdata=breast_cancer[-1,]
name=NULL
for (i in 1:ncol(breast_cancer) ){
  name[i]=as.character(breast_cancer[1,i])
}
colnames(breastdata)=name
indexB=which(breastdata[,11]=="B")
indexM=which(breastdata[,11]=="M")
diag=NULL
diag[indexB]=0;diag[indexM]=1
intercept = rep(1,nrow(breastdata))
newBreast=cbind(breastdata,diag,intercept)
xMatrix=newBreast[,-11:-12]
xNew = mat.or.vec(569, 11) 

for (i in 1:11){
xNew[,i]=as.numeric(as.character(xMatrix[,i]))}
######################################

logpibeta = function ( y , X, beta, beta.0, Sigma.0.inv)
{
  logp_ybeta = log(dmnorm (  t(beta) , mean = t(beta.0) , varcov = Sigma.0.inv))
  for ( i in 1:length(y)){
    xbeta = X[i,] %*% beta
    log_1eu = log (1+ exp (xbeta) )
    if (y[i]==1) logp_ybeta = logp_ybeta + xbeta - log_1eu
    else logp_ybeta = logp_ybeta -log_1eu
  }
  return(logp_ybeta)
}
#########################################
beta.0 = matrix(mat.or.vec(11, 1))  
Sigma.0.inv =1000* diag(11)

bayes.logreg <- function(y,X,beta.0,Sigma.0.inv,
                         niter=100000,burnin=10000,
                         retune=100,verbose=TRUE){
  beta = mat.or.vec(niter+burnin, 11)
  accept=NULL
  beta[1,]=beta.0
  for (t in 1: (niter+burnin-1) ){
    beta_star = t(rmnorm(1, beta[t,], Sigma.0.inv))
    u = runif(1, min=0, max=1)
    logalpha= logpibeta(y , X, beta_star, beta.0, Sigma.0.inv)-
      logpibeta(y , X, beta[t,], beta.0, Sigma.0.inv)
    if ( log(u) < logalpha )  beta[t+1,]=beta_star  
             else beta[t+1,]=beta[t,]
    if ( t%%retune==0 & t<=burnin ) # adjust sigma
    {
      accept = length(which(beta[(t-retune+2):(t+1),1]-beta[(t-retune+1):t,1]!=0))/retune
    # if (accept < 0.3 ) Sigma.0.inv=Sigma.0.inv/5
     #if (accept > 0.7 ) Sigma.0.inv=Sigma.0.inv*5
      if (accept < 0.3 | accept > 0.7) Sigma.0.inv=(cov(beta[t-retune+1:t,])+diag(1e-10,11))/100
    
      print(accept)
     # print(Sigma.0.inv)
    }    
  }
  return(beta)
}

beta_sample = NULL
xScale=scale(xNew)
xScale[,11]=1
beta_sample = bayes.logreg(newBreast[,12],xNew,beta.0,Sigma.0.inv)

par( mfrow = c( 3, 2 ) )
for (i in 1:11) {
  plot(beta_sample[,i],type="l",xlab= "Number of iterations", ylab="Beta sample",
       main =paste("The plot of beta for the variable", colnames(xMatrix)[i]))
}

par( mfrow = c( 3, 2 ) )
for (i in 1:11) {
  acf(beta_sample[,i], xlab = "Lag autocorrelation", 
      main = paste("The beta of the variable", colnames(xMatrix)[i]))
}

for (i in 1:11) {
  plot(quantile(beta_sample[,i], probs = seq(0.01,0.99,by=0.01)))
}


