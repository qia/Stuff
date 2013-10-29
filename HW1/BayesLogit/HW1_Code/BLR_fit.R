#============================== Setup for running on Gauss... ==============================#

args <- commandArgs(TRUE)

# This will print the command line argument e.g., a number from 1 to 100
cat("Command-line arguments:\n")
print(args)

####
# sim_start ==> Lowest dataset number to be analyzed by this particular batch job
###

###################
sim_start <- 1000
###################


if (length(args)==0){
  set.seed(121231)
 num <- sample(1:200,1)
 sim_num <- sim_start + num
  
} else {
  sim_num <- sim_start + as.numeric(args[1])
  set.seed(762*sim_num + 121231)
}



cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))

#============================== Run the simulation study ==============================#

gauss <- TRUE

if (gauss){
  setwd("~/Stuff/HW1/BayesLogit/data/")
}



# Read data corresponding to appropriate sim_num:
rawdata = read.csv(sprintf("blr_data_%d.csv", sim_num), header = TRUE)
data = as.matrix(rawdata)


beta.0 = mat.or.vec(2, 1)  
Sigma.0.inv = matrix(c(1,0,0,1),ncol=2)
beta = mat.or.vec(11000, 2)
accept=NULL
###########################
dmnorm=function (x, mean = rep(0, d), varcov, log = FALSE) 
{
  d <- if (is.matrix(varcov)) 
    ncol(varcov)
  else 1
  if (d == 1) 
    return(dnorm(x, mean, sqrt(varcov), log = log))
  x <- if (is.vector(x)) 
    matrix(x, 1, d)
  else data.matrix(x)
  if (is.vector(mean)) 
    mean <- outer(rep(1, nrow(x)), mean)
  if (is.matrix(mean) && (nrow(mean) != nrow(x) || ncol(mean) != 
                            ncol(x))) 
    stop("mismatch of dimensions of 'x' and 'mean'")
  if (is.vector(mean)) 
    mean <- outer(rep(1, nrow(x)), mean)
  X <- t(x - mean)
  conc <- pd.solve(varcov, log.det = TRUE)
  Q <- apply((conc %*% X) * X, 2, sum)
  log.det <- attr(conc, "log.det")
  logPDF <- as.vector(Q + d * logb(2 * pi) + log.det)/(-2)
  if (log) 
    logPDF
  else exp(logPDF)
}
###################
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
rmnorm=function (n = 1, mean = rep(0, d), varcov) 
{
  d <- if (is.matrix(varcov)) 
    ncol(varcov)
  else 1
  z <- matrix(rnorm(n * d), n, d) %*% chol(varcov)
  y <- t(mean + t(z))
  return(y)
}

pd.solve = function (x, silent = FALSE, log.det = FALSE) 
{
  if (is.null(x)) 
    return(NULL)
  if (any(is.na(x))) {
    if (silent) 
      return(NULL)
    else stop("NA's in x")
  }
  if (length(x) == 1) 
    x <- as.matrix(x)
  if (!is.matrix(x)) {
    if (silent) 
      return(NULL)
    else stop("x is not a matrix")
  }
  if (max(abs(x - t(x))) > .Machine$double.eps) {
    if (silent) 
      return(NULL)
    else stop("x appears to be not symmetric")
  }
  x <- (x + t(x))/2
  u <- try(chol(x, pivot = FALSE), silent = silent)
  if (class(u) == "try-error") {
    if (silent) 
      return(NULL)
    else stop("x appears to be not positive definite")
  }
  inv <- chol2inv(u)
  if (log.det) 
    attr(inv, "log.det") <- 2 * sum(log(diag(u)))
  return(inv)
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
    

    if ( log(u) < logalpha)  beta[t+1,]=beta_star  
    else beta[t+1,]=beta[t,]
    if ( t%%retune==0 & t<=burnin ) # adjust sigma
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

#length(unique(beta_sample[,1]))


###########################################

outpicfile = paste("~/Stuff/HW1/BayesLogit/results/graph",sim_num, ".png", sep ="")
png(filename=outpicfile)
par( mfrow = c( 2, 1 ) )
plot(beta_sample[,1],type="l",col="blue")
plot(beta_sample[,2],type="l",col="blue")
dev.off()


outputfile = paste("~/Stuff/HW1/BayesLogit/results/quantile",sim_num, ".csv", sep ="")
q=t(rbind(quantile(beta_sample[,1], probs = seq(0.01,0.99,by=0.01)),
          quantile(beta_sample[,2], probs = seq(0.01,0.99,by=0.01))))
cat(paste("Output to: ",outputfile,"\n",sep=""))
write.csv(q, file = outputfile)

rm(list=setdiff(ls(), "x"))
