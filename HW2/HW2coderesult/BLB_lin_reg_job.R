mini <- F
if (mini){
  d <- 40
  s <- 5
  r <- 50
  rootfilename <- "blb_lin_reg_mini"
} else {
  d <- 1000
  s <- 5
  r <- 50
  rootfilename <- "blb_lin_reg_data"
}

#============================== Setup for running on Gauss... ==============================#

args <- commandArgs(TRUE)

cat("Command-line arguments:\n")
print(args)

####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <- 0
###################

if (length(args)==0){
  sim_num <- sim_start + 1
  set.seed(121231)
} else {
  # SLURM can use either 0- or 1-indexing...
  # Lets use 1-indexing here...
  sim_num <- sim_start + as.numeric(args[1])
  sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))

# Find r and s indices:
s_index<-as.integer( (sim_num - 1) / 50) + 1;s_index
r_index<-(sim_num - 1) %% 50 + 1;r_index


#============================== Run the simulation study ==============================#

# Load packages:
library(BH)
library(bigmemory.sri)
library(bigmemory)
library(biganalytics)

# I/O specifications:
datapath <- "/home/pdbaines/data"
outpath <- "output/"
# Filenames:
# Set up I/O stuff:
# Attach big.matrix :
# Remaining BLB specs:
# Extract the subset:
# Reset simulation seed:
# Bootstrap dataset:
# Fit lm:
# Output file:
# Save estimates to file:
################################################ my own part####################
data<-attach.big.matrix(sprintf("%s.desc", rootfilename), backingpath=datapath)
n = nrow (data) ; d = ncol (data)-1 ;gamma = 0.7 ; 

#decide sub sample size b
b = round(n^gamma)

#get index of sub-sample 
set.seed(s_index);index=sample(1:n,b)
sub_data = data[index,]
#resample from sub-sample
re_index = rmultinom( 1 , n , rep((1/b),b))

#number of columns

fit = lm(sub_data[,(d+1)]~sub_data[,1:d]+0, weights = re_index)
outfile = paste0(outpath,"coef_",sprintf("%02d",s_index),"_",sprintf("%02d",r_index),".txt")
write.table(file=outfile,fit$coefficients,row.names=F,quote=F)
