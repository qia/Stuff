library("ggplot2")
df=as.data.frame(x)
ggplot(df, aes(x=x)) + 
  geom_histogram(aes(y=..density..),binwidth=.05,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(x)), color="black", linetype="dashed", size=1)+
  ggtitle("The histogram of truncated normal distribution TN(2,1;(0,1.5)) in CPU")
df=as.data.frame(x)
ggplot(df, aes(x=x)) + 
  geom_histogram(aes(y=..density..),binwidth=.05,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#3399FF") +
  geom_vline(aes(xintercept=mean(x)), color="black", linetype="dashed", size=1)+
  ggtitle("The histogram of truncated normal distribution TN(2,1;(0,1.5)) in GPU")
time=mat.or.vec(8,3)
time[1,]=c(0.144,0.000,0.144)
time[2,]=c(0.144,0.000,0.143)
time[3,]=c(0.140,0.004,0.140)
time[4,]=c(0.144,0.000,0.146)
time[5,]=c(0.224,0.008,0.235)
time[6,]=c(0.572,0.028,0.605)
time[7,]=c(1.832,0.472,2.304)
time[8,]=c(7.792,4.148,11.941)
colnames(time)= c("user","system","elapsed")
GPU_time=time
time2=mat.or.vec(24,3)
time2[1:8,1]=time[,1]
time2[9:16,1]=time[,2]
time2[17:24,1]=time[,3]
time2=as.data.frame(time2)
time2[,2]=c(rep("user",8),rep("system",8),rep("elapsed",8))
time2[,3]=c(rep(seq(1:8),3))
colnames(time2) = c("Time","Type","K")
ggplot(data=time2, aes(x=K, y=Time, group=Type, colour=Type)) +
  geom_line() + geom_point()+
  ggtitle("Plot the total runtimes vs K in GPU")

r_time
cputime=time2
cputime[1:3,1]=c(0,0,0)
cputime[4:6,1]=c(0.000,0.000,0.001)
cputime[7:9,1]=c(0.001,0.000,0.001)
cputime[10:12,1]=c(0.004,0.000,0.004)
cputime[13:15,1]=c(0.045,0.006,0.051)
cputime[16:18,1]=c(1.132,0.064,1.196)
cputime[19:21,1]=c(5.443,0.678,6.112)
cputime[22:24,1]=c(54.134,26.367,126.611)
colnames(cputime) = c("Time","Type","K")
ggplot(data=cputime, aes(x=K, y=Time, group=Type, colour=Type)) +
  geom_line() + geom_point()+
  ggtitle("Plot the total runtimes vs K in CPU")

CPU_time= mat.or.vec(8,3)
CPU_time[1,]=c(0,0,0)
CPU_time[2,]=c(0.000,0.000,0.001)
CPU_time[3,]=c(0.001,0.000,0.001)
CPU_time[4,]=c(0.004,0.000,0.004)
CPU_time[5,]=c(0.045,0.006,0.051)
CPU_time[6,]=c(1.132,0.064,1.196)
CPU_time[7,]=c(5.443,0.678,6.112)
CPU_time[8,]=c(54.134,26.367,126.611)

df=as.data.frame(x)
ggplot(df, aes(x=x)) + 
  geom_histogram(aes(y=..density..),binwidth=.05,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(x)), color="black", linetype="dashed", size=1)+
  ggtitle("The histogram of truncated normal distribution TN(3,1;(-Inf,4))")
df=as.data.frame(x)
ggplot(df, aes(x=x)) + 
  geom_histogram(aes(y=..density..),binwidth=.05,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(x)), color="black", linetype="dashed", size=1)+
  ggtitle("The histogram of truncated normal distribution TN(2,1;(3,Inf))")

df=as.data.frame(x)
ggplot(df, aes(x=x)) + 
  geom_histogram(aes(y=..density..),binwidth=.05,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(x)), color="black", linetype="dashed", size=1)+
ggtitle("The histogram of truncated normal distribution TN(0,1;(-Inf,-10)) in CPU")
df=as.data.frame(x)
ggplot(df, aes(x=x)) + 
  geom_histogram(aes(y=..density..),binwidth=.05,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#3399FF") +
  geom_vline(aes(xintercept=mean(x)), color="black", linetype="dashed", size=1)+
  ggtitle("The histogram of truncated normal distribution TN(0,1;(-Inf,-10)) in GPU")

plot(beta.sample[,1],ylim=c(0.08,0.10))
dat=as.data.frame(cbind(beta.sample,seq(1:2000)))
colnames(dat)=c("Beta0","Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","niter")
ggplot(dat, aes(y=Beta0, x=niter)) + ylim(0.08, 0.10) +
geom_point(shape = 0,size = 0.8,colour="#FF6666") +
 ggtitle("The scatter plot of estimated beta0 in CPU for data_02")
ggplot(dat, aes(y=Beta1, x=niter))  + ylim(0.1, 0.12)+
  geom_point(shape = 0,size = 0.8,colour="#FF6666") +
  ggtitle("The scatter plot of estimated beta1 in CPU for data_02")
ggplot(dat, aes(y=Beta2, x=niter))  + ylim(-0.55, -0.4)+
  geom_point(shape = 0,size = 0.8,colour="#3399FF") +
  ggtitle("The scatter plot of estimated beta2 in GPU for data_02")
ggplot(dat, aes(y=Beta3, x=niter))  + ylim(0.07, 0.1)+
  geom_point(shape = 0,size = 0.8,colour="#3399FF") +
  ggtitle("The scatter plot of estimated beta3 in GPU for data_02")
