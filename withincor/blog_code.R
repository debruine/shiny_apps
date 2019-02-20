if(!require(ggplot2)){install.packages('ggplot2')}
library(ggplot2)
if(!require(Rcpp)){install.packages('Rcpp')}
library(Rcpp)
if(!require(MASS)){install.packages('MASS')}
library(MASS)
options(digits=10,scipen=999)


#Set color palette
cbbPalette<-c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

n<-10000  #set the sample size for each group
mx<-100  #set the mean in group 1
sdx<-15  #set the standard deviation in group 1
my<-106  #set the mean in group 2
sdy<-15  #set the standard deviation in group 2
cor.true <- 0.0 #set true correlation
#randomly draw data
cov.mat <- matrix(c(1.0, cor.true, cor.true, 1.0), nrow = 2, byrow = T)
mu <- c(0,0)
mat <- mvrnorm(n, Sigma = cov.mat, mu = mu, empirical = FALSE)
x<-mat[,1]*sdx+mx
y<-mat[,2]*sdy+my
dif<-x-y
datasetplot<-data.frame(x,y)
DV<-c(x,y) #combine the two samples into a single variable
IV<-as.factor(c(rep("1", n1), rep("2", n2))) #create the independent variable (1 and 2) 
dataset<-data.frame(IV,DV) #create a dataframe (to make the plot)
t.test(x, y, alternative = "two.sided", paired = FALSE, var.equal = TRUE, conf.level = 0.95) #t-test

#plot graph two groups
p1 <- ggplot(dataset, aes(DV, fill = as.factor(IV)))  + 
  geom_histogram(alpha=0.4, binwidth=2, position="identity", colour="black", aes(y = ..density..)) +
  scale_fill_manual(values=cbbPalette, name = "Condition") +
  stat_function(fun=dnorm, args=c(mean=mx,sd=sdx), size=1, color="#E69F00", lty=2) +
  stat_function(fun=dnorm, args=c(mean=my,sd=sdy), size=1, color="#56B4E9", lty=2) +
  xlab("IQ") + ylab("number of people")  + ggtitle("Data") + theme_bw(base_size=20) + 
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) + 
  geom_vline(xintercept=mean(x), colour="black", linetype="dashed", size=1) + 
  geom_vline(xintercept=mean(y), colour="black", linetype="dashed", size=1) + 
  coord_cartesian(xlim=c(50,150)) + scale_x_continuous(breaks=c(50,60,70,80,90,100,110,120,130,140,150)) +
  annotate("text", x = 70, y = 0.02, label = paste("Mean X = ",round(mean(x)),"\n","SD = ",round(sd(x)),sep="")) +
  annotate("text", x = 130, y = 0.02, label = paste("Mean Y = ",round(mean(y)),"\n","SD = ",round(sd(y)),sep="")) +
  theme(plot.title = element_text(hjust = 0.5))


#plot data differences
p2 <- ggplot(as.data.frame(dif), aes(dif))  + 
  geom_histogram(colour="black", fill="grey", aes(y=..density..), binwidth=2) +
  #  geom_density(fill=NA, colour="black", size = 1) +
  xlab("IQ dif") + ylab("number of people")  + ggtitle("Data") + theme_bw(base_size=20) + 
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(), panel.grid.minor.x = element_blank()) + 
  geom_vline(xintercept=mean(dif), colour="gray20", linetype="dashed") + 
  coord_cartesian(xlim=c(-80:80)) + scale_x_continuous(breaks=c(seq(-80, 80, 10))) +
  annotate("text", x = mean(dif), y = 0.01, label = paste("Mean = ",round(mean(dif)),"\n","SD = ",round(sd(dif)),sep="")) +
  theme(plot.title = element_text(hjust = 0.5))

#Plot correlation
p3 <- ggplot(datasetplot, aes(x=x, y=y)) +
  geom_point(size=2) +    # Use hollow circles
  geom_smooth(method=lm, colour="#E69F00", size = 1, fill = "#56B4E9") + # Add linear regression line
  coord_cartesian(xlim=c(40,160), ylim=c(40,160)) +
  scale_x_continuous(breaks=c(seq(40, 160, 20))) + scale_y_continuous(breaks=c(seq(40, 160, 20))) +
  xlab("IQ twin 1") + ylab("IQ twin 2")  + ggtitle(paste("Correlation = ",round(cor(x,y),digits=2),sep="")) + theme_bw(base_size=20) + 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  coord_fixed(ratio = 1)  +
  theme(plot.title = element_text(hjust = 0.5))


p1
p2
p3

png(file="MeansPlot.png",width=4000,height=3000, , units = "px", res = 500)
p1
dev.off()

png(file="DifPlot.png",width=4000,height=3000, , units = "px", res = 500)
p2
dev.off()

png(file="CorPlot.png",width=4000,height=4000, , units = "px", res = 500)
p3
dev.off()
