#import data
library("YRmisc")
library(readxl)
spMerge <- read_excel("C:/Users/Amritpal/Desktop/BUA 633/Data/spMerge.xlsx",sheet = "Sheet1")
View(spMerge)
spData<-spMerge
dim(spData)
names(spData)
spMerge <- read_excel("C:/Users/Amritpal/Desktop/BUA 633/Data/spMerge.xlsx",sheet = "Sheet2")
View(spMerge)
spInfo<-spMerge
dim(spInfo)
names(spInfo)
names(spData)
names(spInfo)
# note to self merge 2 dfs
spdf<-merge(spData,spInfo,by="tkr")
dim(spdf)
names(spdf)
spdf$date
spdf$year<-as.numeric(substring(spdf$date,7,10))
#TIME SERIES REGRESSION - choose a company tkr
unique(spdf$tkr)
names(spdf)
tsdf<-spdf[spdf$tkr=="AAPL",c("tkr","price","eps","bvps","cr","dta","year","name")]
tsdf
tsdf<-df.sortcol(tsdf,"year",FALSE)
dim(tsdf)
names(tsdf)
tsdf$obs<-1:23
names(tsdf)
tsdf[,2:6]<-round(tsdf[,2:6],2)


#Graphical Methods
#Histograms
par(mfrow=c(2,2))
hist(tsdf$price,xlab="Price",ylab="Frequency",main="Fig. 1 Hist of Price", col="darkseagreen")		
hist(tsdf$eps,xlab="Earnings Per Share",ylab="Frequency",main="Fig. 2 Hist of EPS", col="chocolate1")
hist(tsdf$bvps,xlab="Book Value Per Share",ylab="Frequency",main="Fig. 3 Hist of BVPS", col="cyan3")	

#Time Series Plots
par(mfrow=c(2,2))
ts.plot(tsdf$price,xlab="Time",ylab="Price",main="Fig. 4 TSPlot of Price", col="darkseagreen")		
ts.plot(tsdf$eps,xlab="Time",ylab="EPS",main="Fig. 5 TSPlot of EPS", col="chocolate1")
ts.plot(tsdf$bvps,xlab="Time",ylab="BVPS",main="Fig. 6 TSPlot of BVPS", col="cyan3")

#Scatter Plots
par(mfrow=c(2,2))
scatter.smooth(tsdf$eps,tsdf$price,xlab="Earnings Per Share",ylab="Price",main="Fig. 7 EPS vs. Price",type="n")
text(tsdf$eps,tsdf$price,as.character(tsdf$year),cex=.8, col="chocolate1") 
scatter.smooth(tsdf$bvps,tsdf$price,xlab="Book Value Per Share",ylab="Price",main="Fig. 8 BVPS vs. Price",type="n")
text(tsdf$bvps,tsdf$price,as.character(tsdf$year),cex=.8, col="cyan3")


#Analytical Methods
#Decriptive Statistics
names(tsdf)
ds.summ(tsdf[,c("price","eps","bvps","obs")],2)[,-c(3,4,7,8)]

#Correlation Matrix
round(cor(na.omit(tsdf[,c("price","eps","bvps","obs")])),3)

#Regression - Linear Model (Parametric)
fit<-lm(price~eps+bvps+obs,na.action=na.omit,data=tsdf)
summary(fit)

# Residual Plots
par(mfrow=c(2,2))
hist(fit$residuals,main = "Fig. 9 Hist of LM Residuals", xlab="Residuals", ylab="Frequency", col="blueviolet")
scatter.smooth(tsdf$price,fit$fitted.values,main="Fig. 10 Predicted vs Actual Price LM", xlab="Predicted Price", ylab="Actual
Price", type="n")
text(fit$fitted.values,tsdf$price,as.character(tsdf$year),cex=.8, col="blueviolet")
pl.2ts(tsdf$price,fit$fitted.values,"Fig. 11 TSPlot LM")

#Regression - robust (linear parametric with outlier mitigation)
library("robust")
fit<-lmRob(price~eps+bvps+obs,na.action=na.omit,data=tsdf) 
summary(fit)
par(mfrow=c(2,2))
hist(fit$residuals, main = "Fig. 12 Hist of Rob Residuals", xlab="Residuals", ylab="Frequency", col="darkgoldenrod1")
scatter.smooth(fit$fitted.values,tsdf$price, main="Fig. 13 Predicted vs Actual Price Rob", xlab="Predicted Price", ylab="Actual
Price", type="n")
text(fit$fitted.values,tsdf$price,as.character(tsdf$year),cex=.8, col="darkgoldenrod1")
pl.2ts(tsdf$price,fit$fitted.values,"Fig. 14 TSPlot Rob")

#Regression - General Additive Model (nonlinear nonparametric)
library("gam")
fit<-gam(price~s(eps,2)+s(bvps,2)+s(obs,2),na.action=na.omit,data=tsdf)
plot(fit)
par(mfrow=c(2,2))
plot(fit, main="Fig. 15 Gam of EPS", col="chocolate1") #repeat for all 4 and combine into a final image
cor(tsdf$price, fit$fitted.values)^2

