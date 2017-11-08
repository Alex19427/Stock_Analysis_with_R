library(stats)
library(matrixStats)
library(e1071)
library(corrplot)

stock_data <- read.csv("E:/BDAP/Stock_Analysis/stock data.csv")
stock_data <- stock_data[-1]
class(stock_data)
head(stock_data)
summary(stock_data)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Mean
Mean <- sapply(stock_data,mean,na.rm=TRUE)

#Median
Median <- sapply(stock_data,median,na.rm=TRUE)

#Mode
Mode <- sapply(stock_data,getmode)

#Standard Deviation
Standard_Deviation <-round(sapply(stock_data,sd,na.rm=TRUE),2)

#Variance
Variance <- round(sapply(stock_data,var,na.rm=TRUE),2)

#MAX and MIN value
Max <- sapply(stock_data,max,na.rm=TRUE)
Min <- sapply(stock_data,min,na.rm=TRUE)

#Inter Quantile Range IQR
IQR <- sapply(stock_data,iqr,na.rm=TRUE)

#Quantile range from 0 to 100%
Quantile <- sapply(stock_data,quantile,seq(0,1,0.25),na.rm=TRUE)
Quantile

#Skewness and Kurtosis
skewness <- round(sapply(stock_data,skewness,na.rm=TRUE),3)
Kurtosis <- round(sapply(stock_data,kurtosis,na.rm=TRUE),3)

#Standard Error
Standard_Error <-Standard_Deviation/sqrt(length(stock_data$ACC))

stock_analysis <- data.frame(Mean,Median,Mode,Standard_Deviation,Variance,Max,Min,IQR,skewness,Kurtosis,Standard_Error)
stock_analysis <- stock_analysis

write.csv(stock_analysis,"E:/BDAP/Stock_Analysis/Analysis.csv")

#Density Plot
par(mfrow=c(4,13))
for (i in names(stock_data)){
  pl = density(as.matrix(stock_data[i]),na.rm=TRUE)
  plot(pl,main=names(stock_data[i]),xlab = "")
}


#library(help="stats")

# Log noram
par(mfrow=c(4,13))
for (i in names(stock_data)){
  pl = density(log(as.matrix(stock_data[i])),na.rm=TRUE)
  plot(pl,main=names(stock_data[i]),xlab = "")
}


#plot(density(dlnorm(stock_data$POWERGRID,mean(log(stock_data$POWERGRID),na.rm=TRUE),sd(log(stock_data$POWERGRID),na.rm=TRUE))))

# CDF 
par(mfrow=c(4,13))
for (i in names(stock_data)){
  pl1 = ecdf(log(as.matrix(stock_data[i])))
  plot(pl1,main=names(stock_data[i]),xlab = "")
}

#plot(ecdf(log(stock_data$ACC)))


#Returns
return<-matrix(nrow=nrow(stock_data)-1, ncol=ncol(stock_data))
for(i in 1:ncol(stock_data)){
  for(j in 2:nrow(stock_data)){
    return[j-1,i]<-stock_data[j,i]-stock_data[j-1,i]
  }
}
colnames(return)<-colnames(stock_data)
return <- as.data.frame(return)

# Plot Returns
par(mfrow=c(4,13))
for (i in names(return)){
  pl = density(as.matrix(return[i]),na.rm=TRUE)
  plot(pl,main=names(return[i]),xlab = "")
}

#Log returns
log_return<-matrix(nrow=nrow(stock_data)-1, ncol=ncol(stock_data))
for(i in 1:ncol(stock_data)){
  for(j in 2:nrow(stock_data)){
    log_return[j-1,i]<-log(stock_data[j,i])-log(stock_data[j-1,i])
  }
}
colnames(log_return)<-colnames(stock_data)
log_return<-as.data.frame(log_return)

head(log_return)
summary(log_return)


# Plot Log returns
par(mfrow=c(4,13))
for (i in names(log_return)){
  pl = density(log(as.matrix(log_return[i])),na.rm=TRUE)
  plot(pl,main=names(log_return[i]),xlab = "")
}

#Return Mean
Return_mean <- sapply(return,mean,na.rm=TRUE)

#Return Median
Return_median <- sapply(return,median,na.rm=TRUE)

#Return Mode
Return_mode <- sapply(return,getmode)

#Return Standard Deviation
Return_Std <-round(sapply(return,sd,na.rm=TRUE),2)

#Return Variance
Return_variance <- round(sapply(return,var,na.rm=TRUE),2)

#Log Return Mean
log_return_mean <- sapply(log_return,mean,na.rm=TRUE)

#Log Return Median
log_return_median <- sapply(log_return,median,na.rm=TRUE)

#Log Return Mode
log_return_mode <- sapply(log_return,getmode)

#Log Return Standard Deviation
log_return_Std <- sapply(log_return,sd,na.rm=TRUE)

#Log Return Variance
log_return_variance <- sapply(log_return,var,na.rm=TRUE)

stock_analysis2 <- data.frame(Return_mean,Return_median,Return_mode,Return_Std,Return_variance,log_return_mean,log_return_median,log_return_mode,log_return_variance,log_rReturn_Std,daily_geometric_return)
stock_analysis2 <- stock_analysis2
write.csv(stock_analysis2,"E:/BDAP/Stock_Analysis/Analysis2.csv")


# Correlation 

m <- cor(stock_data,use = "na.or.complete" )
head(m)
corrplot(m,"circle",type="lower")
covar_stocks<-cov(stock_data,use = "na.or.complete")
head(covar_stocks)

m1 <- cor(log_return, use = "na.or.complete")
corrplot(m1,"circle",type="lower")


daily_geometric_return <- numeric()
for(i in 1:ncol(stock_data)){
  
  daily_geometric_return[i]<-round((((stock_data[939,i]/stock_data[1,i])^(1/939))-1)*100,4)*100
  
}
daily_geometric_return <- as.data.frame(daily_geometric_return)
colnames(daily_geometric_return) <- colnames(stock_data)






