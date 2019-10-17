


data1 <- read.csv("C:/Users/dell/Desktop/SL 7/r/acme.csv",header = TRUE)
print(data1)
colMarket = data.frame(data1) 
colnames(colMarket);rownames(colMarket)
colMarket[,c("market","acme")] #get only two columns which are needed
y_market = colMarket[,c("market")] #predictor variable
x_acme = colMarket[,c("acme")] #response variable
#apply leastsquare regression
LeastSquareRegModel = lm(x_acme ~ y_market)
# 3.predicted x_acme values will be obtained by following command
LeastSquareRegModel$fitted.values
xfit = LeastSquareRegModel$fitted.values
#3.make available scatterplot
plot(y_market,x_acme)

#3.to plot our model into the scatterplot
lines(y_market,xfit)

#4.make png file to save plot 
png(file="LReg.png")



#3.plot the graph
plot(y_market,x_acme,col="red",main="Monthly Excess Returns Regression Study",abline(LeastSquareRegModel),cex=1.3,pch=16,xlab="acme",ylab="market")

residual = x_acme-x_acme


dev.off()
