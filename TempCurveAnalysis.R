install.packages("MESS")
require(MESS)

#this returns the area under the curve

#now with real temperature data

#load in data
Temps=read.csv(file=file.choose()) #load cover data for pre-burn
attach(Temps)


#create simple plot
par(mfrow=c(1,1))
plot(Time1, Temp1, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400), xlab = "Time", lwd = 2, main="Burn Temperature", col="navyblue")
lines(Time2, Temp2, lwd = 2, col = "mediumspringgreen")
lines(Time3, Temp3, lwd = 2, col = "blue")
lines(Time4, Temp4, lwd = 2, col = "cadetblue")
lines(Time5, Temp5, lwd = 2, col = "darkmagenta")
lines(Time6, Temp6, lwd = 2, col = "indianred1")
legend("topright", legend=c("Plot1", "Plot2", "Plot3", "Plot4", "Plot5", "Plot6"), 
       col=c("navyblue", "mediumspringgreen", "blue", "cadetblue", "darkmagenta", "indianred1"), 
       lty=1, lwd=2, bty="n")

#can calculate max temp for each curve
MaxTemps = c(max(Temp1, na.rm=TRUE), 
             max(Temp2, na.rm=TRUE),
             max(Temp3, na.rm=TRUE),
             max(Temp4, na.rm=TRUE),
             max(Temp5, na.rm=TRUE),
             max(Temp6, na.rm=TRUE))

#calculate area under each curve
Area = c(auc(Time1,Temp1, type = 'spline', from=min(Time1, na.rm=TRUE), to=max(Time1, na.rm=TRUE)),
         auc(Time2,Temp2, type = 'spline', from=min(Time1, na.rm=TRUE), to=max(Time1, na.rm=TRUE)),
         auc(Time3,Temp3, type = 'spline', from=min(Time1, na.rm=TRUE), to=max(Time1, na.rm=TRUE)),
         auc(Time4,Temp4, type = 'spline', from=min(Time1, na.rm=TRUE), to=max(Time1, na.rm=TRUE)),
         auc(Time5,Temp5, type = 'spline', from=min(Time1, na.rm=TRUE), to=max(Time1, na.rm=TRUE)),
         auc(Time6,Temp6, type = 'spline', from=min(Time1, na.rm=TRUE), to=max(Time1, na.rm=TRUE)))

#create table with max temp and auc

Summary=data.frame(Plot = as.vector(1:6), MaxTemp = MaxTemps, AUC = Area)
Summary

#create plots for each curve with area shaded
par(mfrow=c(3,2), mar=c(2, 2, 2, 2))
plot(Time1, Temp1, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400), xlab = "Time", lwd = 2, main="Plot 1")
      cord.x <- c(1, Time1[!is.na(Time1)], max(Time1, na.rm=TRUE))
      cord.y <- c(0, Temp1[!is.na(Temp1)], 0)
      polygon(cord.x,cord.y,col='navyblue')
sum = vector('expression',2)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                   list(MYVALUE = format(Summary[1,2],dig=5)))[2]
sum[2] = substitute(expression(Area == MYOTHERVALUE), 
                   list(MYOTHERVALUE = format(Summary[1,3], digits = 6)))[2]
legend('topright', legend = sum, bty = 'n')
plot(Time2, Temp2, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 2")
      cord.x <- c(1, Time2[!is.na(Time2)], max(Time2, na.rm=TRUE))
      cord.y <- c(0, Temp2[!is.na(Temp2)], 0)
      polygon(cord.x,cord.y,col='mediumspringgreen')
sum = vector('expression',2)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[2,2],dig=5)))[2]
sum[2] = substitute(expression(Area == MYOTHERVALUE), 
                    list(MYOTHERVALUE = format(Summary[2,3], digits = 6)))[2]
legend('topright', legend = sum, bty = 'n')
plot(Time3, Temp3, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 3")
      cord.x <- c(1, Time3[!is.na(Time3)], max(Time3, na.rm=TRUE))
      cord.y <- c(0, Temp3[!is.na(Temp3)], 0)
      polygon(cord.x,cord.y,col='blue')
sum = vector('expression',2)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[3,2],dig=5)))[2]
sum[2] = substitute(expression(Area == MYOTHERVALUE), 
                    list(MYOTHERVALUE = format(Summary[3,3], digits = 7)))[2]
legend('topright', legend = sum, bty = 'n')
plot(Time4, Temp4, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 4")
      cord.x <- c(1, Time4[!is.na(Time4)], max(Time4, na.rm=TRUE))
      cord.y <- c(0, Temp4[!is.na(Temp4)], 0)
      polygon(cord.x,cord.y,col='cadetblue')
sum = vector('expression',2)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[4,2],dig=5)))[2]
sum[2] = substitute(expression(Area == MYOTHERVALUE), 
                    list(MYOTHERVALUE = format(Summary[4,3], digits = 7)))[2]
legend('topright', legend = sum, bty = 'n')
plot(Time5, Temp5, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 5")
      cord.x <- c(1, Time5[!is.na(Time5)], max(Time5, na.rm=TRUE))
      cord.y <- c(0, Temp5[!is.na(Temp5)], 0)
      polygon(cord.x,cord.y,col='darkmagenta')
sum = vector('expression',2)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[5,2],dig=5)))[2]
sum[2] = substitute(expression(Area == MYOTHERVALUE), 
                    list(MYOTHERVALUE = format(Summary[5,3], digits = 7)))[2]
legend('topright', legend = sum, bty = 'n')
plot(Time6, Temp6, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 6")
      cord.x <- c(1, Time6[!is.na(Time6)], max(Time6, na.rm=TRUE))
      cord.y <- c(0, Temp6[!is.na(Temp6)], 0)
      polygon(cord.x,cord.y,col='indianred1')
sum = vector('expression',2)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[6,2],dig=5)))[2]
sum[2] = substitute(expression(Area == MYOTHERVALUE), 
                    list(MYOTHERVALUE = format(Summary[6,3], digits = 7)))[2]
legend('topright', legend = sum, bty = 'n')


#calculate summary values
se<-function (x){ sd(x)/sqrt(length(x))} 
Summary
mean(Summary$MaxTemp)
mean(Summary$AUC)
se(Summary$MaxTemp)
se(Summary$AUC)


