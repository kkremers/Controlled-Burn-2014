install.packages("MESS")
require(MESS)

#create simple plot - use CB_temperature_processed
Temps=read.csv(file=file.choose()) #load cover data for pre-burn
attach(Temps)
head(Temps)
par(mfrow=c(1,1))
plot(Time3, Temp3, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400), xlab = "Time", lwd = 2, main="Burn Temperature", col="navyblue")
lines(Time4, Temp4, lwd = 2, col = "mediumspringgreen")
lines(Time5, Temp5, lwd = 2, col = "blue")
lines(Time6, Temp6, lwd = 2, col = "cadetblue")
lines(Time7, Temp7, lwd = 2, col = "darkmagenta")
lines(Time8, Temp8, lwd = 2, col = "indianred1")
legend("topright", legend=c("Plot3", "Plot4", "Plot5", "Plot6", "Plot7", "Plot8"), 
       col=c("navyblue", "mediumspringgreen", "blue", "cadetblue", "darkmagenta", "indianred1"), 
       lty=1, lwd=2, bty="n")

detach(Temps)


#use CB_temperature_processed2 for rest of analysis
Temps=read.csv(file=file.choose()) #load cover data for pre-burn
#remove NAs
Temp3 = Temps$Temp3[!is.na(Temps$Temp3)]
Time3 = Temps$Time3[!is.na(Temps$Time3)]
Temp4 = Temps$Temp4[!is.na(Temps$Temp4)]
Time4 = Temps$Time4[!is.na(Temps$Time4)]
Temp5 = Temps$Temp5[!is.na(Temps$Temp5)]
Time5 = Temps$Time5[!is.na(Temps$Time5)]
Temp6 = Temps$Temp6[!is.na(Temps$Temp6)]
Time6 = Temps$Time6[!is.na(Temps$Time6)]
Temp7 = Temps$Temp7[!is.na(Temps$Temp7)]
Time7 = Temps$Time7[!is.na(Temps$Time7)]
Temp8 = Temps$Temp8[!is.na(Temps$Temp8)]
Time8 = Temps$Time8[!is.na(Temps$Time8)]


#can calculate max temp for each curve
MaxTemps = c(max(Temp3), 
             max(Temp4),
             max(Temp5),
             max(Temp6),
             max(Temp7),
             max(Temp8))

Amplitude = c(max(Temp3)-Temp3[1], 
             max(Temp4)-Temp4[1],
             max(Temp5)-Temp5[1],
             max(Temp6)-Temp6[1],
             max(Temp7)-Temp7[1],
             max(Temp8)-Temp8[1])

TempInit = c(Temp3[1],
             Temp4[1],
             Temp5[1],
             Temp6[1],
             Temp7[1],
             Temp8[1])

TempFinal = c(Temp3[length(Temp3)],
             Temp4[length(Temp4)],
             Temp5[length(Temp5)],
             Temp6[length(Temp6)],
             Temp7[length(Temp7)],
             Temp8[length(Temp8)])

TempDiff = TempFinal-TempInit

#calculate area under each curve
Area = c(auc(Time3,Temp3-Temp3[1], type = 'spline', from=min(Time3), to=max(Time3)),
         auc(Time4,Temp4-Temp4[1], type = 'spline', from=min(Time4), to=max(Time4)),
         auc(Time5,Temp5-Temp5[1], type = 'spline', from=min(Time5), to=max(Time5)),
         auc(Time6,Temp6-Temp6[1], type = 'spline', from=min(Time6), to=max(Time6)),
         auc(Time7,Temp7-Temp7[1], type = 'spline', from=min(Time7), to=max(Time7)),
         auc(Time8,Temp8-Temp8[1], type = 'spline', from=min(Time8), to=max(Time8)))

#create table with max temp and auc

Summary=data.frame(Plot = as.vector(3:8), MaxTemp = MaxTemps, Amplitude = Amplitude, TempDiff = TempDiff, AUC = Area)
Summary
Summary[2,4] = NA #not using TempDiff or AUC calculated for plots 4 and 6 because the temp data didn't go out far enough to catch full decline
Summary[2,5] = NA
Summary[4,4] = NA
Summary[4,5] = NA

#create plots for each curve with area shaded
par(mfrow=c(3,2), mar=c(2, 2, 2, 2))
plot(Time3, Temp3, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400), xlab = "Time", lwd = 2, main="Plot 3")
      cord.x <- c(Time3[1], Time3, max(Time3))
      cord.y <- c(Temp3[1], Temp3, Temp3[1])
      polygon(cord.x,cord.y,col='navyblue')
sum = vector('expression',4)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[1,2],dig=4)))[2]
sum[2] = substitute(expression(Amplitude == MYVALUE1), 
                    list(MYVALUE1 = format(Summary[1,3],dig=4)))[2]
sum[3] = substitute(expression(TempDiff == MYVALUE2), 
                    list(MYVALUE2 = format(Summary[1,4], digits = 3)))[2]
sum[4] = substitute(expression(Area == MYVALUE3), 
                    list(MYVALUE3 = format(Summary[1,5], digits = 6)))[2]
legend('topright', legend = sum, bty = 'n')




plot(Time4, Temp4, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 4")
      cord.x <- c(Time4[1], Time4, max(Time4))
      cord.y <- c(Temp4[1], Temp4, Temp4[1])
      polygon(cord.x,cord.y,col='mediumspringgreen')
sum = vector('expression',4)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[2,2],dig=4)))[2]
sum[2] = substitute(expression(Amplitude == MYVALUE1), 
                    list(MYVALUE1 = format(Summary[2,3],dig=4)))[2]
sum[3] = substitute(expression(TempDiff == MYVALUE2), 
                    list(MYVALUE2 = format(Summary[2,4], digits = 3)))[2]
sum[4] = substitute(expression(Area == MYVALUE3), 
                    list(MYVALUE3 = format(Summary[2,5], digits = 6)))[2]
legend('topright', legend = sum, bty = 'n')




plot(Time5, Temp5, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 5")
      cord.x <- c(Time5[1], Time5, max(Time5))
      cord.y <- c(Temp5[1], Temp5, Temp5[1])
      polygon(cord.x,cord.y,col='blue')
sum = vector('expression',4)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[3,2],dig=4)))[2]
sum[2] = substitute(expression(Amplitude == MYVALUE1), 
                    list(MYVALUE1 = format(Summary[3,3],dig=4)))[2]
sum[3] = substitute(expression(TempDiff == MYVALUE2), 
                    list(MYVALUE2 = format(Summary[3,4], digits = 3)))[2]
sum[4] = substitute(expression(Area == MYVALUE3), 
                    list(MYVALUE3 = format(Summary[3,5], digits = 6)))[2]
legend('topright', legend = sum, bty = 'n')


plot(Time6, Temp6, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 6")
        cord.x <- c(Time6[1], Time6, max(Time6))
        cord.y <- c(Temp6[1], Temp6, Temp6[1])
        polygon(cord.x,cord.y,col='cadetblue')
sum = vector('expression',4)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[4,2],dig=3)))[2]
sum[2] = substitute(expression(Amplitude == MYVALUE1), 
                    list(MYVALUE1 = format(Summary[4,3],dig=3)))[2]
sum[3] = substitute(expression(TempDiff == MYVALUE2), 
                    list(MYVALUE2 = format(Summary[4,4], digits = 3)))[2]
sum[4] = substitute(expression(Area == MYVALUE3), 
                    list(MYVALUE3 = format(Summary[4,5], digits = 6)))[2]
legend('topright', legend = sum, bty = 'n')


plot(Time7, Temp7, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 7")
      cord.x <- c(Time7[1], Time7, max(Time7))
      cord.y <- c(Temp7[1], Temp7, Temp7[1])
      polygon(cord.x,cord.y,col='darkmagenta')
sum = vector('expression',4)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[5,2],dig=4)))[2]
sum[2] = substitute(expression(Amplitude == MYVALUE1), 
                    list(MYVALUE1 = format(Summary[5,3],dig=3)))[2]
sum[3] = substitute(expression(TempDiff == MYVALUE2), 
                    list(MYVALUE2 = format(Summary[5,4], digits = 2)))[2]
sum[4] = substitute(expression(Area == MYVALUE3), 
                    list(MYVALUE3 = format(Summary[5,5], digits = 6)))[2]
legend('topright', legend = sum, bty = 'n')

plot(Time8, Temp8, type="l", ylab = "Temperature (deg C)", ylim=c(0, 400),xlab = "Time", lwd = 2, main="Plot 8")
      cord.x <- c(Time8[1], Time8, max(Time8))
      cord.y <- c(Temp8[1], Temp8, Temp8[1])
      polygon(cord.x,cord.y,col='indianred1')
sum = vector('expression',4)
sum[1] = substitute(expression(MaximumTemp == MYVALUE), 
                    list(MYVALUE = format(Summary[6,2],dig=4)))[2]
sum[2] = substitute(expression(Amplitude == MYVALUE1), 
                    list(MYVALUE1 = format(Summary[6,3],dig=4)))[2]
sum[3] = substitute(expression(TempDiff == MYVALUE2), 
                    list(MYVALUE2 = format(Summary[6,4], digits = 2)))[2]
sum[4] = substitute(expression(Area == MYVALUE3), 
                    list(MYVALUE3 = format(Summary[6,5], digits = 6)))[2]
legend('topright', legend = sum, bty = 'n')


#calculate summary values
se<-function (x){ sd(x)/sqrt(length(x))} 
Summary
mean(Summary$MaxTemp)
mean(Summary$Amplitude)
mean(Summary$TempDiff[c(-2,-4)])
mean(Summary$AUC[c(-2,-4)])
se(Summary$MaxTemp)
se(Summary$Amplitude)
se(Summary$TempDiff[c(-2,-4)])
se(Summary$AUC[c(-2,-4)])











