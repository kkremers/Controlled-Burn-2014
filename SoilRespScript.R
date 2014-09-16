#####Soil Respiration Analysis#####

#load data
dat0708=read.csv(file=file.choose()) 
dat0729=read.csv(file=file.choose()) 
dat0806=read.csv(file=file.choose()) 

head(dat0708) #check to make sure loaded properly



#want to calculate the slope of Cdry~time for each plot

TreatPlot = as.vector(unique(dat0708$TreatPlot))
N = length(TreatPlot)

Summary0708 = data.frame(Treat=numeric(N), Plot=numeric(N), TreatPlot = numeric(N), dCdt = numeric(N), SEslope = numeric(N), AvgDens = numeric(N))

for (i in 1:N){
  
  TreatPlot.i = TreatPlot[i]
  data.i = subset(dat0708, TreatPlot == TreatPlot.i)
  
  model = lm(Cdry ~ ETime, data = data.i) #I am forcing the intercept to be 0 so that when I predict biomass, I won't get a negative number.
  
  n=length(data.i$ETime)
  Slope=summary(model)$coefficients[2,1]
  SE = summary(model)$coefficients[2,2]
  
  AvgDens = mean(data.i$Density)
  Plot = data.i$Plot[1]
  Treat = as.character(data.i$Treat[1])
  
  Summary0708[i,] = c(Treat, Plot, TreatPlot.i, Slope, SE, AvgDens)
  
}

TreatPlot = as.vector(unique(dat0729$TreatPlot))
N = length(TreatPlot)

Summary0729 = data.frame(Treat=numeric(N), Plot=numeric(N), TreatPlot = numeric(N), dCdt = numeric(N), SEslope = numeric(N), AvgDens = numeric(N))

for (i in 1:N){
  
  TreatPlot.i = TreatPlot[i]
  data.i = subset(dat0729, TreatPlot == TreatPlot.i)
  
  model = lm(Cdry ~ ETime, data = data.i) #I am forcing the intercept to be 0 so that when I predict biomass, I won't get a negative number.
  
  n=length(data.i$ETime)
  Slope=summary(model)$coefficients[2,1]
  SE = summary(model)$coefficients[2,2]
  
  AvgDens = mean(data.i$Density)
  Plot = data.i$Plot[1]
  Treat = as.character(data.i$Treat[1])
  
  Summary0729[i,] = c(Treat, Plot, TreatPlot.i, Slope, SE, AvgDens)
  
}

TreatPlot = as.vector(unique(dat0806$TreatPlot))
N = length(TreatPlot)

Summary0806 = data.frame(Treat=numeric(N), Plot=numeric(N), TreatPlot = numeric(N), dCdt = numeric(N), SEslope = numeric(N), AvgDens = numeric(N))

for (i in 1:N){
  
  TreatPlot.i = TreatPlot[i]
  data.i = subset(dat0806, TreatPlot == TreatPlot.i)
  
  model = lm(Cdry ~ ETime, data = data.i) #I am forcing the intercept to be 0 so that when I predict biomass, I won't get a negative number.
  
  n=length(data.i$ETime)
  Slope=summary(model)$coefficients[2,1]
  SE = summary(model)$coefficients[2,2]
  
  AvgDens = mean(data.i$Density)
  Plot = data.i$Plot[1]
  Treat = as.character(data.i$Treat[1])
  
  Summary0806[i,] = c(Treat, Plot, TreatPlot.i, Slope, SE, AvgDens)
  
}


#check outputs
Summary0708
Summary0729
Summary0806


Vol = 0.000633 #volume of chamber in m3
SA = 0.0052 #area of chamber in m2


flux <- function(x, Vol, SA, dens) { #function to calculate cflux
    (dens*Vol*x)/SA
}

se = function(x) {sd(x)/sqrt(length(x))} #define function for standard error calculation

Cflux0708 = flux(as.numeric(Summary0708$dCdt), Vol, SA, as.numeric(Summary0708$AvgDens))
Cflux0729 = flux(as.numeric(Summary0729$dCdt), Vol, SA, as.numeric(Summary0729$AvgDens))
Cflux0806 = flux(as.numeric(Summary0806$dCdt), Vol, SA, as.numeric(Summary0806$AvgDens))

Summary0708 = data.frame(Summary0708, Cflux = Cflux0708)
Summary0729 = data.frame(Summary0729, Cflux = Cflux0729)
Summary0806 = data.frame(Summary0806, Cflux = Cflux0806)

#check outputs
Summary0708
Summary0729
Summary0806

#Convert units of Cflux
Summary0708$Cflux = Summary0708$Cflux*1E-6*44*86400
Summary0729$Cflux = Summary0729$Cflux*1E-6*44*86400
Summary0806$Cflux = Summary0806$Cflux*1E-6*44*86400



mean.dCdt0708 = as.vector(tapply(as.numeric(Summary0708$dCdt), Summary0708$Treat, mean))
mean.dCdt0729 = as.vector(tapply(as.numeric(Summary0729$dCdt), Summary0729$Treat, mean))
mean.dCdt0806 = as.vector(tapply(as.numeric(Summary0806$dCdt), Summary0806$Treat, mean))
se.dCdt0708 = as.vector(tapply(as.numeric(Summary0708$dCdt), Summary0708$Treat, se))
se.dCdt0729 = as.vector(tapply(as.numeric(Summary0729$dCdt), Summary0729$Treat, se))
se.dCdt0806 = as.vector(tapply(as.numeric(Summary0806$dCdt), Summary0806$Treat, se))

mean.flux0708 = as.vector(tapply(Summary0708$Cflux, Summary0708$Treat, mean))
mean.flux0729 = as.vector(tapply(Summary0729$Cflux, Summary0729$Treat, mean))
mean.flux0806 = as.vector(tapply(Summary0806$Cflux, Summary0806$Treat, mean))
se.flux0708 = as.vector(tapply(Summary0708$Cflux, Summary0708$Treat, se))
se.flux0729 = as.vector(tapply(Summary0729$Cflux, Summary0729$Treat, se))
se.flux0806 = as.vector(tapply(Summary0806$Cflux, Summary0806$Treat, se))

Date = c("July 7", "July 29", "August 6")
DaysSinceFire = c(1, 22, 30)
Burned.flux.mean = c(mean.flux0708[1], mean.flux0729[1], mean.flux0806[1])
Cont.flux.mean = c(mean.flux0708[2], mean.flux0729[2], mean.flux0806[2])
Burned.flux.SE = c(se.flux0708[1], se.flux0729[1], se.flux0806[1])
Cont.flux.SE = c(se.flux0708[2], se.flux0729[2], se.flux0806[2])
Burned.dCdt.mean = c(mean.dCdt0708[1], mean.dCdt0729[1], mean.dCdt0806[1])
Cont.dCdt.mean = c(mean.dCdt0708[2], mean.dCdt0729[2], mean.dCdt0806[2])
Burned.dCdt.SE = c(se.dCdt0708[1], se.dCdt0729[1], se.dCdt0806[1])
Cont.dCdt.SE = c(se.dCdt0708[2], se.dCdt0729[2], se.dCdt0806[2])

Summary = data.frame(Date, DaysSinceFire, B.flux.m = Burned.flux.mean, C.flux.m = Cont.flux.mean, B.flux.se = Burned.flux.SE, C.flux.se = Cont.flux.SE, 
                     B.dCdt.m = Burned.dCdt.mean, C.dCdt.m = Cont.dCdt.mean, B.dCdt.se = Burned.dCdt.SE, C.dCdt.se = Cont.dCdt.SE)


Summary # View final table


plot((B.flux.m)~DaysSinceFire, data = Summary, type="o", pch = 16, col = "red", ylim=c(0,7),  
    xlab = "Days Since Fire", ylab="Carbon Flux (g CO2/m2/day")
lines((C.flux.m)~DaysSinceFire, data=Summary, col = "blue")
points((C.flux.m)~DaysSinceFire, data=Summary, col = "blue", pch=16)
legend("topright", # places a legend at the appropriate place 
       c("Burned","Control"), # puts text in the legend 
       lwd = 1, pch = 16, bty= "n", cex=1.25,
       col=c("red","blue")) # gives the legend lines the correct color and width

plot(B.dCdt.m~DaysSinceFire, data = Summary, type="o", pch = 16, col = "red", ylim = c(0 , 0.3), 
     xlab = "Days Since Fire", ylab="dC/dt")
lines(C.dCdt.m~DaysSinceFire, data=Summary, col = "blue")
points(C.dCdt.m~DaysSinceFire, data=Summary, col = "blue", pch=16)
legend("topright", # places a legend at the appropriate place 
       c("Burned","Control"), # puts text in the legend 
       lwd = 1, pch = 16, bty= "n", cex=1.25,
       col=c("red","blue")) # gives the legend lines the correct color and width


#adding error bars
#function is from http://chitchatr.wordpress.com/2013/06/25/add-error-bars-to-a-plot-in-r/
Plot_ErrorBars<-function(x, y, x.err, y.err, xbar = F,
                         ybar = F, cap.scaling = 50, xcolor = "black",
                         ycolor = "black", lwidth = 1, ...){
  ycap.width <- (par("usr")[4] - par("usr")[3])/cap.scaling
  xcap.width <- (par("usr")[2] - par("usr")[1])/cap.scaling
  if(xbar == T){
    for(i in 1:length(x.err)){
      segments(x0 = x[i] + x.err[i], y0 = y[i],
               x1= x[i] - x.err[i],  y1 = y[i],
               lwd = lwidth, col = xcolor)
      segments(x0 = x[i] + x.err[i], y0 = y[i] + ycap.width,
               x1=x[i] + x.err[i],   y1 = y[i] - ycap.width,
               lwd = lwidth, col = xcolor)
      segments(x0 = x[i] - x.err[i], y0 = y[i] + ycap.width,
               x1=x[i] - x.err[i],   y1 = y[i] - ycap.width,
               lwd = lwidth, col = xcolor)
    }
  }
  if(ybar == T){
    for(i in 1:length(y.err)){
      segments(x0 = x[i], y0 = y[i] + y.err[i],
               x1= x[i],  y1 = y[i] - y.err[i],
               lwd = lwidth, col = ycolor)
      segments(x0 = x[i] + xcap.width, y0 = y[i] + y.err[i],
               x1=x[i] - xcap.width,   y1 = y[i] + y.err[i],
               lwd = lwidth, col = ycolor)
      segments(x0 = x[i] + xcap.width, y0 = y[i] - y.err[i],
               x1=x[i] - xcap.width,   y1 = y[i] - y.err[i],
               lwd = lwidth, col = ycolor)
    }
  }
}


Plot_ErrorBars(Summary$DaysSinceFire, Summary$B.flux.m, y.err = Summary$B.flux.se, ybar = T, ycolor = "red", cap.scaling = 100)
Plot_ErrorBars(Summary$DaysSinceFire, Summary$C.flux.m, y.err = Summary$C.flux.se, ybar = T, ycolor = "blue", cap.scaling = 100)
