####################pointframe data analysis#################

############predicting mass burned using models#############

before=read.csv(file=file.choose()) #load cover data for pre-burn
after=read.csv(file=file.choose()) #load cover data for post-burn

head(before) #check tables
head(after)

#now we need to add growth form information into each table
#to do this, we will upload a table with information for each species and use a function similar to vlookup in excel

class=read.csv(file=file.choose()) #load table with growth form info for each species
BroGF1 = rep(NA, length(before$Spp)) #create empty vector for GF information to go in
BroGF2 = rep(NA, length(after$Spp)) #create empty vector for GF information to go in
before = data.frame(before, BroGF = BroGF1) #add empty column to existing dat table to fill in with broad growth form 
after = data.frame(after, BroGF=BroGF2) #add empty column to existing dat table to fill in with broad growth form 
before$BroGF <- class$BroadGF1[match(before$Spp, class$Species)] #add GF values to table by matching the SppStat column
after$BroGF <- class$BroadGF1[match(after$Spp, class$Species)] #add GF values to table by matching the SppStat column
#the above works like vlookup in excel

GFPlot1 = paste(before$BroGF, before$Plot, sep="") #create a concantenated column of growth form and plot
GFPlot2 = paste(after$BroGF, after$Plot, sep="")
before = data.frame(before, GFPlot1) #add new vector as column to dataframe
after = data.frame(after, GFPlot2)

head(before)
head(after)

#to calculate cover for each plot, need to know the number of points for each plot
#create a table with this information
numpts.before= matrix(NA, 10, 2)
for (i in 1:10) {
  plot.i = i
  data = subset(before, Plot == plot.i)
  pts = unique(data$Coord)
  numpts = length(pts)
  numpts.before[i,1] = plot.i
  numpts.before[i,2] = numpts
}
numpts.before= data.frame(numpts.before)
colnames(numpts.before) = c("Plot", "NumPts")

numpts.after= matrix(NA, 10, 2)
for (i in 1:10) {
  plot.i = i
  data = subset(after, Plot == plot.i)
  pts = unique(data$Coord)
  numpts = length(pts)
  numpts.after[i,1] = plot.i
  numpts.after[i,2] = numpts
}
numpts.after = data.frame(numpts.after)
colnames(numpts.after) = c("Plot", "NumPts")

#processing of before data: calculate cover for each species in each plot
counts1 = as.data.frame(table(before$GFPlot1)) #count number of times each species occurs for each plot (this is why we made that new column)
GF1=rep(c("Bryophyte", "Deciduous", "Evergreen", "Forb", "Graminoid", "Litter"), c(10,10,10,3,10,10)) #vector of growth forms
Plot1=c(1,10,2,3,4,5,6,7,8,9, 1,10,2,3,4,5,6,7,8,9, 1,10,2,3,4,5,6,7,8,9, 1,4,6, 1,10,2,3,4,5,6,7,8,9, 1,10,2,3,4,5,6,7,8,9)#vector of plot numbers
before = data.frame(Plot1, GF1, counts1[,2]) #make new dataframe with wanted data
colnames(before) = c("Plot", "BroGF", "Count") #change column names
head(before)
NumPts = rep(NA, length(before$BroGF)) #create empty vector for information to go in
before = data.frame(before, NumPts = NumPts) #add empty column to existing dat table to fill in with numper of points in that plot
before$NumPts <- numpts.before$NumPts[match(before$Plot, numpts.before$Plot)] #lookup number of points in each plot and add to table
Cover1 = rep(NA, length(before$BroGF)) #create empty vector for information to go in
before = data.frame(before, Cover = Cover1)
before$Cover = before$Count/before$NumPts
head(before)


#repeat above steps for "after" data
counts2 = as.data.frame(table(after$GFPlot2)) #count number of times each species occurs for each plot (this is why we made that new column)
GF2=rep(c("Ash", "Bare Ground", "Bryophyte", "Deciduous", "Evergreen", "Forb", "Graminoid", "Litter"), c(6,3,10,9,10,2,10,10)) #vector of growth forms
Plot2=c(1,4,5,7,8,9, 4,7,9, 1,10,2,3,4,5,6,7,8,9, 1,10,2,3,4,5,6,7,8, 1,10,2,3,4,5,6,7,8,9, 1,5, 1,10,2,3,4,5,6,7,8,9, 1,10,2,3,4,5,6,7,8,9)#vector of plot numbers
after = data.frame(Plot2, GF2, counts2[,2]) #make new dataframe with wanted data
colnames(after) = c("Plot", "BroGF", "Count") #change column names
head(after)
NumPts = rep(NA, length(after$BroGF)) #create empty vector for information to go in
after = data.frame(after, NumPts = NumPts) #add empty column to existing dat table to fill in with numper of points in that plot
after$NumPts <- numpts.after$NumPts[match(after$Plot, numpts.after$Plot)] #lookup number of points in each plot and add to table
Cover2 = rep(NA, length(after$BroGF)) #create empty vector for information to go in
after = data.frame(after, Cover = Cover2)
after$Cover = after$Count/after$NumPts
head(after)


#Now we want to predict mass based on the linear relationships we developed 
#script for linear relationships is called "CoverMassModelScript" and is part of this R project

#create table of model parameters
Params=read.csv(file=file.choose()) #load table with parameters
Params #check output

#Now we will use a for loop with if statements to fill in the cover table with predictions
#there is a predict function in R that will do this if you have the model output available, but this is easier for me with how the tables are currently formated

#first, add an empty column for predictions into each dataframe
Pred1 = rep(NA, length(before$BroGF)) #create empty vector for GF information to go in
Pred2 = rep(NA, length(after$BroGF)) #create empty vector for GF information to go in
before = data.frame(before, MassPred= Pred1) #add empty column to existing dat table to fill in with broad growth form 
after = data.frame(after, MassPred=Pred2) #add empty column to existing dat table to fill in with broad growth form 
head(before)
head(after)
#next we will fill in the new columns with model predictions based on growthform

for(i in 1:length(before$BroGF)) {
  if(before[i,2] == "Deciduous") {
    before$MassPred[i] = Params[1,2]*before$Cover[i] 
  }
  if(before[i,2] == "Evergreen") {
    before$MassPred[i] = Params[1,2]*before$Cover[i] 
  }
  if(before[i,2] == "Forb") {
    before$MassPred[i] = Params[2,2]*before$Cover[i] 
  }
  if(before[i,2] == "Graminoid") {
    before$MassPred[i] = Params[3,2]*before$Cover[i] 
  }
  if(before[i,2] == "Bryophyte") {
    before$MassPred[i] = Params[4,2]*before$Cover[i] 
  }
  if(before[i,2] == "Litter") {
    before$MassPred[i] = Params[4,2]*before$Cover[i] 
  }
}

for(i in 1:length(after$BroGF)) {
  if(after[i,2] == "Deciduous") {
    after$MassPred[i] = Params[1,2]*after$Cover[i] 
  }
  if(after[i,2] == "Evergreen") {
    after$MassPred[i] = Params[1,2]*after$Cover[i] 
  }
  if(after[i,2] == "Forb") {
    after$MassPred[i] = Params[2,2]*after$Cover[i] 
  }
  if(after[i,2] == "Graminoid") {
    after$MassPred[i] = Params[3,2]*after$Cover[i] 
  }
  if(after[i,2] == "Bryophyte") {
    after$MassPred[i] = Params[4,2]*after$Cover[i] 
  }
  if(after[i,2] == "Litter") {
    after$MassPred[i] = Params[4,2]*after$Cover[i] 
  }
}

#check outputs
head(before)
head(after) #some variables may be NA if they were not predicted (i.e, points that were just ash)

after = after[complete.cases(after),] #remove rows with NAs (this is only the "ash" rows)
head(after)

#now we want to make tables that have the total mass before and after for each growth form
#we should  be able to use tapply for this

before.sums= data.frame(tapply(before$MassPred, list(before$BroGF, before$Plot), sum))
after.sums = data.frame(tapply(after$MassPred, list(after$BroGF, after$Plot), sum))[3:8,] 
after.sums[2,9] = 0
before.sums = before.sums[-4,]
after.sums = after.sums[-4,]
Mass.before = apply(before.sums, 2, sum, na.rm = TRUE) #mass before by plot
Mass.after = apply(after.sums, 2, sum, na.rm = TRUE) #mass after by plot
#define function for standard error calculation
se<-function (x){ sd(x)/sqrt(length(x))} 

GrowthForm = c("Bryophyte", "Deciduous", "Evergreen", "Graminoid", "Litter")
DiffPlot1 = before.sums$X1 - after.sums$X1
DiffPlot2 = before.sums$X2 - after.sums$X2
DiffPlot3 = before.sums$X3 - after.sums$X3
DiffPlot4 = before.sums$X4 - after.sums$X4
DiffPlot5 = before.sums$X5 - after.sums$X5
DiffPlot6 = before.sums$X6 - after.sums$X6
DiffPlot7 = before.sums$X7 - after.sums$X7
DiffPlot8 = before.sums$X8 - after.sums$X8
DiffPlot9 = before.sums$X9 - after.sums$X9
DiffPlot10 = before.sums$X10 - after.sums$X10
MassBurn = data.frame(GrowthForm, DiffPlot1, DiffPlot2, DiffPlot3, DiffPlot4, DiffPlot5, DiffPlot6, DiffPlot7, DiffPlot8, DiffPlot9, DiffPlot10)
MassBurn #these masses are in units of g / m2

#Calculate mean and SE mass burned by growth form
MeanBurnGF = apply(MassBurn[,2:11], 1, mean)
SEBurnGF = apply(MassBurn[,2:11], 1, se)
Mass.lostGF = data.frame(GrowthForm, MeanBurnGF, SEBurnGF)
Mass.lostGF

#Calculate mean and SE Carbon burned by growth form
CarbonBurn = MassBurn[,2:11] * 0.5
MeanCBurnGF= apply(CarbonBurn, 1, mean)
SECBurnGF= apply(CarbonBurn, 1, se)
C.lostGF = data.frame(GrowthForm, MeanCBurnGF, SECBurnGF)
C.lostGF


#calculate mean and SE of Carbon before and after fire & put in table for boxplots
Means.C = c(mean(Mass.before*0.5), mean(Mass.after*0.5))
SE.C = c(se(Mass.before*0.5), se(Mass.after*0.5))
Time = c("Before", "After")
C.lost = data.frame(Time, Means.C, SE.C)
C.lost

#calculate mean and SE of Mass before and after fire & put in table for boxplots
Means.mass = c(mean(Mass.before), mean(Mass.after))
SE.mass = c(se(Mass.before), se(Mass.after))
Mass.lost = data.frame(Time, Means.mass, SE.mass)
Mass.lost



#Summary tables
Burned.summary = data.frame(Mass.lost, C.lost[,-1])
BurnedGF.summary = data.frame(Mass.lostGF, C.lostGF[,-1])
Burned.summary
BurnedGF.summary



#Now we want to do boxplots
#First need to create data tables for boxplots

plots = rep(c(1,2,3,4,5,6,7,8,9,10), c(5,5,5,5,5,5,5,5,5,5))
growthform = rep(c("Bryophyte", "Deciduous", "Evergreen", "Graminoid", "Litter"), 10)

massburned = c(MassBurn[,2], MassBurn[,3], MassBurn[,4], MassBurn[,5], MassBurn[,6], MassBurn[,7],
               MassBurn[,8], MassBurn[,9], MassBurn[,10], MassBurn[,11])
MassBurnGF.plots = data.frame(growthform, plots, massburned)
MassBurnGF.plots

time = rep(c("1", "2"), c(10,10))
Mass = c(as.vector(Mass.before), as.vector(Mass.after))
MassBurn.plots = data.frame(time, Mass)


massburnGF = c(before.sums[,1], before.sums[,2], before.sums[,3], before.sums[,4], before.sums[,5], before.sums[,6], before.sums[,7], before.sums[,8], before.sums[,9], before.sums[,10],
               after.sums[,1], after.sums[,2], after.sums[,3], after.sums[,4], after.sums[,5], after.sums[,6], after.sums[,7], after.sums[,8], after.sums[,9], after.sums[,10])
time2 = rep(c("1", "2"), c(50, 50))
growthform2 = rep(c("Bryophyte", "Deciduous", "Evergreen", "Graminoid", "Litter"), 20)
MassBurnGFTime = data.frame(time2, growthform2, massburnGF)

#now we can make plots
boxplot(massburnGF~time2*growthform2, data=MassBurnGFTime, col=c("darkolivegreen1", "springgreen4"),
        names=c("Bryophyte", "Bryophyte", "Deciduous", "Deciduous", "Evergreen", "Evergreen", "Graminoid", "Graminoid", "Litter", "Litter"),
        xlab = "Growth Forms Pre- & Post-burn", ylab = "Biomass g/m2" )
legend("topright", # places a legend at the appropriate place 
       c("Pre-burn","Post-burn"), # puts text in the legend 
       pch = 15, bty= "n", cex=1.25,
       col=c("darkolivegreen1","springgreen4")) # gives the legend lines the correct color and width

boxplot(Mass~time, data=MassBurn.plots, names=c("Pre-burn", "Post-burn"), col=c("darkolivegreen1", "springgreen4"),
        ylab = "Biomass g/m2")

