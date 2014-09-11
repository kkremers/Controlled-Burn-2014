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
before$BroGF <- class$BroadGF[match(before$Spp, class$Species)] #add GF values to table by matching the SppStat column
after$BroGF <- class$BroadGF[match(after$Spp, class$Species)] #add GF values to table by matching the SppStat column
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
counts1 = as.data.frame(table(before$GFPlot)) #count number of times each species occurs for each plot (this is why we made that new column)
GF1=rep(c("Bryophyte", "Forb", "Graminoid", "Litter", "Shrub"), c(10,3,10,10,10)) #vector of growth forms
Plot1=c(1,10,2,3,4,5,6,7, 8, 9, 1,4,6,1,10,2,3,4,5,6,7, 8, 9,1,10,2,3,4,5,6,7, 8, 9,1,10,2,3,4,5,6,7, 8, 9)#vector of plot numbers
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
counts2 = as.data.frame(table(after$GFPlot)) #count number of times each species occurs for each plot (this is why we made that new column)
GF2=rep(c("Ash", "Bare Ground", "Bryophyte", "Forb", "Graminoid", "Litter", "Shrub"), c(6,3,10,2,10,10,10)) #vector of growth forms
Plot2=c(1,4,5,7,8,9,4,7,9,1,10,2,3,4,5,6,7, 8, 9, 1,5,1,10,2,3,4,5,6,7, 8, 9,1,10,2,3,4,5,6,7, 8, 9,1,10,2,3,4,5,6,7, 8, 9)#vector of plot numbers
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
  if(before[i,2] == "Shrub") {
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
  if(after[i,2] == "Shrub") {
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

before.sums = as.vector(tapply(before$MassPred, before$BroGF, sum))
after.sums = as.vector(tapply(after$MassPred, after$BroGF, sum))[3:7] #have brackets there because items 1 and 2 were NA because they corresponded to "Ash" and "Bare Ground". Not sure why these aren't removed when I remove the NAs above...
GrowthForm = c("Bryophyte", "Forb", "Graminoid", "Litter", "Shrub")
MassBurn = data.frame(GrowthForm, before.sums, after.sums)
colnames(MassBurn) = c("BroGF", "MassBefore", "MassAfter")
MassBurn #these masses are in units of g / area burned, where area burned = 0.2m2 ; need to convert to m2

MassDiff = MassBurn$MassBefore - MassBurn$MassAfter
Estimate = 0.5*sum(MassDiff)






