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

GFPlot1 = paste(before$BroGF, before$Plot, sep="_") #create a concantenated column of growth form and plot
GFPlot2 = paste(after$BroGF, after$Plot, sep="_")
before = data.frame(before, GFPlot = GFPlot1) #add new vector as column to dataframe
after = data.frame(after, GFPlot = GFPlot2)
GFPlotStat1 = paste(before$GFPlot, before$Stat, sep="_")
GFPlotStat2 = paste(after$GFPlot, after$Stat, sep="_")
before = data.frame(before, GFPlotStat = GFPlotStat1) #add new vector as column to dataframe
after = data.frame(after, GFPlotStat = GFPlotStat2)
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

#processing of before data: calculate cover for each species at each status in each plot
counts1 = as.data.frame(table(before$GFPlotStat)) #count number of times each species occurs for each plot (this is why we made that new column)
splits = data.frame(do.call(rbind, strsplit(as.vector(counts1$Var1), split = "_")))
before = data.frame(splits, counts1) #make new dataframe with wanted data
colnames(before) = c("BroGF", "Plot", "Stat", "GFPlotStat", "Count")
head(before)
NumPts = rep(NA, length(before$BroGF)) #create empty vector for information to go in
before = data.frame(before, NumPts = NumPts) #add empty column to existing dat table to fill in with numper of points in that plot
before$NumPts <- numpts.before$NumPts[match(before$Plot, numpts.before$Plot)] #lookup number of points in each plot and add to table
Cover1 = rep(NA, length(before$BroGF)) #create empty vector for information to go in
before = data.frame(before, Cover = Cover1)
before$Cover = before$Count/before$NumPts
head(before)


#repeat above steps for "after" data
counts2 = as.data.frame(table(after$GFPlotStat)) #count number of times each species occurs for each plot (this is why we made that new column)
splits = data.frame(do.call(rbind, strsplit(as.vector(counts2$Var1), split = "_")))
after = data.frame(splits, counts2) #make new dataframe with wanted data
colnames(after) = c("BroGF", "Plot", "Stat", "GFPlotStat", "Count")
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
  if(before[i,1] == "DECI") {
    before$MassPred[i] = Params[1,2]*before$Cover[i] 
  }
  if(before[i,1] == "EVER") {
    before$MassPred[i] = Params[1,2]*before$Cover[i] 
  }
  if(before[i,1] == "FORB") {
    before$MassPred[i] = Params[2,2]*before$Cover[i] 
  }
  if(before[i,1] == "GRAM") {
    before$MassPred[i] = Params[3,2]*before$Cover[i] 
  }
  if(before[i,1] == "BRYO") {
    before$MassPred[i] = Params[4,2]*before$Cover[i] 
  }
  if(before[i,1] == "LITT") {
    before$MassPred[i] = Params[4,2]*before$Cover[i] 
  }
}

head(before)

for(i in 1:length(after$BroGF)) {
  if(after[i,1] == "DECI") {
    after$MassPred[i] = Params[1,2]*after$Cover[i] 
  }
  if(after[i,1] == "EVER") {
    after$MassPred[i] = Params[1,2]*after$Cover[i] 
  }
  if(after[i,1] == "FORB") {
    after$MassPred[i] = Params[2,2]*after$Cover[i] 
  }
  if(after[i,1] == "GRAM") {
    after$MassPred[i] = Params[3,2]*after$Cover[i] 
  }
  if(after[i,1] == "BRYO") {
    after$MassPred[i] = Params[4,2]*after$Cover[i] 
  }
  if(after[i,1] == "LITT") {
    after$MassPred[i] = Params[4,2]*after$Cover[i] 
  }
}

head(after)
after = after[10:length(after[,1]),] #removes the NAs

#output tables
write.csv(before, "c:/Users/Rocha Lab/Desktop/Kelsey/CB_CoverBefore.csv")
write.csv(after, "c:/Users/Rocha Lab/Desktop/Kelsey/CB_CoverAfter.csv")

#now we want to make tables that have the total mass before and after for each growth form
#we should  be able to use tapply for this

before.sums= as.vector(tapply(before$MassPred, before$GFPlotStat, sum))
after.sums = as.vector(tapply(after$MassPred, after$GFPlotStat, sum))
after.sums = data.frame(GFPlotStat = after$GFPlotStat, Mass.after = after.sums[10:80])
Mass.after = rep(NA, length(before.sums))
Mass.sums = data.frame(GFPlotStat = before$GFPlotStat, Mass.before = before.sums, Mass.after = Mass.after)
Mass.sums$Mass.after <- after.sums$Mass.after[match(Mass.sums$GFPlotStat, after.sums$GFPlotStat)]
Mass.sums[is.na(Mass.sums)] = 0
Mass.sums
Mass.diff = rep(NA, length(Mass.sums[,1]))
Mass.sums = data.frame(Mass.sums, Mass.diff)
Mass.sums$Mass.diff = Mass.sums$Mass.before-Mass.sums$Mass.after
Mass.sums

for(i in 1:length(Mass.sums$Mass.diff)){
  if(Mass.sums$Mass.diff[i] < 0){
    Mass.sums$Mass.diff[i] = NA
  } 
}

Mass.sums
Mass.sums = Mass.sums[complete.cases(Mass.sums),]
write.csv(Mass.sums, "c:/Users/Rocha Lab/Desktop/Kelsey/CB_MassBurnedbyGF.csv")


#split up GFPlotStat column
groups = unlist(strsplit(as.character(Mass.sums$GFPlotStat), "_"))
cols = matrix(groups, ncol = 3, byrow=TRUE)
GF = as.vector(cols[,1])
Plot = as.vector(as.numeric(cols[,2]))
Stat = as.vector(cols[,3])
GFStat = paste(GF, Stat, sep="_")
Mass.sums = data.frame(GFPlotStat = Mass.sums$GFPlotStat, GFStat = GFStat, GF = GF, Plot=Plot, Stat=Stat, Mass.sums[,2:4])



#Calculate mean and SE mass burned by growth form and status
se<-function (x){ sd(x)/sqrt(length(x))} #define function for standard error calculation
MeanBeforeGF = tapply(Mass.sums$Mass.before, Mass.sums$GFStat, mean)
SEBeforeGF = tapply(Mass.sums$Mass.before, Mass.sums$GFStat, se)

MeanAfterGF = tapply(Mass.sums$Mass.after, Mass.sums$GFStat, mean)
SEAfterGF = tapply(Mass.sums$Mass.after, Mass.sums$GFStat, se)

MeanDiffGF = tapply(Mass.sums$Mass.diff, Mass.sums$GFStat, mean)
SEDiffGF = tapply(Mass.sums$Mass.diff, Mass.sums$GFStat, se)

GrowthForm = c("BRYO_A", "DECI_A", "EVER_A", "EVER_D", "FORB_A", "GRAM_A", "GRAM_D", "LITT_D")
MassGF = data.frame(GrowthForm, MassBef_mean = as.vector(MeanBeforeGF), 
                    MassBef_SE = as.vector(SEBeforeGF), 
                    MassAft_mean= as.vector(MeanAfterGF), 
                    MassAft_SE = as.vector(SEAfterGF), 
                    MassDiff_mean = as.vector(MeanDiffGF),
                    MassDiff_SE = as.vector(SEDiffGF))
MassGF

#Calculate mean and SE Carbon burned by growth form and status
C.sums = Mass.sums[,6:7]*0.5
colnames(C.sums)=c("C.before", "C.after")
C.diff=rep(NA, length(C.sums[,1]))
C.sums = data.frame(Mass.sums[,1:5], C.sums, C.diff)
C.sums$C.diff = C.sums$C.before-C.sums$C.after
C.sums

MeanBeforeGF_C = tapply(C.sums$C.before, C.sums$GFStat, mean)
SEBeforeGF_C = tapply(C.sums$C.before, C.sums$GFStat, se)

MeanAfterGF_C = tapply(C.sums$C.after, C.sums$GFStat, mean)
SEAfterGF_C = tapply(C.sums$C.after, C.sums$GFStat, se)

MeanDiffGF_C = tapply(C.sums$C.diff, C.sums$GFStat, mean)
SEDiffGF_C = tapply(C.sums$C.diff, C.sums$GFStat, se)


CGF = data.frame(GrowthForm, CBef_mean = as.vector(MeanBeforeGF_C), 
                    CBef_SE = as.vector(SEBeforeGF_C), 
                    CAft_mean= as.vector(MeanAfterGF_C), 
                    CAft_SE = as.vector(SEAfterGF_C), 
                    CDiff_mean = as.vector(MeanDiffGF_C),
                    CDiff_SE = as.vector(SEDiffGF_C))

SummaryGF = data.frame(MassGF, CGF[,2:7])

write.csv(SummaryGF, "c:/Users/Rocha Lab/Desktop/Kelsey/CB_SummaryGF.csv")


#calculate mean and SE of Mass before and after fire 
MassBefore = tapply(Mass.sums$Mass.before, Mass.sums$Plot, sum)
MassAfter = tapply(Mass.sums$Mass.after, Mass.sums$Plot, sum)
CBefore = 0.5*MassBefore
CAfter=0.5*MassAfter
Plot = seq(1, 10, length=10)
Summary = data.frame(Plot, MassBefore=as.vector(MassBefore), 
                     MassAfter=as.vector(MassAfter),
                     CBefore=as.vector(CBefore),
                     CAfter=as.vector(CAfter))
write.csv(Summary, "c:/Users/Rocha Lab/Desktop/Kelsey/CB_Summary.csv")

Mean_MassBefore = mean(Summary$MassBefore)
SE_MassBefore = se(Summary$MassBefore)
Mean_MassAfter = mean(Summary$MassAfter)
Se_MassAfter = se(Summary$MassAfter)
Mean_CBefore = mean(Summary$CBefore)
SE_CBefore = se(Summary$CBefore)
Mean_CAfter = mean(Summary$CAfter)
Se_CAfter = se(Summary$CAfter)

#Now we want to do boxplots

#now we can make plots
boxplot(Mass.diff~GFStat, data=Mass.sums, 
        col="darkolivegreen1",
        names=c("Bryophyte", "Deciduous Alive", "Evergreen Alive", "Evergreen Dead", 
                "Forb Alive", "Graminoid Alive", "Graminoid Dead", "Litter"),
        ylab = "Biomass Burned (g/m2)" )


time=rep(c(1,2), c(10,10))
Mass = c(MassBefore, MassAfter)
MassTot = data.frame(time, Mass=as.vector(Mass))

boxplot(Mass~time, data=MassTot, names=c("Pre-burn", "Post-burn"), col=c("darkolivegreen1", "springgreen4"),
        ylab = "Biomass g/m2")

