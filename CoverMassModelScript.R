#Kelseyann Kremers
#Rocha lab, University of Notre Dame
#kkremers@nd.edu

############pointframe data analysis########################

###########controls - biomass and cover relationships#######


##STEP 1: create tables with mass and cover by growth form

ctrl=read.csv(file=file.choose()) #load cover data
head(ctrl)
SppStat = paste(ctrl$Spp, ctrl$Stat, sep="")
ctrl = data.frame(ctrl, SppStat)
plots = unique(ctrl$Plot)  #list unique plots
data1 = array(1, c(15, 4, 3)) #create empty array
dim(data1) #check dimensions

for (i in 1:3){ #run for loop to calculate cover for each species in each plot
  plot.i = plots[i]
  data = subset(ctrl, Plot == plot.i)
  pts = unique(data$Coord)
  numpts = length(pts)
  counts = as.data.frame(table(data$SppStat))
  cover = counts$Freq/numpts
  plot.ii = rep(i, length(cover))
  spp=as.vector(counts$Var1)
  data1[,1,i] = plot.ii
  data1[,2,i] = spp
  data1[,3,i] = counts$Freq
  data1[,4,i] = cover
}
  
dat = data.frame(rbind(data1[,,1], data1[,,2], data1[,,3])) #changing format
colnames(dat) = c("Plot", "SppStat", "Freq", "Cover") #change column names
head(dat) #check data
class = read.csv(file=file.choose()) #import GF information
NarGF = rep(NA, 45) #create empty vector for GF information to go in
BroGF = rep(NA, 45) #create empty vector for GF information to go in
dat = data.frame(dat, NarGF, BroGF) #add empty column to existing dat table
dat$NarGF <- class$NarGFStat[match(dat$SppStat, class$SppStat)] #add GF values to table by matching the SppStat column
dat$BroGF <- class$BroGFStat[match(dat$SppStat, class$SppStat)] #add GF values to table by matching the SppStat column
#the above works like vlookup in excel

dat$Cover = as.numeric(as.character(dat$Cover)) #convert to numeric for calculations
head(dat) #check data

Mass = read.csv(file=file.choose()) #upload mass data
Mass #check, we want to use MASS PER METER SQUARED which is column 5 (Mass_m2)

#calculate sums by GF
NGF.sums = as.data.frame(tapply(dat$Cover, list(dat$NarGF, dat$Plot), sum)) 
Mass.sumsNGF = as.data.frame(tapply(Mass$Mass_m2, list(Mass$NarGFStat, Mass$Plot), sum))
BGF.sums = as.data.frame(tapply(dat$Cover, list(dat$BroGF, dat$Plot), sum)) 
Mass.sumsBGF = as.data.frame(tapply(Mass$Mass_m2, list(Mass$BroGFStat, Mass$Plot), sum))


NGF.sums = NGF.sums[complete.cases(NGF.sums),] #remove rows with NAs
BGF.sums = BGF.sums[complete.cases(BGF.sums),] #remove rows with NAs
Mass.sumsNGF = Mass.sumsNGF[complete.cases(Mass.sumsNGF),] #remove rows with NAs
Mass.sumsBGF = Mass.sumsBGF[complete.cases(Mass.sumsBGF),] #remove rows with NAs
NGF.sums=NGF.sums[c(-5, -11),]
BGF.sums=BGF.sums[c(-2, -4),]


#create vectors for final tables

NGFStat = rep(c("DeciduousA", "EvergreenA", "EvergreenD", "ForbA", "GraminoidA", "GraminoidD", "LichenA", "LitterD", "MossA"), 3)
BGFStat1 = rep(c("ShrubA", "ShrubA", "ShrubD", "ForbA", "GraminoidA", "GraminoidD", "BryophyteA", "LitterD", "BryophyteA"), 3)
BGFStat2 = rep(c("BryophyteA", "ForbA", "GraminoidA", "GraminoidD", "LitterD", "ShrubA", "ShrubD"))
LitGFStat = rep(c("ShrubA", "ShrubA", "LitterD", "ForbA", "GraminoidA", "LitterD", "BryophyteA", "LitterD", "BryophyteA"), 3)
NGF = rep(c("Deciduous", "Evergreen", "Evergreen", "Forb", "Graminoid", "Graminoid", "Lichen", "Litter", "Moss"), 3)
BGF1 = rep(c("Shrub", "Shrub", "Shrub", "Forb", "Graminoid", "Graminoid", "Bryophyte", "Litter", "Bryophyte"), 3)
BGF2 = rep(c("Bryophyte", "Forb", "Graminoid", "Graminoid", "Litter", "Shrub", "Shrub"))
LitGF = rep(c("Shrub", "Shrub", "Litter", "Forb", "Graminoid", "Litter", "Bryophyte", "Litter", "Bryophyte"), 3)
NGF.rows= c(NGF.sums[,1], NGF.sums[,2], NGF.sums[,3])
BGF.rows1= c(NGF.sums[,1], NGF.sums[,2], NGF.sums[,3])
BGF.rows2= c(BGF.sums[,1], BGF.sums[,2], BGF.sums[,3])
LitGF.rows= c(NGF.sums[,1], NGF.sums[,2], NGF.sums[,3])
NGFmass.rows = c(Mass.sumsNGF[,1],Mass.sumsNGF[,2],Mass.sumsNGF[,3])
BGFmass.rows1 = c(Mass.sumsNGF[,1],Mass.sumsNGF[,2],Mass.sumsNGF[,3])
BGFmass.rows2 = c(Mass.sumsBGF[,1],Mass.sumsBGF[,2],Mass.sumsBGF[,3])
LitGFmass.rows = c(Mass.sumsNGF[,1],Mass.sumsNGF[,2],Mass.sumsNGF[,3])
Plots = rep(c(1, 2, 3), c(9, 9, 9)) #for NGF, BGF1, and LitGF
Plots2 = rep(c(1, 2, 3), c(7, 7, 7)) #for BGF2


#create final tables including cover and mass
CoverByNGF = data.frame(Plots, NGF, NGFStat, NGF.rows, NGFmass.rows)
colnames(CoverByNGF) = c("Plot", "NGF", "NGFStat", "Cover", "Mass")

CoverByBGF1 = data.frame(Plots, BGF1, BGFStat1, BGF.rows1, BGFmass.rows1)
colnames(CoverByBGF1) = c("Plot", "BGF", "BGFStat", "Cover", "Mass")

CoverByBGF2 = data.frame(Plots2, BGF2, BGFStat2, BGF.rows2, BGFmass.rows2)
colnames(CoverByBGF2) = c("Plot", "BGF", "BGFStat", "Cover", "Mass")

CoverByLitGF = data.frame(Plots, LitGF, LitGFStat, LitGF.rows, LitGFmass.rows)
colnames(CoverByLitGF) = c("Plot", "BGF", "BGFStat", "Cover", "Mass")


#check final tables
head(CoverByNGF)
head(CoverByBGF1)
head(CoverByBGF2)
head(CoverByLitGF)


##STEP 2: linear regressions to predict biomass

#need to run a loop that will do a regression for each growth form

#start with Narrow Growth Form
growthform = as.vector(unique(CoverByNGF$NGF))
N = length(growthform)

ParamsNGF = data.frame(NarrowGF = numeric(N), Slope = numeric(N), Rsquared = numeric(N), adjRsquared = numeric(N), pvalue = numeric(N), n = numeric(N), SEmass = numeric(N))

for (i in 1:N){
  
  growthform.i = growthform[i]
  data.i = subset(CoverByNGF, NGF == growthform.i)
  
  model = lm(Mass ~ Cover + 0, data = data.i) #I am forcing the intercept to be 0 so that when I predict biomass, I won't get a negative number.
  
  n=length(data.i$Cover)
  Slope=summary(model)$coefficients[1,1]
  Rsquared=summary(model)$r.squared
  adjRsquared=summary(model)$adj.r.squared
  pvalue=summary(model)$coefficients[1,4]
  SE = summary(model)$coefficients[1,2]


  
  ParamsNGF[i,] = c(growthform.i, Slope, Rsquared, adjRsquared, pvalue, n, SE)
    
}



#Broad Growth Form 1
growthform = as.vector(unique(CoverByBGF1$BGF))
N = length(growthform)

ParamsBGF1 = data.frame(BroadGF1 = numeric(N), Slope = numeric(N), Rsquared = numeric(N), adjRsquared = numeric(N), pvalue = numeric(N), n = numeric(N), SEmass = numeric(N))

for (i in 1:N){
  
  growthform.i = growthform[i]
  data.i = subset(CoverByBGF1, BGF == growthform.i)
  
  model = lm(Mass ~ Cover + 0, data = data.i)
  
  n=length(data.i$Cover)
  Slope=summary(model)$coefficients[1,1]
  Rsquared=summary(model)$r.squared
  adjRsquared=summary(model)$adj.r.squared
  pvalue=summary(model)$coefficients[1,4]
  SE = summary(model)$coefficients[1,2]
  
  ParamsBGF1[i,] = c(growthform.i, Slope, Rsquared, adjRsquared, pvalue,n, SE)
  
}


#Broad Growth Form 2
growthform = as.vector(unique(CoverByBGF2$BGF))
N = length(growthform)

ParamsBGF2 = data.frame(BroadGF2 = numeric(N), Slope = numeric(N), Rsquared = numeric(N), adjRsquared = numeric(N), pvalue = numeric(N), n = numeric(N), SEmass = numeric(N))

for (i in 1:N){
  
  growthform.i = growthform[i]
  data.i = subset(CoverByBGF2, BGF == growthform.i)
  
  model = lm(Mass ~ Cover + 0, data = data.i)
  
  n=length(data.i$Cover)
  Slope=summary(model)$coefficients[1,1]
  Rsquared=summary(model)$r.squared
  adjRsquared=summary(model)$adj.r.squared
  pvalue=summary(model)$coefficients[1,4]
  SE = summary(model)$coefficients[1,2]
  
  
  ParamsBGF2[i,] = c(growthform.i, Slope, Rsquared, adjRsquared, pvalue,n, SE)
  
}

#Litter GF
growthform = as.vector(unique(CoverByLitGF$BGF))
N = length(growthform)

ParamsLitGF = data.frame(BroadGF2 = numeric(N), Slope = numeric(N), Rsquared = numeric(N), adjRsquared = numeric(N), pvalue = numeric(N), n = numeric(N), SEmass = numeric(N))

for (i in 1:N){
  
  growthform.i = growthform[i]
  data.i = subset(CoverByLitGF, BGF == growthform.i)
  
  model = lm(Mass ~ Cover + 0, data = data.i)
  
  n=length(data.i$Cover)
  Slope=summary(model)$coefficients[1,1]
  Rsquared=summary(model)$r.squared
  adjRsquared=summary(model)$adj.r.squared
  pvalue=summary(model)$coefficients[1,4]
  SE = summary(model)$coefficients[1,2]
  
  
  ParamsLitGF[i,] = c(growthform.i, Slope, Rsquared, adjRsquared, pvalue,n, SE)
  
}


#output all the parameter tables to csv files

write.csv(ParamsNGF, "c:/Users/Rocha Lab/Desktop/Kelsey/StatsNGF.csv")
write.csv(ParamsBGF1, "c:/Users/Rocha Lab/Desktop/Kelsey/StatsBGF1.csv")
write.csv(ParamsBGF2, "c:/Users/Rocha Lab/Desktop/Kelsey/StatsBGF2.csv")
write.csv(ParamsLitGF, "c:/Users/Rocha Lab/Desktop/Kelsey/StatsLitGF.csv")


#####Create plots###

#NGF
growthform = as.vector(unique(CoverByNGF$NGF))
N = length(growthform)

pdf("c:/Users/Rocha Lab/Desktop/Kelsey/CoverMassModels_NGF.pdf")


for (i in 1:N){
  
  growthform.i = growthform[i]
  data.i = subset(CoverByNGF, NGF == growthform.i)
  
  model = lm(Mass~Cover + 0, data = data.i)
  
  adjRsquared=summary(model)$adj.r.squared
  pvalue=summary(model)$coefficients[1,4]

  # predicts + interval
  NewCover <- seq(min(data.i$Cover), max(data.i$Cover), length.out = length(data.i[,1]))
  newdata = data.frame(Cover=NewCover)
  Preds <- predict(model, newdata, interval = 'confidence')
  
  #plot
  plot(data.i$Mass ~ data.i$Cover, main = growthform.i, xlab = "Cover", ylab = "Mass")
  
  # add fill
  polygon(c(NewCover, rev(NewCover)), c(Preds[ ,2], rev(Preds[ ,3])), col = 'grey80', border = NA)
  # model
  abline(model)
  #data points
  points(data.i$Cover, data.i$Mass, pch = 16 )
  # intervals
  lines(NewCover, Preds[ ,3], lty = 'dashed', col = 'red')
  lines(NewCover, Preds[ ,2], lty = 'dashed', col = 'red')
  #legend
  rp = vector('expression',2)
  rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                     list(MYVALUE = format(adjRsquared,dig=2)))[2]
  rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                     list(MYOTHERVALUE = format(pvalue, digits = 2)))[2]
  legend('topleft', legend = rp, bty = 'n')
  
}

dev.off()



#BGF1
growthform = as.vector(unique(CoverByBGF1$BGF))
N = length(growthform)

pdf("c:/Users/Rocha Lab/Desktop/Kelsey/CoverMassModels_BGF1.pdf")


for (i in 1:N){
  
  growthform.i = growthform[i]
  data.i = subset(CoverByBGF1, BGF == growthform.i)
  
  model = lm(Mass~Cover + 0, data = data.i)
  
  adjRsquared=summary(model)$adj.r.squared
  pvalue=summary(model)$coefficients[1,4]
  
  # predicts + interval
  NewCover <- seq(min(data.i$Cover), max(data.i$Cover), length.out = length(data.i[,1]))
  newdata = data.frame(Cover=NewCover)
  Preds <- predict(model, newdata, interval = 'confidence')
  
  #plot
  plot(data.i$Mass ~ data.i$Cover, main = growthform.i, xlab = "Cover", ylab = "Mass")
  
  # add fill
  polygon(c(NewCover, rev(NewCover)), c(Preds[ ,2], rev(Preds[ ,3])), col = 'grey80', border = NA)
  # model
  abline(model)
  #data points
  points(data.i$Cover, data.i$Mass, pch = 16 )
  # intervals
  lines(NewCover, Preds[ ,3], lty = 'dashed', col = 'red')
  lines(NewCover, Preds[ ,2], lty = 'dashed', col = 'red')
  #legend
  rp = vector('expression',2)
  rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                     list(MYVALUE = format(adjRsquared,dig=2)))[2]
  rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                     list(MYOTHERVALUE = format(pvalue, digits = 2)))[2]
  legend('topleft', legend = rp, bty = 'n')
  
}

dev.off()



#BGF2
growthform = as.vector(unique(CoverByBGF2$BGF))
N = length(growthform)

pdf("c:/Users/Rocha Lab/Desktop/Kelsey/CoverMassModels_BGF2.pdf")


for (i in 1:N){
  
  growthform.i = growthform[i]
  data.i = subset(CoverByBGF2, BGF == growthform.i)
  
  model = lm(Mass~Cover + 0, data = data.i)
  
  adjRsquared=summary(model)$adj.r.squared
  pvalue=summary(model)$coefficients[1,4]
  
  # predicts + interval
  NewCover <- seq(min(data.i$Cover), max(data.i$Cover), length.out = length(data.i[,1]))
  newdata = data.frame(Cover=NewCover)
  Preds <- predict(model, newdata, interval = 'confidence')
  
  #plot
  plot(data.i$Mass ~ data.i$Cover, main = growthform.i, xlab = "Cover", ylab = "Mass")
  
  # add fill
  polygon(c(NewCover, rev(NewCover)), c(Preds[ ,2], rev(Preds[ ,3])), col = 'grey80', border = NA)
  # model
  abline(model)
  #data points
  points(data.i$Cover, data.i$Mass, pch = 16 )
  # intervals
  lines(NewCover, Preds[ ,3], lty = 'dashed', col = 'red')
  lines(NewCover, Preds[ ,2], lty = 'dashed', col = 'red')
  #legend
  rp = vector('expression',2)
  rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                     list(MYVALUE = format(adjRsquared,dig=2)))[2]
  rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                     list(MYOTHERVALUE = format(pvalue, digits = 2)))[2]
  legend('topleft', legend = rp, bty = 'n')
  
}

dev.off()



#LitGF
growthform = as.vector(unique(CoverByLitGF$BGF))
N = length(growthform)

pdf("c:/Users/Rocha Lab/Desktop/Kelsey/CoverMassModels_LitGF.pdf")


for (i in 1:N){
  
  growthform.i = growthform[i]
  data.i = subset(CoverByLitGF, BGF == growthform.i)
  
  model = lm(Mass~Cover + 0, data = data.i)
  
  adjRsquared=summary(model)$adj.r.squared
  pvalue=summary(model)$coefficients[1,4]
  
  # predicts + interval
  NewCover <- seq(min(data.i$Cover), max(data.i$Cover), length.out = length(data.i[,1]))
  newdata = data.frame(Cover=NewCover)
  Preds <- predict(model, newdata, interval = 'confidence')
  
  #plot
  plot(data.i$Mass ~ data.i$Cover, main = growthform.i, xlab = "Cover", ylab = "Mass")
  
  # add fill
  polygon(c(NewCover, rev(NewCover)), c(Preds[ ,2], rev(Preds[ ,3])), col = 'grey80', border = NA)
  # model
  abline(model)
  #data points
  points(data.i$Cover, data.i$Mass, pch = 16 )
  # intervals
  lines(NewCover, Preds[ ,3], lty = 'dashed', col = 'red')
  lines(NewCover, Preds[ ,2], lty = 'dashed', col = 'red')
  #legend
  rp = vector('expression',2)
  rp[1] = substitute(expression(italic(R)^2 == MYVALUE), 
                     list(MYVALUE = format(adjRsquared,dig=2)))[2]
  rp[2] = substitute(expression(italic(p) == MYOTHERVALUE), 
                     list(MYOTHERVALUE = format(pvalue, digits = 2)))[2]
  legend('topleft', legend = rp, bty = 'n')
  
}

dev.off()

