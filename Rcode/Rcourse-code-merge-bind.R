# Merging, binding, table lookup

library(ggplot2)
library(plyr)
library(readxl)

######################################################
# rbind() and plyr::rbind.fill()

df1 <- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","species-count.xlsx"), sheet="df1")
df2 <- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","species-count.xlsx"), sheet="df2")
df1
df2

# simple rbind
# species is stored as a character so not a problem in rbinding()
df.all <- rbind(df1, df2)
df.all
str(df.all)

# what happens if some data is character and some integer?
df1$count2 <- df1$Count
df2$count2 <- as.character(df2$Count)

df.all <- rbind(df1, df2)
df.all
str(df.all)

# what happens with factors?
# factor levels are combined but not reordered
df1$speciesF <- factor(df1$Species)
str(df1)
levels(df1$speciesF)

df2$speciesF <- factor(df2$Species)
str(df2)
levels(df2$speciesF)

df.all <- rbind(df1, df2)
df.all
str(df.all)
levels(df.all$speciesF)



#-----------------
# names do not match
df1$count3 <- df1$Count
df2$Count3 <- df2$Count

df.all <- rbind(df1, df2)
setdiff(names(df1), names(df2))
setdiff(names(df2), names(df1)) # be sure to look both ways
setdiff( union(names(df1), names(df2)), intersect(names(df1), (names(df2))))

# force the stacking even if column names do not match
df.all <- plyr::rbind.fill(df1, df2)
df.all


#-----------------
# unspecified number of data frames, or table driven
sheets.to.read <- readxl::excel_sheets(file.path("Rcourse-code-merge-bind-ds","species-count.xlsx"))
sheets.to.read

data.list <- plyr::llply(sheets.to.read, function(x, workbook){
   df <- readxl::read_excel(workbook, sheet=x)
   df
}, workbook=file.path("Rcourse-code-merge-bind-ds","species-count.xlsx"))

str(data.list)

# try this?
df.all <- rbind(data.list)
df.all

# use do.call to bind a list of unspecified length together
df.all <- do.call(rbind, data.list)
df.all


#-----------------
# Augmenting results over a loop
# See http://www.win-vector.com/blog/2015/07/efficient-accumulation-in-r/
# BAD FORM - augmenting data frames, e.g. from a simulaton.
# Need to make a new copy of data frame in each loop

results <- NULL
for(i in 1:10){
    sim.result <- data.frame(sim=i, result=rnorm(1))
    results <- rbind(results, sim.result)
}
results

# better to define receiving structure and insert, but still not good
results <- data.frame(sim=1:10, sim.result=NA)
for(sim in 1:10){
    sim.result <-rnorm(1)
    results[sim, "sim.result"] <- sim.result
}
results

# best, use ldply to do the simulation. This allows for parallelization of code etc
results <- plyr::ldply(1:10, function(sim){
     sim.result <- data.frame(sim=sim, result=rnorm(1))
})
results

# Never use a FOR loop unless you call me first. 

#-----------------
# Exercise - two different models; find residuals; stock the results
cereal <- read.csv(file.path("..","sampledata",'cereal.csv'), 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal$shelfF <- factor(cereal$shelf)

fit1 <- lm( calories ~ fat, data=cereal)
fit1

fit2 <- lm( calories ~ fat + I(fat^2), data=cereal)
fit2

resid1 <- data.frame(order="linear", fat=cereal$fat, fit=fitted(fit1), resid=resid(fit1))
resid2 <- data.frame(order="quad",   fat=cereal$fat, fit=fitted(fit2), resid=resid(fit2))
resid <- rbind(resid1, resid2)

res.plot <- ggplot(data=resid, aes(x=fat, y=resid))+
   ggtitle("Residual plots from the two models")+
   geom_point()+
   geom_hline(yintercept=0)+
   facet_wrap(~order)
res.plot
ggsave(res.plot,
       file=file.path("..","..","MyStuff","Images","merge-bind-001.png"),
       h=6, w=6, units="in", dpi=300)



################################################################################
################################################################################
################################################################################
################################################################################
#
# cbind() - CAUTIOUS - never assume that data are in the same order and so avoid.


# 1-1 merging
i2000 <- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","IncomeData.xlsx"), sheet="I2000")
i2001 <- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","IncomeData.xlsx"), sheet="I2001")
i2002 <- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","IncomeData.xlsx"), sheet="I2002")

# notice data in different order. Do not use cbind() here.
i2000
i2001
i2002

# merge the data together. 
income <- merge(i2000, i2001)
income


# what happens with missing data
merge(i2000, i2002)
merge(i2000, i2002, all=TRUE)
merge(i2000, i2002, all.x=TRUE)
merge(i2000, i2002, all.y=TRUE)


# merging multiple data frames
# multiple merges - Google is your friend
Reduce(function(...){merge(..., all=TRUE)}, 
    list(i2000, i2001, i2002))


#-----------------
# 1-Many matching
# Data collected at different levels
child<- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","IncomeData.xlsx"), sheet="Children")
child

merge(i2000, child)
merge(i2000, child, all.x=TRUE)
merge(i2000, child, all=TRUE)


# 1-Many matching often used for table lookup
eschool <- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","IncomeData.xlsx"), sheet="ElementarySchool")
eschool

# Propogate elementary school information to each child
child <- merge(child, eschool, all.x=TRUE)  # do NOT use all=TRUE
child




#-----------------
# Using merges to insert implied zeroes
# Species of interest
Species <- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","BirdDetects.xlsx"), sheet="SpeciesList")
Species

# Notice that not all points visited in all years
VisitInfo <- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","BirdDetects.xlsx"), sheet="VisitInfo")
VisitInfo

# Notice that only positive detections listed here
Detects <- readxl::read_excel(file.path("Rcourse-code-merge-bind-ds","BirdDetects.xlsx"), sheet="Detects")
Detects


# Get master set of species x VisitInfo, i.e .all visits x species of interest
dim(VisitInfo)
dim(Species)

VisitInfoSpecies <- merge(VisitInfo, Species)
dim(VisitInfoSpecies)
head(VisitInfoSpecies)

# Now merge with positive counts and impute zeroes
AllCounts <- merge(Detects, VisitInfoSpecies, all.y=TRUE)
dim(Detects)
dim(VisitInfoSpecies)
dim(AllCounts)
head(AllCounts)

# Add the imputed 0's
AllCounts$Count[ is.na(AllCounts$Count)] <- 0
head(AllCounts)

# Now compute the average counts per point by year
plyr::ddply(AllCounts, c("Year", "Species"), plyr::summarize,
            mean.count = mean(Count))


#--------------------------
# Exercise.
# How does proportion of fatality vary with number of vehicles involved in accidents?

#-------------------------------------------------------------------
# Read in the accident data and get the date and fatality variables set
accidents <- read.csv(file.path("..","sampledata","Accidents",'road-accidents-2010.csv'), header=TRUE,
             as.is=TRUE, strip.white=TRUE)

# Convert date to internal date format
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")
# Create the fatality variable
accidents$Fatality <- accidents$Accident_Severity == 1
accidents[1:5,]

# here is something very odd. Look what happens to the Acc_Index variable when read in.
# It looks like a character variable, but is interpretted as a number?
vehicles <- read.csv(file.path("..","sampledata","Accidents",'road-accidents-vehicles-2010.csv'), header=TRUE,
             as.is=TRUE, strip.white=TRUE)
head(vehicles) 
vehicles[1:5, 1:3]

# Count number of vehicles in accident
n.vehicles <- plyr::ddply(vehicles, "Acc_Index", plyr::summarize,
                          n.vehicles=length(Acc_Index),
                          where=min(rownum))
head(n.vehicles)
xtabs(~n.vehicles,data=n.vehicles)


# are there any accidents with missing data?
setdiff(accidents$Accident_Index, n.vehicles$Acc_Index)
setdiff(n.vehicles$Acc_Index, accidents$Accident_Index)

# merge with accident data. Notice key column has a different name in both files
accidents2 <- merge(accidents, n.vehicles, by.x="Accident_Index", by.y="Acc_Index")
dim(accidents2)

# compute the fatality proportion by number of vehicles
p.fatal <- plyr::ddply(accidents2, "n.vehicles", plyr::summarize,
                        p.fatal=mean(Fatality))
head(p.fatal)

# a plot
fatal.plot <- ggplot(data=p.fatal, aes(x=n.vehicles, y=p.fatal))+
  ggtitle("Relationship between fatality proportion and # of vehicles")+
  geom_point()
fatal.plot
ggsave(fatal.plot,
       file=file.path("..","..","MyStuff","Images","merge-bind-pfatal-nvehicles.png"),
       h=6, w=6, units="in", dpi=300)



#-------------------------------------------
# Exercise II - imputing 0 counts in yes/no detections and 
# looking at the probability of detection across time for species

transect <- read.csv(file.path("..","sampledata","BirdDetect","transect.csv"),  header=TRUE, as.is=TRUE, strip.white=TRUE)
field    <- read.csv(file.path("..","sampledata","BirdDetect","FieldInfo.csv"), header=TRUE, as.is=TRUE, strip.white=TRUE)
detect   <- read.csv(file.path("..","sampledata","BirdDetect","detect.csv"),    header=TRUE, as.is=TRUE, strip.white=TRUE)
species  <- read.csv(file.path("..","sampledata","BirdDetect","species.csv"),   header=TRUE, as.is=TRUE, strip.white=TRUE)

head(species)
head(transect)
head(field)
head(detect)

# find the total detections by species and get the top 50 species
total.detects <- plyr::ddply(detect, 'AOU_Code', plyr::summarize,
                             n.detect=length(AOU_Code))
total.detects
sum(total.detects$n.detect > 200)
total.detects <- total.detects[ order(total.detects$n.detect, decreasing=TRUE),]
species.of.interest <- total.detects[1:50,]
species.of.interest

# only select detection records of species of interest
dim(detect)
detect <- detect[ detect$AOU_Code %in% species.of.interest$AOU_Code,]
dim(detect)

# check that all detection records are in field visits.
head(field)
field$Year <- lubridate::year(field$Date)
head(detect)
detect$Year <- lubridate::year(detect$Date)

# create a key with Year, transect,point
field$Key <- paste(field$Year, field$ParkTransectID, field$PointID, sep=".")
detect$Key<- paste(detect$Year, detect$ParkTransectID, detect$PointID, sep=".")
setdiff(detect$Key, field$Key) # this should be empty
setdiff(field$Key, detect$Key) # this may be non-empty


# create species x field visit
dim(field)
field <- merge(field, species.of.interest)
dim(field)


# impute zeroes
names(field)
names(detect)
dim(field)
dim(detect)
detect <- merge(detect, field, all.y=TRUE)
dim(detect)
detect$detect[ is.na(detect$detect)] <- 0

# Now to compute the proportion of birds detected by year on each of the points
p.detect <- plyr::ddply(detect, c("AOU_Code","Year"), plyr::summarize, 
                        p.detect=mean(detect))
xtabs(~AOU_Code+Year, data=p.detect)

detect.plot <- ggplot(data=p.detect, aes(x=Year, y=p.detect, group=AOU_Code))+
  ggtitle("Proportion detected over time of top 50 species")+
  geom_line()+
  scale_x_continuous(breaks=2000:3000)
detect.plot
ggsave(detect.plot,
       file=file.path("..","..","MyStuff","Images","merge-bind-pdetect.png"),
       h=6, w=6, units="in", dpi=300)


