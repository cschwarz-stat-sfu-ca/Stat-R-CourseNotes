# Source code for ggplot() plotting - advanced
# 2015-01-30 CJS Update with new examples and explantions 
# 2014-05-19 CJS First Edition


options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(car)
library(GGally)
library(ggforce)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(plyr)
library(readxl)

set.seed(34234)
#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
# Cereal dataset

cereal <- read.csv(file.path('..',"sampledata",'cereal.csv'), 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal$shelfF <- factor(cereal$shelf)

cereal$fiber.grp <- car::recode(cereal$fiber, "lo:3='low'; 3:hi='high'")
xtabs(~fiber.grp, data=cereal)
cereal$fiber.grpF <- factor(cereal$fiber.grp) 

cereal[1:5,]


#-------------------------------------------------------------------
# Birds 'n Butts dataset
# Relationship between parasite load and number of butts in nest

butts <- readxl::read_excel('../sampledata/bird-butts-data.xlsx',
                           sheet='Correlational',
                           col_names=TRUE,
                           trim_ws = TRUE,
                           skip=1,
                           .name_repair="universal")
butts[1:5,]

# Fix the names
names(butts)[ grepl('wieght', names(butts))] <- "Butts.weight"
butts[1:5,]
butts$log.mites <- log(butts$Number.of.mites)

#-------------------------------------------------------------------
# Read in the accident data and get the date and fatality variables set
accidents <- read.csv(
              file.path('..','sampledata','Accidents','road-accidents-2010.csv'),
              header=TRUE, as.is=TRUE, strip.white=TRUE)
# Convert date to internal date format
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")
# Create the fatality variable
accidents$Fatality <- accidents$Accident_Severity == 1
accidents[1:5,]



###################################################################
###################################################################
###################################################################
###################################################################
###################################################################


#----------------------------------------------------------------
# Facetting
#The data can be split up by one or two variables that vary on the horizontal and/or vertical direction.
#This is done by giving a formula to facet_grid(), of the form vertical ~ horizontal.
#

cerealplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
     ggtitle("Calories vs fat in each serving")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_smooth(method="lm", se=FALSE)+
     facet_wrap(~shelfF, ncol=2)
cerealplot
ggsave(cerealplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-facet-cereal-001.png"),
       h=4, w=6, units="in", dpi=300)


# Plot the $\log(number~mites)$ vs.\ butts. weight with fitted line
# for each combination of species and nest content. 
mitesfacet <- ggplot(data=butts, aes(x=Butts.weight, y=log.mites))+
   ggtitle("Mean number of mites by butts weight")+
   xlab("Butts weight (g)")+ylab("log(number of mites)")+
   geom_point()+
   geom_smooth(method="loess", se=FALSE)+
   facet_grid(Species ~ Nest.content)
mitesfacet
ggsave(mitesfacet, 
    file=file.path("..","..","MyStuff","Images","bird-butts-mites-weight-facet-ggplot-001.png"),
    h=4, w=6,  units="in", dpi=300)

# Use two variables in a facet definition
mitesfacet <- ggplot(data=butts, aes(x=Butts.weight, y=log.mites))+
   ggtitle("Mean number of mites by butts weight")+
   xlab("Butts weight (g)")+ylab("log(number of mites)")+
   geom_point()+
   geom_smooth(method="loess", se=FALSE)+
   facet_wrap( ~Species + Nest.content, ncol=3)
mitesfacet
ggsave(mitesfacet, 
    file=file.path("..","..","MyStuff","Images","bird-butts-mites-weight-facet-ggplot-002.png"),
    h=4, w=6,  units="in", dpi=300)

mitesfacet <- ggplot(data=butts, aes(x=Butts.weight, y=log.mites))+
   ggtitle("Mean number of mites by butts weight")+
   xlab("Butts weight (g)")+ylab("log(number of mites)")+
   geom_point()+
   geom_smooth(method="loess", se=FALSE)+
   facet_wrap( ~Species + Nest.content, ncol=2)
mitesfacet
ggsave(mitesfacet, 
    file=file.path("..","..","MyStuff","Images","bird-butts-mites-weight-facet-ggplot-003.png"),
    h=4, w=6,  units="in", dpi=300)

mitesfacet <- ggplot(data=butts, aes(x=Butts.weight, y=log.mites))+
   ggtitle("Mean number of mites by butts weight")+
   xlab("Butts weight (g)")+ylab("log(number of mites)")+
   geom_point()+
   geom_smooth(method="loess", se=FALSE)+
   facet_wrap( ~Nest.content + Species, ncol=2)
mitesfacet
ggsave(mitesfacet, 
    file=file.path("..","..","MyStuff","Images","bird-butts-mites-weight-facet-ggplot-004.png"),
    h=4, w=6,  units="in", dpi=300)




# Changing labels o facets
# See http://ggplot2.tidyverse.org/reference/labellers.html and
# https://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels

mitesfacet <- ggplot(data=butts, aes(x=Butts.weight, y=log.mites))+
   ggtitle("Mean number of mites by butts weight")+
   xlab("Butts weight (g)")+ylab("log(number of mites)")+
   geom_point()+
   geom_smooth(method="loess", se=FALSE)+
   facet_wrap( ~Species + Nest.content, ncol=3, labeller=label_both)
mitesfacet
ggsave(mitesfacet, 
    file=file.path("..","..","MyStuff","Images","bird-butts-mites-weight-facet-ggplot-005.png"),
    h=4, w=6,  units="in", dpi=300)

# changing what is printed in a facet

# Method 1: Create new variable using car::recode. But order of facets may change
cereal$shelf2 <- car::recode(cereal$shelf, "  1='low'; 2='medium'; 3='high'")
cerealplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
     ggtitle("Calories vs fat in each serving")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_smooth(method="lm", se=FALSE)+
     facet_wrap(~shelf2, ncol=3)
cerealplot
ggsave(cerealplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-facet-cereal-002.png"),
       h=4, w=6, units="in", dpi=300)


# Method 2: Create factor variable with appropriate labels
cereal$shelfF2 <- factor(cereal$shelf, labels=c("low","medium","high"))
  
cerealplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
     ggtitle("Calories vs fat in each serving")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_smooth(method="lm", se=FALSE)+
     facet_wrap(~shelfF2, ncol=3)
cerealplot
ggsave(cerealplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-facet-cereal-003.png"),
       h=4, w=6, units="in", dpi=300)


# Method 3: Use a labeller function
cereal$shelfc <- as.character(cereal$shelf)
shelf_names <- c('1' = "low",
                 '2' = "medium",
                 '3' = "high")
cerealplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
     ggtitle("Calories vs fat in each serving")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_smooth(method="lm", se=FALSE)+
     facet_wrap(~shelfc, ncol=3, labeller=labeller(shelfc=as_labeller(shelf_names)))
cerealplot
ggsave(cerealplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-facet-cereal-004.png"),
       h=4, w=6, units="in", dpi=300)

# What happens if there is mismatch?
cereal$shelfc <- as.character(cereal$shelf)
shelf_names <- c('1' = "low",
                 '2' = "medium",
                 '4' = "high")
cerealplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
     ggtitle("Calories vs fat in each serving")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_smooth(method="lm", se=FALSE)+
     facet_wrap(~shelfc, ncol=3, labeller=labeller(shelfc=as_labeller(shelf_names)))
cerealplot
ggsave(cerealplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-facet-cereal-005.png"),
       h=4, w=6, units="in", dpi=300)



### Too many facets for a page
### The ggforce() package
cereal$shelfc <- as.character(cereal$shelf)
cerealplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
     ggtitle("Calories vs fat in each serving")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_smooth(method="lm", se=FALSE)+
     facet_grid(mfr~shelfc)
cerealplot

ggsave(cerealplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-facet-multi-page-cereal-001.png"),
       h=6, w=6, units="in", dpi=300)


# set up the plot and determine the number of pages
library(ggforce)
page=1
cerealplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
     ggtitle("Calories vs fat in each serving")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_smooth(method="lm", se=FALSE)+
     facet_grid_paginate(mfr~shelfc, nrow=3, ncol=3, page=page)
cerealplot
ggsave(cerealplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-facet-multi-page-cereal-002.png"),
       h=6, w=6, units="in", dpi=300)


pdf(file=file.path("..","..","MyStuff","Images","ggplot2-facet-multi-page-cereal-003.pdf"))
plyr::l_ply(1:3, function(page){
   cerealplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
     ggtitle("Calories vs fat in each serving")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_smooth(method="lm", se=FALSE)+
     facet_grid_paginate(mfr~shelfc, nrow=3, ncol=3, page=page)
   plot(cerealplot) # must be explicit in loops
})
dev.off()



# Exercise
# Got to the accident data and plot, for each WEEK, the breakdown of accidents by severity code 
accidents$week <- lubridate::week(accidents$mydate)

page=1
accident.plot <- ggplot(data=accidents, aes(x=Accident_Severity))+
   ggtitle("Breakdown of accidents by severity for each week")+
   geom_bar()+
   facet_wrap_paginate(~week, ncol=3, nrow=3, page=page, labeller=label_both)
plot(accident.plot)
ggsave(accident.plot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-facet-multi-page-accident-001.png"),
       h=6, w=6, units="in", dpi=300)

pdf("ggplot2-facet-multi-page-accident.pdf")
plyr::l_ply(1:ceiling(53/9), function(page){
   accident.plot <- ggplot(data=accidents, aes(x=Accident_Severity))+
      ggtitle("Breakdown of accidents by severity for each week")+
      geom_bar()+
      facet_wrap_paginate(~week, ncol=3, nrow=3, page=page, labeller=label_both)
   plot(accident.plot)
})
dev.off()

#---------- Multiple (different plots) on a page
library(gridExtra)

p1 <- ggplot(data=cereal, aes(x=fat, y=calories))+
      ggtitle("Calories vs fat")+
      geom_point()
p1

p2 <- ggplot(data=cereal, aes(x=calories))+
      ggtitle("Histogram of calories")+
      geom_histogram()
p2

p3 <- ggplot(data=cereal, aes(x=fat))+
      ggtitle("Histogram of fat")+
      geom_histogram()
p3

p4 <- ggplot(data=cereal, aes(x=shelf, y=calories))+
      ggtitle("Box plot of calories")+
      geom_boxplot()
p4

allplot <- arrangeGrob(p1,p2,p3,p4, nrow=2,
           top="A medley of plots")
plot(allplot)

ggsave(plot=allplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-arrangeGrob-001.png"),
       h=4, w=6, units="in", dpi=300)


# Or using GGally
library(GGally)
plot.list <- list(p1, p2, p3, p4)
allplot <- GGally::ggmatrix(plot.list, 
                            ncol=2, nrow=2)
allplot
ggsave(plot=allplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-ggally-ggmatrix-001.png"),
       h=6, w=6, units="in", dpi=300)


#  Exercise
# Refer to birds and butts.
# Make multiple plots per page with two columns = two species (HOSP, HOFI)
# two rows (number of mites by butts weight) and boxplots of number of mites by nest content

library(gridExtra)

p1 <- ggplot(data=butts[butts$Species=="HOSP",], aes(x=Butts.weight, y=Number.of.mites))+
      ggtitle("Number of mites vs. weight of butts in nest")+
      geom_point()+
      geom_smooth(se=FALSE)+
      facet_wrap(~Species)
p1

p2 <- ggplot(data=butts[butts$Species=="HOFI",], aes(x=Butts.weight, y=Number.of.mites))+
      ggtitle("Number of mites vs. weight of butts in nest")+
      geom_point()+
      geom_smooth(se=FALSE)+
      facet_wrap(~Species)
p2

p3 <- ggplot(data=butts[butts$Species=="HOSP",], aes(x=Nest.content, y=Number.of.mites))+
      ggtitle("Side by side box plots of number of mites by nest content")+
      geom_point( position=position_jitter(w=0.1))+
      geom_boxplot( alpha=0.2, outlier.size=0)
p3

p4 <- ggplot(data=butts[butts$Species=="HOFI",], aes(x=Nest.content, y=Number.of.mites))+
      ggtitle("Side by side box plots of number of mites by nest content")+
      geom_point( position=position_jitter(w=0.1))+
      geom_boxplot( alpha=0.2, outlier.size=0)
p4

allplot <- arrangeGrob(p1,p2,p3,p4, nrow=2,
           top="A medley of plots")
plot(allplot)

ggsave(plot=allplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-arrangeGrob-002.png"),
       h=4, w=6, units="in", dpi=300)


#---------------------------------------------------------------------
# Different legends on each facet
# In general, the legend in facetting refers to the whole set of facets
# and it is not possible to get a separate legend on each facet directly.
# Here we use faceting on individual plots and arrangeGrob() to get a 
# separate legend on each plot.
# Simulated data is used.

Species.grouping.csv <- textConnection(
"Species, Group
deer, ungulates
moose, ungulates
lynx, lynx/hare
hare, lynx/hare")

Species.grouping <- read.csv(Species.grouping.csv,
                             header=TRUE, strip.white=TRUE, as.is=TRUE)


counts <- expand.grid(Species=Species.grouping$Species, weeks=1:52)
counts$count <- rpois(nrow(counts),4)
counts <- merge(counts, Species.grouping)
head(counts)
# Can you get a separate legend in each facet
# see https://stackoverflow.com/questions/34956350/ggplot-different-legends-for-different-facets
# No. Facetting only let you have one legend for ALL the facets=
plot1 <- ggplot(data=counts, aes(x=weeks, y=count, color=Species))+
  geom_line()+
  facet_wrap(~Group, ncol=1)
plot1
ggsave(plot=plot1, 
       file=file.path("..","..","MyStuff","Images","ggplot2-arrangeGrob-010.png"),
       h=4, w=6, units="in", dpi=300)


# Following solution at 
#     https://stackoverflow.com/questions/14840542/place-a-legend-for-each-facet-wrap-grid-in-ggplot2
# Make a separate plot for each group with its own legend
# Use arrangeGrob to put the separate plot together

# BFI method
p1 <- ggplot(data=counts[counts$Group=="ungulates",], 
             aes(x=weeks, y=count, colour = Species)) + 
  geom_line() +
  facet_wrap(~Group, ncol=1)+
  theme(legend.position=c(1,1), legend.justification=c(1,1))
p1

p2 <- ggplot(data=counts[counts$Group=="lynx/hare",], 
             aes(x=weeks, y=count, colour = Species)) + 
  geom_line() +
  facet_wrap(~Group, ncol=1)+
  theme(legend.position=c(1,1), legend.justification=c(1,1))
p2

allplot <- gridExtra::arrangeGrob(p1, p2, ncol=1)
plot(allplot)
ggsave(plot=allplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-arrangeGrob-011.png"),
       h=4, w=6, units="in", dpi=300)



# Using advanced features of R
# make list of plots
ggList <- plyr::dlply(counts,"Group", function(groupdata) {
  ggplot(data=groupdata, aes(x=weeks, y=count, colour = Species)) + 
    geom_line() +
    facet_wrap(~Group, ncol=1)+
    theme(legend.position=c(1,1), legend.justification=c(1,1))
  })
ggList

ggsave(plot=ggList[[1]], 
       file=file.path("..","..","MyStuff","Images","ggplot2-arrangeGrob-012.png"),
       h=4, w=6, units="in", dpi=300)
ggsave(plot=ggList[[2]], 
       file=file.path("..","..","MyStuff","Images","ggplot2-arrangeGrob-013.png"),
       h=4, w=6, units="in", dpi=300)



allplot <- do.call(arrangeGrob, ggList)
plot(allplot)





#----------------------------------------------------------------------
# plotting geographical data
library(ggmap)
sfu.coord <- c(-122.917957, 49.276765 )

geocode("8888 University Drive, Burnaby, Canada", source="dsk") # or get the address directly

my.drive.csv <- textConnection("
long, lat
-122.84378900000002, 49.29009199999999
-122.82799615332033, 49.28426960031931
-122.82696618505861, 49.27755059244836
-122.86679162451173, 49.27676664856581
-122.88790597387697, 49.26276555269492
-122.90833367773439, 49.26534205263451
-122.92532815405275, 49.273518748310764
-122.91434182592775, 49.27766258341439")
my.drive <- read.csv(my.drive.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)

# get the map from stamen. You can fiddle with the zoom to get the right scale
my.map.dl <- ggmap::get_map(c(left=sfu.coord[1]-.02, bottom=sfu.coord[2]-.02, right=sfu.coord[1]+.12, top=sfu.coord[2]+.03), 
               maptype="watercolor",  source="stamen")
my.map <- ggmap(my.map.dl)

plot1 <- my.map +
         ggtitle("My drive to work")+
         geom_point(data=my.drive, aes(x=long, y=lat),size=1, color='red' )+
         geom_path(data=my.drive, aes(x=long, y=lat), color="red", size=2)+
         ylab("Latitude")+xlab("Longitude")
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","ggplot2-ggmap-001.png"),
       h=4, w=6, units="in", dpi=300)



# Exercise
# Plot the accidents data by month using facetting
accidents$month <- lubridate::month(accidents$mydate)
range(accidents$Longitude)
range(accidents$Latitude)

mean.lat <- mean(accidents$Latitude)
mean.long<- mean(accidents$Longitude)

geocode("England")

my.map.dl <- ggmap::get_map(c(left  =min(accidents$Longitude), bottom=min(accidents$Latitude), 
                              right =max(accidents$Longitude), top   =max(accidents$Latitude)), 
                             maptype="watercolor",  source="stamen", zoom=6)

my.map <- ggmap(my.map.dl)

fatal <- accidents[ accidents$Accident_Severity == 1,]
page=1
plot1 <- my.map +
         ggtitle("Location of fatal")+
         geom_point(data=fatal, aes(x=Longitude, y=Latitude), size=.4)+
         ylab("Latitude")+xlab("Longitude")+
         facet_wrap_paginate(~month, ncol=3, nrow=2, page=page)
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","ggplot2-ggmap-002.png"),
       h=4, w=6, units="in", dpi=300)

page=2
plot2 <- my.map +
         ggtitle("Location of fatal")+
         geom_point(data=fatal, aes(x=Longitude, y=Latitude), size=.4)+
         ylab("Latitude")+xlab("Longitude")+
         facet_wrap_paginate(~month, ncol=3, nrow=2, page=page)
plot2
ggsave(plot2, 
       file=file.path("..","..","MyStuff","Images","ggplot2-ggmap-003.png"),
       h=4, w=6, units="in", dpi=300)





#---------------------------------------------
# Other packages that are useful

# lm and glm() diagnostic plots
library(ggfortify)
lm.fit <- lm(calories ~ fat, data=cereal)
diagplot <- autoplot(lm.fit)
diagplot

ggsave(diagplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-ggfortify-001.png"),
       h=4, w=6, units="in", dpi=300)

# scatter plot matrix
library(GGally)
spm <- ggpairs(cereal, col=c(3,4,5,6,7,8,9,10))
spm
ggsave(spm, 
       file=file.path("..","..","MyStuff","Images","ggplot2-ggally-001.png"),
       h=4, w=6, units="in", dpi=300)



