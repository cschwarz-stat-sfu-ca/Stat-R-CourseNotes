# Reshaping data using tidr rather than tidyr
# 2019-01-08 CJS First Edition

options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(car)
library(ggplot2)
library(gridExtra)
library(emmeans)
library(plyr)
library(tidyr)

chick.wide <- read.csv(file.path("..","sampledata","chickweight.csv"),          
           header=TRUE, as.is=TRUE,
          strip.white=TRUE)
head(chick.wide)


# Convert from wide to long
chick.long <- tidyr::gather(chick.wide,
                 key="Time",        # long descriptor
                 value="Weight",   # long values
                 c("Day01","Day02","Day04",
                   "Day06","Day08","Day10",
                   "Day12","Day14","Day16",
                   "Day18","Day20","Day21")
                )
head(chick.long)

head(chick.long[ order(chick.long$Chick, chick.long$Diet),])


# Now to compute the mean weight foe each time x diet combination
meanw <- plyr::ddply(chick.long, c("Time","Diet"), 
                     summarize,
                     day = as.numeric(substring(Time[1],4)),
                     meanw=mean(Weight, na.rm=TRUE))
meanw

plot.meanw <- ggplot2::ggplot(data=meanw, 
                              aes(x=day, y=meanw, 
                              color=Diet, linetype=Diet))+
                geom_point()+
                geom_line()+
                ggtitle("Mean weight over time")
plot.meanw

ggsave(plot=plot.meanw, file=file.path('..','..','MyStuff','Images','reshape-advanced-plot001.png'),
       h=4, w=6, units="in", dpi=300)


#--------------------------------------------------------------------
# reshaping variales for comparative plotting
# I want to compare calories from fat, protein carbohyrates for 3 shelves
# Need to melt the three variables

cereal <- read.csv(file.path('..','sampledata','cereal.csv'), 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal$shelfF <- factor(cereal$shelf)

head(cereal[,c("name","fat","protein","carbo")])

cereal$calories.fat     <- cereal$fat * 9
cereal$calories.protein <- cereal$protein *4
cereal$calories.carbo   <- cereal$carbo * 4

cereal.long <- tidyr::gather(cereal,
                  key="Source",
                  value="Calories",
                  c("calories.fat",
                    "calories.protein",
                    "calories.carbo"))
head(cereal.long)

temp <- cereal.long[ order(cereal.long$name, cereal.long$Source),]
row.names(temp) <- NULL
head(temp)

plot1 <- ggplot(data=cereal.long, aes(x=Source, y=Calories))+
   ggtitle("Comparing sources of calories")+
   geom_point(position=position_jitter(w=0.1))+
   geom_boxplot(alpha=0.2, outlier.size=0)+
   facet_grid(Source ~ shelfF, scales="free")
plot1
ggsave(plot=plot1, file=file.path('..','..','MyStuff','Images','reshape-advanced-plot002.png'),
       h=4, w=6, units="in", dpi=300)


plot2 <- ggplot(data=cereal.long, aes(x=shelfF, y=Calories))+
   ggtitle("Comparing sources of calories")+
   geom_point(position=position_jitter(w=0.1))+
   geom_boxplot(alpha=0.2, outlier.size=0)+
   facet_wrap(~Source )
plot2

ggsave(plot=plot2, file=file.path('..','..','MyStuff','Images','reshape-advanced-plot003.png'),
       h=4, w=6, units="in", dpi=300)


#-------------------------------------------------------------
# Casting

chick.wide2 <- tidyr::spread(chick.long,
                          key="Time",
                          value="Weight")
head(chick.wide2)


#---------------------------------------------------------
# Teeth

teeth.wide <- read.csv(file.path("..","sampledata","Teeth.csv"),
                       header=TRUE, strip.white=TRUE,
                       as.is=TRUE)
teeth.wide

teeth.wide$Incisors <- teeth.wide$Top.incisors + teeth.wide$Bottom.incisors
teeth.wide$Canines  <- teeth.wide$Top.canines  + teeth.wide$Bottom.canines
teeth.wide$Premolars<- teeth.wide$Top.premolars+ teeth.wide$Bottom.premolars
teeth.wide$Molars   <- teeth.wide$Top.molars   + teeth.wide$Bottom.molars

head(teeth.wide[,c("Mammal","Class","Incisors","Canines","Premolars","Molars")])

teeth.long <- tidyr::gather(teeth.wide,
                  key="Tooth.Type",
                  value="Teeth",
                  c("Incisors","Canines","Premolars","Molars")
                  )
head(teeth.long) # notice many other variable carried along

plot1 <-ggplot(data=teeth.long, aes(x=Tooth.Type, y=Teeth))+
  ggtitle("Comparing numbers of teeth")+
  geom_point(position=position_jitter(h=.1, w=.1))+
  geom_boxplot(alpha=0.2, outlier.size=0)+
  facet_wrap(~Class)
plot1

ggsave(plot=plot1, file=file.path('..','..','MyStuff','Images','reshape-advanced-teeth-001.png'),
       h=4, w=6, units="in", dpi=300)

plot2 <- ggplot(data=teeth.long, aes(x=Class, y=Teeth))+
  ggtitle("Comparing numbers of teeth")+
  geom_point(position=position_jitter(h=.1, w=.1))+
  geom_boxplot(alpha=0.2, outlier.size=0)+
  facet_wrap(~Tooth.Type, ncol=2)
plot2

ggsave(plot=plot2, file=file.path('..','..','MyStuff','Images','reshape-advanced-teeth-002.png'),
       h=4, w=6, units="in", dpi=300)

#--------------------------------------------------------------
# Birds 'n Butts

# Get the data
library(readxl)
exp.long <- readxl::read_excel(file.path("..","sampledata","bird-butts-data.xlsx"), 
                       sheet="Experimental", 
                       skip=1,
                       .name_repair="universal")
head(exp.long)

exp.wide <- tidyr::spread(exp.long,
                      key="Treatment",
                      value="Number.of.mites")
exp.wide$c.minus.e <- exp.wide$control - exp.wide$experimental
head(exp.wide)

plot1 <- ggplot(data=exp.wide, aes(x=Species, y=c.minus.e))+
  ggtitle("Comparing diff in number of mites")+
  geom_point(position=position_jitter(w=0.1))+
  geom_boxplot(alpha=0.2, outlier.size=0)+
  geom_hline(yintercept=0,color="red")
plot1
ggsave(plot=plot1, file=file.path('..','..','MyStuff','Images','reshape-advanced-plot004.png'),
       h=4, w=6, units="in", dpi=300)


#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------
# Multiple key-value pairs in melting available in the data.table package

library(readxl)
child.wide <- readxl::read_excel(file.path("..","sampledata","ncs_teaching_childhealth.xlsx"),
                       sheet="Wide-format-partial")
head(child.wide)

library(data.table)
child.long <- data.table::melt(as.data.table(child.wide), 
                  id.var="CHILD_PIDX",
                  measure.vars=list(c("VISIT_WT_6", "VISIT_WT_12",  "VISIT_WT_18",  "VISIT_WT_24"),
                                    c("CHILD_AGE_6","CHILD_AGE_12", "CHILD_AGE_18", "CHILD_AGE_24")),
                  variable.name="Visit",
                  value.name=c("Weight","Age"))
child.long <- child.long[ order(child.long$CHILD_PIDX, child.long$Visit),]
head(child.long)



library(readxl)
child.long <- readxl::read_excel(file.path("..","sampledata","ncs_teaching_childhealth.xlsx"),
                       sheet="Long-format",
                       na="M")
head(child.long)


# Long to wide
# don't forget to declare the long data frame as a data.table 
# If you forget to do this, you get an uninformative error.
child.wide <- data.table::dcast(as.data.table(child.long),  CHILD_PIDX ~ VISIT, 
                                 value.var=c("VISIT_WT","CHILD_AGE","GASTRO","EAR_INFECTION"))
head(child.wide)

