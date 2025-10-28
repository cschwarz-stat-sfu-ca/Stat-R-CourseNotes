# Reshaping ddata
# 2017-10-08 CJS First Edition

options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(car)
library(ggplot2)
library(gridExtra)
library(emmeans)
library(plyr)
library(reshape2)

chick.wide <- read.csv(file.path("..","sampledata","chickweight.csv"),          
           header=TRUE, as.is=TRUE,
          strip.white=TRUE)
head(chick.wide)


chick.long <- reshape2::melt(chick.wide,
                id.vars=c("Chick","Diet"),
                 measure.vars=c("Day01","Day02","Day04",
                                "Day06","Day08","Day10",
                                "Day12","Day14","Day16",
                                "Day18","Day20","Day21"),
                 variable.name="Time",
                 value.name="Weight")
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

ggsave(plot=plot.meanw, file=file,path('..','..','MyStuff','Images','reshape-plot001.png'),
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

cereal.long <- reshape2::melt(cereal,
                  id.var=c("name","shelfF"),
                  measure.var=c("calories.fat",
                                "calories.protein",
                                "calories.carbo"),
                  variable.name="Source",
                  value.name="Calories")
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
ggsave(plot=plot1, file=file.path('..','..','MyStuff','Images/reshape-plot002.png'),
       h=4, w=6, units="in", dpi=300)


plot2 <- ggplot(data=cereal.long, aes(x=shelfF, y=Calories))+
   ggtitle("Comparing sources of calories")+
   geom_point(position=position_jitter(w=0.1))+
   geom_boxplot(alpha=0.2, outlier.size=0)+
   facet_wrap(~Source )
plot2

ggsave(plot=plot2, file=file.path('..','..','MyStuff','Images','reshape-plot003.png'),
       h=4, w=6, units="in", dpi=300)


#-------------------------------------------------------------
# Casting

chick.wide2 <- reshape2::dcast(chick.long,
                          Chick+Diet ~ Time,
                          value.var="Weight")
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

teeth.long <- reshape2::melt(teeth.wide,
                  id.vars=c("Mammal","Class"),
                  measure.vars=c("Incisors","Canines","Premolars","Molars"),
                  value.name="Teeth",
                  variable.name="Tooth.Type")
head(teeth.long)

plot1 <-ggplot(data=teeth.long, aes(x=Tooth.Type, y=Teeth))+
  ggtitle("Comparing numbers of teeth")+
  geom_point(position=position_jitter(h=.1, w=.1))+
  geom_boxplot(alpha=0.2, outlier.size=0)+
  facet_wrap(~Class)
plot1

ggsave(plot=plot1, file=file.path('..','..','MyStuff','Images','reshape-teeth-001.png'),
       h=4, w=6, units="in", dpi=300)

plot2 <- ggplot(data=teeth.long, aes(x=Class, y=Teeth))+
  ggtitle("Comparing numbers of teeth")+
  geom_point(position=position_jitter(h=.1, w=.1))+
  geom_boxplot(alpha=0.2, outlier.size=0)+
  facet_wrap(~Tooth.Type, ncol=2)
plot2

ggsave(plot=plot2, file=file.path('..','..','MyStuff','Images','reshape-teeth-002.png'),
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

exp.wide <- reshape2::dcast(exp.long,
                      Nest+Species+Nest.content ~ Treatment,
                      value.var="Number.of.mites")
exp.wide$c.minus.e <- exp.wide$control - exp.wide$experimental
head(exp.wide)

plot1 <- ggplot(data=exp.wide, aes(x=Species, y=c.minus.e))+
  ggtitle("Comparing diff in number of mites")+
  geom_point(position=position_jitter(w=0.1))+
  geom_boxplot(alpha=0.2, outlier.size=0)+
  geom_hline(yintercept=0,color="red")
plot1
ggsave(plot=plot1, file=file.path('..','..','MyStuff','Images','reshape-plot004.png'),
       h=4, w=6, units="in", dpi=300)


