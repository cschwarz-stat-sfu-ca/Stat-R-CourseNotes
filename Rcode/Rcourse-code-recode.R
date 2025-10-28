# Recoding and derived variables
# 2015-02-01 CJS Updates
# 2014-05-20 CJS First Edition


options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(car)
library(ggplot2)
library(gridExtra)
library(emmeans)
library(plyr)

cereal <- read.csv('../sampledata/cereal.csv', 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal[1:5,]
str(cereal)


# Simple derived variables
cereal$ProteinFatRatio <- cereal$protein/ cereal$fat
cereal$ProteinFatRatio

cereal$TotalCarbs <- cereal$carbo + cereal$sugars
cereal$TotalCarbs





#-------------------------------------------------------------------
#    Vectorizing common operations - avoiding if() statements
#
accidents <- read.csv('../../sampleData/road-accidents-2010.csv', header=TRUE,
             as.is=TRUE, strip.white=TRUE)
names(accidents)

# Avoid looping to recode data
accidents$Fatality <- "Neither"
for(i in 1:length(accidents$Accident_Severity)){
   if( accidents$Accident_Severity[i] ==1) 
      {accidents$Fatality[i] <-  'Fatal'} else
      {accidents$Fatality[i] <- 'Not Fatal'}
}


# A general recode function is available in the car package

# Categorical to Categorical Recodes
library(car)
accidents$Type <- recode(accidents$Accident_Severity,
      " 1='Fatal'; 2='Serious'; 3='Minor'; 
        else='Error'")
accidents$Type <- factor(accidents$Type, levels=c("Minor","Serious","Fatal"),
         ordered=TRUE)
xtabs( ~Accident_Severity+Type, data=accidents) # check recode


unique(accidents$Number_of_Vehicles)
accidents$Nveh <- recode(accidents$Number_of_Vehicles,
    "1='Single'; 2='Two'; 3:6='Multi'; 7:hi='Mess' ")
accidents$Nveh <- factor(accidents$Nveh, levels=c("Single","Two","Multi","Mess"),ordered=TRUE)
xtabs( ~Nveh+Number_of_Vehicles, data=accidents)


# Recode a continuous variable and check using a dot plot
# Change the time-of-day to hours since midnight
accidents[1:5, c("Date","Time")]
accidents$mydt <- as.POSIXct(paste(accidents$Date," ",accidents$Time),
                  format="%d/%m/%Y %H:%M", tz="UCT")
accidents$my.time <- as.numeric(format(accidents$mydt,"%H"))+
       as.numeric(format(accidents$mydt, "%M"))/60
accidents$TimePer <- recode(accidents$my.time,
       "lo:6='Night'; 6:12='Morning';
        12:16='Aft'; 16:20='Evening';
        20:hi='Night'" )
accidents$TimePer <- factor(accidents$TimePer,
        levels=c("Night","Morning","Aft","Evening"),
        ordered=TRUE)

recodetod <- ggplot(data=accidents, aes(x=TimePer, y=my.time))+
  ggtitle("Check recode of time of day")+
  xlab("Time Period")+ylab("Original time")+
  geom_point(position=position_jitter(w=0.2))
recodetod
ggsave(plot=recodetod, file="../../MyStuff/Images/accidents-recode-tod.png",
       h=4, w=6, units="in", dpi=300)


# Using the recode() function
library(car) # load the recode function
cereal$FiberClass <- recode(cereal$fiber,
          '  lo:1="low"; 
              1:5="medium";
              5:hi="high"  ')
cereal$FiberClass <- factor(cereal$FiberClass,
          levels=c("low","medium","high"),
          ordered=TRUE)
# Always check your work
xtabs(~FiberClass+fiber, data=cereal)
plot1 <- ggplot(data=cereal, aes(x=FiberClass, y=fiber))+
  geom_point(position=position_jitter(w=0.2))
plot1
ggsave(plot1, file="../../MyStuff/Images/cereal-recode-fiber.png",
       h=4, w=6, units="in", dpi=300)

#-------------------------------------------------------------------
# Exercise 1


cereal <- read.csv('../sampledata/cereal.csv', 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal[1:5,]
str(cereal)

# recode the type of cereal
library(car)
cereal$type.long <- car::recode(cereal$type, 
                        " 'C'='Cold'; 'H'='Hot'   ")
xtabs(~type+type.long, data=cereal, exclude=NULL, na.action=na.pass)

# recode grams of protein to above or below the median
# we need to construct the recode string
median(cereal$protein)
rec.string <- paste("lo:", median(cereal$protein),"='At or below median'; ", median(cereal$protein),":hi='Above median'", sep="")
rec.string
cereal$protein.class <- car::recode(cereal$protein, rec.string)
plot <- ggplot(data=cereal, aes(x=protein.class, y=protein))+
   ggtitle("Checking protein recode")+
   geom_point(position=position_jitter(w=0.3, h=0))+
   geom_hline(yintercept=median(cereal$protein), color="red")
plot
ggsave(plot=plot, file="../../MyStuff/Images/recode-cereal-protein.png",
       h=4, w=6, units="in", dpi=300)



#-------------------------------------------------------------------

#  Exercise 2
#  Look at impact of high winds and weather conditions on fatality proportion
unique(accidents$Accident_Severity)
accidents$Fatality <- accidents$Accident_Severity==1
xtabs(~Fatality + Accident_Severity, data=accidents)

accidents$Conditions <- recode(accidents$Weather_Conditions,
        "c(1,4)='Fine'; c(2,5)='Rain'; c(3,6)='Snow'; else=NA")
xtabs(~Conditions + Weather_Conditions, data=accidents)

accidents$Wind <- recode(accidents$Weather_Conditions,
        "c(1,2,3)='Not High'; 4:6='High';  else=NA")
xtabs(~Wind + Weather_Conditions, data=accidents)

library(plyr)
pfatal.df <- ddply(accidents, c("Wind","Conditions"), summarize,
                pfatal=mean(Fatality),
                pfatal.se=sqrt(sd(Fatality))/length(Fatality))
pfatal.df$lcl <- pfatal.df$pfatal - qnorm(.975)*pfatal.df$pfatal.se
pfatal.df$ucl <- pfatal.df$pfatal + qnorm(.975)*pfatal.df$pfatal.se
pfatal.df

plotCondWind <- ggplot(data=pfatal.df, aes(x=Conditions, y=pfatal, 
                shape=Wind, color=Wind, linetype=Wind, group=Wind))+
   ggtitle("Comparison of fatality rates under weather conditions")+
   ylab("P(fatal) + 95% ci")+xlab("Precipitation")+
   geom_point(size=5)+
   geom_line(size=3)+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), w=.2, size=1)
plotCondWind
ggsave(plot=plotCondWind, file="../../MyStuff/Images/accidents-fatal-by-wind.png",
   h=4, w=6, units="in", dpi=300)



