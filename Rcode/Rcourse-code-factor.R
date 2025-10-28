# All about factors
# 2015-01-31 CJS First Edition

options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(car)
library(ggplot2)
library(gridExtra)
library(emmeans)
library(plyr)
library(readxl)

#--------------------------------------------------------------
# Factor variables
x <- rep(c("low","medium","high"), times=1:3)
x
str(x)

xF <- factor(x)
xF
str(xF)
as.numeric(xF)  # likely not what you want

xFO <- factor(x, levels=c("low","medium","high"), ordered=TRUE)
xFO
str(xFO)
as.numeric(xFO)  # likely not what you want

# get back the levels from before converting to a factor
levels(xF)[xF]   
levels(xFO)[xFO]
as.character(xF)

# Because numerical codes are converted to character when making a factor
# you may need to convert back 
xx <- c(3,1,3,3,3,1,2,2,3,2,1)
xxF <- factor(xx)
xxF
levels(xxF)[xxF]
as.numeric(levels(xxF)[xxF])
as.numeric(as.character(xxF))


#---------------------------------------------------------------
# default action is to convert strings to factors. We want to turn that OFF
# using the as.is=TRUR or stringsAsFactor=FALSE arguments when
# reading tables or creating data.frames (e.g. return in ddply)
cereal <- read.csv(file.path('..','..','sampledata','cereal.csv'), 
              header=TRUE)
cereal[1:5,]
str(cereal)



cereal <- read.csv(file.path('..','..','sampledata','cereal.csv'), 
              header=TRUE, as.is=TRUE, 
              strip.white=TRUE)
cereal[1:5,]
str(cereal)


# Create a factor for shelf because it is a categorical variable
cereal$type   <- factor(cereal$type)
cereal$shelfF <- factor(cereal$shelf)
str(cereal)


#---------------------------------------------------------------
# data.frame() automatically converts to factors
name  <- c("a","b","c","d","e")
weight <- c(50,75,50,75, 80)
fiber  <- c('low',"low","high","high","high")
cereal2 <- data.frame(name, weight, fiber)
cereal2
str(cereal2)

cereal3 <- data.frame(name, weight, fiber, stringsAsFactors=FALSE)
cereal3$fiberF <- factor(cereal3$fiber)
cereal3
str(cereal3)


#---------------------------------------------------------------
# implications for plotting

cereal <- read.csv(file.path('..','..','sampledata','cereal.csv'), 
              header=TRUE, as.is=TRUE, 
              strip.white=TRUE)
cereal$shelfF <- factor(cereal$shelf)
cereal[1:5,]
str(cereal)

# Notice the difference in ggplot
plot1 <- ggplot(data=cereal, aes(x=shelf, y=calories))+
  ggtitle("Shelf not defined as a factor")+
  geom_jitter(position=position_jitter(w=0.2))+
  geom_boxplot(alpha=0.2, outlier.size=-1)
plot1
ggsave(plot1, file=file.path("..","..","MyStuff","Images","factors-ggplot-001.png"),
       h=4,w=6, units="in", dpi=300)


plot2 <- ggplot(data=cereal, aes(x=shelfF, y=calories))+
  ggtitle("Shelf defined as a factor")+
  geom_jitter(position=position_jitter(w=0.2))+
  geom_boxplot(alpha=0.2, outlier.size=-1)
plot2
ggsave(plot2, file=file.path("..","..","MyStuff","Images","factors-ggplot-002.png"),
       h=4,w=6, units="in", dpi=300)



plot3 <- ggplot(data=cereal, aes(x=as.factor(shelf), y=calories))+
  ggtitle("Shelf not defined as a factor - but as.factor() used")+
  geom_jitter(position=position_jitter(w=0.2))+
  geom_boxplot(alpha=0.2, outlier.size=-1)
plot3
ggsave(plot3, file=file.path("..","..","MyStuff","Images","factors-ggplot-003.png"),
       h=4,w=6, units="in", dpi=300)


plot4 <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelf))+
  ggtitle("Shelf not defined as a factor - defines color")+
  geom_jitter(size=4, position=position_jitter(w=0.2))
plot4
ggsave(plot4, file=file.path("..","..","MyStuff","Images","factors-ggplot-004.png"),
       h=4,w=6, units="in", dpi=300)

plot5 <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfF))+
  ggtitle("Shelf defined as a factor - defines color")+
  geom_jitter(size=4, position=position_jitter(w=0.2))
plot5
ggsave(plot5, file=file.path("..","..","MyStuff","Images","factors-ggplot-005.png"),
       h=4,w=6, units="in", dpi=300)



#---------------------------------------------------------------
# implications for lm()
cereal <- read.csv(file.path('..','..','sampledata','cereal.csv'), 
              header=TRUE, as.is=TRUE, 
              strip.white=TRUE)
cereal$shelfF <- factor(cereal$shelf)
cereal[1:5,]
str(cereal)

# Notice difference in models
result1 <- lm(calories ~ shelf, data=cereal) # regression
anova(result1)
summary(result1)


result2 <- lm(calories ~ shelfF, data=cereal) # anova
anova(result2)
summary(result2)


#-------------------------------------------
# Birds 'n Butts - change order of plotting

library(readxl)
butts <- readxl::read_excel(file.path("..","sampledata","bird-butts-data.xlsx"),
                           sheet='Correlational',
                           col_names=TRUE,
                           trim_ws = TRUE,
                           skip=1,
                           .name_repair="universal")
butts[1:5,]

# Fix the names
names(butts)[ grepl('wieght', names(butts))] <- "Butts.weight"
butts[1:5,]

plot1 <- ggplot(data=butts, aes(x=Nest.content, y=Number.of.mites))+
   ggtitle("Compare # mites by nest content")+
   geom_point( position=position_jitter(w=0.1))+
   geom_boxplot(alpha=0.2, outlier.size=-1)
plot1

ggsave(plot1, file=file.path("..","..","MyStuff","Images","factors-butts-001.png"),
       h=4,w=6, units="in", dpi=300)

butts$Nest.contentF <- factor(butts$Nest.content, levels=c("eggs","chicks","empty"), order=TRUE)

plot2 <- ggplot(data=butts, aes(x=Nest.contentF, y=Number.of.mites))+
   ggtitle("Compare # mites by nest content")+
   geom_point( position=position_jitter(w=0.1))+
   geom_boxplot(alpha=0.2, outlier.size=-1)
plot2

ggsave(plot2, file=file.path("..","..","MyStuff","Images","factors-butts-002.png"),
       h=4,w=6, units="in", dpi=300)


