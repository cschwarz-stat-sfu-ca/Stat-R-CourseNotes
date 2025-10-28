# Source code for introduction to ggplot() plotting
# 2015-01-30 CJS Update with new examples and explantions 
# 2014-05-19 CJS First Edition


options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(car)
library(GGally)
library(ggfortify)
library(ggplot2)
library(gridExtra)
library(plyr)
library(readxl)

#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
# Fun with plotting

cereal <- read.csv(file.path('..','sampledata','cereal.csv'), 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal$shelfF <- factor(cereal$shelf)

cereal$fiber.grp <- car::recode(cereal$fiber, "lo:3='low'; 3:hi='high'")
xtabs(~fiber.grp, data=cereal)
cereal$fiber.grpF <- factor(cereal$fiber.grp) 

cereal[1:5,]


# start a plot. ggplot ONLY uses data frames.
newplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_point()
newplot
ggsave(plot=newplot, file=file.path('..','..','MyStuff','Images','cereal-calories-fat-base.png'),
       h=4, w=6, units="in", dpi=300)

str(newplot)

#-------------------------------------------------------------------
# jittering
newplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter() #too much
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images","cereal-calories-fat-base2.png"),
       h=4, w=6, units="in", dpi=300)

# Controlling the amount of jittering
newplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1))
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base3.png'),
       h=4, w=6, units="in", dpi=300)


# Controling color and size of points
newplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), color="red", size=4)
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base4.png'),
       h=4, w=6, units="in", dpi=300)



#----------------------------------------------------------------------
# Dodging - a consistent across groups, especially useful for error bars in line plots

cereal$fiber.grp <- car::recode(cereal$fiber, "lo:3='low'; 3:hi='high'")
xtabs(~fiber.grp, data=cereal)
cereal$fiber.grpF <- factor(cereal$fiber.grp) 

xtabs(~shelf+fiber.grp, data=cereal, exclude=NULL, na.action=na.pass)

report <- plyr::ddply(cereal, c("shelf","fiber.grp"), sf.simple.summary, variable="calories", crd=TRUE)
report

temp <- report
temp[,c(5:9)] <- round(temp[, 5:9],2)
temp

# Exercise using dodging
newplot <- ggplot2::ggplot(report, aes(x=shelf, y=mean, 
                                       group=fiber.grp, color=fiber.grp, shape=fiber.grp))+
   ggtitle("Illustrating dodging - none applied")+
   geom_point()+
   geom_line()+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.05)+
   ylab("Mean calories/serving (95% ci)")
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-dodging-001.png'),
       h=4, w=6, units="in", dpi=300)


newplot <- ggplot2::ggplot(report, aes(x=shelf, y=mean, color=fiber.grp, shape=fiber.grp))+
   ggtitle("Illustrating dodging - dodging applies")+
   geom_point( position=position_dodge(w=0.1))+
   geom_line(position=position_dodge(w=0.1))+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1, position=position_dodge(w=0.1))+
   ylab("Mean calories/serving (95% ci)")
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-dodging-002.png'),
       h=4, w=6, units="in", dpi=300)

newplot <- ggplot2::ggplot(report, aes(x=as.factor(shelf), y=mean, 
                                       group=fiber.grp, color=fiber.grp, shape=fiber.grp))+
   ggtitle("Illustrating dodging - dodging applies")+
   geom_point( position=position_dodge(w=0.1))+
   geom_line(position=position_dodge(w=0.1))+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1, position=position_dodge(w=0.1))+
   ylab("Mean calories/serving (95% ci)")
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-dodging-003.png'),
       h=4, w=6, units="in", dpi=300)



#-------------------------------------------------------------------
# separate colors by categorical or continuous variables
newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=type))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base10.png'),
       h=4, w=6, units="in", dpi=300)


newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base11.png'),
       h=4, w=6, units="in", dpi=300)

newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfF, shape=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base12.png'),
       h=4, w=6, units="in", dpi=300)

# selecting colors and shapes for categorical variables
# scale_color_manual() is used for points and lines
# Best to use a named vector - assuming that values are categorical

newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfF, shape=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   scale_color_manual( values=c("blue","yellow","orange"))
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base12a.png'),
       h=4, w=6, units="in", dpi=300)

# Better to use a named vector rather than relying on "alphabetical" ordering
# of values - assuming that categorical variables are "character"
cereal$shelfc <- as.character(cereal$shelf)
shelf_colors=c("1"="blue", "2"="yellow", "3"="orange")
newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfc, shape=shelfc))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   scale_color_manual( values=shelf_colors)
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base12b.png'),
       h=4, w=6, units="in", dpi=300)


# Change the order of the legend
# Done using a factor (see later on) or directly in ggplot2
cereal$shelfF.special <- factor(cereal$shelf, levels=c(2,1,3))
newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfF.special, shape=shelfF.special))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base12c.png'),
       h=4, w=6, units="in", dpi=300)

cereal$shelfc <- as.character(cereal$shelf)
newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfc, shape=shelfc))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   scale_color_discrete( limits=c("2","1","3"))+
   scale_shape_discrete( limits=c("2","1","3"))
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base12d.png'),
       h=4, w=6, units="in", dpi=300)

# Simply reversing the ordering on the plot
newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfF, shape=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   scale_color_discrete(guide = guide_legend(reverse=TRUE))+
   scale_shape_discrete(guide = guide_legend(reverse=TRUE))
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base12e.png'),
       h=4, w=6, units="in", dpi=300)


# Selecting the plotting symbol to use
newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=shelfF, shape=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   scale_shape_manual(values=c(2,3,4))
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base12f.png'),
       h=4, w=6, units="in", dpi=300)








newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=protein, shape=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base20.png'),
       h=4, w=6, units="in", dpi=300)

newplot <- ggplot(data=cereal, aes(x=fat, y=calories, color=protein, shape=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   scale_color_gradient(high="black", low="blue")
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base21.png'),
       h=4, w=6, units="in", dpi=300)


#-------------------------------------------------------------------
# add a smoother to the points
newplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   geom_smooth(method="loess", se=FALSE, color="red") 
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base31.png'),
       h=4, w=6, units="in", dpi=300)

newplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1))+
   geom_smooth(method="loess", se=FALSE, color="red") +
   geom_smooth(method="lm", se=FALSE, color="black")
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base32.png'),
       h=4, w=6, units="in", dpi=300)


#-------------------------------------------------------------------
# Different plotting symbols (shape= argument in ggplot)
newplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4, shape=c(1:25,32:83))
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base41.png'),
       h=4, w=6, units="in", dpi=300)



#-----------------------------------------
# Modifying legend attributes
# Refer to 
#   http://www.cookbook-r.com/Graphs/Legends_(ggplot2)
newplot <- ggplot(data=cereal, aes(x=fat, y=calories,
                  shape=shelfF, color=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   theme(legend.position="top")
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base51.png'),
       h=4, w=6, units="in", dpi=300)


# Set the "anchoring point" of the legend 
#     (bottom-left is 0,0; top-right is 1,1)
# Numeric positions are relative to the entire area,
# including titles and labels, not just the plotting area.
newplot <- ggplot(data=cereal, aes(x=fat, y=calories,
                  shape=shelfF, color=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   theme(legend.justification=c(1,0), legend.position=c(1,0))
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base52.png'),
       h=4, w=6, units="in", dpi=300)


# Modify the name of the legend
# The default action is to use the name of the variable creating the
# legend.Notice that you must use ALL scales to modify the names.
newplot <- ggplot(data=cereal, aes(x=fat, y=calories,
                  shape=shelfF, color=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   theme(legend.justification=c(1,0), legend.position=c(1,0))+
   scale_color_discrete(name="Shelf")+
   scale_shape_discrete(name="Shelf")
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base52a.png'),
       h=4, w=6, units="in", dpi=300)

# Modify the name of the legend
# Use a long legend title
newplot <- ggplot(data=cereal, aes(x=fat, y=calories,
                  shape=shelfF, color=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   theme(legend.justification=c(1,0), legend.position=c(1,0))+
   scale_color_discrete(name="Shelf\nHeight")+
   scale_shape_discrete(name="Shelf\nHeight")
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base52aa.png'),
       h=4, w=6, units="in", dpi=300)


# removing the legend title.
newplot <- ggplot(data=cereal, aes(x=fat, y=calories,
                  shape=shelfF, color=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   theme(legend.justification=c(1,0), legend.position=c(1,0))+
   scale_color_discrete(name=NULL)+
   scale_shape_discrete(name=NULL)
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'cereal-calories-fat-base52b.png'),
       h=4, w=6, units="in", dpi=300)


#-----------------------------------------
# add a call out using annotate() command. 
# My favorite cereal is Mueslix Healthy Choice
fc <- cereal[ cereal$name == "Kix",]
fc

newplot <- ggplot(data=cereal, aes(x=fat, y=calories,
                  shape=shelfF, color=shelfF))+
   ggtitle("Calories vs Grams of fat")+
   xlab("fat/serving (g)")+ylab("calories/serving")+
   geom_jitter(position=position_jitter(w=.1, h=.1), size=4)+
   annotate("segment",
            x=4, y=50,
            xend=fc$fat-.1, yend=fc$calories,
            arrow=arrow(ends="last")
            )+
   annotate("text",
            x=4.1, y=50, 
            label="Kix",
            hjust=0)
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images","ggplot2-callout-cereal-001.png"),
       h=4, w=6, unit="in", dpi=300)


#-------------------------------------------------------------------
# Birds 'n Butts

# Relationship between parasite load and number of butts in nest


library(readxl)
butts <- readxl::read_excel(file.path('..','sampledata','bird-butts-data.xlsx'),
                           sheet='Correlational',
                           col_names=TRUE,
                           trim_ws = TRUE,
                           skip=1,
                           .name_repair="universal")
butts[1:5,]

# Fix the names
names(butts)[ grepl('wieght', names(butts))] <- "Butts.weight"
butts[1:5,]

# Look like we need a log-transform on the number of mites
butts$log.mites <- log(butts$Number.of.mites)
outlier <- butts[butts$log.mites<= 0,]
outlier

prelimplotlog <- ggplot(data=butts, 
                   aes(x=Butts.weight, y=log.mites,
                       shape=Species, color=Nest.content))+
     ggtitle("Number of mites vs. butts weight")+
     xlab("Butts weight (g)")+ylab("log(Number of mites")+
     geom_point(size=4)+
     annotate("point",
              x=outlier$Butts.weight,
              y=outlier$log.mites,
              shape="X",size=6)+
     annotate("text", 
              x=outlier$Butts.weight, 
              y=outlier$log.mites+.2,
              label="Outlier")+
     theme(legend.justification=c(1,1), legend.position=c(1,1))
prelimplotlog
ggsave(plot=prelimplotlog, 
       file=file.path("..","..","MyStuff","Images","bird-butts-mites-weight-002.png"))


report <- plyr::ddply(butts, c("Species","Nest.content"), sf.simple.summary, variable="Number.of.mites", crd=TRUE)
report

temp <- report
temp[,c(5:9)] <- round(temp[, 5:9],2)
temp

# Exercise using dodging
newplot <- ggplot2::ggplot(report, aes(x=Nest.content, y=mean, 
                                       group=Species, color=Species, shape=Species))+
   ggtitle("Mean mites by nest content and species")+
   geom_point( position=position_dodge(w=0.1))+
   geom_line(position=position_dodge(w=0.1))+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=.1, position=position_dodge(w=0.1))+
   ylab("Mean mites (95% ci)")
newplot
ggsave(plot=newplot, 
       file=file.path("..","..","MyStuff","Images",'bird-butts-mites-weight-002a.png'),
       h=4, w=6, units="in", dpi=300)






#--------------------------------------------------------------
# Histogram of calories
histplot <- ggplot(data=cereal, aes(x=calories))+
  ggtitle("Histogram of number of calories/serving")+
  xlab("calories/serving")+ylab("Density")+
  geom_histogram()
histplot
ggsave(histplot, 
   file=file.path("..","..","MyStuff","Images","ggplot2-hist-cereal-100.png"),
   h=4, w=6, units="in", dpi=300)

histplot <- ggplot(data=cereal, aes(x=calories))+
  ggtitle("Histogram of number of calories/serving")+
  xlab("calories/serving")+ylab("Density")+
  geom_histogram(binwidth=20 )
histplot
ggsave(histplot, 
   file=file.path("..","..","MyStuff","Images","ggplot2-hist-cereal-101.png"),
   h=4, w=6, units="in", dpi=300)

histplot <- ggplot(data=cereal, aes(x=calories))+
  ggtitle("Histogram of number of calories/serving")+
  xlab("calories/serving")+ylab("Density")+
  geom_histogram(aes(y=..density..),binwidth=20,alpha=0.2)+
  geom_density(color='red')
histplot
ggsave(histplot, 
   file=file.path("..","..","MyStuff","Images","ggplot2-hist-cereal-102.png"),
   h=4, w=6, units="in", dpi=300)


histplot <- ggplot(data=cereal, aes(x=calories))+
  ggtitle("Histogram of number of calories/serving")+
  xlab("calories/serving")+ylab("Density")+
  geom_histogram(aes(y=..density..),binwidth=20,alpha=0.2)+
  geom_density(color='red')+
  facet_grid(shelfF~ .)
histplot
ggsave(histplot, 
    file=file.path("..","..","MyStuff","Images","ggplot2-hist-cereal-103.png"),
    h=4, w=6, units="in", dpi=300)



#--------------------------------------------------------------
# Side-by-side box of calories by shelf

boxplot <- ggplot(data=cereal, aes(x=shelfF, y=calories))+
  ggtitle("Boxplot of number of calories/serving by Shelf")+
  ylab("calories/serving")+xlab("Shelf")+
  geom_boxplot()
boxplot
ggsave(boxplot, 
   file=file.path("..","..","MyStuff","Images","ggplot2-boxplot-cereal-001.png"),
   h=4, w=6, units="in", dpi=300)

boxplot <- ggplot(data=cereal, aes(x=shelfF, y=calories))+
  ggtitle("Boxplot of number of calories/serving by Shelf")+
  ylab("calories/serving")+xlab("Shelf")+
  geom_boxplot(alpha=0.2, notch=TRUE)
boxplot
ggsave(boxplot, 
   file=file.path("..","..","MyStuff","Images","ggplot2-boxplot-cereal-002.png"),
   h=4, w=6, units="in", dpi=300)


boxplot <- ggplot(data=cereal, aes(x=shelfF, y=calories))+
  ggtitle("Boxplot of number of calories/serving by Shelf")+
  ylab("calories/serving")+xlab("Shelf")+
  geom_boxplot(alpha=0.2, notch=TRUE, outlier.size=0)+
  geom_point(position=position_jitter(width=0.1))
boxplot
ggsave(boxplot, 
   file=file.path("..","..","MyStuff","Images","ggplot2-boxplot-cereal-003.png"),
   h=4, w=6, units="in", dpi=300)


cereal$fiber.grp <- car::recode(cereal$fiber, "lo:3='low'; 3:hi='high'")
xtabs(~fiber.grp, data=cereal)
cereal$fiber.grpF <- factor(cereal$fiber.grp) 

boxplot <- ggplot(data=cereal, aes(x=fiber.grpF, y=calories))+
  ggtitle("Boxplot of number of calories/serving by Fiber Group")+
  ylab("calories/serving")+xlab("Fiber Group")+
  geom_boxplot(alpha=0.2, notch=TRUE, outlier.size=0)+
  geom_point(position=position_jitter(width=0.1))
boxplot
ggsave(boxplot,
  file=file.path("..","..","MyStuff","Images","ggplot2-boxplot-cereal-004.png"),
  h=4, w=6, units="in", dpi=300)


cereal$fiber.grpF <- factor(cereal$fiber.grp, 
                      levels=c("low","high"), order=TRUE)
boxplot <- ggplot(data=cereal, aes(x=fiber.grpF, y=calories))+
  ggtitle("Boxplot of number of calories/serving by Fiber Group")+
  ylab("calories/serving")+xlab("Fiber Group")+
  geom_boxplot(alpha=0.2, notch=TRUE, outlier.size=0)+
  geom_point(position=position_jitter(width=0.1))
boxplot
ggsave(boxplot, 
  file=file.path("..","..","MyStuff","Images","ggplot2-boxplot-cereal-004b.png"),
  h=4, w=6, units="in", dpi=300)



#--------------------------------------------------------------
# Exercise

library(readxl)
butts <- readxl::read_excel(file.path('..','sampledata','bird-butts-data.xlsx'),
                           sheet='Correlational',
                           col_names=TRUE,
                           trim_ws = TRUE,
                           skip=1,
                           .name_repair="universal")
names(butts)[ grepl('wieght', names(butts))] <- "Butts.weight"
butts$log.mites <- log(butts$Number.of.mites)


birdbox <- ggplot(data=butts,
                   aes(x=Species, y=Butts.weight))+
            ggtitle("Compare butts weight by species")+
            xlab("Species")+ylab("Butts weight (g)")+
            geom_point(position=position_jitter(w=0.2))+
            geom_boxplot(notch=TRUE, alpha=0.2, outlier.size=0)
birdbox
ggsave(plot=birdbox, 
  file=file.path("..","..","MyStuff","Images",'bird-butts-mites-weight-ggplot-003.png'),
  h=4, w=6, units="in", dpi=300)


birdbox <- ggplot(data=butts,
                   aes(x=Species, y=log(Butts.weight)))+
            ggtitle("Compare butts weight by species")+
            xlab("Species")+ylab("log(Butts weight (g))")+
            geom_point(position=position_jitter(w=0.2))+
            geom_boxplot(notch=TRUE, alpha=0.2, outlier.size=0)
birdbox
ggsave(plot=birdbox, 
  file=file.path("..","..","MyStuff","Images",'bird-butts-mites-weight-ggplot-004.png'),
  h=4, w=6, units="in", dpi=300)


#----------------------------------------------------------------
# Bar charts and line charts with standard errors
#source("schwarz.functions.r")  # load some helper functions
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

# Compute the mean calories/serving by fiber group with se and ci
sumstat.all <- sf.simple.summary(cereal, "calories", crd=TRUE)
sumstat.all

library(plyr)
sumstat.shelf <- ddply(cereal, "shelfF", sf.simple.summary, 
                 variable="calories", crd=TRUE)
sumstat.shelf


# Side-by-side bar charts
cerealbar <- ggplot(data=sumstat.shelf, aes(x=shelfF, y=mean))+
   ggtitle("Mean calories by shelf with 95% ci")+
   xlab("Shelf")+ylab("Mean calories/serving with 95% ci")+
   geom_bar(stat="identity", alpha=0.2)+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)
cerealbar
ggsave(cerealbar, 
  file=file.path("..","..","MyStuff","Images","ggplot2-bar-cereal-001.png"),
  h=4, w=6, units="in", dpi=300)


# A line chart is similar, except you don't need to start
# the bars at zero
cerealline <- ggplot(data=sumstat.shelf, aes(x=shelfF, y=mean))+
   ggtitle("Mean calories by shelf with 95% ci")+
   xlab("Shelf")+ylab("Mean calories/serving with 95% ci")+
   geom_line(aes(group=1))+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)
cerealline
ggsave(cerealline, 
  file=file.path("..","..","MyStuff","Images","ggplot2-line-cereal-001.png"),
  h=4, w=6, units="in", dpi=300)

#----------------------------------------------------------------
# compare the mean butts.weight and number of mites by species/ contents
library(readxl)
butts <- readxl::read_excel(file.path('..','sampledata','bird-butts-data.xlsx'),
                           sheet='Correlational',
                           col_names=TRUE,
                           trim_ws = TRUE,
                           skip=1,
                           .name_repair="universal")
names(butts)[ grepl('wieght', names(butts))] <- "Butts.weight"
butts$log.mites <- log(butts$Number.of.mites)

library(plyr)
sumstat <- ddply(butts, "Nest.content", sf.simple.summary, 
                 variable="Number.of.mites", crd=TRUE)
sumstat$Nest.contentF <- factor(sumstat$Nest.content,
                        levels=c("empty","eggs","chicks"),
                        order=TRUE)
sumstat


# Side-by-side bar charts
nestbar <- ggplot(data=sumstat, aes(x=Nest.contentF, y=mean))+
   ggtitle("Mean number of mites by nest content with 95% ci")+
   xlab("Nest Content")+ylab("Mean number of mites with 95% ci")+
   geom_bar(stat="identity", alpha=0.2)+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)
nestbar
ggsave(nestbar, 
  file=file.path("..","..","MyStuff","Images","bird-butts-mites-weight-bar-ggplot-001.png"),
  h=4, w=6, units="in", dpi=300)


# A line chart is similar, except you don't need to start
# the bars at zero
nestline <- ggplot(data=sumstat, aes(x=Nest.contentF, y=mean))+
   ggtitle("Mean number of mites by nest content with 95% ci")+
   xlab("Nest Content")+ylab("Mean number of mites with 95% ci")+
   geom_line(aes(group=1))+
   geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.2)
nestline
ggsave(nestline, 
  file=file.path("..","..","MyStuff","Images","bird-butts-mites-weight-line-ggplot-001.png"),
  h=4, w=6, units="in", dpi=300)


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


# Very long tick mark labels
# Not sure why the car::recode doesn't work directly
cereal$shelf.special <- car::recode(cereal$shelf,
                                "1='Bottom\nShelf'; 2='Middle\nShelf'; 3='Top\nShelf'")
cereal$shelf.special <- gsub(" ", "\n", cereal$shelf.special)

cerealplot <- ggplot(data=cereal, aes(x=shelf.special, y=calories))+
     ggtitle("Calories vs. shelf")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_boxplot( alpha=0.2, outlier.size=0)
cerealplot
ggsave(cerealplot, 
  file=file.path("..","..","MyStuff","Images","ggplot2-cereal-longtickmarks-001.png"),
  h=4, w=6, units="in", dpi=300)

# Rotat tick mark labels
cerealplot <- ggplot(data=cereal, aes(x=shelf.special, y=calories))+
     ggtitle("Calories vs shelf")+
     xlab("fat/serving (g)")+ylab("calories/serving (g)")+
     geom_point(position=position_jitter(width=0.1))+
     geom_boxplot( alpha=0.2, outlier.size=0)+
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
cerealplot
ggsave(cerealplot, 
  file=file.path("..","..","MyStuff","Images","ggplot2-cereal-longtickmarks-002.png"),
  h=4, w=6, units="in", dpi=300)
