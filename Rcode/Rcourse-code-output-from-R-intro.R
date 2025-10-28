# Getting output from R to the real world
# 2015-01-31 CJS Updates
# 2014-05-20 CJS First edition

library(ggforce)
library(ggplot2)
library(gridExtra)
library(plyr)

#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
# Fun with plotting

cereal <- read.csv(file.path('..','sampledata','cereal.csv'), 
          header=TRUE, as.is=TRUE,
          strip.white=TRUE)
cereal[1:5,]


#---------------------
# Saving graphic output from ggplot.
# Only single pages can be saved
myplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
    geom_point()
myplot
ggsave(myplot, 
       file=file.path('..','..','MyStuff','Images','sample-ggplot-myggplot.png'),
       h=4, w=6, units="in", dpi=300) 
 

# multipage output
xtabs(~mfr, data=cereal, exclude=NULL, na.action=na.pass)
npages=ceiling(length(unique(cereal$mfr))/4)
pdf(file.path('..','..','MyStuff','Images','sample-ggplot-myggplot2.pdf'), h=6, w=6)
plyr::l_ply(1:npages, function (page){
   myplot <- ggplot(data=cereal, aes(x=fat, y=calories))+
     ggtitle("Calories vs grams of fat")+
     geom_point()+
     facet_wrap_paginate(~mfr, nrow=2, ncol=2, page=page)
   plot(myplot)
})
dev.off()


#------------------------------------
# Saving text output
sink(file.path('..','..','MyStuff','Images','sample-textoutput.txt'), split=TRUE)
  fit <- lm(calories ~ fat, data=cereal)
  anova(fit)
  summary(fit)
  confint(fit)
sink()


# Saving data frame
cereal$calories.from.fat <- cereal$fat *4
write.csv(cereal,
          file=file.path('..','..','MyStuff','Images','sample-csvoutput.csv'), row.names=FALSE)


# Saving a table
report <- plyr::ddply(cereal, "mfr", plyr::summarize, 
                      n.cereals=length(mfr),
                      calories.mean = mean(calories, na.rm=TRUE))
report

temp <- report
temp[,3] <- round(temp[,3],1)
temp
write.csv(temp,
          file=file.path('..','..','MyStuff','Images','sample-csvtable.csv'), row.names=FALSE)

# Save objects and environment images
saveRDS("cereal", file=file.path('..','..','MyStuff','Images','sample-csvtable.Rdata'))
new.cereal <- readRDS(file=file.path('..','..','MyStuff','Images','sample-csvtable.Rdata'))

