options(useFancyQuotes=FALSE) # renders summary output corrects
source("schwarz.functions.r")
#source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

# This is a quick demo of using Rstudio
x <- 1:10
x
plot(x,x)

#--------------------------------------------------------
# This script will read in the cereal data set, 
#    do a simple listing,
#    fit a regression line, 
#       draw a scatter plot and add the line to the plot
#    do a single factor crd anova
#       get the compact letter display
#       make some plots


# load required libraries
library(ggplot2)
library(emmeans)

# Read in the cereal data from a csv file
cereal <- read.csv('../SampleData/cereal.csv', 
              header=TRUE, as.is=TRUE, strip.white=TRUE)


cereal3 <- read.table("http://lib.stat.cmu.edu/datasets/1993.expo/cereal", 
                      header=FALSE, as.is=TRUE, strip.white=TRUE)
names(cereal3) <- c('Name','mfr','type','Calories','protein','Fat','sodium','fiber','carbo',
                   'sugars','shelf','potass','vitamins','weight','cups')


# Define new variables and factors (for categorical variables). CHeck the structure of the data frame
cereal$shelfF <- factor(cereal$shelf)
cereal$Calories.fr.Protein <- cereal$protein * 4;

str(cereal)

# List  the first few records
cereal[1:5,]

# List some variables
cereal$calories
cereal[,"calories"]
cereal$fat
cereal[1:5,c("name","fat","calories")]

# Make a basic scatter plot
plotbasic <- ggplot(data=cereal, aes(x=fat, y=calories))+
      ggtitle("Calories vs Fat in cereals")+
      xlab("Grams of Fat")+ylab("Calories/serving")+
      geom_point()
plotbasic
ggsave(plotbasic, file='cal-vs-fat1.png', h=4, w=6, units="in", dpi=300)

plotbasic2 <- ggplot(data=cereal, aes(x=fat, y=calories))+
      ggtitle("Calories vs Fat in cereals")+
      xlab("Grams of Fat")+ylab("Calories/serving")+
      geom_jitter()
plotbasic2
ggsave(plotbasic, file='cal-vs-fat2.png', h=4, w=6, units="in", dpi=300)


# Fit a regression between calories and grams of fat
fit.calories.fat <- lm( calories ~ fat, data=cereal)
summary(fit.calories.fat)
anova(fit.calories.fat) # careful Type I SS
coef(fit.calories.fat)
sqrt(diag(vcov(fit.calories.fat))) # extract the SE
confint(fit.calories.fat) # confidence intervals on parameters

names(summary(fit.calories.fat))
summary(fit.calories.fat)$r.squared
summary(fit.calories.fat)$sigma

class(fit.calories.fat)
methods(class=class(fit.calories.fat))


# Add the fitted line to the scatter plot; and save
plotline <- plotbasic2 +
  geom_abline(intercept=coef(fit.calories.fat)[1],
              slope    =coef(fit.calories.fat)[2])
plotline
ggsave(plot=plotline, file="cal-vs-fat3.png", h=4, w=6, units="in", dpi=300)


# Or, if you don't want' to do the actual fit, use ggplot directly
plot.calories.fat <- ggplot(data=cereal, aes(x=fat, y=calories)) +
    geom_jitter(shape=1) +    # Use hollow circles
    geom_smooth(method=lm,   # Add linear regression line
                se=FALSE)    # Don't add shaded confidence region
plot.calories.fat


# Do a simple single factor ANOVA
# Is the mean number of calories the same for all shelves
# Need to use a FACTOR variable for the categorical variable
fit.sugars.shelf <- lm( sugars ~ shelfF, data=cereal)
anova(fit.sugars.shelf)

# Estimate the marginal means along with confidence limits and Tukey multiple comparison.
fit.sugars.shelf.emmo <- emmeans::emmeans(fit.sugars.shelf, ~shelfF)
fit.sugars.shelf.cld <- multcomp::cld(fit.sugars.shelf.emmo, adjust='tukey')
fit.sugars.shelf.cld
cld.plot <- sf.cld.plot.bar(fit.sugars.shelf.cld, "shelfF", order=FALSE)
cld.plot
ggsave(cld.plot, file="fat-vs-shelf.png",h=4, w=6, units="in", dpi=200)
# Estimate the pairwise differences
pairs(fit.sugars.shelf.emmo)

# an alternate way to look at pairwise comparisons
pwpp(fit.sugars.shelf.emmo)


#--------------------------------------------------------
# Fun with vectors

age <- c(56, 56, 28, 23, 22)
height <- c(185, 162, 185, 167, 190)
f.names <- c('Carl', "Lois", 'Matthew', 'Marianne', 'David')
over.30 <- c(T, T, F, F, F) # AVOID using T/F for TRUE/FALSE

odd <- c(2.3, 'Carl')  # surprising, but look at result!

length(age)
length(family) # number of elements not lengths of elements
str(age)    # what is the structure of age?
str(f.names)



# The c() function is very versatile
# The c() function is very versatile
ah <- c(age, height)
ah
age0age <- c(age, 0, age)
age0age
length(age0age)

odd <- c(f.names, over.30) # ??
odd




#--------------------------------------------------------
# Dataframes 
age <- c(56, 56, 28, 23, 22)
height <- c(185, 162, 185, 167, 190)
f.names <- c('Carl', "Lois", 'Matthew', 'Marianne', 'David')
over.30 <- c(TRUE, TRUE, FALSE, FALSE, FALSE)

schwarz <- data.frame( f.names, age, height, over.30,
          stringsAsFactors=FALSE)
schwarz
str(schwarz)
length(schwarz) # number of vectors, not length of vectors
dim(schwarz)
nrow(schwarz)
ncol(schwarz)
names(schwarz)


# Most commonly created from data.
cereal <- read.csv('../sampledata/cereal.csv', 
                   header=TRUE, as.is=TRUE, strip.white=TRUE)

str(cereal)  # this function is VERY useful when things seem to go wrong
length(cereal) # number of vectors, not length of vectors
dim(cereal)
nrow(cereal)
ncol(cereal)
names(cereal)

# How to refer to parts of data frame
names(cereal)
cereal$name
cereal$calories
cereal[ , "calories"] # first index missing = ALL rows
#calories  # doesn't work because vector is hidden
with(cereal, calories) # careful of case. 


cereal[1,]
cereal[1:5,]
cereal[, 1]
cereal[, 1:5]
cereal[1:4, 1:5]

# Adding removing variables from data frames
cereal$CalPerGramFat <- cereal$calories / cereal$fat
cereal$CalPerGramFat  # some interesting values!
cereal$CalPerGramFat <- NULL # removes this variable from df




#--------------------------------------------------------
# More fun with vectors
# Operations with vectors

age <- c(56, 56, 28, 23, 22)
age.next.year <- age + 1
yob <- 2013- age  # element by element operations if same length

# Operations on vectors
x <- c(0.5, 1, 1.5, 2, 4, 6, 8, 9, 10, 12)
length(x)
str(x)

sqrt(x)  # function applied to EACH element

 # Other useful functions
range(x)
mean(x)
sd(x)
median(x)
summary(x)
sum(x)

# Compare the min() and pmin() functions
min(x, 3)
pmin(x, 3)

# Exercise
mean(cereal$calories)
max(cereal$fat)
min(cereal$fat)
range(cereal$fat)

mean(cereal$weight)
mean(cereal$weight, na.rm=TRUE)


#--------------------------------------------------------
# Simple increments
help(":")  # be sure to put operators in quotes for the help() function
5:10
10.2:3
5:10-1  # careful : is evaluated prior to arithmetic
seq(1, 100, 10)
seq(to=100, from=1, by=10)

# replicate things
x <- c(5, 6, 7)
help(rep)
rep(TRUE, 10)
rep(x, times=2)
rep(x, length.out=8)
rep(x, each=2)


#--------------------------------------------------------
# Indexing
x <- c(5:9, 12:15, 34:37)
x

# Simple indexing
x[2]  # note the use of SQUARE brackets for indexing
x[c(2,3,7)]  # the index can also be a vector
#x[2,3,7]     # oops, not a proper index for a vector

n <- 10
x[n]   # indices can be variables. What does this mean?
inx <- c(3, 5, 7)
#x(inx)   # Oops wrong types of brackets
x[inx]


# using selection vector 
select <- x >6  & x < 10
select
sum(select)
x[select]



# Indexing
x <- c(5:9, 12:15, 34:37)
x

# Simple replacement indexing
x
x[2] <- 100  # note the use of SQUARE brackets for indexing
x

x[c(2,3,7)] <- 200 # the index can also be a vector
x
x[c(2,3,7)] <- c(200,201)  # notice the warning message
x

n <- 10
x[n]  <- 500  # indices can be variables. What does this mean?
inx <- c(8,9,10)
x[inx] <- c(300, 400) 
x


# using selection vector 
select <- x >6  & x < 10
select
sum(select)
x[select] <- -1
x



sum(cereal== -1, na.rm=TRUE)
sum(is.na(cereal))


# Dropping elements
x
x[-2]  # note the use of SQUARE brackets for indexing
x[-c(2,3,7)]  # the index can also be a vector

n <- -10
x[n]   # indices can be variables. What does this mean?
inx <- c(3, 5, 7)
x[-inx]


# Using logical vectors to select elements
x <- c(5:9, 12:15, 34:37)
x

# Selecting elements where entry is TRUE
x > 10
x[ x>10 ]
x[ x > 10 & x < 20]
x[ x %% 2 == 0]  # %note the use of == to test for equality
x[ x>10 ] <- 500
x


# We want to rename "Fiber" to "Fibre"
# Avoid using explicit index (i.e. names(cereal)[9] <-"Fibre" as not robust
names(cereal)
names(cereal)[grepl("fiber",names(cereal))] <- "fibre"
names(cereal)

# Select certain cereal manufacturers
cereal[ cereal$mfr %in% c("P","A"),] # don't forget the last comma

cereal[grep("Bran", cereal$name),] # don't forget the last comma



#---------------------------------------------------------------------
# Fun with functions

# concatenating objects together, especially to make a vector
limits <- c(0, 100)

ggplot(data=cereal, aes(x=fat, y=calories))+
    geom_point()+
    ylim(c(0,100))

# Generating sequences
seq(1,10,2)
seq(1, by=2, length.out=10 )

# Generating all possible combinations
expand.grid( sex=c("m","f"), age=c(10,20,30), stringsAsFactors=FALSE)

# checking for and counting number of missing values; selecting rows without missing values
is.na(cereal$weight)
sum(is.na(cereal$weight))
sum(!is.na(cereal$weight))
select <- is.na(cereal$weight)
cereal[ !select,]

# max and parallel maximum
x <- c(1,2,3,4,5,6)
max(x)
pmax(3, x)

# finding the set of unique values
unique(cereal$type)

# xtabs - counting and checking
xtabs(~type, data=cereal, exclude=NULL, na.action=na.pass)
xtabs(~type+cups, data=cereal, exclude=NULL, na.action=na.pass)

# pasting text together
paste("Analysis of ", nrow(cereal), ' breakfast cereals', sep="")
ggplot(data=cereal, aes(x=fat, y=calories))+
    geom_point()+
    ggtitle(paste("Analysis of ", nrow(cereal), ' breakfast cereals', sep=""))

# pattern matching - Googleis your friend
select <- grepl("bran", cereal$name) # exact match
cereal[select,]

select <- grepl("bran", cereal$name, ignore.case=TRUE)
cereal[select,]

select <- grepl("^bran", cereal$name, ignore.case=TRUE)
cereal[select,] # start with bran

select <- grepl("bran$", cereal$name, ignore.case=TRUE)
cereal[select,] # end with bran

# dealing with strings
toupper(cereal$name)
tolower(cereal$name)
trimws(cereal$name)

substr(cereal$name, 1, 4)
substring(cereal$name, 4)

substr(cereal$name, 1, -1 +regexpr("_", cereal$name, fixed=TRUE))
substr(cereal$name, 1, pmax(5,-1 +regexpr("_", cereal$name, fixed=TRUE)))

gsub("-","_", cereal$name)

# sorting
sort(cereal$name)
cereal[ order(cereal$name), ] # reorder a data frame
cereal[ order(cereal$calories, cereal$name),]


# file path that is device independent
file.path("..","sampledata")


# merging and combining data frames
byear <- data.frame(name =c('Carl', 'Lois', 'Matthew', 'Marianne', 'David'),
                    byear=c( 1956,   1956,    1986,     1990,       1991))

bcity <- data.frame(name =c('Carl', 'Lois', 'Matthew'),
                    city =c('Wpg',  'Brandon', 'Wpg'))

wcity <- data.frame(name =c('Matthew', 'Marianne', 'David'),
                    city =c('Ottawa',  'Vancouver', 'Victoria'))

# cbind must be used with caution 
cbind(bcity, wcity)

# merge - careful of non-matches
merge(byear, bcity)
merge(byear, bcity, all=TRUE)
merge(byear, bcity, all.y=TRUE)

# multiple merges - Google is your friend
Reduce(function(...){merge(..., all=TRUE)}, list(byear, bcity, wcity))

