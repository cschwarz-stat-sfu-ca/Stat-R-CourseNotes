# Different ways to read in data


options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')


# load required libraries
library(readxl)
library(readxl)

# Read in the cereal data from a csv file
cereal <- read.csv(file.path('..','sampledata','cereal.csv'), 
              header=TRUE, as.is=TRUE, strip.white=TRUE)
head(cereal)
str(cereal)


#-------------------------------------------------------

# Reading an Excel workbook

library(readxl)
cereal2 <- readxl::read_excel(file.path('..','sampledata','ALLofDATA.xls'), 
                     sheet='cereal', 
                     col_names=TRUE, 
                     trim_ws=TRUE, 
                     skip=7,
                     .name_repair="universal")
head(cereal2)
str(cereal2)
# notice that cereal2 is a tibble and not a data frame

# tibbles vs. data.frames
# These are mostly interchangeable except for print() method and subsetting
# single columns.
# see help(package=tibble) and vignettes for more details
df1  <- data.frame(v1=c("a", "b"), v2=c(1,2), stringsAsFactors=FALSE)
tib1 <- tibble::tibble    (v1=c("a", "b"), v2=c(1,2)) # notice the _ 

# compare the output from
df1
tib1

# compare the output from
df1$xx
tib1$xx

# compare the output from
df1 [,"v1"]
tib1[,"v1"]
# first is a vector; second is a tibble with 1 columns

# some legacy code gets upset with the latter behaviour
# you can force a tibble to be a  data frame using
df2 <- as.data.frame(tib1)

#-------------------------------------------------------

# Reading white space delimited table from the Internet

cereal4 <- read.table("http://lib.stat.cmu.edu/datasets/1993.expo/cereal", 
                      header=FALSE, as.is=TRUE, strip.white=TRUE)
names(cereal4) <- c('Name','mfr','type','Calories','protein','Fat','sodium','fiber','carbo',
                   'sugars','shelf','potass','vitamins','weight','cups')
head(cereal4)
str(cereal4)


# Reading small table stored with the script
type.code.csv <- textConnection("
type, code
C , Cold Cereal
H , Hot Cereal  ")

type.code <- read.csv(type.code.csv, header=TRUE, strip.white=FALSE, as.is=TRUE)
head(type.code)
str(type.code)
type.code$type == "C"
type.code$type

# oops forgot to strip white space
type.code.csv <- textConnection("
type, code
C , Cold Cereal
H , Hot Cereal  ")

type.code <- read.csv(type.code.csv, header=TRUE, strip.white=TRUE, as.is=TRUE)
head(type.code)
str(type.code)
type.code$type == "C"
type.code$type



#-------------------------------------------------------
# Correcting variable names

sample.csv <- textConnection("
Bird #, Wieght, Length mm, Mass (g)
1, 100, 101, 102
2, 200, 201, 202")

sample <- read.csv(sample.csv, header=TRUE, 
                   strip.white=TRUE, as.is=TRUE)
head(sample)
str(sample)
sample$Bird..


sample.csv <- textConnection("
Bird #, Wieght, Length mm, Mass (g)
1, 100, 101, 102
2, 200, 201, 202")
sample <- read.csv(sample.csv, header=TRUE, 
                   strip.white=TRUE, as.is=TRUE,
                   check.names=FALSE)
head(sample)
str(sample)
sample$Bird..
sample$"Bird #"


# Using the names function to convert variable names
sample2 <- sample
names(sample2)
names(sample2) <- c("Bird","Weight","Length","Mass")
head(sample2)


# you can access individual names, but this is fragile
# in case the order of the columns changes
sample2 <- sample
names(sample2)
names(sample2)[2] <- c("Weight")
head(sample2)


# you can access individual names through a selection process
# be paranoid about change
sample2 <- sample
names(sample2)

select <- grepl("Wieght", names(sample2))
select
sum(select)
names(sample2)[select]

names(sample2)[select] <- c("Weight")
head(sample2)


# the plyr package also has a useful function
# you can rename all or selected columns
sample3 <- sample
head(sample3)
sample3 <- plyr::rename(sample3,
                        c("Bird #"="Bird",
                          "Wieght"="Weight",
                          "Length mm"="Length",
                          "Mass (g)"="Mass"))
head(sample3)


#-----------------------------------------------------------
# Exercise

library(readxl)
butts <- readxl::read_excel(file.path('..','sampledata','bird-butts-data.xlsx'),
                           sheet='Correlational',
                           col_names=TRUE,
                           trim_ws = TRUE,
                           skip=1,
                           .name_repair="universal")

butts[1:5,]
dim(butts)
str(butts)

# Or, save the sheet from the Excel file and read the csv file
butts <- read.csv(file.path("..","sampledata","bird-butts-data-correlational.csv"), 
                  header=TRUE, skip=1, as.is=TRUE, strip.white=TRUE)
butts[1:5,]
dim(butts)
str(butts)


# Fix the names
select <-  grepl('wieght', names(butts))
select
sum(select)
names(butts)[select]

names(butts)[ select] <- "Butts.weight"
butts[1:5,]

# Or use the rename
butts <- plyr::rename(butts, c("Butts.wieght"="Butts.weight"))
head(butts)
