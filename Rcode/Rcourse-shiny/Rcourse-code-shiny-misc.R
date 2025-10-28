## Shiny apps
## Miscellaneous code

library(ggplot2)

#-------------------------------------------------------------------

# Read in the cereal data from a csv file
cereal <- read.csv(file.path('..','..','..','sampledata','cereal.csv'), 
              header=TRUE, as.is=TRUE, strip.white=TRUE)
head(cereal)


#-------------------------------------------------------------------
# Read in the accident data and get the date and fatality variables set
accidents <- read.csv('../sampledata/road-accidents-2010.csv', header=TRUE,
             as.is=TRUE, strip.white=TRUE)
# Convert date to internal date format
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")
# Create the fatality variable
accidents$Fatality <- accidents$Accident_Severity == 1
accidents[1:5,]

#-------------------------------------------------------------------
# Read in the USarrest records
us.arrests <- read.csv('../sampledata/USArrests-1973.csv', header=TRUE,
             as.is=TRUE, strip.white=TRUE)
head(us.arrests)


#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------
#-------------------------------------------------------------------

# The perils of non-standard evaluation
# Consider the following plot
ggplot(data=cereal, aes(y=calories, x=fat))+
  geom_point()

# Suppose we wish to "program" the variables using something like
xvar <- 'fat'
yvar <- 'calories'

# this fails because of non-standard evaluation
ggplot(data=cereal, aes(y=yvar, x=xvar))+
  geom_point()

# this fails because of non-standard evaluation
cereal$yvar


# use the aes_string() function in ggplot
xvar <- 'fat'
yvar <- 'calories'
ggplot(data=cereal, aes_string(y=yvar, x=xvar))+
  geom_point()

# use [] in regular data frames 
yvar <- 'calories'
cereal$yvar # returns null
cereal$"yvar" # also returns null
cereal[, yvar, drop=FALSE]
