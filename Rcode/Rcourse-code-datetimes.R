# Dates and Times.
# 2015-01-31 CJS Updates and minor revisions (use POSIXct instead of POSIXlt)
# 2014-04-20 CJS First Edition

options(useFancyQuotes=FALSE) # renders summary output corrects
#source("schwarz.functions.r")
source('http://www.stat.sfu.ca/~cschwarz/Stat-650/Notes/MyPrograms/schwarz.functions.r')

library(boot)
library(car)
library(ggplot2)
library(gridExtra)
library(emmeans)
library(plyr)

library(lubridate)
library(nycflights13)

#------------------- Dates (without times) ------------------------

# Create a virtual csv file for the test data in this exercise

testDates.csv <- textConnection("
d1c,   d2c,    d3c
23aug01, 1970-07-10, 13/07/1956
14sep01, 1972-07-11, 14/07/1956
30feb01, 1972-13-12, 15/07/1956
1mar2001,72-08-14,   16/07/1956") # example of inputing data inline

my.dates <- read.csv(testDates.csv, header=TRUE, # notice no quotes around virtal file name 
            as.is=TRUE, strip.white=TRUE)
my.dates
str(my.dates)

# Convert from character to internal Date representation
my.dates$d1 <- as.Date(my.dates$d1c, format="%d%b%y")
my.dates$d1
as.numeric(my.dates$d1)

my.dates$d2 <- as.Date(my.dates$d2c, format="%Y-%m-%d")
my.dates$d2
as.numeric(my.dates$d2)

my.dates$d3 <- as.Date(my.dates$d3c, format="%d/%m/%Y")
my.dates$d3
as.numeric(my.dates$d3)

# arithmetic operations on dates and time
my.dates$d3 + 20  # adds 20 days

mean(my.dates$d3)
as.numeric(mean(my.dates$d3))


seq(from=my.dates$d3[1], by="3 weeks", length.out=3)

seq(from=my.dates$d3[1], by="3 weeks", length.out=3) #sequen
seq(from=my.dates$d3[1], by="2 months", length.out=3)
seq(from=my.dates$d3[1], by="1 year", length.out=3)


# Extracting parts of the date
# Caution - results of format are always character
format(my.dates$d3, "%d")
#format(my.dates$d3, "%d") + 1  # oops
as.numeric(format(my.dates$d3, "%d"))

# Extract day of the week (0=Sunday)
as.numeric(format(my.dates$d3, "%w"))

# Julian day (number of days since 1 Jan of that year) 001-366
as.numeric(format(my.dates$d3, "%j"))

# Other useful functions from the lubridate package
ymd("2017-01-31")
ymd("17/01/31")  # careful - how do you know if 1900 or 2000?
dmy(31012017)

# putting together multiple fields
library(nycflights13)
head(nycflights13::flights)
data(flights)
head(flights[, c("year","month","day")])
flights$Date <- lubridate::make_date(year =flights$year,
                                     month=flights$month,
                                     day  =flights$day)
head(flights[, c("year","month","day","Date")])


# Extracting parts of a date using lubridate functions rather than format()
lubridate::year("2017-01-31")
lubridate::month("2017-01-31")
lubridate::day("2017-01-31")



#-------- Exercise 1
naccidents <- read.csv(
    file.path('..','sampledata','road-accidents-2010-summary.csv'), header=TRUE,
    as.is=TRUE, strip.white=TRUE)
naccidents[1:5,]
str(naccidents)

# Convert date to internal date format
naccidents$mydate <- as.Date(naccidents$Date, format="%d/%m/%Y")
sum(is.na(naccidents$mydate))
naccidents[1:5,]
str(naccidents)


# Plot the number of acidents by day of the year
plotnacc <- ggplot(data=naccidents, aes(x=mydate, y=naccidents))+
     ggtitle("Number of accidents/day by date")+
     xlab("Date")+ylab("Number of accidents/day")+
     geom_point()+
     geom_smooth()
plotnacc
ggsave(plot=plotnacc, 
  file=file.path("..","..","MyStuff","Images","accidents-summary-by-date.png"), 
  h=4, w=6, units="in", dpi=300)

# look at proportion of fatalities
naccidents$pfatal <- naccidents$nfatal /
                     naccidents$naccidents
plotpfatal <- ggplot(data=naccidents, aes(x=mydate, y=pfatal))+
     ggtitle("P(fatal) by date")+
     xlab("Date")+ylab("p(fatal)")+
     geom_point()+
     geom_smooth()
plotpfatal
ggsave(plot=plotpfatal, 
  file=file.path("..","..","MyStuff","Images","accidents-summary-by-date2.png"), 
  h=4, w=6, units="in", dpi=300)

# melt the data set and plot
plotdata <- reshape2::melt(naccidents,
                id.var="mydate",
                measure.var=c("naccidents","pfatal"),
                variable.name="Measure",
                value.name="value")
head(plotdata,n=2)
tail(plotdata,n=2)

plotboth <- ggplot(data=plotdata, aes(x=mydate, y=value))+
     ggtitle("P(fatal) and # accidents by date")+
     xlab("Date")+ylab("p(fatal) or # accidents")+
     geom_point()+
     geom_smooth()+
     facet_wrap(~Measure, ncol=1, scales="free_y")
plotboth
ggsave(plot=plotboth, 
  file=file.path("..","..","MyStuff","Images","accidents-summary-by-date3.png"), 
  h=6, w=6, units="in", dpi=300)



naccidents$weekday <- format(naccidents$mydate, format="%w")
naccidents[1:10,]

plotnacc2 <- ggplot(data=naccidents, aes(x=weekday, y=naccidents))+
  ggtitle("Number of accidents/day by day of the week")+
  xlab("Day of the week")+ylab("Number of accidents/day")+
  geom_point(position=position_jitter(w=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2, outlier.size=-1, outlier.colour="red")
plotnacc2
ggsave(plot=plotnacc2, 
  file=file.path("..","..","MyStuff","Images","accidents-summary-by-weekday.png"),
  h=4, w=6, units="in", dpi=300)




#-------- Exercise 2
# The accident data
accidents <- read.csv(
     file.path('..','sampledata','Accidents','road-accidents-2010.csv'), 
     header=TRUE,
     as.is=TRUE, strip.white=TRUE)
accidents[1:5,]
str(accidents)

# Convert date to internal date format
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")
sum(is.na(accidents$mydate))
accidents[1:5,]
str(accidents)


# tabulate number of accidents on each date
# The summarize function from the plyr package is very useful for group processing
library(plyr)
naccidents <- ddply(accidents, "mydate", summarize,
                    freq=length(Accident_Index))
naccidents[1:5,]
str(naccidents)


# Plot the number of acidents by day of the year
plotnacc <- ggplot(data=naccidents, aes(x=mydate, y=freq))+
     ggtitle("Number of accidents/day by date")+
     xlab("Date")+ylab("Number of accidents/day")+
     geom_point()+
     geom_smooth()
plotnacc
ggsave(plot=plotnacc, 
  file=file.path("..","..","MyStuff","Images","accidents-by-date.png"), 
  h=4, w=6, units="in", dpi=300)


# Extract day of the week
# Leave in character form so that box plots work properly
naccidents$weekday <- format(naccidents$mydate, format="%w")
naccidents[1:10,]

plotnacc2 <- ggplot(data=naccidents, aes(x=weekday, y=freq))+
  ggtitle("Number of accidents/day by day of the week")+
  xlab("Day of the week")+ylab("Number of accidents/day")+
  geom_point(position=position_jitter(w=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2, outlier.size=-1, outlier.colour="red")
plotnacc2
ggsave(plot=plotnacc2, 
  file=file.path("..","..","MyStuff","Images","accidents-by-weekday.png"),
  h=4, w=6, units="in", dpi=300)


# Repeat for proportion of fatalities
names(accidents)
unique(accidents$Accident_Severity)
library(car)
accidents$Fatality <- car::recode(accidents$Accident_Severity,
              ' 1=1; 2:hi=0')
accidents[1:5, c("Accident_Severity", "Fatality")]

xtabs(~Fatality + Accident_Severity, data=accidents)

# find the proportion of fatalities.
# Note that the mean of a 0/1 variable is a proportion
library(plyr)
pfatal.df <- ddply(accidents, "mydate", summarize,
            freq=length(mydate),
            pfatal=mean(Fatality))
pfatal.df[1:5,]

# Plot the number of acidents by day of the year
plotpfatal <- ggplot(data=pfatal.df, aes(x=mydate, y=pfatal))+
     ggtitle("P(fatal) by date")+
     xlab("Date")+ylab("P(fatal)")+
     geom_point()+
     geom_smooth()
plotpfatal
ggsave(plot=plotpfatal, 
  file=file.path("..","..","MyStuff","Images","accidents-pfatal-by-date.png"),
  h=4, w=6, units="in", dpi=300)



# Extract day of the week
# Leave as character so that the boxplots work properly
pfatal.df$weekday <- format(pfatal.df$mydate, format="%w") # leave as character
pfatal.df[1:10,]

plotpfatal2 <- ggplot(data=pfatal.df, aes(x=weekday, y=pfatal))+
  ggtitle("P(fatal) by day of the week")+
  xlab("Day of the week")+ylab("P(fatal)")+
  geom_point(position=position_jitter(w=0.2))+
  geom_boxplot(notch=TRUE, alpha=0.2, outlier.size=-1)
plotpfatal2
ggsave(plot=plotpfatal2, 
  file=file.path("..","..","MyStuff","Images","accidents-pfatal-by-weekday.png"),
  h=4, w=6, units="in", dpi=300)



#-------------------------------------
# Exercise IV
# Number of flights by day from nyc
# Notice how you can read a zip file directly

flights<- read.csv(
   unz(file.path('..','sampledata','nycflights13','flights.csv.zip'), "flights.csv"), 
   header=TRUE, as.is=TRUE, strip.white=TRUE)
flights<- readr::read_csv(file.path('..','sampledata','nycflights13','flights.csv.zip'))

head(flights)

# make date by combining the columns
flights$Date <- lubridate::make_date(year=flights$year,
                                     month=flights$month,
                                     day  =flights$day)
nflights <- plyr::ddply(flights, "Date", plyr::summarize,
                        n.flights=length(Date))
ggplot(data=nflights, aes(x=Date, y=n.flights))+
  ggtitle("Number of flights leaving NYC")+
  geom_point()+
  geom_line()

# Classify dates by day of the week
nflights$dow <- wday(nflights$Date) # notice 1=Monday - see help(wday)

ggplot(data=nflights, aes(x=as.factor(dow), y=n.flights))+
  ggtitle("Number of flights by day of week")+
  geom_point(position=position_jitter(w=.2))+
  geom_boxplot(alpha=.20, outlier.size=0)




###########################################################################
###########################################################################
###########################################################################
###########################################################################
###########################################################################

#------------------- Dates and Times in R -------------------
# Use the POSIXct (constant time with tz=UTC) representation when every possible
# Create a virtual file with the test data
testDateTimes.csv <- textConnection("
dt1c,   dt2c   
2013-10-23 10:23, 2012-07-13 1:23:00 
2013-11-23 25:23, 2012-07-14 3:23:00 
2013-12-23 13:23, 2012-07-15 5:23:10 
2013-12-24 10:62, 2013-07-15 7:23:23 ") # example of inputing data inline

my.dt <- read.csv(testDateTimes.csv, header=TRUE,  # notice no quotes around virtual file name
            as.is=TRUE, strip.white=TRUE)
my.dt
str(my.dt) 

# Convert from character to internal DateTime representation
my.dt$dt1 <- as.POSIXct(my.dt$dt1c, 
              format="%Y-%m-%d %H:%M", tz="UTC") 
my.dt$dt1
as.numeric(my.dt$dt1)
str(my.dt)

# Look what happens if you don't specify a time zone
my.dt$dt1b <- as.POSIXct(my.dt$dt1c, 
              format="%Y-%m-%d %H:%M") 
my.dt$dt1b
as.numeric(my.dt$dt1b)
str(my.dt)

# read in the the other data values
my.dt$dt2 <- as.POSIXct(my.dt$dt2c, 
              format="%Y-%m-%d %H:%M", tz="UTC") 
my.dt$dt2
as.numeric(my.dt$dt2)


# Arithmetic operations allowed
mean(my.dt$dt2)
as.numeric(mean(my.dt$dt2))

# sequences of dates etc
seq(from=my.dt$dt2[1], by="3 weeks", length.out=3)


# extract the various bits from the date-time format
as.numeric(format(my.dt$dt2, "%d"))
as.numeric(format(my.dt$dt2, "%H"))


# Beware of daylight saving changes
# In BC, this occurred at 2013-11-03 at 2:00 am
# Compare the behaviour of 
mytime <- as.POSIXct("2013-11-03 01:57:00", tz="UTC")
mytime
seq(mytime, by='1 min', length.out=10)
as.numeric(seq(mytime, by='1 min', length.out=10))

mytime <- as.POSIXct("2013-11-03 01:57:00")
mytime
seq(mytime, by='1 min', length.out=10)
as.numeric(seq(mytime, by='1 min', length.out=10))




# But be careful of daylight savings time problems
# In BC, this occurred at 2013-11-03 at 2:00 am

mytime <- as.POSIXct("2013-11-03 01:57:00", tz="UTC")
as.numeric(format(seq(mytime, by='1 min', length.out=10), "%H"))

mytime <- as.POSIXct("2013-11-03 01:57:00")
as.numeric(format(seq(mytime, by='1 min', length.out=10),"%H"))


# useful functions from lubridate
arrive1 <- lubridate::ymd_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")
arrive1
arrive2 <- lubridate::ymd_hms("2011-06-04 12:00:00") # default tz="UTC"
arrive2
arrive1-arrive2

lubridate::second("2011-06-04 12:01:02")

# time intervals
vacation.interval <- lubridate::interval(lubridate::ymd("2017-07-01"),
                                        lubridate::ymd('2017-07-22'))
vacation.interval

# Other functions that work with intervals include int_start, int_end, int_flip,
#    int_shift, int_aligns, union, intersect, setdiff, and %within%.
int_length(vacation.interval) # in seconds
int_length(vacation.interval)/(24*3600) # in days


# Dealing with time zone if you must
# List of current time zone recognized by R
OlsonNames()


#---------------- Exercise with the accident data -----------------
# The accident data
accidents <- read.csv(
  file.path('..','sampledata','Accidents','road-accidents-2010.csv'),
  header=TRUE,
  as.is=TRUE, strip.white=TRUE)

# Convert date to internal date format
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")

accidents$DateTime <- paste(accidents$Date, accidents$Time)
accidents[1:5,c("Date","Time","DateTime")]

# Convert date & time to internal DateTime format
accidents$mydt <- as.POSIXct(accidents$DateTime,
                  format="%d/%m/%Y %H:%M", tz="UTC")
accidents[1:5,c("Date","Time","DateTime","mydt")]
str(accidents)

# look at missing values
sum(is.na(accidents$mydt))
accidents[is.na(accidents$mydt),c("Accident_Index","Date","Time","mydt")]

# Plot the time of the accidents by the minute it occurred
# Extract the minute of the accident
accidents$min <- as.numeric(format(accidents$mydt, "%M"))
accidents[1:5, c("Date","Time","mydt","min")]

plotaccmin <- ggplot(data=accidents, aes(x=min))+
  ggtitle("Minute of when accidents occur")+
  xlab("Minute of the hour")+ylab("Number of accidents")+
  geom_histogram(binwidth=1, alpha=0.2)
plotaccmin
ggsave(plot=plotaccmin, 
  file=file.path("..","..","MyStuff","Images","accidents-time-by-minute.png"),
  h=4, w=6, units="in", dpi=300)


# find the proportion of fatalities by the hour of the day
# We currently ignore any effects of different daylight hours.
# Note that the mean of a 0/1 variable is a proportion
names(accidents)
unique(accidents$Accident_Severity)
accidents$Fatality <- accidents$Accident_Severity==1
xtabs(~Fatality + Accident_Severity, data=accidents)

accidents$hour <- as.numeric(format(accidents$mydt, "%H"))
xtabs(~hour, data=accidents)
sum(is.na(accidents$hour))
accidents[1:5, c("Date","Time","mydt","Fatality","hour")]

library(plyr)
pfatal.df <- ddply(accidents, "hour", summarize,
              pfatal=mean(Fatality))
pfatal.df[1:5,]

plotpfatalhour <- ggplot(data=pfatal.df, aes(x=hour, y=pfatal))+
  ggtitle("Proportion of accidents by hour of the day")+
  xlab("Hour of the day")+ylab("P(fatal)")+
  geom_point()+
  geom_line()
plotpfatalhour
ggsave(plot=plotpfatalhour,
  file=file.path("..","..","MyStuff","Images","accidents-pfatal-by-hour.png"),
  h=4, w=6, units="in", dpi=300)



#-------------------------------------
# Exercise II
# Number of flights by day from nyc
# Notice how you can read a zip file directly

flights<- read.csv(
  unz(file.path('..','sampledata','nycflights13','flights.csv.zip'), "flights.csv"), 
  header=TRUE, as.is=TRUE, strip.white=TRUE)
flights<- readr::read_csv(file.path('..','sampledata','nycflights13','flights.csv.zip'))

head(flights)

# make date by combining the columns
flights$DateTime <- lubridate::make_datetime(year=flights$year,
                                     month=flights$month,
                                     day  =flights$day,
                                     hour =floor(flights$dep_time / 100),
                                     min  = flights$dep_time %% 100)
head(flights[,c("year","month","day","dep_time","DateTime")])

flights$hour <- lubridate::hour(flights$DateTime)
flights$min  <- lubridate::minute(flights$DateTime)

ggplot(data=flights, aes(x=hour))+
  ggtitle("Number of flights by hour of the day")+
  geom_histogram(breaks=0:24)

ggplot(data=flights, aes(x=min))+
  ggtitle("Number of flights by hour of the day")+
  geom_histogram(breaks=0:60)





#########################################################################
#########################################################################
#########################################################################
#########################################################################

#--------------------------------------------------------------------
# Duration data

library(lubridate)
testDuration.csv <- textConnection("
du1c,   du2c   
10:23, 1:23:00 
25:23, 3:23:00 
13:23, 5:23:10 
10:62, 7:23:23 ") # example of inputing data inline

my.du <- read.csv(testDuration.csv, header=TRUE,  # notice no quotes around virtual file name
            as.is=TRUE, strip.white=TRUE)
my.du
str(my.du) 

# Notice that first duration is ambiguous, is it HH:MM or MM:SS?
my.du$du1a <- lubridate::ms(my.du$du1c)
my.du$du1a
as.numeric(my.du$du1a)

my.du$du1b <- lubridate::hm(my.du$du1c)
my.du$du1b
as.numeric(my.du$du1b)

my.du$du2 <- lubridate::hms(my.du$du2c)
my.du$du2
as.numeric(my.du$du2)

mean(my.du$du1a)  #???????
mean(as.duration(my.du$du1a))




#-------------------------------------
# Exercise I
# Number of flights by day from nyc
# Notice how you can read a zip file directly

flights<- read.csv(
  unz(file.path('..','sampledata','nycflights13','flights.csv.zip'), "flights.csv"), 
  header=TRUE,
  as.is=TRUE, strip.white=TRUE)
flights<- readr::read_csv(file.path('..','sampledata','nycflights13','flights.csv.zip'))

head(flights)

# Create duration data
flights$dep_delay2 <- lubridate::dminutes(flights$dep_delay)
flights$arr_delay2 <- lubridate::dminutes(flights$arr_delay)

head(flights[,c("year","month","day","dep_delay","dep_delay2","arr_delay","arr_delay2")])

# Hmmm bug in lubridate package when plotting
# See https://github.com/tidyverse/ggplot2/issues/2414
ggplot(data=flights, aes(x=dep_delay2@.Data))+
  ggtitle("Departure delays")+
  geom_histogram()

ggplot(data=flights, aes(x=air_time, y=dep_delay2@.Data))+
  ggtitle("Relationship between air time and departure delay")+
  geom_point()


#########################################################################
#########################################################################
#########################################################################
#########################################################################

#--------------------------------------------------------------------
# Time of day data

# is the first column hh:mm? 

library(hms)
testTod.csv <- textConnection("
tod1c,   tod2c   
10:23, 1:23:00 
25:23, 3:23:00 
13:23, 5:23:10 
10:62, 7:23:23 ") # example of inputing data inline

my.tod <- read.csv(testTod.csv, header=TRUE,  # notice no quotes around virtual file name
               as.is=TRUE, strip.white=TRUE)
my.tod
str(my.tod) 

# convert to time of day
my.tod$tod1 <- hms::parse_hm(my.tod$tod1c) 
my.tod$tod2 <- hms::parse_hm(my.tod$tod2c) 
my.tod

# does averaging work? properly
as.hms(mean(my.tod$tod2))
as.hms(mean( c(hms::parse_hm("23:50"), hms::parse_hm("00:20"))))

my.tod$tod2[1]-my.tod$tod2[4]
my.tod$tod2[4]-my.tod$tod2[1]
hms::parse_hm("23:50") - hms::parse_hm("00:20")
hms::parse_hm("00:20") - hms::parse_hm("23:50") 




#------------------------------------------------------------------------------------
# Make accident summary by date

# The accident data
accidents <- read.csv(
  file.path('..','sampledata','Accidents','road-accidents-2010.csv'),
  header=TRUE,
  as.is=TRUE, strip.white=TRUE)
accidents[1:5,]

accident.summary <- plyr::ddply(accidents, "Date", plyr::summarize,
                                naccidents=length(Date),
                                nfatal    =sum(Accident_Severity==1))
accident.summary
write.csv(accident.summary, "accident-summary.csv", row.names=FALSE)
