# Introduction to lists
# 2015-02-01 CJS extracted to separate file
options(useFancyQuotes=FALSE) # renders summary output corrects

library(ggplot2)

#--------------------------------
# Lists
# 
# example of a list

cereal <- read.csv(
   file.path('..','..','sampledata','cereal.csv'), 
   header=TRUE, as.is=TRUE, 
   strip.white=TRUE)
cereal[1:5,]
str(cereal)


fit.cal.fat <- lm( calories ~ fat, data=cereal)
str(fit.cal.fat)

fit.cal.fat$coefficients  # better to use coef() function
fit.cal.fat$coefficients[1]
x <- fit.cal.fat[1]
x
str(x)
x[1]

y <- fit.cal.fat[[1]]
y
str(y)
y[1]

fit.cal.fat$rank

fit.cal.fat$model$Calories


# Creating lists
age <- c(56, 56, 28, 23, 22)
height <- c(185, 162, 185, 167, 190)
f.names <- c('Carl', "Lois", 'Matthew', 'Marianne', 'David')
people <- list(yob=2013-age, height=height, names=f.names)
str(people)

type <- c("dog", "cat")
p.names <- c("fido", "roger")
pets <- list(species=type, names=p.names)
str(pets)

schwarz <- list(humans=people, animals=pets)
str(schwarz)
schwarz$humans
schwarz$humans$yob


schwarz[[1]]$yob
schwarz[1]$yob
schwarz[1]
str(schwarz[1])
schwarz[[1]]
str(schwarz[[1]])


#-----------------------------------------------
# Exercise
cereal <- read.csv(
  file.path('..','..','sampledata','cereal.csv'), 
  header=TRUE, as.is=TRUE, 
  strip.white=TRUE)
cereal[1:5,]
str(cereal)

# Make the plot
plot <- ggplot2::ggplot(data=cereal, aes(x=fat, y=calories))+
  ggtitle("Number of calories vs. grams of fat")+
  geom_point( position=position_jitter(h=.5, w=.1))+
  geom_smooth()
plot

# Do the fit
reg.fit <- lm(calories ~ fat, data=cereal)
reg.fit


# create a list structure
results <- list(data=cereal, plot=plot, fit=reg.fit)
results

# access the individual parts
names(results)

str(results[1])
str(results[[1]])

results[1][1:5,]
results[[1]][1:5,]
results$data[1:5,]

results$plot

results[3]$coefficients
results[[3]]$coefficients
results$fit$coefficients
summary(results$fit)$r.squared
