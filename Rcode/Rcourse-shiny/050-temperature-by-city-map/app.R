# Temperature time series data from Environment Canada for 2018
# 

library(ggplot2)
library(dplyr)
library(maps)
library(sf)
library(units)


# Get the data
climate <- read.csv(file.path("..","..","..","sampledata","Climate-Daily-Canada-2018","climate-daily.csv"),
                    header=TRUE, as.is=TRUE, strip.white=TRUE)
climate$Date <- lubridate::ymd( paste(climate$LOCAL_YEAR, "-",climate$LOCAL_MONTH, "-",climate$LOCAL_DAY,sep=""))
range(climate$Date)


# Find the locations of the monitoring stations
stations <- unique( climate[, c("x","y","STATION_NAME")])




# get map of Canada
# This is stored in zip file that contains the shape file.
# We unzip to a temporary directory; read the shape file etc.
# The temporary directory will then disappear at the end of the session.
canada.map.dir <- tempdir()
unzip(file.path("..","..","..","sampledata","Climate-Daily-Canada-2018","CanadaMap","gpr_000b11a_e.zip"),  exdir=canada.map.dir)

canada <- sf::st_read(file.path(canada.map.dir,"gpr_000b11a_e.shp"), stringsAsFactors=FALSE)
canada <- sf::st_simplify(canada, dTolerance=units::set_units(.1, degree))
s.canada.sf <- canada[ !canada$PRNAME %in% c("Nunavut","Northwest Territories / Territoires du Nord-Ouest","Yukon"),]
names(canada)
canada$PRNAME

plot1 <- ggplot()+
  geom_sf(data=s.canada.sf, aes(fill=NULL))
plot1
ggsave(plot1, 
       file="canada.png", h=6, w=6, units="in", dpi=300)


# Create the database of stations with same projection as Canada
stations.sf <- st_as_sf(x = stations,                         
           coords = c("x", "y"),
           crs = st_crs(canada))

# Intersect the stations.sf with that of s.canada
stations.sf <- st_intersection(stations.sf, s.canada.sf)

ggplot()+
  geom_sf(data=s.canada.sf, aes(fill=NULL))+
  geom_sf(data=stations.sf)


# reduce the climate dataset
climate <- climate[ climate$STATION_NAME %in% stations.sf$STATION_NAME,]


#----------------------
## app.R ##
server <- function(input, output) {
  output$climateplot <- renderPlot({
    #browser()
    myclimate <- climate[ abs(climate$Date-input$Date)<5,]
    myclimate$var.to.analyze <- myclimate[, input$variable]
    # get the mean for each station
    mean.climate <-
      myclimate %>%
        group_by(STATION_NAME,x, y) %>%
          summarize(
             meanval=mean(var.to.analyze, na.rm=TRUE)
          )
    mean.climate.sf <- st_as_sf(x = mean.climate, coords = c("x", "y"), crs = st_crs(canada)) 
    plot1 <- ggplot() +
         ggtitle(paste("Mean ", input$variable," within +/- 5 days of ", format(input$Date, "%Y-%m-%d"),sep=""))+
         geom_sf(data=s.canada.sf, aes(fill=NULL))+
         geom_sf(data=mean.climate.sf, aes(color=meanval), size=4)+
         scale_color_gradient2(midpoint=10, low="darkblue", high="darkred")
    plot1 # be sure to return plot a end
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
        sliderInput("Date",
                     "Select midpoint of +/- 5 day interval",
                    min=as.Date('2018-01-01'), max=as.Date('2018-12-31'), value=as.Date('2018-07-01'),
                    round=TRUE),
        selectInput("variable",
                    "Select which variable to plot",
                    c("Daily Min"="MIN_TEMPERATURE",
                      "Daily Max"="MAX_TEMPERATURE"))
    ), # end of sidebar Panel 
    mainPanel(
       plotOutput("climateplot")
    )  # end of mainPanel
  )
)

# Run the application 
shinyApp(ui = ui, server = server)

