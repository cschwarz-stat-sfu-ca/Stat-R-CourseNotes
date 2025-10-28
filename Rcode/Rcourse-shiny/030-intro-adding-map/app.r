## Shiny apps
## Accident data base. 
## Use a slider to select a 30 day window in the year and display the fatal accidents

library(ggmap)
library(ggplot2)
library(lubridate)

# Read in the accident data base from a csv file
accidents <- read.csv(file.path('..',"..","..","sampledata","Accidents",'road-accidents-2010.csv'), 
              header=TRUE, as.is=TRUE, strip.white=TRUE)
accidents$Date <- as.Date(accidents$Date, "%d/%m/%Y")
accidents$month <- lubridate::month(accidents$Date)
range(accidents$Longitude)
range(accidents$Latitude)

fatal <- accidents[ accidents$Accident_Severity == 1,]

mean.lat <- mean(accidents$Latitude)
mean.long<- mean(accidents$Longitude)


my.map.dl <- ggmap::get_map(c(left  =min(accidents$Longitude), bottom=min(accidents$Latitude), 
                              right =max(accidents$Longitude), top   =max(accidents$Latitude)), 
                             maptype="watercolor",  source="stamen", zoom=6)

my.map <- ggmap(my.map.dl)



## app.R ##
server <- function(input, output) {
  output$fatalplot <- renderPlot({
    myfatal <- fatal[ abs(fatal$Date-input$Date)<30,]
    
    plot1 <- my.map +
         ggtitle(paste("Location of fatal accidents within 30 day of ", format(input$Date, "%Y-%m-%d"),sep=""))+
         geom_point(data=myfatal, aes(x=Longitude, y=Latitude), size=.4)+
         ylab("Latitude")+xlab("Longitude")
    plot1 # be sure to return plot a end
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
        sliderInput("Date",
                     "Select midpoint of 30 day interval",
                    min=as.Date('2010-01-01'), max=as.Date('2010-12-31'), value=as.Date('2010-07-01'),
                    round=TRUE) 
    ), # end of sidebar Panel 
    mainPanel(
       plotOutput("fatalplot")
    )  # end of mainPanel
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
