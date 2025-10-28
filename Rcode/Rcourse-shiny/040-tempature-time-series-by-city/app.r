## Shiny apps
## Temperature time series in 2018 for Canadian cities
## Allow the user to select among max/min/precip for 10 major cities
## Use a slider to select the dates in 2018 to compare

library(ggplot2)
library(lubridate)

# Read in the climate data
climate <- read.csv(file.path("..","..","..","sampledata","Climate-Daily-Canada-2018","climate-daily.csv"),
                    header=TRUE, as.is=TRUE, strip.white=TRUE)
climate$Date <- lubridate::ymd( paste(climate$LOCAL_YEAR, "-",climate$LOCAL_MONTH, "-",climate$LOCAL_DAY,sep=""))
range(climate$Date)

length(unique(climate$STATION_NAME))
sort(unique(climate$STATION_NAME))


## app.R ##
server <- function(input, output) {
  output$climateplot <- renderPlot({
    
    myclimate <- climate[ climate$Date >= input$Date[1] & climate$Date <= input$Date[2],]
    myclimate <- myclimate[ myclimate$STATION_NAME %in% input$stations,]
    plot1 <- ggplot(data=myclimate, aes_string(x="Date", y=input$variable, color="STATION_NAME")) +
         ggtitle(paste(input$variable, " across selected stations"))+
#         geom_point()+
         geom_line()+
         ylab(input$variable)+xlab("Date")+
         theme(legend.position=c(0,1),legend.justification=c(0,1))
    plot1 # be sure to return plot a end
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
        sliderInput("Date",
                     "Select range of dates to see",
                    min=as.Date('2018-01-01'), max=as.Date('2018-12-31'), value=as.Date(c('2018-01-01','2018-12-31')),
                    round=TRUE), 
        selectInput("stations",
                    "Select which stations to plot",
                    c("Vancouver"="VANCOUVER INTL A",
                      "Victoria" ="VICTORIA INTL A",
                      "Edmonton" = "EDMONTON INTERNATIONAL CS",
                      "Calgary"  = "CALGARY INT'L CS",
                      "Winnipeg" = "WINNIPEG A CS",
                      "Toronto"  = "TORONTO INTL A"),
                    multiple=TRUE),
       selectInput("variable",
                    "Select which variable to plot",
                    c("Daily Min"="MIN_TEMPERATURE",
                      "Daily Max"="MAX_TEMPERATURE",
                      "Total Precip"="TOTAL_PRECIPITATION"))
    ), # end of sidebar Panel 
    mainPanel(
       plotOutput("climateplot")
    )  # end of mainPanel
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
