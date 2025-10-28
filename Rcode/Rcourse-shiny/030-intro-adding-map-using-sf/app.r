## Shiny apps
## Accident data base. 
## Use a slider to select a 30 day window in the year and display the fatal accidents
## Use the sf package to deal with the mapping issues.

## Hmmm this doesn't seem to work properly because of issues with st_ functions in sf library??

library(ggplot2)
library(lubridate)
library(spData)
library(sf)

uk <- spData::world[ spData::world$name_long == "United Kingdom",]

# Read in the accident data base from a csv file
accidents <- read.csv(file.path('..',"..","..","sampledata","Accidents",'road-accidents-2010.csv'), 
              header=TRUE, as.is=TRUE, strip.white=TRUE)
accidents$Date <- as.Date(accidents$Date, "%d/%m/%Y")
accidents$month <- lubridate::month(accidents$Date)

fatal <- accidents[ accidents$Accident_Severity == 1,]


## app.R ##
server <- function(input, output) {
  output$fatalplot <- renderPlot({
    myfatal <- fatal[ abs(fatal$Date-input$Date)<30,c("Longitude","Latitude")]
    myfatal <- st_sfc(st_multipoint(as.matrix(myfatal)), crs=st_crs(uk))
    plot1 <- ggplot()+ geom_sf(data=uk, aes(fill=NULL))+
         ggtitle(paste("Location of fatal accidents within 30 day of ", format(input$Date, "%Y-%m-%d"),sep=""))+
         geom_sf(data=myfatal)+
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
