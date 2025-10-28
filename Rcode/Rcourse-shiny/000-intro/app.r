## Shiny apps
## Looks fat vs cereal and control the amount of jittering of the points

library(ggplot2)

# Read in the cereal data from a csv file
cereal <- read.csv(file.path('..',"..","..","sampledata",'cereal.csv'), 
              header=TRUE, as.is=TRUE, strip.white=TRUE)


## app.R ##
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    ggplot(data=cereal, aes(x=fat, y=calories))+
      ggtitle("Plot of calories vs fat")+
      geom_point(position=position_jitter(h=input$jy*mean(cereal$calories)/100, w=input$jx*mean(cereal$fat)/100))
  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
         sliderInput("jx",
                     "jitter in x direction (%) ",
                     min = 0,
                     max = 10,
                     value = 0),
         sliderInput("jy",
                     "jitter in y direction (%) ",
                     min = 0,
                     max = 10,
                     value = 0)
    ), # end of sidebar Panel 
    mainPanel(
       plotOutput("scatterplot")
    )  # end of mainPanel
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
