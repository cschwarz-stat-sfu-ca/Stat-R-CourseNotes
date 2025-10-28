## Shiny apps
## Looks fat vs cereal and control the amount of jittering of the points
## Add a slider for the size of the points
## Add a radio button to add the regression line.

library(ggplot2)

# Read in the cereal data from a csv file
cereal <- read.csv(file.path('..',"..","..","sampledata",'cereal.csv'), 
              header=TRUE, as.is=TRUE, strip.white=TRUE)


## app.R ##
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    plot1 <- ggplot(data=cereal, aes(x=fat, y=calories))+
      ggtitle("Plot of calories vs fat")+
      geom_point(position=position_jitter(
           h=input$jy*mean(cereal$calories)/100, 
           w=input$jx*mean(cereal$fat)/100),
           size=input$psize)
    if(input$fitline == "Yes"){ plot1 <- plot1 + geom_smooth(method="lm")}
    plot1 # be sure to return plot a end
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
                     value = 0),
        sliderInput("psize",
                     "Point size",
                     min = 0,
                     max = 10,
                     value = 1),
        radioButtons("fitline",
                     "Show the regression line?",
                     c("Yes"="Yes",
                       "No" ="No") )
    ), # end of sidebar Panel 
    mainPanel(
       plotOutput("scatterplot")
    )  # end of mainPanel
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
