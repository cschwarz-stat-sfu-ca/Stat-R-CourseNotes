## Shiny apps
## Looks fat vs cereal and control the amount of jittering of the points
## Add a slider for the size of the points
## Add a radio button to add the regression line.
## Add a drop down list to select the x variable of interest -- notice non-standard evaluation
## Add a text box with estimated standard error etc

library(ggplot2)

# Read in the cereal data from a csv file
cereal <- read.csv(file.path('..',"..","..","sampledata",'cereal.csv'), 
              header=TRUE, as.is=TRUE, strip.white=TRUE)


## app.R ##
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    plot1 <- ggplot(data=cereal, aes_string(x=input$xvar, y="calories"))+
      ggtitle(paste("Plot of calories vs ", input$xvar, sep=""))+
      geom_point(position=position_jitter(
           h=input$jy*mean(cereal$calories)/100, 
           w=input$jx*mean(cereal[, xvar,drop=TRUE])/100),
           size=input$psize)
    if(input$fitline == "Yes"){ plot1 <- plot1 + geom_smooth(method="lm")}
    plot1 # be sure to return plot a end
  })
  output$slope <- renderTable({
     fit <- lm( calories ~ cereal[, input$xvar], data=cereal)
     data.frame(summary(fit)$coefficients[2,, drop=FALSE])
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
                       "No" ="No") ),
        selectInput("xvar",
                    "Select the X-variable for the plot",
                    c("Fat" ='fat', "Carbohydrates"='carbo', "Sugars"='sugars', 'Protein'='protein'))
    ), # end of sidebar Panel 
    mainPanel(
       plotOutput("scatterplot"),
       tableOutput("slope")
    )  # end of mainPanel
  )
)

# Run the application 
shinyApp(ui = ui, server = server)
