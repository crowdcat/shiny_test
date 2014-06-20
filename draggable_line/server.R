library(shiny)
library(plyr)
library(rCharts)
shinyServer(function(input, output,session){
  output$h1chart <- renderChart({
    h1 <- rCharts::Highcharts$new()
    h1$series(data = list(list(0,270),
                          list(11,270)), 
              type='line',draggable = T,
              draggableSeries = T,
              dragMin = -100,
              marker = list(enabled = F))
    
    h1$series(data=list(),
              type='column',
              dragMin=0,
              isThresholder = T,
              showInLegend = T)
    
    h1$series(data = list(0, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4),
              type = 'column',
              isThresholder = F,
              dragMin = 0)
    
    h1$set(dom = "h1chart")
    return(h1)
  })
  output$selectedOut <- renderUI({
    numericInput("selected", "", value=2.5)
  })
  output$windowOut <- renderUI({    
    sliderInput(inputId="window",label="Window size around selected point:",min=1,max=5,value=2)
  })
})#end server