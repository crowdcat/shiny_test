library(shiny)
library(plyr)
library(rCharts)
shinyServer(function(input, output,session){
  output$chart1 <- renderChart({
    h1 <- rCharts::Highcharts$new()
    h1$chart(animation = F)
    h1$series(data = list(list(0,0),
                          list(11,0)), # 11 is just the length of the bubble series, or the coordinate of the last bubble
              type='line',draggable = T,
              draggableSeries = T,
              dragMin = -100,
              marker = list(enabled = F))
    
    h1$series(data=list(),
              type='bubble',
              dragMin=0,
              isThresholder = T,
              showInLegend = T)
    
    h1$series(data = list(0, 71.5, 106.4, 129.2, 144.0, 176.0, 135.6, 148.5, 216.4, 194.1, 95.6, 54.4),
              type = 'bubble',
              isThresholder = F,
              dragMin = 0)
    
    h1$plotOptions(
      series = list(
        cursor = 'ns-resize',
        point = list(
          events = list(
            drop = "#! function() {
              $('#report').html(
                this.category + ' was set to ' + Highcharts.numberFormat(this.y, 2));
            } !#"
          )
        )
      )
    )
    
    h1$set(dom = "chart1")
    return(h1)
  })
  output$selectedOut <- renderUI({
    numericInput("selected", "", value=2.5)
  })
  output$windowOut <- renderUI({    
    sliderInput(inputId="window",label="Window size around selected point:",min=1,max=5,value=2)
  })
})#end server