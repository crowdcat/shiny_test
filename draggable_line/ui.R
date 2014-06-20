library(shiny)
library(plyr)
library(rCharts)
shinyUI(pageWithSidebar(
  headerPanel(""),
  sidebarPanel(
    wellPanel(
      h6("Change here should update plotband:"),
      uiOutput("selectedOut"),
      uiOutput("windowOut")
    ),
    tags$head(tags$style(type="text/css", ".jslider { max-width: 245px; }"),tags$style(type='text/css', ".well { max-width: 250px; }"),
              tags$style(type='text/css', ".row-fluid .span4 {width: 20%}\n.row-fluid .span8 {width: 75%}"))
  ),
  mainPanel(
    HTML('<script type="text/javascript" src="draggable_functions.js"></script>'),
    showOutput("h1chart","highcharts"),
    tags$script('Shiny.addCustomMessageHandler("customMsg", function(bandOpts){
     chartXAxis = $("#h1chart").highcharts().xAxis[0]
     chartXAxis.removePlotBand()
     chartXAxis.addPlotBand(bandOpts)
   })')
    
  )
))