#Sample R script to show the manipulation of event data in shiny/plotly
#Author: M Wang
#
library(shiny)
library(plotly)
library(htmlwidgets)
library(shinyjs)

jsCode1 <- c(
  "function(el, x, inputName){",
  "  var id = el.getAttribute('id');",
  "  var out = [];",
  "  for(var i = 0; i < document.getElementById(id).data.length; i++) {",
  "    out.push('trace ' + i);",
  "  }",
  "  Shiny.setInputValue(inputName, out);",
  "}")

jsCode2 <- "shinyjs.plotlyGetTraces =  function(id,inputName='tracesPlot1'){
        var out = [];
        for(var i = 0; i < document.getElementById(id).data.length; i++) {
            out.push('trace ' + i);
        }
        Shiny.setInputValue(inputName, out);
    }"
ui <- fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode2),
    div(
        plotlyOutput("plot"),
        style = "border: solid black 1px"
    ),
    p('Click on a point to highlight a point with text; double click to clear the highlight.'),
    verbatimTextOutput("hoverLog"),
    verbatimTextOutput("tracesLog")
)

server <- function(input, output, session) {

  output$plot <- renderPlotly({
    dat1 <- data.frame(x=runif(10,min=-10,max=10),y=runif(10,min=-10,max=10),label=LETTERS[1:10])
    p1 <- plot_ly(iris) %>% add_trace(
      x = ~Sepal.Length,
      y = ~Sepal.Width,
      type = 'scatter',
      mode = 'markers',
      key = ~ Species
    )  %>% layout(
      title = "Edgar Anderson's Iris Data",
      showlegend = FALSE,
      margin = list(t=40)
    )
    p1 %>% onRender(jsCode1, data = "tracesPlot1")
  })
  
  output$tracesLog<- renderPrint({ paste0('Traces: ', paste(input$tracesPlot1,collapse=', '))  })
  observeEvent(event_data("plotly_hover"), {
      d <- event_data("plotly_hover")
      output$hoverLog <- renderPrint({ d  })
  })

  traces <- reactiveVal()
  observeEvent(event_data("plotly_click"), {
    if(is.null(traces())) traces(input$tracesPlot1)
    d <- event_data("plotly_click")
    if(!is.null(traces()) && !paste('trace',d$curveNumber) %in% traces()){
        plotlyProxy("plot", session) %>% plotlyProxyInvoke("deleteTraces", list(d$curveNumber))
    }else{
        plotlyProxy("plot", session) %>% plotlyProxyInvoke("addTraces", list(x = list(d$x), 
                                          y = list(d$y), 
                                          type = 'scatter',
                                          text = d$key,
                                          textfont = list(size=10),
                                          mode = 'markers+text'))
    }
    js$plotlyGetTraces('plot')
  })
  observeEvent(event_data("plotly_doubleclick"), {
    if(is.null(traces())) return(NULL)
    d <- setdiff(input$tracesPlot1,traces())
    if(length(d)==0) return(NULL)
    j = rev(as.integer(sub('trace ','',d)))
    for(i in j){
        plotlyProxy("plot", session) %>% plotlyProxyInvoke("deleteTraces", list(i))
    }
    js$plotlyGetTraces('plot')
  })

}

shinyApp(ui, server)
