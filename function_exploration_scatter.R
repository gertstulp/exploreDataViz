exploration_scatter <- function(dataInput, heightplot, heightshiny) { 
  require(shiny)
  require(plotly)
  
  nms <- names(dataInput)
  
  nmsNonFactors <- names(Filter(function(x) is.integer(x) || is.numeric(x) || is.double(x), dataInput)) # Make list of variables that are not factors
  nmsFactors <- names(Filter(function(x) is.factor(x) || is.logical(x) || is.character(x), dataInput)) # Make list of variables that are not factors
  
  ui <- fluidPage(
    
    headerPanel("Scatter Explorer"),
    sidebarPanel(
      selectInput('x', 'X', choices = nms, if (length(nmsNonFactors)>0) selected = nmsNonFactors[1] else selected = nms[1]),
      # Select 2nd variable in list if dataset is large enough
      selectInput('y', 'Y', choices = nms, if (length(nmsNonFactors)>1) selected = nmsNonFactors[2] else selected = nms[1]),
      # Select 3rd variable in list if dataset is large enough
      selectInput('color', 'Color', choices = c("No colours" = '.', nms)),
  
      selectInput('facet_row', 'Facet Row', c(None = '.', nmsFactors)), # Only factors
      selectInput('facet_col', 'Facet Column', c(None = '.', nmsFactors)), # Only factors
      checkboxInput(inputId = "line",
                    label = strong("Show regression line"),
                    value = FALSE),
      conditionalPanel(condition = "input.line == true",
                       selectInput('smooth', 'Smoothening function', choices = c("lm", "loess", "gam"))
      ),
      conditionalPanel(condition = "input.line == true",
                       checkboxInput(inputId = "se",
                                     label = strong("Show confidence interval"),
                                     value = FALSE)
      )
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput('trendPlot', height=heightplot)),
                  tabPanel("Data", dataTableOutput('table'))
      )
    )
  )
  
  server <- function(input, output) {
    
    output$table <- renderDataTable(dataInput)
    
    output$trendPlot <- renderPlotly({
      
      # build graph with ggplot syntax
      p <- ggplot(dataInput, aes_string(x = input$x, y = input$y)) + theme_bw()
      
      if(input$color != ".") {
        p <- ggplot(dataInput, aes_string(x = input$x, y = input$y, colour = input$color)) + theme_bw() + geom_point() 
      } else if(input$color == ".") {
        p <- ggplot(dataInput, aes_string(x = input$x, y = input$y)) + theme_bw() + geom_point() 
      }
      
      if(input$line) { # If line = true add regression line
        p <- p + geom_smooth(se=input$se, method=input$smooth)
      }
      
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- p + facet_grid(facets)
      
      ggplotly(p)
      
    })
  }
  shinyApp(ui, server, options = list(height = heightshiny))
}


