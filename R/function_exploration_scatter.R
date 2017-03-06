exploration_scatter <- function(dataInput) { 
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
                  tabPanel("Plot", plotlyOutput('trendPlot')),
                  tabPanel("R-code", verbatimTextOutput('Rcode')),
                  tabPanel("Data", dataTableOutput('table'))
      )
    )
  )
  
  server <- function(input, output) {
    
    
    stringCode <- reactive({
      
      if(input$color != ".") {
        p <- "ggplot(dataInput, aes(x = input$x, y = input$y, colour = input$color)) + geom_point()" 
      } else if(input$color == ".") {
        p <- "ggplot(dataInput, aes(x = input$x, y = input$y)) + geom_point()"
      }
      
      if(input$line) { # If line is selected add regression line
        p <- paste(p, "+", "geom_smooth(se=input$se, method=input$smooth)")
      }
      
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- paste(p, "+", "facet_grid(", facets, ")")  
      
      p <- paste(p, "+ theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))")
      
      # Replace name of variables by values
      p <- str_replace_all(p, "input\\$x", input$x)
      p <- str_replace_all(p, "input\\$y", input$y)
      p <- str_replace_all(p, "input\\$color", input$color)
      p <- str_replace_all(p, "input\\$se", input$se)
      p <- str_replace_all(p, "input\\$smooth", input$smooth)
      
      p
      
    })
    
    output$trendPlot <- renderPlotly({
      
      # evaluate the string RCode as code
      p <- eval(parse(text=stringCode()))
      
      ggplotly(p)
      
    })
    
    # Give the R-code as output
    output$Rcode <- renderText({ 
      q <- stringCode()
      q <- str_replace_all(q, "\\+ ", "+\n  ")
      paste("# You can use the below code to generate the graph\n# Don't forget to replace the 'dataInput' with the name of your dataframe\n", q)
    })
    
    output$table <- renderDataTable(dataInput)
  }
  shinyApp(ui, server)
}

exploration_scatter(mpg)
