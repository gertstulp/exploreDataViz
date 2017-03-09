exploration_distributions <- function(dataInput) { 
  library(shiny)
  library(plotly)
  library(stringr)
  
  nms <- names(dataInput)
  
  nmsNonFactors <- names(Filter(function(x) is.integer(x) || is.numeric(x) || is.double(x), dataInput)) # Make list of variables that are not factors
  nmsFactors <- names(Filter(function(x) is.factor(x) || is.logical(x) || is.character(x), dataInput)) # Make list of variables that are not factors
  
  
  ui <- fluidPage(
    
    headerPanel("Distribution Explorer"),
    sidebarPanel(
      selectInput('Variable', 'Variable', choices = nmsNonFactors, selected = nmsNonFactors[1]),
      # Select 2nd variable in list if dataset is large enough
      selectInput('Group', 'Group', choices = if (length(nmsFactors)>1) c("No groups" = '.', nmsFactors) else "No factors available" = '.'),
      
      selectInput('facet_row', 'Facet Row', c(None = '.', nmsFactors)), # Only factors
      selectInput('facet_col', 'Facet Column', c(None = '.', nmsFactors)), # Only factors
      selectInput(inputId = "Type",
                  label = "Display distribution:",
                  choices = c("Boxplot", "Violin", "Density", "Histogram"),
                  selected = "Boxplot"),
      
      conditionalPanel(condition = "input.Type == 'Boxplot' || input.Type == 'Violin'",
                       checkboxInput(inputId = "jitter",
                                     label = strong("Show data points"),
                                     value = FALSE)
      ),
      # Display this only if histogram is selected
      conditionalPanel(condition = "input.Type == 'Density' || input.Type == 'Histogram'",
                       sliderInput("alpha", "Opaqueness:", min = 0, max = 1, value = 0.8)
      ),
      conditionalPanel(condition = "input.Type == 'Histogram'",
                       numericInput("binwidth", "Binwidth:", value=1)
      ),
      
      ## Display this only if density is selected
      conditionalPanel(condition = "input.Type == 'Density' || input.Type == 'Violin'",
                       sliderInput(inputId = "bw_adjust",
                                   label = "Bandwidth adjustment:",
                                   min = 0.01, max = 2, value = 1, step = 0.1)
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
    
    output$table <- renderDataTable(dataInput)

    stringCode <- reactive({

        if(input$Type=="Histogram") {
          if(input$Group != ".") {
            p <- "ggplot(dataInput, aes(x = input$Variable)) + geom_histogram(aes(fill = input$Group), position = 'identity', alpha = input$alpha, binwidth = input$binwidth)" 
          } else if(input$Group == ".") {
            p <- "ggplot(dataInput, aes(x = input$Variable)) + geom_histogram(position = 'identity', alpha = input$alpha, binwidth = input$binwidth)" 
          }
        } else if(input$Type=="Density") {
          if(input$Group != ".") {
            p <- "ggplot(dataInput, aes(x = input$Variable)) + geom_density(aes(fill = input$Group), position = 'identity', alpha = input$alpha, adjust = input$bw_adjust)" 
          } else if(input$Group == ".") {
            p <- "ggplot(dataInput, aes(x = input$Variable)) + geom_density(position = 'identity', alpha = input$alpha, adjust = input$bw_adjust)" 
          }
        } else if(input$Type=="Boxplot") {
          if(input$Group != ".") {
            p <- "ggplot(dataInput, aes(y = input$Variable, x = input$Group)) + geom_boxplot()" 
          } else if(input$Group == ".") {
            p <- "ggplot(dataInput, aes(y = input$Variable, x = 1)) + geom_boxplot()"
          }
          if(input$jitter) p <- paste(p, "+", "geom_jitter(size = 1, alpha = 0.2, width = 0.25, colour = 'blue')")
        } else if(input$Type=="Violin") {
          if(input$Group != ".") {
            p <- "ggplot(dataInput, aes(y = input$Variable, x = input$Group)) + geom_violin(adjust = input$bw_adjust)" 
          } else if(input$Group == ".") {
            p <- "ggplot(dataInput, aes(y = input$Variable, x = 1)) + geom_violin(adjust = input$bw_adjust)"
          }
          if(input$jitter) p <- paste(p, "+", "geom_jitter(size = 1, alpha = 0.2, width = 0.25, colour = 'blue')")
        }
        
        # if at least one facet column/row is specified, add it
        facets <- paste(input$facet_row, '~', input$facet_col)
        if (facets != '. ~ .') p <- paste(p, "+", "facet_grid(", facets, ")")  
        
        p <- paste(p, "+ theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))")
        
        # Replace name of variables by values
        p <- str_replace_all(p, "input\\$Variable", input$Variable)
        p <- str_replace_all(p, "input\\$Group", input$Group)
        p <- str_replace_all(p, "input\\$bw_adjust", input$bw_adjust)
        p <- str_replace_all(p, "input\\$alpha", input$alpha)
        
        p
    })
    
    output$trendPlot <- renderPlotly({
      
      # evaluate the string RCode as code
      p <- eval(parse(text=stringCode()))
          
      ggplotly(p)
      
    })
    
    # Give the R-code as output
    output$Rcode <- renderText({ 
      
      begin_text <- "# You can use the below code to generate the graph\n# Don't forget to replace the 'dataInput' with the name of your dataframe"
      package_text <- paste("# You need the following package(s): \n", "library(ggplot2)", sep="")
      graph_text <- "# The code below will generate the graph:"
      gg_text <- stringCode()
      gg_text <- str_replace_all(gg_text, "\\+ ", "+\n  ")
      gg_text <- paste("graph <- ", gg_text, "\ngraph", sep="")
      package_plotly_text <- paste("# If you want the plot to be interactive, you need the following package(s): \n", "library(plotly)", sep="")
      plotly_text <- paste("ggplotly(graph)")
      save_text <- "# If you would like to save your graph, you can use:"
      save_code <- "ggsave('my_graph.pdf', graph, width = 10, height = 10, unit = 'cm')"
      
      paste(begin_text,
            "\n\n", 
            package_text,
            "\n\n", 
            graph_text,
            "\n",
            gg_text,
            "\n\n", 
            package_plotly_text,
            "\n\n",
            plotly_text, 
            "\n\n",
            save_text,
            "\n",
            save_code,
            sep="")

    })
    
  }
  shinyApp(ui, server)
}

