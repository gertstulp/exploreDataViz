exploration_means <- function(dataInput) { 
  library(shiny)
  library(plotly)
  library(stringr) 
  library(dplyr)
  
  nms <- names(dataInput)
  
  nmsNonFactors <- names(Filter(function(x) is.integer(x) || is.numeric(x) || is.double(x), dataInput)) # Make list of variables that are not factors
  nmsFactors <- names(Filter(function(x) is.factor(x) || is.logical(x) || is.character(x), dataInput)) # Make list of variables that are not factors
  
  ui <- fluidPage(
    
    headerPanel("Mean Explorer"),
    sidebarPanel(
      selectInput('Variable', 'Variable', choices = nmsNonFactors, selected = nmsNonFactors[1]),
      # Select 2nd variable in list if dataset is large enough
      selectInput('Group', 'Group', choices = if (length(nmsFactors)>1) c("No groups" = '.', nmsFactors) else "No factors available" = '.'),
      
      selectInput('facet_row', 'Facet Row', c(None = '.', nmsFactors)), # Only factors
      selectInput('facet_col', 'Facet Column', c(None = '.', nmsFactors)), # Only factors
      
      checkboxInput(inputId = "jitter",
                    label = strong("Show data points"),
                    value = FALSE),
      
      conditionalPanel(condition = "input.Group != '.'",
                       checkboxInput(inputId = "order",
                                     label = strong("Order variable x-axis"),
                                     value = FALSE)
      ),
      checkboxInput(inputId = "label_axes",
                    label = strong("Change labels axes"),
                    value = FALSE),
      conditionalPanel(condition = "input.label_axes == true",
                       textInput("xaxis", "X-axis:", value="label x-axis")
      ),
      conditionalPanel(condition = "input.label_axes == true",
                       textInput("yaxis", "Y-axis:", value="label y-axis")
      ),
      checkboxInput(inputId = "downloadCheck",
                    label = strong("Download summary table"),
                    value = FALSE),
      conditionalPanel(condition = "input.downloadCheck == true",
                       radioButtons("ext", "Format type:",
                                    choices = c("Excel (CSV)"="csv", "Word"="docx"))
      ),
      conditionalPanel(condition = "input.ext == 'docx'",
                       helpText("It might be necessary to download additional (free) software (e.g., Java, XQuartz on Mac) for the Word-file to correctly download")
      ),
      conditionalPanel(condition = "input.downloadCheck == true",
                       downloadButton('downloadData', 'Download')
      )
  ),
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotlyOutput('trendPlot')),
                  tabPanel("R-code", verbatimTextOutput('Rcode')),
                  tabPanel("Summary", dataTableOutput('SummaryTable')),
                  tabPanel("Data", dataTableOutput('table'))
      )
    )
  )
  
  server <- function(input, output) {
  
    stringCodeTable <- reactive({
      
      ### This creates statistics by group
      df_GroupVars <- data.frame(
        Group = c(input$Group),
        facet_row = c(input$facet_row),
        facet_col = c(input$facet_col)
      )
      countGroupVars <- sum(df_GroupVars[1,] != ".") # Count how many grouping variables there are
      indexGroupVars <- which(df_GroupVars[1,] != ".") # Index of grouping variables
      
      functionInString <- paste("dplyr::summarise(",
                                "Mean = mean(input$Variable, na.rm = TRUE),",
                                "SD = sd(input$Variable, na.rm = TRUE),",
                                "n = n(),",
                                "SE = SD/sqrt(n),",
                                "Min = min(input$Variable, na.rm = TRUE),",
                                "Max = max(input$Variable, na.rm = TRUE),",
                                "Missing = sum(is.na(input$Variable)))", sep="")
      
      # This (not so pretty) if-else structure is needed to make statistics with appropriate groupings
      if(countGroupVars==0) {
        tableCode <- paste("dataInput %>%", functionInString)
      } else if(countGroupVars==1) {
        tableCode <- paste("dataInput %>% ", "group_by(",
                    df_GroupVars[1,indexGroupVars[1]], ")", " %>% ", functionInString, sep="")
      } else if(countGroupVars==2) {
        tableCode <- paste("dataInput %>% ", "group_by(",
                    df_GroupVars[1,indexGroupVars[1]], ", ",
                    df_GroupVars[1,indexGroupVars[2]], ")", " %>% ", functionInString, sep="")
      } else if(countGroupVars==3) {
        tableCode <- paste("dataInput %>% ", "group_by(",
                           df_GroupVars[1,indexGroupVars[1]], ", ",
                           df_GroupVars[1,indexGroupVars[2]], ", ",
                           df_GroupVars[1,indexGroupVars[3]], ")", " %>% ", functionInString, sep="")
      }
      # Replace name of variables by values
      tableCode <- str_replace_all(tableCode, "input\\$Variable", input$Variable)
      
      tableCodeOutput <- tableCode
      tableCodeOutput <- str_replace_all(tableCodeOutput, "\\+ ", "+\n  ")
      tableCodeOutput <- str_replace_all(tableCodeOutput, "%>% ", "%>%\n  ")
      tableCodeOutput <- str_replace_all(tableCodeOutput, "\\),", "\\),\n\t")

      list(tableCode=tableCode, tableCodeOutput=tableCodeOutput)
    })
    
    dataset <- reactive({
      
      df <- eval(parse(text=stringCodeTable()[['tableCode']]))
      
      data.frame(lapply(df, function(y) if(is.numeric(y)) round(y, 2) else y)) 
      
    })
    
    stringCode <- reactive({
      
      # build graph with ggplot syntax
      if(input$Group != ".") {
        p <- "ggplot(dataset(), aes(x=input$Group, y=Mean)) + geom_point(size=2, colour='#56B4E9') + geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE),size=2, colour='#56B4E9', width=0)"
        if(input$jitter) p <- paste(p, "+", "geom_jitter(data=dataInput, aes(y = input$Variable), size=1, alpha=0.2, width=0.25, colour='#CC79A7')")
      } else if(input$Group == ".") {
        p <- "ggplot(dataset(), aes(x='', y = Mean)) + geom_point(size=2, colour='#56B4E9') + geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE),size=2, colour='#56B4E9', width=0)"
        if(input$jitter) p <- paste(p, "+", "geom_jitter(data=dataInput, aes(y = input$Variable), size=1, alpha=0.2, width=0.25, colour='#CC79A7')")
      }
      
      # This sorts x-axis according to means on y-axis
      if(input$order) {
        p <- paste(p, "+", "scale_x_discrete(limits=dataset()[['input$Group']][order(dataset()[['Mean']])])") # Dit sorteert de x-as op grootte
      }
      
      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- paste(p, "+", "facet_grid(", facets, ")")  
      
      # Specify axis labels
      if(input$label_axes) {
        p <- paste(p, "+", "labs(x='input$xaxis', y='input$yaxis')")
      } else {
        p <- paste(p, " + ", "labs(y='", input$Variable, " (mean \u00B1 standard error)", "')", sep="") 
      }
      
      p <- paste(p, "+", "theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))") 
      
      # Replace name of variables by values
      p <- str_replace_all(p, "input\\$Variable", input$Variable)
      p <- str_replace_all(p, "input\\$Group", input$Group)
      p <- str_replace_all(p, "input\\$xaxis", as.character(input$xaxis))
      p <- str_replace_all(p, "input\\$yaxis", as.character(input$yaxis))
      
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
      package_text <- paste("# You need the following package(s): \n", "library(ggplot2)\nlibrary(dplyr)", sep="")
      table_text <- "# The code below will generate the required dataframe with means:"
      table_code_text <-  paste("df_means <-", stringCodeTable()[['tableCodeOutput']]) 
      graph_text <- "# The code below will generate the graph:"
      gg_text <- stringCode()
      gg_text <- str_replace_all(gg_text, "\\+ ", "+\n  ")
      gg_text <- str_replace_all(gg_text, "dataset\\(\\)", "df_means")
      gg_text <- paste("graph <- ", gg_text, "\ngraph", sep="")
      package_plotly_text <- paste("# If you want the plot to be interactive, you need the following package(s): \n", "library(plotly)", sep="")
      plotly_text <- paste("ggplotly(graph)")
      save_text <- "# If you would like to save your graph, you can use:"
      save_code <- "ggsave('my_graph.pdf', graph, width = 10, height = 10, unit = 'cm')"
      
      paste(begin_text,
            "\n\n", 
            package_text,
            "\n\n", 
            table_text,
            "\n",
            table_code_text,
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
    
    output$SummaryTable <- renderDataTable(
      dataset() 
    )
    
    output$table <- renderDataTable(dataInput)
    
    ############ Download handler for the download button ####################
    # downloadHandler() takes two arguments, both functions.
    # The content function is passed a filename as an argument, and
    #   it should write out data to that filename.
    output$downloadData <- downloadHandler(
      
      # This function returns a string which tells the client
      # browser what name to use when saving the file.
      filename = function() {
        paste(paste("SummaryTable", Sys.time(), sep="_"), input$ext, sep = ".")  
      },
      
      # This function should write data to a file given to it by
      # the argument 'file'.
      content = function(file) {
        sep <- switch(input$ext, "csv" = ",", "docx" = " ")
        
        # Write to a file specified by the 'file' argument
        if(input$ext=="csv") {
          write.table(dataset(), file, sep = sep,
                      row.names = FALSE)
        } else if(input$ext=="docx") {
          library(ReporteRs)
          doc <- docx()
          doc <- addFlexTable( doc, vanilla.table(dataset()))
          writeDoc(doc, file=file)
        }
      }
    )
    
  }
  shinyApp(ui, server)
}