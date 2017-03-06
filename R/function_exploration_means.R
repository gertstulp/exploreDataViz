exploration_means <- function(dataInput) { 
  require(shiny)
  require(plotly)
  require(stringr) 
  require(dplyr)
  
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
                  #tabPanel("Plot", plotlyOutput('trendPlot', height=heightplot)),
                  tabPanel("Plot", plotlyOutput('trendPlot')),
                  tabPanel("R-code", verbatimTextOutput('Rcode')),
                  tabPanel("Summary", dataTableOutput('SummaryTable')),
                  tabPanel("R-code table", verbatimTextOutput('RcodeTable')),
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
      tableCode
    })
    
    dataset <- reactive({
      
      df <- eval(parse(text=stringCodeTable()))
      
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
      
      lab <- paste(input$Variable, "(mean ± standard error)")
      p <- paste(p, " + ", "labs(y='", paste(lab), "')", sep="") 
      p <- paste(p, "+", "theme_bw() + theme(axis.text.x = element_text(angle=45, hjust=1))") 
      
      # Replace name of variables by values
      p <- str_replace_all(p, "input\\$Variable", input$Variable)
      p <- str_replace_all(p, "input\\$Group", input$Group)

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
    
    # Give the R-code for the table as output
    output$RcodeTable <- renderText({ 
      q <- stringCodeTable()
      q <- str_replace_all(q, "\\+ ", "+\n  ")
      q <- str_replace_all(q, "%>% ", "%>%\n  ")
      q <- str_replace_all(q, "\\),", "\\),\n\t")
      paste("# You can use the below code to generate the table\n# Don't forget to replace the 'dataInput' with the name of your dataframe\n", q)
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
          require(ReporteRs)
          doc <- docx()
          doc <- addFlexTable( doc, vanilla.table(dataset()))
          writeDoc(doc, file=file)
        }
      }
    )
    
  }
  #shinyApp(ui, server, options = list(height = heightshiny))
  shinyApp(ui, server)
}
exploration_means(mpg)