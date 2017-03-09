exploration_networks <- function(dataInput) { 
  library(shiny)
  library(ggplot2)
  library(stringr)
  library(ggnetwork)
  library(intergraph)

  ui <- fluidPage(
  
    headerPanel("Network Explorer"),
    sidebarPanel(
      uiOutput("networkShape"),
      uiOutput("networkColour"),
      uiOutput("networkSize"),
      uiOutput("networkTies"),
      selectInput('layout', 'Layout', choices = c("adj","circle","circrand","eigen","fruchtermanreingold","geodist",
                                                  "hall","kamadakawai","mds","princoord","random",
                                                  "rmds","segeo","seham","spring","springrepulse","target"), 
                                      selected = "fruchtermanreingold"),
      uiOutput("networkFacet"),
      checkboxInput(inputId = "namesNodes",
                    label = strong("Show node names"),
                    value = FALSE),
      conditionalPanel(condition = "input.namesNodes == true",
                       uiOutput("networkNamesNodes")),
      checkboxInput(inputId = "directed",
                    label = strong("Directed arrows"),
                    value = FALSE),
      numericInput("seed", "Set seed:", value = 1)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("trendPlot")),
                  tabPanel("R-code", verbatimTextOutput('Rcode')),
                  tabPanel("Data transformed", dataTableOutput('table_transformed')),
                  tabPanel("Data input", verbatimTextOutput('table'))
      )
    )
  )
  
  server <- function(input, output) {

    output$table <- renderPrint({ 
      summary(dataInput) 
    })

    gen_network <- reactive({
      set.seed(input$seed)
      
      gg_gen_network <- ggnetwork(dataInput, layout = input$layout, cell.jitter = 0.75)
      nms <- names(gg_gen_network)
      nmsFactors <- names(Filter(function(x) is.factor(x) || is.logical(x) || is.character(x), gg_gen_network)) # Make list of variables that are factors
      nmsNonFactors <- names(Filter(function(x) is.integer(x) || is.numeric(x) || is.double(x), gg_gen_network)) # Make list of variables that are not factors
      
      list(Network = gg_gen_network, 
           Names = nms,
           NamesFactors = nmsFactors,
           NamesNonFactors = nmsNonFactors
      )
    })
    
    gen_network_groups <- reactive({
      if(input$Facet != ".") {
        set.seed(input$seed)
        ggnetwork(dataInput, layout = input$layout, cell.jitter = 0.75, by = input$Facet)
      } 
    })
    
    output$table_transformed <- renderDataTable( 
      if(input$Facet == ".") gen_network()[["Network"]] else gen_network_groups() 
    )

    output$networkShape <- renderUI({
      selectInput("Shape", "Shape of nodes", choices = c("None" = '.', gen_network()[["NamesFactors"]]))
    })
        
    output$networkColour <- renderUI({
      selectInput("Colour", "Colour of nodes", choices = c("None" = '.', gen_network()[["Names"]]))
    })
    
    output$networkSize <- renderUI({
      selectInput("Size", "Size of nodes", choices = c("None" = '.', gen_network()[["NamesNonFactors"]]))
    })
    
    output$networkTies <- renderUI({
      selectInput("Ties", "Ties", choices = c("None" = '.', gen_network()[["NamesFactors"]]))
    })
    
    output$networkFacet <- renderUI({
      selectInput("Facet", "Comparison networks (e.g., over time)", choices = c("None" = '.', gen_network()[["NamesFactors"]]))
    })
    
    output$networkNamesNodes <- renderUI({
      selectInput("ChooseNames", "Choose the names variable", choices = c("None" = '.', gen_network()[["Names"]]))
    })
    
    stringCode <- reactive({
      
      if(input$Facet == ".") {
        p <- "ggplot(gen_network()[['Network']], aes(x = x, y = y, xend = xend, yend = yend))"
      } else if(input$Facet != ".") {
        p <- paste("ggplot(gen_network_groups(), aes(x = x, y = y, xend = xend, yend = yend)) + facet_wrap(input$Facet)")
      } 
      
      if(input$Ties == '.') {
        if(!input$directed) {
          p <- paste(p, "+", "geom_edges(color = 'grey50')")
        } else if(input$directed) {
          p <- paste(p, "+", "geom_edges(arrow = arrow(length = unit(6, 'pt'), type = 'closed'), color = 'grey50')")
        }
      } else if(input$Ties != '.') { 
        if(!input$directed) {
          p <- paste(p, "+", "geom_edges(aes(linetype = input$Ties), color = 'grey50')")
        } else if(input$directed) {
          p <- paste(p, "+", "geom_edges(aes(linetype = input$Ties), color = 'grey50', arrow = arrow(length = unit(6, 'pt'), type = 'closed'))") 
        }
      }
      
      if (input$Shape != '.') { 
        if(input$Colour != '.') {
          if(input$Size != '.') {
            p <- paste(p, "+", "geom_nodes(aes(shape=input$Shape, colour=input$Colour, size=input$Size)) + scale_size(range=c(1,10))")
          } else if(input$Size == '.') { 
            p <- paste(p, "+", "geom_nodes(aes(shape=input$Shape, colour=input$Colour), size=5)")
          }
        } else if(input$Colour == '.') {
          if(input$Size != '.') {
            p <- paste(p, "+", "geom_nodes(aes(shape=input$Shape, size=input$Size)) + scale_size(range=c(1,10))")
          } else if(input$Size == '.') { 
            p <- paste(p, "+", "geom_nodes(aes(shape=input$Shape), size=5)")
          }
        }
      } else if(input$Shape == '.') { 
        if(input$Colour != '.') {
          if(input$Size != '.') {
            p <- paste(p, "+", "geom_nodes(aes(colour=input$Colour, size=input$Size)) + scale_size(range=c(1,10))")
          } else if(input$Size == '.'){ 
            p <- paste(p, "+", "geom_nodes(aes(colour=input$Colour), size=5)")
          }
        } else if(input$Colour == '.') {
          if(input$Size != '.') {
            p <- paste(p, "+", "geom_nodes(aes(size=input$Size)) + scale_size(range=c(1,10))")
          } else if(input$Size == '.'){ 
            p <- paste(p, "+", "geom_nodes(size=5)")
          }
        }
      }
      
      if(input$namesNodes && input$ChooseNames != ".") p <- paste(p, "+", "geom_nodetext(aes(label = input$ChooseNames ), size = 10, colour = 'blue')") 
      
      p <- paste(p, "+", "theme_blank()") 
      
      # Replace name of variables by values
      p <- str_replace_all(p, "input\\$Ties", input$Ties)
      p <- str_replace_all(p, "input\\$Shape", input$Shape)
      p <- str_replace_all(p, "input\\$Colour", input$Colour)
      p <- str_replace_all(p, "input\\$Size", input$Size)
      p <- str_replace_all(p, "input\\$ChooseNames", input$ChooseNames)
      
      p
    })
    
    output$trendPlot <- renderPlot({
      
      # evaluate the string RCode as code
      p <- eval(parse(text=stringCode()))
      p
      
    })
    
    # Give the R-code as output
    output$Rcode <- renderText({ 

      begin_text <- "# You can use the below code to generate the graph\n# Don't forget to replace the 'dataInput' with the name of your dataframe"
      package_text <- paste("# You need the following package(s): \n", "library(ggplot2)\nlibrary(ggnetwork)", sep="")
      network_text <- "# The code below will generate the required network-dataframe for ggplot:"
      network_code_text <-  paste(paste("set.seed(", input$seed, ")\n"), 
                                  "nw_gg <- ggnetwork( dataInput, layout = '", input$layout, "', cell.jitter = 0.75", 
                                  if(input$Facet == ".") ")" else paste(", by = ", input$Facet, ")"), sep="")
      graph_text <- "# The code below will generate the graph:"
      gg_text <- stringCode()
      gg_text <- str_replace_all(gg_text, "\\+ ", "+\n  ")
      gg_text <- str_replace_all(gg_text, "gen_network\\(\\)\\[\\['Network'\\]\\]", "nw_gg")
      gg_text <- str_replace_all(gg_text, "gen_network_groups\\(\\)", "nw_gg")
      gg_text <- str_replace_all(gg_text, "input\\$Facet", input$Facet) # This is needed because facet uses a formula rather than string CHECK
      gg_text <- paste("graph <- ", gg_text, "\ngraph", sep="")
      save_text <- "# If you would like to save your graph, you can use:"
      save_code <- "ggsave('my_graph.pdf', graph, width = 10, height = 10, unit = 'cm')"
      
      paste(begin_text,
            "\n\n", 
            package_text,
            "\n\n", 
            network_text,
            "\n",
            network_code_text,
            "\n\n",
            graph_text,
            "\n",
            gg_text,
            "\n\n", 
            save_text,
            "\n",
            save_code,
            sep="")

    })
    
    
  }
  shinyApp(ui, server)
}
