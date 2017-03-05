exploration_networks <- function(dataInput, seed, heightshiny) { 
  require(shiny)
  require(plotly)
  require(ggplot2)
  require(ggnetwork)
  require(network)
  require(sna)
  require(intergraph)

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
                    value = FALSE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("trendPlot")),
                  tabPanel("Data transformed", dataTableOutput('table_transformed')),
                  tabPanel("Data input", verbatimTextOutput('table'))
      )
    )
  )
  
  server <- function(input, output) {

    output$table <- renderPrint({ 
      summary(dataInput) #      c(data()) 
    })

    gen_network <- reactive({
      set.seed(input$seed)
      
      gg_gen_network <- ggnetwork(dataInput, layout = input$layout, cell.jitter = 0.75)
      nms <- names(gg_gen_network)
      nmsFactors <- names(Filter(function(x) is.factor(x) || is.logical(x) || is.character(x), gg_gen_network)) # Make list of variables that are not factors
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
    
    
    
    output$trendPlot <- renderPlot({
      
      if(input$Facet == ".") {
        nw_for_gg <- gen_network()[["Network"]]
        p <- ggplot(nw_for_gg, aes(x = x, y = y, xend = xend, yend = yend)) + theme_blank()
      } else if(input$Facet != ".") {
        nw_for_gg <- gen_network_groups()
        p <- ggplot(nw_for_gg, aes(x = x, y = y, xend = xend, yend = yend)) + theme_blank() + facet_wrap(input$Facet)
      } 
      
      if(input$Ties == '.') {
        if(!input$directed) {
          p <- p + geom_edges(color = "grey50")
        } else if(input$directed) {
          p <- p + geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed"), color = "grey50") 
        }
      } else if(input$Ties != '.') { 
        if(!input$directed) {
          p <- p + geom_edges(aes_string(linetype = input$Ties), color = "grey50")       
        } else if(input$directed) {
          p <- p + geom_edges(aes_string(linetype = input$Ties), color = "grey50", arrow = arrow(length = unit(6, "pt"), type = "closed")) 
        }
      }
    
      #geom_edges(arrow = arrow(length = unit(6, "pt"), type = "closed")) +
        
      if (input$Shape != '.') { 
        if(input$Colour != '.') {
          if(input$Size != '.') {
            p <- p + geom_nodes(aes_string(shape=input$Shape, colour=input$Colour, size=input$Size)) + scale_size(range=c(1,10))
          } else if(input$Size == '.') { 
            p <- p + geom_nodes(aes_string(shape=input$Shape, colour=input$Colour), size=5)
          }
        } else if(input$Colour == '.') {
          if(input$Size != '.') {
            p <- p + geom_nodes(aes_string(shape=input$Shape, size=input$Size)) + scale_size(range=c(1,10))
          } else if(input$Size == '.') { 
            p <- p + geom_nodes(aes_string(shape=input$Shape), size=5)
          }
        }
      } else if(input$Shape == '.') { 
        if(input$Colour != '.') {
          if(input$Size != '.') {
            p <- p + geom_nodes(aes_string(colour=input$Colour, size=input$Size)) + scale_size(range=c(1,10))
          } else if(input$Size == '.'){ 
            p <- p + geom_nodes(aes_string(colour=input$Colour), size=5)
          }
        } else if(input$Colour == '.') {
          if(input$Size != '.') {
            p <- p + geom_nodes(aes_string(size=input$Size)) + scale_size(range=c(1,10))
          } else if(input$Size == '.'){ 
            p <- p + geom_nodes(size=5)
          }
        }
      }
      
      if(input$namesNodes && input$ChooseNames != ".") p <- p + geom_nodetext(aes_string(label = input$ChooseNames ), size=10, colour="blue") 
      
      p

    })
  }
  shinyApp(ui, server, options = list(height = heightshiny))
}


