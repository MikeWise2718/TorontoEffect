library(shiny)
# old df1
dfaa <- data.frame(A = c( 1L, 4L, 0L, 1L), 
                   B = c("3","*","*","2"), 
                   C = c("4","5","2","*"), 
                   D = c("*","9","*","4"),stringsAsFactors = F) 
# old df2
dfbb <- data.frame(variable = c("A","B","C","D"), 
                  Value    = c( 2L, 1L, 9L, 0L),stringsAsFactors = F)

ui <-  fluidPage(titlePanel("Sample"),
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h3("Select Variable"), 
                  choices = unique(dfbb$variable), 
                  selected = unique(dfbb$variable)[1]),
      numericInput("num", label = h3("Replace * in A with"), 
                   value = unique(dfbb$Value)[1]),
      actionButton("applyChanges", "Apply Changes specified in B to A")),
    mainPanel(
      h3("Table A"),  dataTableOutput(outputId="tableA"),
      h3("Table B"),  dataTableOutput(outputId="tableB")
)))

server <- function(input, output) {
  rv <- reactiveValues(dfA=dfaa,dfB=dfbb)
  observe({
    # update dfB immediately when the variable or value in the ui changes
    rv$dfB$Value[rv$dfB$variable==input$select] <- input$num
  })
  
  observeEvent(input$applyChanges,{
    # Here we apply the changes that were specified
    dfAcol <-as.character(rv$dfB$variable)
    rv$dfA[dfAcol] <- 
          Map(function(x, y) replace(x, x=="*", y), rv$dfA[dfAcol], rv$dfB$Value)
  })
  output$tableB <- renderDataTable({ rv$dfB })
  output$tableA <- renderDataTable({ rv$dfA })
}
shinyApp(ui=ui,server=server)