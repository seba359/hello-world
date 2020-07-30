library(shiny)
library(ggplot2)
ui <- fluidPage(
  fluidRow(
    column(5,
      fluidRow(fileInput("num", "Choose CSV File", accept = ".csv")),
      fluidRow(checkboxInput("header", "Header", TRUE)),
      fluidRow(checkboxGroupInput(inputId = "nucleo" ,label = "Nucleo", choiceNames = c("Gaussiano","Triangular","Rectangular","Epacheknikov"), choiceValues = c("gaussian","triangular","epanechnikov","rectangular")),align="center")
      ,align="center"),
    column(7,
      fluidRow(sliderInput(inputId= "h", label = "Ventana", min = 0.1, max = 10, value = 2 )),
      fluidRow(sliderInput(inputId= "bin", label = "Bin", min = 0.1, max = 10, value = 2 )),
      fluidRow(numericInput(inputId = "Xnew", label = "Valor a predecir", value = 165))
    ,align="center")
  ),
  fluidRow(
    plotOutput("hist")
  ),
  fluidRow(
    verbatimTextOutput("stats")
  )
)
server <- function(input, output) {
  data <- reactive({
    input$num  
  })
  output$hist <- renderPlot({
    file<-input$num
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    dat<-as.data.frame(read.csv(file$datapath,header = input$header, sep = ","))
    fest<-approxfun(density(dat$altura, bw=input$h, kernel = input$nucleo))
    densidad<-sapply(dat$altura,fest)
    ggplot(dat, aes(x=altura))+
      labs(y='Densidad')+
      geom_histogram(aes(y=stat(count)/sum(count)),binwidth = input$bin,colour='red',fill='darkgreen')+
      geom_line(aes(y=densidad),lwd=2,colour='blue')+
      geom_point(aes(x=input$Xnew,y=fest(input$Xnew)),colour='yellow',shape=4,size=5)
  })
  output$stats <- renderPrint({
    summary(data())
  })
}
shinyApp(ui = ui, server = server)

