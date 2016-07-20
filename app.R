library(shiny)
library(ggplot2)

ui<-fluidPage(
  sidebarLayout(
    sidebarPanel(
      
  tags$h3("Input Variables"),
  sliderInput("n", "Manning's n", min=0.000, max=0.300, value=c(0.025,0.035)),
  sliderInput("WidthT", "Top Width (ft)", min=1, max=1000, value=c(100, 120)),
  sliderInput("WidthB", "Bottom Width (ft)", min=1, max=1000, value=c(20, 40)),
  sliderInput("Depth", "Depth (ft)", min=0.5, max=100, value=c(10,15)),
  sliderInput("SlopeB", "Bed Slope (ft/ft)", min=0.0001, max=0.5, value=c(0.002,0.005)),
  
  img(src="Capture.png"),
  downloadButton("mycsv", "Download csv file")
    ),
  mainPanel(
    tags$h2("Monte Carlo Analysis of Manning's Equation"),
  
  #tableOutput("table"),
  "An app demonstrating impacts of input value uncertainty on results of Manning's equation for open channel flow using Monte Carlo analysis.  See notes at bottom of screen for details.",
  
  plotOutput("box"),
  verbatimTextOutput("stats"),
  #p("All data are monthly mean effluent nutrient concentrations submitted to the Delaware River Basin Commission
    #between 2007 and 2014.  For full details please refer to ")
  "Programmed by John Yagecic, P.E.  (JYagecic@gmail.com)",
  tags$br(),
  tags$br(),
  tags$a(href="https://en.wikipedia.org/wiki/Manning_formula", "More about Manning's Equation."),
  tags$br(),
  tags$a(href="https://en.wikipedia.org/wiki/Monte_Carlo_method", "More about Monte Carlo method"),
  plotOutput("fourpanel"),
  #verbatimTextOutput("ManningDF"),
  "All distributions are uniform distributions with minimum and maximum values set by the slider bars.",
  "Input variable vectors have a length of 10,000 corresponding to 10,000 Monte Carlo iterations.",
  "Computation assumes a symmetrical trapezoidal channel.",
  "Bottom width should be less than top width for reasonable results.",
  tags$br(),
  "If you use this product or the underlying code in any professional or academic product, please consider ",
  "using a citation such as:",
  tags$br(),
  tags$br(),
  "Yagecic, John, July 2016.  Monte Carlo Analysis of Manning's Equation: a web app demonstrating impacts of input value uncertainty on results of Manning's equation for open channel flow using Monte Carlo analysis.",
  tags$br(),
  tags$br(),
  tags$a(href="https://github.com/JohnYagecic", "Get the script")
  )
  )
)

server<-function(input, output){
  
  n <- reactive({runif(10000, input$n[1], input$n[2])})
  WidthT <- reactive({runif(10000, input$WidthT[1], input$WidthT[2])})
  WidthB <- reactive({runif(10000, input$WidthB[1], input$WidthB[2])})
  Depth <- reactive({runif(10000, input$Depth[1], input$Depth[2])})
  SlopeB <- reactive({runif(10000, input$SlopeB[1], input$SlopeB[2])})
  Adj <- reactive({(WidthT() - WidthB())/2})
  Opp <- reactive({Depth()})
  Hyp <- reactive({sqrt(Adj()^2 + Opp()^2)})
  AreaSide <- reactive({0.5*Adj()*Opp()})
  AreaTotal <- reactive({(Depth() * WidthB()) + 2*AreaSide()})
  WettedPerim <- reactive({WidthB() + 2*Hyp()})
  RadiusHyd <- reactive({AreaTotal() / WettedPerim()})
  Vel <- reactive({(1.49/n())*((RadiusHyd())^(2/3))*(SlopeB()^0.5)})
  Q<-reactive({Vel()*AreaTotal()})
  
  ManningDF <- reactive({
    data.frame(n=n(), TopWidth=WidthT(),BottomWidth=WidthB(),Depth=Depth(),
               BedSlope=SlopeB(), Area=AreaTotal(),WettedPerimeter=WettedPerim(),
               HydraulicRadius=RadiusHyd(),Velocity=Vel(),Discharge=Q())
  })
  
  
  
  output$box<-renderPlot({
    par(mfrow=c(1,2))
    hist(Q(), breaks=20, main="Histogram of Q (CFS)", xlab="Q (CFS)")
    boxplot(Q(), ylab="Q (CFS)", main="Boxplot of Q (CFS)")
  })
  output$stats <- renderPrint({summary(Q())})
  
  
  output$fourpanel <- renderPlot({
    par(mfrow=c(2,2))
    hist(AreaTotal(),main="Histogram of Area", xlab="Area (sq. ft)", breaks=20, col="blue")
    hist(Vel(), main="Histogram of Velocity", xlab="Velocity (ft/s)",breaks=20, col="red")
    hist(WettedPerim(), main="Histogram of Wetted Perminter", xlab="Wetted Perimeter (ft)", breaks=20, col="green")
    hist(RadiusHyd(), main="Histogram of Hydraulic Radius", xlab="Hydraulic Radius", breaks=20, col="purple")
  })
  
  #output$ManningDF <-renderPrint({head(ManningDF())})
  
  output$mycsv <- downloadHandler(
    filename = c('ManningMCdata.csv'),
    content = function(file) {
      setwd(tempdir())
      write.csv(ManningDF(), file)
    }
  )
  
    
}

shinyApp(ui=ui, server=server)