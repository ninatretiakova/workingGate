#Install and update before starting:

#install.packages("installr")
#library(installr)
#updateR()
#biocLite("BiocUpgrade")
#source("https://bioconductor.org/biocLite.R")
#biocLite("flowCore")
#install.packages("flowCore")

library(shiny) #Load shiny library.
library("flowCore") #Load flowCore library.
library("flowViz")  #Load flowViz library.

setwd("C:/Users/ninat/Documents/") #Set working directory.

mydata = read.FCS('20160810ndbg/B01 control.fcs') # Read an FCS file.

E <- exprs(mydata) #Create object with rows as cells and columns as channels. 

mymat <- as.data.frame(E) #Coerce an object into a data frame.

#Rename column names according to the experiment design.
colnames(mymat) <- c("FCSA", "SSCA", "FL1A", "FL2A", "FL3A", "FL4A", "FSCH", "SSCH", "FL1H", "FL2H", "FL3H", "FL4H", "Width", "Time")

x <- as.vector(colnames(mymat), mode = "character") #Coerce column names into a character vector.

######################################################
#Warning in charToRaw(enc2utf8(text)) :
  #argument should be a character vector of length 1
#all but the first element will be ignored
######################################################

#colnames(mymat) <- c('FCSA', 'SSCA', 'FL1A', 'FL2A', 'FL3A', 'FL4A', 'FSCH', 'SSCH', 'FL1H', 'FL2H', 'FL3H', 'FL4H', 'Width', 'Time')

#setNames(data.frame(matrix(ncol = 14, nrow = 0)), c("FCSA", "SSCA", "FL1A", "FL2A", "FL3A", "FL4A", "FSCH", "SSCH", "FL1H", "FL2H", "FL3H", "FL4H", "Width", "Time"))

#df <- data.frame(matrix(ncol = 14, nrow = 0))
#x <- c("FCSA", "SSCA", "FL1A", "FL2A", "FL3A", "FL4A", "FSCH", "SSCH", "FL1H", "FL2H", "FL3H", "FL4H", "Width", "Time")
#colnames(df) <- x

#####
#App#
#####

ui <- fluidPage(
  
  titlePanel("Gating Example"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      sliderInput(inputId = "numx",
                  label = "Choose x-axis limit",
                  value = c(200,900), min = 0, max = 1000000),
      sliderInput(inputId = "numy",
                  label = "Choose y-axis limit",
                  value = c(200, 3000), min = 0, max = 1000000),
      
      selectInput('chan1', "Choose x-axis channel name", colnames(mymat)),
      selectInput('chan2', "Choose y-axis channel name", colnames(mymat)),
      selectInput('getchannel', "Choose the channel to rename", colnames(mymat)),
      textInput('newchannel', "Type new channel name")
      #actionButton("newplot", "New plot")
    ),
    
    mainPanel(
      
      p("Number of events in the sample: ", nrow(mymat)),
      p("Number of channels in the sample: ", ncol(mymat)),
      p("The channel names are: ", x, sep=", "),
  
      plotOutput("plot"),
      plotOutput("hist")
      
    
    )
    
    # mainPanel(
    #   textOutput("channels")
    # ),
    # 
    # mainPanel(
    #   textOutput("newnames")
    # )
  )  
)



server <- function(input, output) {
  
  selectedData <- reactive({
    mymat[, c(input$chan1, input$chan2)]
  })
  
  sliderValues <- reactive({
    
    #Compose data frame.
    data.frame(
      Name = c("numx", "numy"),
      
      Value = c(input$numx,
                input$numy),
      
      stringsAsFactors=FALSE)
  })
  
  
  output$plot <- renderPlot({
    
    #rewrite with selected column numbers
    #plot(mysample[1:5000, input$chan1:input$chan2], main = "Test", xlim = input$numx, ylim = input$numy)
    #plot(mysample[, 1:2], main = "Test", xlim = input$numx, ylim = input$numy)
    #plot(selectedData, main = "Test", xlim = input$numx, ylim = input$numy)
    plot(selectedData(),xlim = input$numx, ylim = input$numy)
    
  })
  
  output$hist <- renderPlot({
    
    hist(mymat$FCSA)
    
  })
  
  # output$newnames <- renderText({ 
  #   paste("Choose new names for the channels.")
  # })
}

shinyApp(ui = ui, server = server)