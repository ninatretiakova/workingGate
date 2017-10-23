#Install and update before starting:

#install.packages("installr")
#library(installr)
#updateR()
#biocLite("BiocUpgrade")
#source("https://bioconductor.org/biocLite.R")
#biocLite("flowCore")
#install.packages("flowCore")

#library(shiny) #Load shiny library.
#library("flowCore") #Load flowCore library.
#library("flowViz")  #Load flowViz library.

#setwd("C:/Users/ninat/Documents/") #Set working directory.

#mydata = read.FCS('20160810ndbg/B01 control.fcs') 

#Read an FCS file.

mydata = read.FCS('20160810ndbg/B02 24hr-2ndbg-notoxo.fcs')

#Create object with rows as cells and columns as channels.

E <- exprs(mydata) 

#Coerce an object into a data frame.

mymat <- as.data.frame(E) 

colnames(mymat)
x <- colnames(mymat)
toString(x)

#########################################
#App#
#########################################

ui <- fluidPage(
  
  titlePanel("Gating Example"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      
      #Let the user choose the channels to visualize.
      
      selectInput('chan1', "Choose x-axis channel name", colnames(mymat)),
      selectInput('chan2', "Choose y-axis channel name", colnames(mymat)),
      
      #Get the gate limits from the user.
      
      sliderInput(inputId = "numx",
                  label = "Choose x-axis limit",
                  value = c(200,900), min = 0, max = 250000),
      sliderInput(inputId = "numy",
                  label = "Choose y-axis limit",
                  value = c(200, 3000), min = 0, max = 50000),
      
      #Let the user rename the channel names in accordance with the study.
      
      selectInput('getchannel', "Choose the channel to rename", colnames(mymat)),
      textInput('newchannel', "Type new channel name"),
      
      #Let the user select the channel to visualize as a histogram.
      
      selectInput('gethist', "Select a channel to construct a histogram", colnames(mymat))
    ),
    
    
    mainPanel(
      
      p("Number of events in the sample: ", nrow(mymat)),
      p("Number of channels in the sample: ", ncol(mymat)),
      p("The channel names are: ", toString(x)),
      p("Use the sliders to set gate limits."),
      p("You may want to rename the channels in accordance with your study. 
        Select the channel(s) you wish to rename and type in the new name(s)."),
  
      plotOutput("plot"),
      plotOutput("hist")
    )
  )  
)

server <- function(input, output, session) {
  
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

  observe({
    
    val1 <- mymat[, input$chan1]
    val2 <- mymat[, input$chan2]
   
    updateSliderInput(session, "numx", value = 100, min = min(val1), max = max(val1))
    updateSliderInput(session, "numy", value = 100, min = min(val2), max = max(val2))
  })
  
  output$plot <- renderPlot({
    
    plot(selectedData(),xlim = input$numx, ylim = input$numy, main = "Gate Area")
    
  })
  
  selectedChannel <- reactive({
    mymat[, input$gethist]
  })
 
  output$hist <- renderPlot({
    
    hist(log10(mymat[, input$gethist]), main = "Median Fluorescence Intensity (MFI)", col = "lightblue")
    #plot(density(log10(mymat[, input$gethist])), main = "Median Fluorescence Intensity (MFI)", toString(x[mymat[, input$gethist]]))
    abline(v = median(log10(mymat[, input$gethist])), col = 'orange', lwd = 2, lty = "dashed")
    legend("topleft", legend = paste("MFI:", median(log10(mymat[, input$gethist]))), col = "red", pch = 19) #Median fluorescence intensity

  })
  
  
  # output$newnames <- renderText({ 
  #   paste("Choose new names for the channels.")
  # })
}

shinyApp(ui = ui, server = server)