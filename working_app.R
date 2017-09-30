library(shiny)

#install.packages("installr")
#library(installr)
#updateR()
#biocLite("BiocUpgrade")
#source("https://bioconductor.org/biocLite.R")
#biocLite("flowCore")
#install.packages("flowCore")

library("flowCore")
library("flowViz")

mydata = read.FCS('20160810ndbg/B01 control.fcs') # Read an FCS file. 6B pneumococcus.
E <- exprs(mydata)
mysample <- E[1:5000, 1:12]
mymat <- as.data.frame(mysample)

names(mymat)[names(mymat)=="FITC-A"] <- "FITC" #rename column
names(mymat)[names(mymat)=="FSC-A"] <-"FSCA"
names(mymat)[names(mymat)=="FSC-H"] <-"FSCH"
names(mymat)[names(mymat)=="FSC-W"] <-"FSCW"
names(mymat)[names(mymat)=="SSC-A"] <-"SSCA"
names(mymat)[names(mymat)=="PE-A"] <-"PEA"
names(mymat)[names(mymat)=="PE-Cy7-A"] <-"PECY7A"
names(mymat)[names(mymat)=="Pacific Blue-A"] <-"PACBLUEA"
names(mymat)[names(mymat)=="APC-A"] <-"APCA"
names(mymat)[names(mymat)=="Alexa Fluor 700-A"] <-"ALEXAFLUOR"
names(mymat)[names(mymat)=="APC-Cy7-A"] <-"APCCY7"
names(mymat)[names(mymat)=="ALEXAFLUOR"] <-"ALEXA700"


library(shiny)

ui <- fluidPage(
  
  titlePanel("Gating Example"),
  
  "Number of entries in the sample: ", nrow(mysample),
  "\nNumber of channels in the sample: ", ncol(mysample),
  "\nThe channel names are: ", names(mymat),
  
  sidebarLayout(
    
  sidebarPanel(
    
    #selectInput("dataset","Choose dataset",
                  	#choices = names(mymat)),
    sliderInput(inputId = "numx",
                label = "Choose x-axis limit",
                value = c(200,900), min = 0, max = 1000000),
    sliderInput(inputId = "numy",
                label = "Choose y-axis limit",
                value = c(200, 3000), min = 0, max = 1000000),
    
    selectInput("chan", "Choose channel name", names(mymat)),
    actionButton("newplot", "New plot")
  ),
  
  mainPanel(
    plotOutput("plot")
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


#plotOutput("hist")
server <- function(input, output) {
  
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("numx", "numy"),
      
      Value = c(input$numx,
               input$numy),
      
      stringsAsFactors=FALSE)
  })

  
  output$plot <- renderPlot({
   # , xlim = input$numx, ylim = input$numy
    View(mysample)
    plot(mysample[1:5000, 1:2], main = "Test", xlim = input$numx, ylim = input$numy)

  })
  
  # output$num_row <- renderText({ 
  #   paste("The number of entries our sample consists of is: ", nrow(mysample))
  # })
  # 
  # output$num_col <- renderText({ 
  #   paste("The number of available channels is:", ncol(mysample))
  # })
  # 
  # output$channels <- renderText({ 
  #   paste("The names of available channels are:", names(mymat))
  # })
  # 
  # output$newnames <- renderText({ 
  #   paste("Choose new names for the channels.")
  # })
}

shinyApp(ui = ui, server = server)

##################################
# PROBLEMS
##################################

# How to select different channels? Assign column names to respective numbers?
# Write cases: if I choose channel A, then I will work with such and such columns
