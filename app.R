#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse) # Data wrangling
library(lubridate) # Dates operations
library(daymetr)
library(chirps)
library(nasapower) # Weather databases
library(vegan) # Shannon Diversity Index
library(dplyr)
library(DT)
library(leaflet)


source("download_nasapower.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  
  
  # Application title
  titlePanel("Obtain Data Using nasapower library"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("latitude",
                   "Latitude",
                   min = -180,
                   max = 180,
                   value = -1,step = 0.1
      ),
      numericInput("longitude",
                   "Longitude",
                   min = -180,
                   max = 180,
                   value = 36,step =0.1 ),
      dateRangeInput("dates",
                     "Date Range",start=as.Date("2015-01-01"),
                     end=as.Date("2021-01-01"),min =as.Date("1981-01-01"),
                     max =as.Date(today())
      ),
      textInput("label","Enter ID Label",value="My Site"),
      actionButton("go","Get Data")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Location",textOutput("message"),leafletOutput("map")),
        tabPanel("Data view",    DTOutput("data1")),
        tabPanel("Data download",
                 selectInput(inputId = "style",label ="Output data format",
                             choices = c("","Full Data","Daycent Input") ),
                 conditionalPanel("input.style!='' & input.go!=0",
                                  downloadLink('downloadData', 'Download Data'))),
        
        tabPanel("Info",textOutput("info")))
    )
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  out_data<-NULL
  out_data1<-NULL
  
  
  output$info<-renderText({
    paste("These data were obtained from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program via the nasapower R library. \n\n
      Data available from 1981 onwards only.\n\n
            Metadata and other information to be added here")
  })
  
  output$map<-renderLeaflet({
    
    mpdata<- data.frame(longitude=as.numeric(input$longitude),
                        latitude =as.numeric(input$latitude) ,
                        ID=input$label)
    
    leaflet(mpdata) %>%
      setView(zoom=6,lng =mpdata$longitude ,lat = mpdata$latitude) %>%
      addCircleMarkers(~longitude, ~input$latitude,popup = ~ID) %>%
      addProviderTiles("OpenStreetMap.Mapnik")   
  })
  
  observeEvent(input$go,{
    
    if(as.Date(input$dates[1])<as.Date("1981-01-01")){
      min_date<-"1981-01-01"
    }
    else{
      min_date<-input$dates[1]
    }
    
    out_data<- download_nasapower(Start=as.character(min_date),
                                  End=as.character(input$dates[2]),
                                  longitude=as.numeric(input$longitude),
                                  latitude =as.numeric(input$latitude) ,
                                  ID=input$label,map = FALSE)
    
    
    output$message<-renderText({
      if(nrow(out_data)>0){
        paste(nrow(out_data),"observations succesfully obtained in range",
              as.character(min(out_data$Date)),"to",
              as.character(max(out_data$Date)))
      }
      else{
        paste("Data download attempt failed")
      }
      
    })
    output$data1<-DT::renderDT(out_data)
    
    
    
    output$downloadData <- downloadHandler(
      
      filename = function() {
        paste(input$label,'-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        if(input$style=="Full Data"){
          write.csv(out_data, con,row.names=FALSE)
        }
        if(input$style=="Daycent Input"){
          write.csv( dplyr::select(out_data,DOY,MM,YEAR,DD,tmax,tmin,prec),
                     con,row.names=FALSE,col.names = FALSE)
        }
      }
    )
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



