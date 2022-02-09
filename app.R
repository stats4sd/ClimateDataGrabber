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
library(knitr)
library(kableExtra)
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
                     end=as.Date("2021-12-31")
      ),
      textOutput("txt1"),
      textInput("label","(Optional) Enter ID Label",value="My Site"),
      actionButton("go","Get Data")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Location",textOutput("message"),leafletOutput("map")),
        tabPanel("Data view",conditionalPanel("input.go!=0",
             selectInput(inputId = "style",label ="Output data format",
                  choices = c("","Full Data","Daycent Input") ),
               conditionalPanel("input.style!=''",
              downloadLink('downloadData', 'Click to Download Data'))),DTOutput("data1")),
        tabPanel("Info",
                 htmlOutput("info"),htmlOutput("meta")
        )
        
        )
    )
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  out_data<-NULL
  out_data1<-NULL
  metadata1<-read.csv("metadata.csv")
  
  
  output$meta <- renderText({
    kable(metadata1) %>%
      kable_styling(
        font_size = 15,
        bootstrap_options = c("striped", "hover", "condensed")
      ) 
  })
  
  
  
  output$info<-renderText({
    HTML(paste("These data were obtained from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program via the nasapower R library. \n\n
      Data available from 1981 onwards only.\n\n
      Further information available from <a href='https://docs.ropensci.org/nasapower/'>Here</a><br><br>"))
  })
  output$txt1<-renderText({
    HTML("NOTE: Data available from 1981/01/01 to 2021/31/12")})
  
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
    out_data<-NULL
    if(as.Date(input$dates[1])<as.Date("1981-01-01")){
      min_date<-"1981-01-01"
    }
    else{
      min_date<-input$dates[1]
    }
    if(as.Date(input$dates[2])>as.Date("2021-12-31")){
      max_date<-"2021-12-31"
    }
    else{
      max_date<-input$dates[2]
    }
    out_data<- download_nasapower(Start=as.character(min_date),
                                  End=as.character(max_date),
                                  longitude=as.numeric(input$longitude),
                                  latitude =as.numeric(input$latitude) ,
                                  ID=input$label,map = FALSE)
    
    
    output$message<-renderText({
      m<-""
      if(nrow(out_data)>0){
        
        if(as.Date(input$dates[1])<as.Date("1981-01-01")){
          m<-"No data available before 1981.\n"
        }
        if(as.Date(input$dates[2])>as.Date("2021-12-31")){
          m<-paste(m,"No data available after 31st December 2021. \n")
        }
        paste(m,nrow(out_data),"observations succesfully obtained in range",
              as.character(min(out_data$Date)),"to",
              as.character(max(out_data$Date)))
      }
      else{
        paste("Data download attempt failed")
      }
      
    })
    output$data1<-DT::renderDT({
      
      
      if(input$style=="Daycent Input"){
        out_data1<-out_data %>% mutate(prec=prec/10) %>% 
        dplyr::select(DD,MM,YEAR,DOY,tmax,tmin,prec)
      }
      else{
        out_data1<-out_data
      }
      out_data1
      
      })

    
    output$downloadData <- downloadHandler(
      
      filename = function() {
        if(input$style=="Full Data"){
        paste(input$label,'-', Sys.Date(), '.csv', sep='')
        }
        else{
          paste(input$label,'-', Sys.Date(), '.wth', sep='')
        }
      },
      content = function(con) {
        if(input$style=="Full Data"){
          write.csv(out_data, con,row.names=FALSE)
        }
        if(input$style=="Daycent Input"){
          write.table(
            out_data %>% mutate(prec=prec/10) %>% 
              dplyr::select(DD,MM,YEAR,DOY,tmax,tmin,prec),
                     con,row.names=FALSE,col.names = FALSE)
        }
      }
    )
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



