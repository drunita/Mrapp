#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#devtools::install_github("drunita/mr-model")
library(shiny)
require(modelmr)
library(tibble)
library(purrr)
library(extrafont)
library(ggplot2)
library(grid)
library(gridExtra)
library(readr)
library(patchwork)
library(DT)
library(dplyr)
library(shinydashboard)
library(httr)
library(jsonlite)
library(lobstr)
library(utils)
library(lubridate)
library(readr)
library(rhandsontable)


DF <- data.frame(Fumigation_dates =  c(Sys.Date(),rep(NA, 23)), Remotion_dates = c(Sys.Date(),rep(NA, 23)),
                 stringsAsFactors = FALSE)
sporulated<-0
mature<-0
modeling_days<-1
R<-0
q1 = 'SELECT mean("temp") as temp,mean("hum") as hum, difference(mean("rain")) as rain,  mean("windv") as wind FROM /^DL00011$/ WHERE time >= now() - '
q2= 'd GROUP BY time(1h) fill(none)'
cultivares<-c("1","2")
modelo_WH_i<-c("CART","TH","WH prototipo")
locations<-c("NCH","Prototipo","Necocli")
rse<-0.98
rle<-0.5

readRenviron(".Renviron")
API_USERNAME <- Sys.getenv("USER_NAME")
API_PASSWORD <-Sys.getenv("PASSWORD")

ui <- fluidPage(
    
    dashboardHeader(title = span("Modelo de prediccion de riesgo para la moniliasis en cacao", 
                                 style = "color: black; font-size: 42px")),
    
    sidebarPanel(
        dateInput("Crop_starting_date", "Inicio de la cosecha (año-mes-dia):"),
        numericInput("croping_frequency", "Dias entre recolecta de mazorcas",value=8),
        numericInput("new_pods", "Numero de arboles activos", value = 100),
        selectInput("cultivar", "Cultivar", cultivares),
        selectInput("modelo_WH_i", "Seleccione metdodo de estimacion de las horas de mojabilidad:", modelo_WH_i),
        selectInput("locations", "Seleccione ciudad", locations),
        fileInput("clima_data","Datos climatologicos",accept = c(".csv")),
        
    ),
    
    
    
    mainPanel(
        tabsetPanel(
            tabPanel("Acciones de control", rHandsontableOutput("hot")),
            tabPanel("Riesgo actual", plotOutput("plot2")), 
            tabPanel("Progreso de la enfermedad", plotOutput("plot1")), 
            tabPanel("Caracteristicas del cultivo", tableOutput("x")),
            tabPanel("clima", tableOutput("head"))
            
        )
    )
    #tableOutput("head"),)
)



server <- function(input, output, session) {
    
    
    values <- reactiveValues()
    
    ## Handsontable for control actions
    observe({
        if (!is.null(input$hot)) {
            DF = hot_to_r(input$hot)
        } else {
            if (is.null(values[["DF"]]))
                DF <- DF
            else
                DF <- values[["DF"]]
        }
        values[["DF"]] <- DF
    })
    output$hot <- renderRHandsontable({
        DF <- values[["DF"]]
        if (!is.null(DF))
            rhandsontable(DF, useTypes = TRUE, stretchH = "all")
    })
    control_data<-reactive({
        DF <- values[["DF"]]
    })
    
    
    csd<-reactive({
        format(input$Crop_starting_date,format="%Y-%m-%d" )
    })
    
    q<-reactive({
        q3<-as.numeric(Sys.Date()-as.Date(as.character(csd()), format="%Y-%m-%d"))
        paste(q1,q3,q2,sep="")
        
    })
    
    model_WH<-reactive({
        input$modelo_WH_i
    })
    
    
    
    np<-reactive({
        input$new_pods*0.02
    })
    ####only the 2% gives a pod dayly
    cul<-reactive({
        input$cultivar
    })
    cf<-reactive({
        input$croping_frequency
    })
    locations<-reactive({
        input$locations
    })
    
    pod_data<-reactive({
        data.frame("Healthy"=rep(np()*0.99,100),"maturity"=seq(0,0.99,0.01),"Disease"=rep(np()*0.01,100),"sporulate_level"=seq(0,0.99,0.01))
    })
    plot_data<-reactive({
        
        pod_data<-pod_data()  
        data.frame("Days"=1,"Healthy"=sum(pod_data$Healthy),"Mature"=0,"Latent"=sum(pod_data$Disease),"Sporulated"=0, "Harvest"= 0,"Latent_removed"=0, "Sporulated_removed"=0,
                   "Risk_factor"=0)
    })
    
    clima<-reactive({
        
        if (locations()=="NCH"){
            newdf<-weather_data_NC(q(),API_USERNAME,API_PASSWORD)
            LWD_estimation(newdf)}
        
        else if (locations()=="Prototipo"){
            file1 <- input$clima_data
            req(file1)
            newdf<-weather_data_prototipo(as.data.frame(read.csv(file1$datapath)))
            LWD_estimation(newdf)
            
        }
        else if (locations()=="Necocli"){
            req(input$clima_data)
            ext <- tools::file_ext(input$clima_data$name)
            switch(ext,
                   csv = vroom::vroom(input$clima_data$datapath, delim = ","),
                   validate("Invalid file; Please upload a .csv"))
        }
        
    })
    
    
    weather_data1<-renderTable({
        
        if ((locations()=="NCH")){
            parameter<-q()
            newdf1<-as.data.frame(weather_data_NC(parameter,API_USERNAME,API_PASSWORD))
            newdf<-LWD_estimation(newdf1)
            head(newdf,n=200)
        }
        
        else if (locations()=="Prototipo"){
            
            file1 <- input$clima_data
            req(file1)
            newdf<-weather_data_prototipo(as.data.frame(read.csv(file1$datapath)))
            newdf1<-LWD_estimation(newdf)
            head(newdf1,n=10)
        }
        else if (locations()=="Necocli"){
            parameter<-"05490000"
            
            newdf<-as.data.frame(weather_data(parameter))
            head(newdf,n=10)  
        }
        
    })
    
    
    
    
    x1 <- reactive({data.frame("Crop_starting_date"=csd(),
                               "new_pods"=np(),
                               "cultivar"=cul(), 
                               "croping_frequency" = cf() , 
                               "removing_sporulated_eficacy"=rse, 
                               "removing_latent_eficacy"=rle)
    })
    y1<-renderTable({x1()})
    
    
    
    plot1<-renderPlot({
        
        pod_data<-na.omit(as.data.frame(pod_data()))  
        plot_data<-na.omit(as.data.frame(plot_data()))
        newdf<-na.omit(as.data.frame(clima()))
        newdf<-mutate(newdf,Fecha=format(Fecha,format="%Y-%m-%d"))
        weather_data<-subset(newdf,newdf$Fecha>=csd())
        x<-na.omit(as.data.frame(x1()))
        newdf<-as.data.frame(control_data())
        newdf<-mutate(newdf,Fumigation_dates=format(Fumigation_dates,format="%Y-%m-%d"))
        y<-mutate(newdf,Remotion_dates=format(Remotion_dates,format="%Y-%m-%d"))
        WH_mod<-model_WH()
        
        plot_data<-running_simulation(weather_data,modeling_days,pod_data,
                                      plot_data,x,mature,sporulated,R,y,WH_mod)
        
        ploting_the_disease(plot_data)
        
    })
    plot2<-renderPlot({
        pod_data<-na.omit(as.data.frame(pod_data()))  
        plot_data<-na.omit(as.data.frame(plot_data()))
        newdf<-na.omit(as.data.frame(clima()))
        newdf<-mutate(newdf,Fecha=format(Fecha,format="%Y-%m-%d"))
        weather_data<-subset(newdf,newdf$Fecha>=csd())
        x<-na.omit(as.data.frame(x1()))
        
        newdf<-as.data.frame(control_data())
        newdf<-mutate(newdf,Fumigation_dates=format(Fumigation_dates,format="%Y-%m-%d"))
        y<-mutate(newdf,Remotion_dates=format(Remotion_dates,format="%Y-%m-%d"))
        WH_mod<-model_WH()
        
        
        
        plot_data<-running_simulation(weather_data,modeling_days,pod_data,
                                      plot_data,x,mature,sporulated,R,y,WH_mod)
        ploting_the_Risk(plot_data)})
    
    
    output$head <-weather_data1
    output$x<-y1
    output$plot1 <-plot1
    output$plot2 <-plot2
    
}

shinyApp(ui, server)