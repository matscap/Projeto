library(dplyr)
library(ggplot2)
shinyServer(
  
  function(input,output,session){
    output$myPlot <- renderPlot({
      regiao <- input$Regiao
      
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      #Dados para portugal apenas:
      
      dadosapenasportugal <- data %>%
        filter(ReportingCountry == "PT")
      #Dados por região:
      if(input$Regiao=="ALENTEJO"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR01")
      }else if(regiao == "ALGARVE"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR02")
      }else if(regiao == "AZORES"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR03")
      }else if(regiao == "CENTRO"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR04")
      }else if(regiao == "LISBOA"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR05")
      }else if(regiao == "MADEIRA"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR06")
      }else if(regiao == "NORTE"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR07")
      }else{
        escolha <- dadosapenasportugal %>%
          filter(Region == "PT")
      }
      
      
      
      #Dados por GRUPOS ETÁRIOS:
      Age18_24 <- dadosapenasportugal %>%
        filter(TargetGroup == "Age18_24")
      Age25_49 <- dadosapenasportugal %>%
        filter(TargetGroup == "Age25_49")
      Age50_59 <- dadosapenasportugal %>%
        filter(TargetGroup == "Age50_59")
      Age60_69 <- dadosapenasportugal %>%
        filter(TargetGroup == "Age60_69")
      Age70_79 <- dadosapenasportugal %>%
        filter(TargetGroup == "Age70_79")
      Age80 <- dadosapenasportugal %>%
        filter(TargetGroup == "Age80+")
      
      ##########################################################################
      #histogram de doses por região:
      
      Data = unlist(escolha["YearWeekISO"])
      Doses=unlist(escolha["FirstDose"])
      
      df <- data.frame(
        Data,
        Doses
      )
      if(input$CHECKBOX)
        ggplot(df, aes(x=Data, y= Doses)) + geom_bar(stat='identity')
      else
        ggplot(df, aes(x=Data, y=cumsum(Doses))) + geom_line() + geom_point()
      
      ##########################################################################
      
    })
    
  }
)
