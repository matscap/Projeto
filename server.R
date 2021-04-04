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
      if(regiao=="ALENTEJO"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR01")
      }else if(regiao == "ALGARVE"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR02")
      }else if(regiao == "AZORES"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR03")
        print("AZORES")
      }else if(regiao == "CENTRO"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR04")
        print("CENTRO")
      }else if(regiao == "LISBOA"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR05")
        print("LISBOA")
      }else if(regiao == "MADEIRA"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR06")
        print("MADEIRA")
      }else if(regiao == "NORTE"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR07")
        print("NORTE")
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
      
      anosemana = unlist(escolha["YearWeekISO"])
      primeiradose=unlist(escolha["FirstDose"])
      
      df <- data.frame(
        anosemana,
        primeiradose
      )
      ggplot(df, aes(x=anosemana, y= primeiradose)) + geom_bar(stat='identity')
      
      ##########################################################################
      
    })
    
  }
)
