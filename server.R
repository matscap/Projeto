library(dplyr)
library(ggplot2)
shinyServer(
  
  function(input,output,session){
    
    myReactiveDat <- reactive({
      
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      #Dados para portugal apenas:
      dadosapenasportugal <- data %>%
        filter(ReportingCountry == "PT")
      
      #Dados por região:
      if(input$opcao=="Por região"){
        
          regiao <- input$Regiao
          
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
      }
      
      
      #Dados por GRUPOS ETÁRIOS:
      if(input$opcao=="Por idades"){
        
        fetaria <- input$fetaria
        
        if(fetaria=="Age18_24"){
          escolha <- dadosapenasportugal %>%
            filter(TargetGroup == "Age18_24")
        }else if(fetaria == "Age25_49"){
          escolha <- dadosapenasportugal %>%
            filter(TargetGroup == "Age25_49")
        }else if(fetaria == "Age50_59"){
          escolha <- dadosapenasportugal %>%
            filter(TargetGroup == "Age50_59")
        }else if(fetaria == "Age60_69"){
          escolha <- dadosapenasportugal %>%
            filter(TargetGroup == "Age60_69")
        }else if(fetaria == "Age70_79"){
          escolha <- dadosapenasportugal %>%
            filter(TargetGroup == "Age70_79")
        }else if(fetaria == "Age80+"){
          escolha <- dadosapenasportugal %>%
            filter(TargetGroup == "Age80+")
        }
      }
      ##########################################################################
      #histogram de doses por região:
      Data = unlist(escolha["YearWeekISO"])
      Doses=unlist(escolha["FirstDose"])
      
      df <- data.frame(
        Data,
        Doses
      )
    })
    
    output$myPlot <- renderPlot({
      res <- myReactiveDat()
      if(input$grafico=="Vacinação semanal")
        ggplot(res$df, aes(x=res$Data, y= res$Doses)) + geom_bar(stat='identity')
      else if(input$grafico=="Acumulação de vacinas")
        ggplot(res$df, aes(x=res$Data, y=cumsum(res$Doses))) + geom_line() + geom_point()
      
      ##########################################################################
      
      })
   
    
  }
)
