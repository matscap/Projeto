library(dplyr)
library(ggplot2)
shinyServer(
  function(input,output,session){
    
    myReactiveDat <- reactive({
      
      #população por região:
      
      #Algarve
      popalgarve <- 438635
      popalentejo <- 705.018
      popnorte <- 3573961
      poparealisboa <- 2854802
      popazores <- 241966
      popmadeira <- 246081
      popcentro <- 2348162
      
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
          View(escolha)
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
    
    myReactiveDat2 <- reactive({
      
      #população por região:
      
      #Algarve
      popalgarve <- 438635
      popalentejo <- 705018
      popnorte <- 3573961
      poparealisboa <- 2854802
      popazores <- 241966
      popmadeira <- 246081
      popcentro <- 2348162
      
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      #Dados para portugal apenas:
      dadosapenasportugal <- data %>%
        filter(ReportingCountry == "PT")
        
        fetaria <- input$fetaria
        
        regiao <- input$Regiao
        #ALENTEJO
        regiao1 <- dadosapenasportugal %>%
          filter(Region == "PTCSR01")
        totregiao1 <- sum(regiao1['SecondDose'])
        #ALGARVE
        regiao2 <- dadosapenasportugal %>%
          filter(Region == "PTCSR02")
        totregiao2 <- sum(regiao2['SecondDose'])
        #AÇORES
        regiao3 <- dadosapenasportugal %>%
          filter(Region == "PTCSR03")
        totregiao3 <- sum(regiao3['SecondDose'])
        #CENTRO
        regiao4 <- dadosapenasportugal %>%
          filter(Region == "PTCSR04")
        totregiao4 <- sum(regiao4['SecondDose'])
        #LISBOA
        regiao5 <- dadosapenasportugal %>%
          filter(Region == "PTCSR05")
        totregiao5 <- sum(regiao5['SecondDose'])
        #MADEIRA
        regiao6 <- dadosapenasportugal %>%
          filter(Region == "PTCSR06")
        totregiao6 <- sum(regiao6['SecondDose'])
        #NORTE
        regiao7 <- dadosapenasportugal %>%
          filter(Region == "PTCSR07")
        totregiao7 <- sum(regiao7['SecondDose'])
        
        
     ##############################################     
          escolha1 <- dadosapenasportugal %>%
            filter(TargetGroup == "Age18_24")
          tot1 <- escolha1['SecondDose']
          
          escolha2 <- dadosapenasportugal %>%
            filter(TargetGroup == "Age25_49")
          tot2 <- escolha2['SecondDose']
          
          escolha3 <- dadosapenasportugal %>%
            filter(TargetGroup == "Age50_59")
          tot3 <- escolha3['SecondDose']
          
          escolha4 <- dadosapenasportugal %>%
            filter(TargetGroup == "Age60_69")
          tot4 <- escolha4['SecondDose']
          
          escolha5 <- dadosapenasportugal %>%
            filter(TargetGroup == "Age70_79")
          tot5 <- escolha5['SecondDose']
          
          escolha6 <- dadosapenasportugal %>%
            filter(TargetGroup == "Age80+")
          tot6 <- escolha6['SecondDose']
          
          
          Data  = c("ALENTEJO","ALGRAVE","AÇORES","CENTRO","LISBOA","MADEIRA","NORTE")
          Doses = c(totregiao1/popalentejo*100,totregiao2/popalgarve*100,totregiao3/popazores*100,totregiao4/popcentro*100,totregiao5/poparealisboa*100,totregiao6/popmadeira*100,totregiao7/popnorte*100)

          df <- data.frame(
            Data,
            Doses
          )

    })
    
    output$myPlot <- renderPlot({
      res <- myReactiveDat()
      if(input$grafico=="Vacinação semanal")
        (ggplot(res$df, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
         )
      else if(input$grafico=="Acumulação de vacinas")
        ( ggplot(res$df, aes(x=res$Data, y=cumsum(res$Doses))) 
            + geom_line()  
            + geom_point() 
            + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20))  
            + xlab('Week') 
            + ylab('Doses') 
        )
      ##########################################################################
      
      })
    output$myPlot2 <- renderPlot({
      res <- myReactiveDat2()
     
      (ggplot(res$df, aes(x=res$Data, y=res$Doses)) 
        + geom_line() 
        + geom_bar(stat='identity')
        + theme(axis.text.x = element_text( vjust = 0.5),
                axis.title = element_text(face="bold", size=15),
                title = element_text(size = 20))
        + xlab('Week') 
        + ylab('Doses')
        + labs(title = "Percentagem de pessoas por região que já levaram as duas doses")
      )##########################################################################

    })
   
    
  }
)
