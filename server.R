library(dplyr)
library(ggplot2)
shinyServer(
  function(input,output,session){
    
    loadedData <- reactiveVal()
    
    observe({
      loadedData(read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")) 
    })
    
    Vacinas <- reactive({
      
      #Dados para portugal apenas:
      dadosapenasportugal <- loadedData() %>%
        filter(ReportingCountry == "PT")
      
      dadosapenasportugal <- dadosapenasportugal %>%
        filter(Region == "PT")
      
      Todos <- dadosapenasportugal <- dadosapenasportugal %>%
        filter(TargetGroup == "ALL")
      
      Pfizer <- dadosapenasportugal %>%
        filter(Vaccine == "COM")
      AZ <- dadosapenasportugal %>%
        filter(Vaccine == "AZ")
      MOD <- dadosapenasportugal %>%
        filter(Vaccine == "MOD")
      
      #Totais:
      
      totpfizer = sum(Pfizer["FirstDose"])+sum(Pfizer["SecondDose"])
      totAZ = sum(AZ["FirstDose"])+sum(AZ["SecondDose"])
      totMOD = sum(MOD["FirstDose"])+sum(MOD["SecondDose"])
      
      Totais = c(totpfizer,totAZ,totMOD)
      Nomes = c("Pfizer","AstraZeneca","Moderna")
      
      ##########################################################################
      #histogram de doses por região:
      
      df <- data.frame(
        Totais,
        Nomes
      )
    })
    
    
    myReactiveDat2 <- reactive({
      
      #Dados para portugal apenas:
      dadosapenasportugal <- loadedData() %>%
        filter(ReportingCountry == "PT")
      
      dadosapenasportugal <- dadosapenasportugal %>%
        filter(Region == "PT")

      
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
      Data = unlist(c("Age18_24","Age25_49","Age50_59","Age60_69","Age70_79","Age80"))
      Doses=unlist(c(sum(Age18_24['FirstDose']),sum(Age25_49['FirstDose']),sum(Age50_59['FirstDose']),sum(Age60_69['FirstDose']),sum(Age70_79['FirstDose']),sum(Age80['FirstDose'])))
      
      df <- data.frame(
        Data,
        Doses
      )
    })
    
    output$myPlotPais <- renderPlot({
      opcao <- input$dist
      
      #vac <- Vacinas()
      if(opcao=="norm"){
        res <- myReactiveDat2()
        ggplot(res$df, aes(x=res$Data, y= res$Doses)) + 
            geom_bar(stat='identity')+ 
            theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20)) + 
            xlab('Week') +
            ylab('Doses') 
      }else if(opcao=="unif"){
        res <- Vacinas()
        ggplot(res$df, aes(x=res$Nomes, y= res$Totais)) + 
          geom_bar(stat='identity')+ 
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                axis.title = element_text(face="bold", size=18),
                title = element_text(size = 20)) + 
          xlab('Vacinas') +
          ylab('Doses') 
      }
    })
    
    myReactiveDat <- reactive({
      
      tab <- input$tabs
      fetaria <- input$fetaria
      
      #Dados para portugal apenas:
      dadosapenasportugal <- loadedData() %>%
        filter(ReportingCountry == "PT")
      tipo <- input$radiovac
      if(tab=="Alentejo"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR01")
        tipo <- input$radiovac1
        if(tipo=="mod"){
          marca <-escolha %>%
            filter(Vaccine == "MOD")
        }else if(tipo=='az'){
          marca <-escolha %>%
            filter(Vaccine == "AZ")
        }else{
          marca <-escolha %>%
            filter(Vaccine == "COM")
        }
      }else if(tab == "Algarve"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR02")
        tipo <- input$radiovac2
        if(tipo=="mod"){
          marca <-escolha %>%
            filter(Vaccine == "MOD")
        }else if(tipo=='az'){
          marca <-escolha %>%
            filter(Vaccine == "AZ")
        }else{
          marca <-escolha %>%
            filter(Vaccine == "COM")
        }
      }else if(tab == "Açores"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR03")
        tipo <- input$radiovac3
        if(tipo=="mod"){
          marca <-escolha %>%
            filter(Vaccine == "MOD")
        }else if(tipo=='az'){
          marca <-escolha %>%
            filter(Vaccine == "AZ")
        }else{
          marca <-escolha %>%
            filter(Vaccine == "COM")
        }
      }else if(tab == "Centro"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR04")
        tipo <- input$radiovac4
        if(tipo=="mod"){
          marca <-escolha %>%
            filter(Vaccine == "MOD")
        }else if(tipo=='az'){
          marca <-escolha %>%
            filter(Vaccine == "AZ")
        }else{
          marca <-escolha %>%
            filter(Vaccine == "COM")
        }
      }else if(tab == "Lisboa"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR05")
        tipo <- input$radiovac5
        
        if(tipo=="mod"){
          marca <-escolha %>%
            filter(Vaccine == "MOD")
        }else if(tipo=='az'){
          marca <-escolha %>%
            filter(Vaccine == "AZ")
        }else{
          marca <-escolha %>%
            filter(Vaccine == "COM")
        }
        
      }else if(tab == "Madeira"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR06")
        tipo <- input$radiovac6
        if(tipo=="mod"){
          marca <-escolha %>%
            filter(Vaccine == "MOD")
        }else if(tipo=='az'){
          marca <-escolha %>%
            filter(Vaccine == "AZ")
        }else{
          marca <-escolha %>%
            filter(Vaccine == "COM")
        }
      }else if(tab == "Norte"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR07")
        tipo <- input$radiovac7
        if(tipo=="mod"){
          marca <-escolha %>%
            filter(Vaccine == "MOD")
        }else if(tipo=='az'){
          marca <-escolha %>%
            filter(Vaccine == "AZ")
        }else{
          marca <-escolha %>%
            filter(Vaccine == "COM")
        }
      }else{
        escolha <- dadosapenasportugal %>%
          filter(Region == "PT")
      }
      
      
      #Pfizer <- escolha %>%
      #  filter(Vaccine == "COM")
      #AZ <- escolha %>%
      #  filter(Vaccine == "AZ")
      #MOD <- escolha %>%
     #   filter(Vaccine == "MOD")
      #View(Pfizer)
      #View(AZ)
      #View(MOD)
     

      Data = unlist(marca["YearWeekISO"])
      Doses=unlist(marca["FirstDose"]+marca["SecondDose"])
      
     
      df2 <- data.frame(
        Data,
        Doses
      )
      
    })
    
    ##################################################################################
                        #        Algarve          #
    ##################################################################################
    
    
    output$myPlotB <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico=="Vacinação semanal")
        (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') +labs(title = "Para a Pfizer apenas. Fazer para as outras!!")
        )
      else if(input$grafico=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
          + geom_line()  
          + geom_point() 
          + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20))  
          + xlab('Week') 
          + ylab('Doses') +labs(title = "Para a Pfizer apenas. Fazer para as outras!!")
        )
      
    })
    
    ##################################################################################
    #        Alentejo          #
    ##################################################################################
    
    output$myPlot2 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico1=="Vacinação semanal")
        (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses')
        )
      else if(input$grafico1=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
    
    output$myPlot3 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico2=="Vacinação semanal")
        (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico2=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
    
    output$myPlot4 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico3=="Vacinação semanal")
        (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico3=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
    
    output$myPlot5 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico4=="Vacinação semanal")
        (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico4=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
    
    output$myPlot6 <- renderPlot({
      
      res <- myReactiveDat()
      if(input$grafico5=="Vacinação semanal")
        (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico5=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
    
    output$myPlot7 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico6=="Vacinação semanal")
        (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico6=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
    
  }
)
