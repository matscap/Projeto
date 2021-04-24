library(dplyr)
library(ggplot2)
library(tidyr)
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
    
    
    myReactiveDataPais <- reactive({
      
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      #Dados para portugal apenas:
      dadosapenasportugal <- data %>%
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
      
      
      ### RAFA ###################################################################
      #Ideia para ver o total de gente vacinada (corretamente)
      #Temos de filtrar a df dadosapenasportugal para apenas uma linha de cada week com os dados de ALL
      ## Region == "PT"
      ## TargetGroup == "All"
      totalPopulacao = 10295909
      totalGenteVacinada <- dadosapenasportugal %>%
        filter(TargetGroup == "ALL") %>%
        filter(grepl("PT$",Region))
      
      #tpv -> total de pessoas vacinadas pela vacina _ ###
      tpv_MOD <- totalGenteVacinada %>%
        filter(Vaccine == "MOD")
      tpv_COM <- totalGenteVacinada %>%
        filter(Vaccine == "COM")
      tpv_AZ <- totalGenteVacinada %>%
        filter(Vaccine == "AZ")
      
      TotalVacinados = unlist(c(sum(tpv_MOD["SecondDose"])) + c(sum(tpv_COM["SecondDose"])) + c(sum(tpv_AZ["SecondDose"])))
      Percentagem_tpv = (TotalVacinados/totalPopulacao)*100
      Percentagem_tpNv = ((totalPopulacao-TotalVacinados)/totalPopulacao)*100
      ##########################################################################
      #histogram de doses por região:
      Idades = unlist(c("18-24","25-49","50-59","60-69","70-79", "80+"))
      Doses=unlist(c(sum(Age18_24['FirstDose']),sum(Age25_49['FirstDose']),sum(Age50_59['FirstDose']),sum(Age60_69['FirstDose']),sum(Age70_79['FirstDose']),sum(Age80['FirstDose'])))
      
      dfpais <- data.frame(
        Idades,
        Doses
      )
    })
    c5 = c("18-24","25-49","50-59","60-69","70-79", "80+")
    output$myPlotPais <- renderPlot({
      res <- myReactiveDataPais()
      #vac <- Vacinas()
        ggplot(data = res$dfpais, aes(y=as.factor(res$Idades), x= as.factor(res$Doses), fill = c5)) + 
          geom_bar(stat='identity', orientation = "y")+
          scale_fill_manual(values = c("18-24"="#D8BFD8",
                                       "25-49"="#B0E0E6",
                                       "50-59"="#FFDAB9",
                                       "60-69"="#FA8072",
                                       "70-79"="#98FB98",
                                       "80+"="#FFFACD")) +
          theme(
            legend.position = "none",
            axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20)
          ) + 
          labs(title = 'Por o titulo desta cena') +
          xlab('Nº de pessoas Vacinadas') +
          ylab('') 
      
    })
    c6 = c("AZ","MOD","COM")
    output$myPlotPais2 <- renderPlot({
      
      res <- Vacinas()
      ggplot(res$df, aes(x=res$Nomes, y= res$Totais, fill = c6)) + 
        geom_bar(stat='identity')+ 
        scale_fill_manual(values = c("AZ"="#D8BFD8",
                                     "MOD"="#B0E0E6",
                                     "COM"="#FFDAB9")) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)
        ) + 
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
              axis.title = element_text(face="bold", size=18),
              title = element_text(size = 20)) + 
        xlab('Vacinas') +
        ylab('Doses') 
    })
    
    output$myPie <- renderPlot({
      
      slices <- c(6,94)
      #slices = c(Percentagem_tpv, Percentagem_tpNv)
      nomenclatura <- c("Vacinados","Nao Vacinados")
      #ggplot() + 
      pie(slices, labels = nomenclatura, main="qq coisa")
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
