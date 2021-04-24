library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

shinyServer(
  function(input,output,session){
    
    
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
    

    
    
    output$myPie <- renderPlot({
   
        slices <- c(6,94)
        #slices = c(Percentagem_tpv, Percentagem_tpNv)
        nomenclatura <- c("Vacinados","Nao Vacinados")
        #ggplot() + 
        pie(slices, labels = nomenclatura, main="qq coisa")
    })
    
    
    
    ##############################################################################
    
    myReactiveDat <- reactive({
      
      tab <- input$tabs
      fetaria <- input$fetaria
      
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      #Dados para portugal apenas:
      dadosapenasportugal <- data %>%
        filter(ReportingCountry == "PT")
      
      
      
      if(tab=="Alentejo"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR01")
      }else if(tab == "Algarve"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR02")
      }else if(tab == "Açores"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR03")
      }else if(tab == "Centro"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR04")
      }else if(tab == "Lisboa"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR05")
      }else if(tab == "Madeira"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR06")
      }else if(tab == "Norte"){
        escolha <- dadosapenasportugal %>%
          filter(Region == "PTCSR07")
      }else{
        escolha <- dadosapenasportugal %>%
          filter(Region == "PT")
      }
      
      
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
      
      res <- myReactiveDat()
      
      if(input$grafico1=="Vacinação semanal")
        (ggplot(res$df, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico1=="Acumulação de vacinas")
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
    
    output$myPlot3 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico2=="Vacinação semanal")
        (ggplot(res$df, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico2=="Acumulação de vacinas")
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
    
    output$myPlot4 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico3=="Vacinação semanal")
        (ggplot(res$df, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico3=="Acumulação de vacinas")
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
    
    output$myPlot5 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico4=="Vacinação semanal")
        (ggplot(res$df, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico4=="Acumulação de vacinas")
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
    
    output$myPlot6 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico5=="Vacinação semanal")
        (ggplot(res$df, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico5=="Acumulação de vacinas")
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
    
    output$myPlot7 <- renderPlot({
      
      res <- myReactiveDat()
      
      if(input$grafico6=="Vacinação semanal")
        (ggplot(res$df, aes(x=res$Data, y= res$Doses)) + 
           geom_bar(stat='identity')+ 
           theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                 axis.title = element_text(face="bold", size=18),
                 title = element_text(size = 20)) + 
           xlab('Week') +
           ylab('Doses') 
        )
      else if(input$grafico6=="Acumulação de vacinas")
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
    
    
    
    myReactiveDat2 <- reactive({
      
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
      
    })
    
    
    
     
      #TotalVacinados = unlist(c(sum(tpv_MOD["SecondDose"])) + c(sum(tpv_COM["SecondDose"])) + c(sum(tpv_AZ["SecondDose"])))
      #Percentagem_tpv = (TotalVacinados/totalPopulacao)*100
      #Percentagem_tpNv = ((totalPopulacao-TotalVacinados)/totalPopulacao)*100
        
    
    
    myReactiveData <- reactive({
      
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      #Dados para portugal apenas:
      dadosapenasportugal <- data %>%
        filter(ReportingCountry == "PT")
      
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
      
    #Por IDADES
        
        # Totalmente Vacinadas p/Semana
        ## Dado que há vacinas com doses diferentes para vacinar temos
        
        # Filtrar os datasets. De acordo com a vacina escolher primeira ou segunda dose
        MOD_18_24Vacinados<- Age18_24 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
        COM_18_24Vacinados<- Age18_24 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
        AZ_18_24Vacinados<- Age18_24 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
        
        #Juntar os datasets e retirrar os NA's
        joinedDS_18_24 <- left_join(COM_18_24Vacinados, left_join(MOD_18_24Vacinados, AZ_18_24Vacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
          tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
        
        #Somar as colunas, retirara as que não necessitamos e temos o resultado
        TVpS_18_24 <-joinedDS_18_24 %>% 
          mutate( TotalVacinados = rowSums(.[2:4])) %>% 
            select(YearWeekISO, TotalVacinados) 
        
        Datas = unlist(TVpS_18_24["YearWeekISO"])
        Vacinados = unlist(TVpS_18_24["TotalVacinados"])
          
        TV1824 <- data.frame(
          Datas,
          Vacinados
        )
        
      })

    output$plot1824 <- renderPlotly({
  
      res <- myReactiveData()
      ggplot(res$TV1824, aes(x = res$Datas, y = res$Vacinados), group=res$Vacinados) + 
        geom_line() +
        geom_point(shape=21, color="black", fill="#69b3a2", size=5) 
    
    })
    
  }
)
