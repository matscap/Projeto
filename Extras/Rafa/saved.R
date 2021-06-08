library(dplyr)
library(ggplot2)
library(tidyr)
shinyServer(
  function(input,output,session){
    
    c5 = c("18-24","25-49","50-59","60-69","70-79", "80+")
    
    
    loadedData <- reactiveVal()
    
    observe({
      loadedData(read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%
                   filter(ReportingCountry == "PT")
      ) 
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
    myReactiveDataPais3 <- reactive({
      
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      #Dados para portugal apenas:
      dadosapenasportugal <- data %>%
        filter(ReportingCountry == "PT")
      
      dadosapenasportugal <- dadosapenasportugal %>%
        filter(Region == "PT")
      
      
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
      
      Nomenclatura = c("Vacinados","Nao Vacinados")
      Percentagens = c(Percentagem_tpv, Percentagem_tpNv)
      
      dfPercentagens <-data.frame(
        Nomenclatura,
        Percentagens
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
      
      Nomenclatura = c("Vacinados","Nao Vacinados")
      Percentagens = c(Percentagem_tpv, Percentagem_tpNv)
      
      dfPercentagens <-data.frame(
        Nomenclatura,
        Percentagens
      )
      #histogram de doses por região:
      Idades = unlist(c("18-24","25-49","50-59","60-69","70-79", "80+"))
      Doses=unlist(c(sum(Age18_24['FirstDose']),sum(Age25_49['FirstDose']),sum(Age50_59['FirstDose']),sum(Age60_69['FirstDose']),sum(Age70_79['FirstDose']),sum(Age80['FirstDose'])))
      
      dfpais <- data.frame(
        Idades,
        Doses
      )
      
      
    })
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
        labs(title = 'Total de pessoas vacinadas por faixa etária') +
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
      
      resPie <- myReactiveDataPais3()
      
      ggplot(resPie$dfPercentagens, aes(x="", y=resPie$Percentagens, fill=resPie$Nomenclatura)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0)+
        theme_void()+
        labs(title = "Percentagem pessoas vacinadas/não vacinadas ",
             subtitle = "Falta por percentagens nesta cena")
      
    })
    
    myReactiveDat <- reactive({
      
      tab <- input$tabs
      fetaria <- input$fetaria
      
      #Dados para portugal apenas:
      dadosapenasportugal <- loadedData() %>%
        filter(ReportingCountry == "PT")
      
      switch (tab,
              "Alentejo" = {escolha <- dadosapenasportugal %>%
                filter(Region == "PTCSR01")
              
              tipo <- input$radiovac1
              
              switch (tipo,
                      "mod" = marca <-escolha %>% filter(Vaccine == "MOD"),
                      "az" = marca <-escolha %>% filter(Vaccine == "AZ"),
                      "pf" = marca <-escolha %>% filter(Vaccine == "COM")
              )
              
              },
              "Algarve" = { escolha <- dadosapenasportugal %>%
                filter(Region == "PTCSR02")
              
              tipo <- input$radiovac2
              
              switch (tipo,
                      "mod" = marca <-escolha %>% filter(Vaccine == "MOD"),
                      "az" = marca <-escolha %>% filter(Vaccine == "AZ"),
                      "pf" = marca <-escolha %>% filter(Vaccine == "COM")
              )
              
              },
              "Açores" = { escolha <- dadosapenasportugal %>%
                filter(Region == "PTCSR03")
              
              tipo <- input$radiovac3
              
              switch (tipo,
                      "mod" = marca <-escolha %>% filter(Vaccine == "MOD"),
                      "az" = marca <-escolha %>% filter(Vaccine == "AZ"),
                      "pf" = marca <-escolha %>% filter(Vaccine == "COM")
              )
              
              },
              "Centro" = { escolha <- dadosapenasportugal %>%
                filter(Region == "PTCSR04")
              
              tipo <- input$radiovac4
              
              switch (tipo,
                      "mod" = marca <-escolha %>% filter(Vaccine == "MOD"),
                      "az" = marca <-escolha %>% filter(Vaccine == "AZ"),
                      "pf" = marca <-escolha %>% filter(Vaccine == "COM")
              )
              
              },
              "Lisboa" = { escolha <- dadosapenasportugal %>%
                filter(Region == "PTCSR05")
              
              tipo <- input$radiovac5
              
              switch (tipo,
                      "mod" = marca <-escolha %>% filter(Vaccine == "MOD"),
                      "az" = marca <-escolha %>% filter(Vaccine == "AZ"),
                      "pf" = marca <-escolha %>% filter(Vaccine == "COM")
              )
              
              },
              "Madeira" = { escolha <- dadosapenasportugal %>%
                filter(Region == "PTCSR06")
              
              tipo <- input$radiovac6
              
              switch (tipo,
                      "mod" = marca <-escolha %>% filter(Vaccine == "MOD"),
                      "az" = marca <-escolha %>% filter(Vaccine == "AZ"),
                      "pf" = marca <-escolha %>% filter(Vaccine == "COM")
              )
              
              },
              "Norte" = { escolha <- dadosapenasportugal %>%
                filter(Region == "PTCSR07")
              
              tipo <- input$radiovac7
              
              switch (tipo,
                      "mod" = marca <-escolha %>% filter(Vaccine == "MOD"),
                      "az" = marca <-escolha %>% filter(Vaccine == "AZ"),
                      "pf" = marca <-escolha %>% filter(Vaccine == "COM")
              )
              
              },
              escolha <- dadosapenasportugal %>% filter(Region == "PT") #default
      )
      
      
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
    
    
    ############################## QQ merda de mal foi o Rafa
    
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
      
      MOD_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_80Vacinados<- Age80 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_80Vacinados<- Age80 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_80Vacinados<- Age80 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      
      
      #Juntar os datasets e retirrar os NA's
      joinedDS_18_24 <- left_join(COM_18_24Vacinados, left_join(MOD_18_24Vacinados, AZ_18_24Vacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_25_49 <- left_join(COM_25_49Vacinados, left_join(MOD_25_49Vacinados, AZ_25_49Vacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_50_59 <- left_join(COM_50_59Vacinados, left_join(MOD_50_59Vacinados, AZ_50_59Vacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_60_69 <- left_join(COM_60_69Vacinados, left_join(MOD_60_69Vacinados, AZ_60_69Vacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_70_79 <- left_join(COM_70_79Vacinados, left_join(MOD_70_79Vacinados, AZ_70_79Vacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_80 <- left_join(COM_80Vacinados, left_join(MOD_80Vacinados, AZ_80Vacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      
      
      #Somar as colunas, retirara as que não necessitamos e temos o resultado
      TVpS_18_24 <-joinedDS_18_24 %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados) 
      
      TVpS_25_49 <-joinedDS_25_49 %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados) 
      
      TVpS_50_59 <-joinedDS_50_59 %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados)
      
      TVpS_60_69 <-joinedDS_60_69 %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados)
      
      TVpS_70_79 <-joinedDS_70_79 %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados)
      
      TVpS_80 <-joinedDS_80 %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados)
      #Bug no dataset não tem a semana 52
      aux52 <- data.frame("2020-W52",0)
      names(aux52) <- c("YearWeekISO", "TotalVacinados")
      TVpS_80 <-rbind(aux52,TVpS_80)
      
      
      Datas = unlist(TVpS_80["YearWeekISO"])
      Vacinados18_24 = unlist(TVpS_18_24["TotalVacinados"])
      Vacinados25_49 = unlist(TVpS_25_49["TotalVacinados"])
      Vacinados50_59 = unlist(TVpS_50_59["TotalVacinados"])
      Vacinados60_69 = unlist(TVpS_60_69["TotalVacinados"])
      Vacinados70_79 = unlist(TVpS_70_79["TotalVacinados"])
      Vacinados80    = unlist(TVpS_80["TotalVacinados"])
      
      TV <- data.frame(
        Datas,
        Vacinados18_24,
        Vacinados25_49,
        Vacinados50_59,
        Vacinados60_69,
        Vacinados70_79,
        Vacinados80
      )
      
    })
    
    output$plot1824 <- renderPlot({
      
      res <- myReactiveData()
      ggplot(data = res$TV, aes(x = as.numeric(1:length(res$Datas)))) +
        geom_point(aes(y = res$Vacinados18_24, colour="18-24", group = 1), size=3) +
        geom_line(aes(y = res$Vacinados18_24, colour="18-24", group = 1))+
        
        geom_point(aes(y = res$Vacinados25_49, colour="25-49", group =2), size=3) +
        geom_line(aes(y = res$Vacinados25_49, colour="25-49", group = 2))+
        
        geom_point(aes(y = res$Vacinados50_59, colour="50-59", group =3), size=3) +
        geom_line(aes(y = res$Vacinados50_59, colour="50-59", group = 3))+
        
        geom_point(aes(y = res$Vacinados60_69, colour="60-69", group =4), size=3) +
        geom_line(aes(y = res$Vacinados60_69, colour="60-69", group = 4))+
        
        geom_point(aes(y = res$Vacinados70_79, colour="70-79", group =5), size=3) +
        geom_line(aes(y = res$Vacinados70_79, colour="70-79", group = 5))+
        
        geom_point(aes(y = res$Vacinados80, colour="80+", group =6), size=3) +
        geom_line(aes(y = res$Vacinados80, colour="80+", group = 6))+
        
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
              axis.title = element_text(face="bold", size=18),
              title = element_text(size = 20)) + 
        labs(title = "Quantidade de pessoas vacinadas por faixas etárias, por semana", 
             subtitle = "Uma pessoa vacinada é aquela que completou o plano de vacinação.",
             x = "Semanas", 
             y = "Pessoas Vacinadas") +
        scale_x_continuous(breaks = c(as.numeric(1:length(res$Datas))) ,labels = res$Datas)
      
    })
    
  }
)