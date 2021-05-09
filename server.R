library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape)
library(gganimate)

shinyServer(
  function(input,output,session){
    
    loadedData <- reactiveVal()
    loadedData2 <- reactiveVal()
    
    observe({
      loadedData(read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")) 
      dadosapenasportugal <- loadedData() %>%
        filter(ReportingCountry == "PT")
      loadedData2(dadosapenasportugal)
    })
    
    
    observeEvent(input$ajuda, {
      showModal(modalDialog(
        title = "Ajuda",
        "Uma pessoa vacinada é aquela que completou o plano de vacinação",
        easyClose = TRUE
      ))
    })
    
    Vacinas <- reactive({
      
      #Dados para portugal apenas:
      
      dadosapenasportugal <- loadedData2() %>%
        filter(Region == "PT")
      
      Todos <- dadosapenasportugal <- dadosapenasportugal %>%
        filter(TargetGroup == "ALL")
      
      Pfizer <- dadosapenasportugal %>%
        filter(Vaccine == "COM")
      AZ <- dadosapenasportugal %>%
        filter(Vaccine == "AZ")
      MOD <- dadosapenasportugal %>%
        filter(Vaccine == "MOD")
      JJ <- dadosapenasportugal %>%
        filter(Vaccine == "JANSS")
      
      #Totais:
      
      totpfizer = sum(Pfizer["FirstDose"])+sum(Pfizer["SecondDose"])
      totAZ = sum(AZ["FirstDose"])+sum(AZ["SecondDose"])
      totMOD = sum(MOD["FirstDose"])+sum(MOD["SecondDose"])
      totJJ = sum(JJ["FirstDose"])
      
      Totais = c(totpfizer,totAZ,totMOD, totJJ)
      Nomes = c("Pfizer","AstraZeneca","Moderna", "Jonhson & Johnson")
      
      
      df <- data.frame(
        Totais,
        Nomes
      )
    })
    myReactiveDataPais3 <- reactive({
      
      #Dados para portugal apenas:
      
      dadosapenasportugal <- loadedData2() %>%
        filter(Region == "PT")
      
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
      
      #Dados para portugal apenas:
      
      dadosapenasportugal <- loadedData2() %>%
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
      
      TotalVacinados = unlist(c(sum(tpv_MOD["SecondDose"])) + c(sum(tpv_COM["SecondDose"])) + c(sum(tpv_AZ["SecondDose"])))
      Percentagem_tpv = (TotalVacinados/totalPopulacao)*100
      Percentagem_tpNv = ((totalPopulacao-TotalVacinados)/totalPopulacao)*100
      
      Nomenclatura = c("Vacinados","Não Vacinados")
      Percentagens = c(Percentagem_tpv, Percentagem_tpNv)
      
      dfPercentagens <-data.frame(
        Nomenclatura,
        Percentagens
      )
      
      MOD_18_24Vacinados<- Age18_24 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_18_24Vacinados<- Age18_24 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_18_24Vacinados<- Age18_24 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_18_24Vacinados<- Age18_24 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_80Vacinados<- Age80 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_80Vacinados<- Age80 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_80Vacinados<- Age80 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_80Vacinados<- Age80 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      
      
      #histogram de doses por região:
      Idades = unlist( rep(c("18-24","25-49","50-59","60-69","70-79", "80+"), each=4))
      Vac = unlist(rep(c("Pfizer","Moderna","AstraZeneca", "Jonhson & Jonhson"),6))
      Doses=unlist(c(sum(COM_18_24Vacinados['SecondDose']),
                     sum(MOD_18_24Vacinados['SecondDose']),
                     sum(AZ_18_24Vacinados['SecondDose']),
                     #sum(JJ_18_24Vacinados['FirstDose']),
                     0,
                     sum(COM_25_49Vacinados['SecondDose']),
                     sum(MOD_25_49Vacinados['SecondDose']),
                     sum(AZ_25_49Vacinados['SecondDose']),
                     sum(JJ_25_49Vacinados['FirstDose']),
                     sum(COM_50_59Vacinados['SecondDose']),
                     sum(MOD_50_59Vacinados['SecondDose']),
                     sum(AZ_50_59Vacinados['SecondDose']),
                     sum(JJ_50_59Vacinados['FirstDose']),
                     sum(COM_60_69Vacinados['SecondDose']),
                     sum(MOD_60_69Vacinados['SecondDose']),
                     sum(AZ_60_69Vacinados['SecondDose']),
                     sum(JJ_60_69Vacinados['FirstDose']),
                     sum(COM_70_79Vacinados['SecondDose']),
                     sum(MOD_70_79Vacinados['SecondDose']),
                     sum(AZ_70_79Vacinados['SecondDose']),
                     sum(JJ_70_79Vacinados['FirstDose']),
                     sum(COM_80Vacinados['SecondDose']),
                     sum(MOD_80Vacinados['SecondDose']),
                     sum(AZ_80Vacinados['SecondDose']),
                     sum(JJ_80Vacinados['FirstDose'])
      ))
      
      dfpais <- data.frame(
        Idades,
        Vac,
        Doses
      )
      
      
    })
    c5 = c("18-24","25-49","50-59","60-69","70-79", "80+")
    output$myPlotPais <- renderPlot({
      
      res <- myReactiveDataPais()
      ggplot(data = res$dfpais, aes(x= res$Vac, y=as.factor(res$Doses), fill = res$Idades)) + 
        geom_bar(stat='identity', position = position_dodge())+
        geom_text(aes(label = as.factor(res$Doses)), vjust = -0.2,
                  position = position_dodge(0.9), size = 3) +
        theme(
          axis.text.x = element_text(vjust = 1),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)
        ) + 
        guides(fill= guide_legend(title = "")) +
        xlab('Vacinas') + 
        ylab('') 
      
    })
    
    
    
    
    c6 = c("AZ","MOD","COM","JJ")
    output$myPlotPais2 <- renderPlot({
      
      res <- Vacinas()
      ggplot(res$df, aes(x=res$Nomes, y= as.factor(res$Totais), fill = c6)) + 
        geom_bar(stat='identity')+ 
        scale_fill_manual(values = c("AZ"="#D8BFD8",
                                     "MOD"="#B0E0E6",
                                     "COM"="#FFDAB9",
                                     "JJ"="#c6ffb3")) +
        geom_text(aes(label=as.factor(res$Totais)), vjust = -0.6, size = 4)+
        theme(
          legend.position = "none",
          axis.text.x = element_text(vjust = 0.5, size = 10),
          axis.title = element_text(face="bold", size=18),
          title = element_text(vjust = 1, size = 20)
        ) + 
        xlab('Vacinas') +
        ylab('Doses') 
    })
    
    output$myPie <- renderPlot({
      
      resPie <- myReactiveDataPais3()
      
      ggplot(resPie$dfPercentagens, aes(x="", y=resPie$Percentagens, fill=resPie$Nomenclatura)) +
        geom_bar( stat="identity", width=1, color="white") +
        coord_polar("y", start=0)+
        theme_void()+
        geom_text(aes(y=resPie$Percentagens, label = paste(round(resPie$Percentagens, digits=2),"%")), size = 7, color=c("black","white")) +
        guides(fill= guide_legend(title = ""))
    })
    
    myReactiveDat <- reactive({
      
      tab <- input$tabs
      fetaria <- input$fetaria
      
      #Dados para portugal apenas:
      
      tipo <- input$radiovac
      if(tab=="Alentejo"){
        escolha <- loadedData2() %>%
          filter(Region == "PTCSR01")
      }else if(tab == "Algarve"){
        escolha <- loadedData2() %>%
          filter(Region == "PTCSR02")
      }else if(tab == "Açores"){
        escolha <- loadedData2() %>%
          filter(Region == "PTCSR03")
      }else if(tab == "Centro"){
        escolha <- loadedData2() %>%
          filter(Region == "PTCSR04")
      }else if(tab == "Lisboa"){
        escolha <- loadedData2() %>%
          filter(Region == "PTCSR05")
      }else if(tab == "Madeira"){
        escolha <- loadedData2() %>%
          filter(Region == "PTCSR06")
      }else if(tab == "Norte"){
        escolha <- loadedData2() %>%
          filter(Region == "PTCSR07")
      }else{
        escolha <- loadedData2() %>%
          filter(Region == "PT")
      }
      
      datas = unique(escolha["YearWeekISO"])
      
      escolha$FirstDoseRefused <- NULL
      escolha$UnknownDose <- NULL
      escolha$NumberDosesReceived <- NULL
      escolha$Population <- NULL
      escolha$ReportingCountry <- NULL
      escolha$TargetGroup <- NULL
      escolha$Denominator <- NULL
      escolha$Region <- NULL
      aux=escolha[1,2]+escolha[1,3]
      
      dosestotais = c()
      datas = c()
      datas = c(datas,escolha[1,1])
      for(i in 2:nrow(escolha)) {
        # for-loop over rows
        if(escolha[i,1]==escolha[i-1,1]){
          aux = aux + escolha[i,2]+escolha[i,3]
          
          if(i==nrow(escolha)){
            dosestotais <- c(dosestotais, aux)
          }
        }else{
          datas = c(datas,escolha[i,1])
          dosestotais <- c(dosestotais, aux)
          aux = escolha[i,2]+escolha[i,3]
          if(i==nrow(escolha)){
            dosestotais <- c(dosestotais, aux)
          }
        }
        
      }
      
      Data = datas
      Doses=dosestotais
      
      
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
           ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
        )
      else if(input$grafico=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
          + geom_line()  
          + geom_point() 
          + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20))  
          + xlab('Week') 
          + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
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
           ylab('Doses')+labs(title = "Número de vacinas administradas por semana")
        )
      else if(input$grafico1=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
          + geom_line()  
          + geom_point() 
          + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20))  
          + xlab('Week') 
          + ylab('Doses')+labs(title = "Acumulação de vacinas semana após semana")
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
           ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
        )
      else if(input$grafico2=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
          + geom_line()  
          + geom_point() 
          + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20))  
          + xlab('Week') 
          + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
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
           ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
        )
      else if(input$grafico3=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
          + geom_line()  
          + geom_point() 
          + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20))  
          + xlab('Week') 
          + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
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
           ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
        )
      else if(input$grafico4=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
          + geom_line()  
          + geom_point() 
          + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20))  
          + xlab('Week') 
          + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
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
           ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
        )
      else if(input$grafico5=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
          + geom_line()  
          + geom_point() 
          + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20))  
          + xlab('Week') 
          + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
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
           ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
        )
      else if(input$grafico6=="Acumulação de vacinas")
        ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
          + geom_line()  
          + geom_point() 
          + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                  axis.title = element_text(face="bold", size=18),
                  title = element_text(size = 20))  
          + xlab('Week') 
          + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
        )
      ##########################################################################
      
    })
    
    
    output$myEvolucao <- renderPlot({
      
      Alentejo <- loadedData2() %>%
        filter(Region == "PTCSR01")
      
      Alentejo$FirstDoseRefused <- NULL
      Alentejo$UnknownDose <- NULL
      Alentejo$NumberDosesReceived <- NULL
      Alentejo$Population <- NULL
      Alentejo$ReportingCountry <- NULL
      Alentejo$TargetGroup <- NULL
      Alentejo$Denominator <- NULL
      Alentejo$Region <- NULL
      
      datas = unique(Alentejo["YearWeekISO"])
      
      aux=Alentejo[1,2]+Alentejo[1,3]
      
      dosestotaisAlentejo = c()
      
      for(i in 2:nrow(Alentejo)) {
        # for-loop over rows
        if(Alentejo[i,1]==Alentejo[i-1,1]){
          aux = aux + Alentejo[i,2]+Alentejo[i,3]
          
          if(i==nrow(Alentejo)){
            dosestotaisAlentejo <- c(dosestotaisAlentejo, aux)
          }
        }else{
          dosestotaisAlentejo <- c(dosestotaisAlentejo, aux)
          aux = Alentejo[i,2]+Alentejo[i,3]
          if(i==nrow(Alentejo)){
            dosestotaisAlentejo <- c(dosestotaisAlentejo, aux)
          }
        }
        
      }
      
      dfAlentejo <- data.frame(
        datas,
        dosestotaisAlentejo
      )
      
      #######################################
      Algarve <- loadedData2() %>%
        filter(Region == "PTCSR02")
      
      Algarve$FirstDoseRefused <- NULL
      Algarve$UnknownDose <- NULL
      Algarve$NumberDosesReceived <- NULL
      Algarve$Population <- NULL
      Algarve$ReportingCountry <- NULL
      Algarve$TargetGroup <- NULL
      Algarve$Denominator <- NULL
      Algarve$Region <- NULL
      
      datas = unique(Algarve["YearWeekISO"])
      
      aux=Algarve[1,2]+Algarve[1,3]
      
      dosestotaisAlgarve = c()
      
      for(i in 2:nrow(Algarve)) {
        # for-loop over rows
        if(Algarve[i,1]==Algarve[i-1,1]){
          aux = aux + Algarve[i,2]+Algarve[i,3]
          
          if(i==nrow(Algarve)){
            dosestotaisAlgarve <- c(dosestotaisAlgarve, aux)
          }
        }else{
          dosestotaisAlgarve <- c(dosestotaisAlgarve, aux)
          aux = Algarve[i,2]+Algarve[i,3]
          if(i==nrow(Algarve)){
            dosestotaisAlgarve <- c(dosestotaisAlgarve, aux)
          }
        }
        
      }
      
      dfAlgarve <- data.frame(
        datas,
        dosestotaisAlgarve
      )
      
      #######################################
      Açores <- loadedData2() %>%
        filter(Region == "PTCSR03")
      
      Açores$FirstDoseRefused <- NULL
      Açores$UnknownDose <- NULL
      Açores$NumberDosesReceived <- NULL
      Açores$Population <- NULL
      Açores$ReportingCountry <- NULL
      Açores$TargetGroup <- NULL
      Açores$Denominator <- NULL
      Açores$Region <- NULL
      
      datasAçores = unique(Açores["YearWeekISO"])
      
      aux=Açores[1,2]+Açores[1,3]
      
      dosestotaisAçores = c()
      
      for(i in 2:nrow(Açores)) {
        # for-loop over rows
        if(Açores[i,1]==Açores[i-1,1]){
          aux = aux + Açores[i,2]+Açores[i,3]
          
          if(i==nrow(Açores)){
            dosestotaisAçores <- c(dosestotaisAçores, aux)
          }
        }else{
          dosestotaisAçores <- c(dosestotaisAçores, aux)
          aux = Açores[i,2]+Açores[i,3]
          if(i==nrow(Açores)){
            dosestotaisAçores <- c(dosestotaisAçores, aux)
          }
        }
        
      }
      
      dfAçores <- data.frame(
        datasAçores,
        dosestotaisAçores
      )
      
      #######################################
      Centro <- loadedData2() %>%
        filter(Region == "PTCSR04")
      
      Centro$FirstDoseRefused <- NULL
      Centro$UnknownDose <- NULL
      Centro$NumberDosesReceived <- NULL
      Centro$Population <- NULL
      Centro$ReportingCountry <- NULL
      Centro$TargetGroup <- NULL
      Centro$Denominator <- NULL
      Centro$Region <- NULL
      
      datasCentro = unique(Centro["YearWeekISO"])
      
      aux=Centro[1,2]+Centro[1,3]
      
      dosestotaisCentro = c()
      
      for(i in 2:nrow(Centro)) {
        # for-loop over rows
        if(Centro[i,1]==Centro[i-1,1]){
          aux = aux + Centro[i,2]+Centro[i,3]
          
          if(i==nrow(Centro)){
            dosestotaisCentro <- c(dosestotaisCentro, aux)
          }
        }else{
          dosestotaisCentro <- c(dosestotaisCentro, aux)
          aux = Centro[i,2]+Centro[i,3]
          if(i==nrow(Centro)){
            dosestotaisCentro <- c(dosestotaisCentro, aux)
          }
        }
        
      }
      
      dfCentro <- data.frame(
        datasCentro,
        dosestotaisCentro
      )
      
      #######################################
      Lisboa <- loadedData2() %>%
        filter(Region == "PTCSR05")
      
      Lisboa$FirstDoseRefused <- NULL
      Lisboa$UnknownDose <- NULL
      Lisboa$NumberDosesReceived <- NULL
      Lisboa$Population <- NULL
      Lisboa$ReportingCountry <- NULL
      Lisboa$TargetGroup <- NULL
      Lisboa$Denominator <- NULL
      Lisboa$Region <- NULL
      
      datasLisboa = unique(Lisboa["YearWeekISO"])
      
      aux=Lisboa[1,2]+Lisboa[1,3]
      
      dosestotaisLisboa = c()
      
      for(i in 2:nrow(Lisboa)) {
        # for-loop over rows
        if(Lisboa[i,1]==Lisboa[i-1,1]){
          aux = aux + Lisboa[i,2]+Lisboa[i,3]
          
          if(i==nrow(Lisboa)){
            dosestotaisLisboa <- c(dosestotaisLisboa, aux)
          }
        }else{
          dosestotaisLisboa <- c(dosestotaisLisboa, aux)
          aux = Lisboa[i,2]+Lisboa[i,3]
          if(i==nrow(Lisboa)){
            dosestotaisLisboa <- c(dosestotaisLisboa, aux)
          }
        }
        
      }
      
      dfLisboa <- data.frame(
        datasLisboa,
        dosestotaisLisboa
      )
      
      #######################################
      Madeira <- loadedData2() %>%
        filter(Region == "PTCSR06")
      
      Madeira$FirstDoseRefused <- NULL
      Madeira$UnknownDose <- NULL
      Madeira$NumberDosesReceived <- NULL
      Madeira$Population <- NULL
      Madeira$ReportingCountry <- NULL
      Madeira$TargetGroup <- NULL
      Madeira$Denominator <- NULL
      Madeira$Region <- NULL
      
      datasMadeira = unique(Madeira["YearWeekISO"])
      
      aux=Madeira[1,2]+Madeira[1,3]
      
      dosestotaisMadeira = c()
      
      for(i in 2:nrow(Madeira)) {
        # for-loop over rows
        if(Madeira[i,1]==Madeira[i-1,1]){
          aux = aux + Madeira[i,2]+Madeira[i,3]
          
          if(i==nrow(Madeira)){
            dosestotaisMadeira <- c(dosestotaisMadeira, aux)
          }
        }else{
          dosestotaisMadeira <- c(dosestotaisMadeira, aux)
          aux = Madeira[i,2]+Madeira[i,3]
          if(i==nrow(Madeira)){
            dosestotaisMadeira <- c(dosestotaisMadeira, aux)
          }
        }
        
      }
      
      dfMadeira <- data.frame(
        datasMadeira,
        dosestotaisMadeira
      )
      
      #######################################
      Norte <- loadedData2() %>%
        filter(Region == "PTCSR07")
      
      Norte$FirstDoseRefused <- NULL
      Norte$UnknownDose <- NULL
      Norte$NumberDosesReceived <- NULL
      Norte$Population <- NULL
      Norte$ReportingCountry <- NULL
      Norte$TargetGroup <- NULL
      Norte$Denominator <- NULL
      Norte$Region <- NULL
      
      datasNorte = unique(Norte["YearWeekISO"])
      
      aux=Norte[1,2]+Norte[1,3]
      
      dosestotaisNorte = c()
      
      for(i in 2:nrow(Norte)) {
        # for-loop over rows
        if(Norte[i,1]==Norte[i-1,1]){
          aux = aux + Norte[i,2]+Norte[i,3]
          
          if(i==nrow(Norte)){
            dosestotaisNorte <- c(dosestotaisNorte, aux)
          }
        }else{
          dosestotaisNorte <- c(dosestotaisNorte, aux)
          aux = Norte[i,2]+Norte[i,3]
          if(i==nrow(Norte)){
            dosestotaisNorte <- c(dosestotaisNorte, aux)
          }
        }
        
      }
      
      dfNorte <- data.frame(
        datasNorte,
        dosestotaisNorte
      )
      
      
      #######################################
      
      
      df <- merge(dfAlentejo,dfAlgarve,by="YearWeekISO",all=TRUE)
      df <- merge(df,dfAçores,by="YearWeekISO",all=TRUE)
      df <- merge(df,dfNorte,by="YearWeekISO",all=TRUE)
      df <- merge(df,dfCentro,by="YearWeekISO",all=TRUE)
      df <- merge(df,dfLisboa,by="YearWeekISO",all=TRUE)
      df <- merge(df,dfMadeira,by="YearWeekISO",all=TRUE)
      
      mydate <- as.Date("2020-12-24")
      
      for(i in 1:nrow(df)) {
        # for-loop over rows
        df[i,1]=i
      }
      
      df      <- as.data.frame(t(df))
      df$regiao <- rownames(df)
      df      <- melt(df, id.vars=c("regiao"))
      df$variable <- as.character(df$variable)
      
      df$variable[df$variable=="V1"] <-"1"
      df$variable[df$variable=="V2"] <-"2"
      df$variable[df$variable=="V3"] <-"3"
      df$variable[df$variable=="V4"] <-"4"
      df$variable[df$variable=="V5"] <-"5"
      df$variable[df$variable=="V6"] <-"6"
      df$variable[df$variable=="V7"] <-"7"
      df$variable[df$variable=="V8"] <-"8"
      df$variable[df$variable=="V9"] <-"9"
      df$variable[df$variable=="V10"] <-"10"
      df$variable[df$variable=="V11"] <-"11"
      df$variable[df$variable=="V12"] <-"12"
      df$variable[df$variable=="V13"] <-"13"
      df$variable[df$variable=="V14"] <-"14"
      df$variable[df$variable=="V15"] <-"15"
      df$variable[df$variable=="V16"] <-"16"
      df$variable[df$variable=="V17"] <-"17"
      df$variable[df$variable=="V18"] <-"18"
      df$variable[df$variable=="V19"] <-"19"
      df$variable[df$variable=="V20"] <-"20"
      df$variable <- as.numeric(df$variable)
      
      
      ggplot(df, 
             aes(x = regiao,
                 y = value,
                 label = '')) +
        geom_point(stat = "identity", 
                   size = 3) +
        
        geom_text(color = 'black',
                  size = 3,
                  nudge_x = 0.7) +
        coord_flip() +
        
        theme(legend.position = "none",
              axis.title = element_text(size = 20,
                                        face = 'bold'),
              plot.subtitle = element_text(size = 20,
                                           face = "bold",
                                           color = 'red'),
              axis.text = element_text(size = 5,
                                       face = "bold"),
              axis.text.x = element_text(angle = 90,size = 12,
                                         face = "bold")) +
        labs(x = 'Regiões',
             y = 'Doses administradas',
             title ='Evolução das doses administradas'
        )
      #+
      # transition_time(variable) +
      #ease_aes('cubic-in-out')
      
      
    })
    
    ############################## QQ merda de mal foi o Rafa
    
    myReactiveData <- reactive({
      
      #Dados para portugal apenas:
      dadosapenasportugal <- loadedData() %>%
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
      JJ_18_24Vacinados<- Age18_24 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_25_49Vacinados<- Age25_49 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_50_59Vacinados<- Age50_59 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_60_69Vacinados<- Age60_69 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_70_79Vacinados<- Age70_79 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      MOD_80Vacinados<- Age80 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_80Vacinados<- Age80 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_80Vacinados<- Age80 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      JJ_80Vacinados<- Age80 %>% filter(Vaccine == "JANSS") %>% select(YearWeekISO, FirstDose)
      
      
      
      #Juntar os datasets e retirrar os NA's
      joinedDS_18_24 <- left_join(COM_18_24Vacinados, left_join(MOD_18_24Vacinados, left_join(AZ_18_24Vacinados, JJ_18_24Vacinados, by="YearWeekISO"), by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0, FirstDose=0))
      
      joinedDS_25_49 <- left_join(COM_25_49Vacinados, left_join(MOD_25_49Vacinados, left_join(AZ_25_49Vacinados, JJ_25_49Vacinados, by="YearWeekISO"), by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0, FirstDose=0))
      
      joinedDS_50_59 <- left_join(COM_50_59Vacinados, left_join(MOD_50_59Vacinados, left_join(AZ_50_59Vacinados, JJ_50_59Vacinados, by="YearWeekISO"), by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0, FirstDose=0))
      
      joinedDS_60_69 <- left_join(COM_60_69Vacinados, left_join(MOD_60_69Vacinados, left_join(AZ_60_69Vacinados, JJ_60_69Vacinados, by="YearWeekISO"), by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0, FirstDose=0))
      
      joinedDS_70_79 <- left_join(COM_70_79Vacinados, left_join(MOD_70_79Vacinados, left_join(AZ_70_79Vacinados, JJ_70_79Vacinados, by="YearWeekISO"), by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0, FirstDose=0))
      
      joinedDS_80 <- left_join(COM_80Vacinados, left_join(MOD_80Vacinados, left_join(AZ_80Vacinados, JJ_80Vacinados, by="YearWeekISO"), by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0, FirstDose=0))
      
      
      
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
      p <- ggplot(data = res$TV, aes(x = as.numeric(1:length(res$Datas)))) +
        
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
              axis.title = element_text(face="bold", size=18),
              title = element_text(size = 20),
              legend.title = element_blank()) + 
        labs(title = "", 
             x = "Semanas", 
             y = "Pessoas Vacinadas") +
        scale_x_continuous(breaks = c(as.numeric(1:length(res$Datas))) ,labels = res$Datas) 
      
      switch (input$radioOption,
              "yup" = {
                if(input$option18_24)
                  p <- p + geom_point(aes(y = res$Vacinados18_24, colour="18-24", group = 1), size=3) +
                    geom_line(aes(y = res$Vacinados18_24, colour="18-24", group = 1)) +
                    geom_label(aes(y = res$Vacinados18_24, label= res$Vacinados18_24))
                
                if(input$option25_49)
                  p <- p + geom_point(aes(y = res$Vacinados25_49, colour="25-49", group =2), size=3) +
                    geom_line(aes(y = res$Vacinados25_49, colour="25-49", group = 2))+
                    geom_label(aes(y = res$Vacinados25_49, label= res$Vacinados25_49))
                
                if(input$option50_59)
                  p <- p + geom_point(aes(y = res$Vacinados50_59, colour="50-59", group =3), size=3) +
                    geom_line(aes(y = res$Vacinados50_59, colour="50-59", group = 3))+
                    geom_label(aes(y = res$Vacinados50_59, label= res$Vacinados50_59))
                
                if(input$option60_69)
                  p <- p + geom_point(aes(y = res$Vacinados60_69, colour="60-69", group =4), size=3) +
                    geom_line(aes(y = res$Vacinados60_69, colour="60-69", group = 4)) +
                    geom_label(aes(y = res$Vacinados60_69, label= res$Vacinados60_69))
                
                if(input$option70_79)
                  p <- p + geom_point(aes(y = res$Vacinados70_79, colour="70-79", group =5), size=3) +
                    geom_line(aes(y = res$Vacinados70_79, colour="70-79", group = 5)) +
                    geom_label(aes(y = res$Vacinados70_79, label= res$Vacinados70_79))
                
                if (input$option80)  
                  p <- p + geom_point(aes(y = res$Vacinados80, colour="80+", group =6), size=3) +
                    geom_line(aes(y = res$Vacinados80, colour="80+", group = 6)) +
                    geom_label(aes(y = res$Vacinados80, label= res$Vacinados80))
              },
              "nop" = {
                if(input$option18_24)
                  p <- p + geom_point(aes(y = res$Vacinados18_24, colour="18-24", group = 1), size=3) +
                    geom_line(aes(y = res$Vacinados18_24, colour="18-24", group = 1))
                
                if(input$option25_49)
                  p <- p + geom_point(aes(y = res$Vacinados25_49, colour="25-49", group =2), size=3) +
                    geom_line(aes(y = res$Vacinados25_49, colour="25-49", group = 2))
                
                if(input$option50_59)
                  p <- p + geom_point(aes(y = res$Vacinados50_59, colour="50-59", group =3), size=3) +
                    geom_line(aes(y = res$Vacinados50_59, colour="50-59", group = 3))
                
                if(input$option60_69)
                  p <- p + geom_point(aes(y = res$Vacinados60_69, colour="60-69", group =4), size=3) +
                    geom_line(aes(y = res$Vacinados60_69, colour="60-69", group = 4)) 
                
                if(input$option70_79)
                  p <- p + geom_point(aes(y = res$Vacinados70_79, colour="70-79", group =5), size=3) +
                    geom_line(aes(y = res$Vacinados70_79, colour="70-79", group = 5)) 
                
                if (input$option80)  
                  p <- p + geom_point(aes(y = res$Vacinados80, colour="80+", group =6), size=3) +
                    geom_line(aes(y = res$Vacinados80, colour="80+", group = 6))
              }
      )
      p
    })
    myReactiveData2 <- reactive({
      
      data1 <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      dadosapenasportugal1 <- data1 %>%
        filter(ReportingCountry == "PT")
      
      
      Region1 <- dadosapenasportugal1 %>%
        filter(Region == "PTCSR01")
      
      Region2 <- dadosapenasportugal1 %>%
        filter(Region == "PTCSR02")
      
      Region3 <- dadosapenasportugal1 %>%
        filter(Region == "PTCSR03")
      
      Region4 <- dadosapenasportugal1 %>%
        filter(Region == "PTCSR04")
      
      Region5 <- dadosapenasportugal1 %>%
        filter(Region == "PTCSR05")
      
      Region6 <- dadosapenasportugal1 %>%
        filter (Region == "PTCSR06")
      
      Region7 <- dadosapenasportugal1 %>%
        filter(Region == "PTCSR07")
      
      
      
      MOD_AlentejoVacinados<- Region1 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_AlentejoVacinados<- Region1 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_AlentejoVacinados<- Region1 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_AlgarveVacinados<- Region2 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_AlgarveVacinados<- Region2 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_AlgarveVacinados<- Region2 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_AçoresVacinados<- Region3 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_AçoresVacinados<- Region3 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_AçoresVacinados<- Region3 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_CentroVacinados<- Region4 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_CentroVacinados<- Region4 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_CentroVacinados<- Region4 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_LisboaVacinados<- Region5 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_LisboaVacinados<- Region5 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_LisboaVacinados<- Region5 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_MadeiraVacinados<- Region6 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_MadeiraVacinados<- Region6 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_MadeiraVacinados<- Region6 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      MOD_NorteVacinados<- Region7 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
      COM_NorteVacinados<- Region7 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
      AZ_NorteVacinados<- Region7 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
      
      
      
      joinedDS_Alentejo <- left_join(COM_AlentejoVacinados, left_join(MOD_AlentejoVacinados, AZ_AlentejoVacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_Algarve <- left_join(COM_AlgarveVacinados, left_join(MOD_AlgarveVacinados, AZ_AlgarveVacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_Açores <- left_join(COM_AçoresVacinados, left_join(MOD_AçoresVacinados, AZ_AçoresVacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_Centro <- left_join(COM_CentroVacinados, left_join(MOD_CentroVacinados, AZ_CentroVacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_Lisboa <- left_join(COM_LisboaVacinados, left_join(MOD_LisboaVacinados, AZ_LisboaVacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_Madeira <- left_join(COM_MadeiraVacinados, left_join(MOD_MadeiraVacinados, AZ_MadeiraVacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      joinedDS_Norte <- left_join(COM_NorteVacinados, left_join(MOD_NorteVacinados, AZ_NorteVacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
        tidyr::replace_na(list(0,SecondDose=0,SecondDose.x = 0, SecondDose.y = 0))
      
      
      TVpS_Alentejo <-joinedDS_Alentejo %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados) 
      
      TVpS_Algarve <-joinedDS_Algarve %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados) 
      
      TVpS_Açores <-joinedDS_Açores %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados)
      
      aux51 <- data.frame("2020-W52",0)
      names(aux51) <- c("YearWeekISO", "TotalVacinados")
      TVpS_Açores <-rbind(aux51,TVpS_Açores)
      
      aux53 <- data.frame("2021-W18",0)
      names(aux53) <- c("YearWeekISO", "TotalVacinados")
      TVpS_Açores <-rbind(aux53,TVpS_Açores)
      
      TVpS_Centro <-joinedDS_Centro %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados)
      
      TVpS_Lisboa <-joinedDS_Lisboa %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados)
      
      TVpS_Madeira <-joinedDS_Madeira %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados)
      
      aux54<- data.frame("2020-W52",0)
      names(aux54) <- c("YearWeekISO", "TotalVacinados")
      TVpS_Madeira <-rbind(aux54,TVpS_Madeira)
      
      aux55 <- data.frame("2021-W18",0)
      names(aux55) <- c("YearWeekISO", "TotalVacinados")
      TVpS_Madeira <-rbind(aux55,TVpS_Madeira)
      
      TVpS_Norte <-joinedDS_Norte %>% 
        mutate( TotalVacinados = rowSums(.[2:4])) %>% 
        select(YearWeekISO, TotalVacinados) 
      
      
      Datas1 = unlist(TVpS_Norte["YearWeekISO"])
      VacinadosAlentejo = unlist(TVpS_Alentejo["TotalVacinados"])
      VacinadosAlgarve = unlist(TVpS_Algarve["TotalVacinados"])
      VacinadosAçores = unlist(TVpS_Açores["TotalVacinados"])
      VacinadosCentro = unlist(TVpS_Centro["TotalVacinados"])
      VacinadosLisboa = unlist(TVpS_Lisboa["TotalVacinados"])
      VacinadosMadeira = unlist(TVpS_Madeira["TotalVacinados"])
      VacinadosNorte    = unlist(TVpS_Norte["TotalVacinados"])
      
      
      TV1 <- data.frame(
        Datas1,
        VacinadosAlentejo,
        VacinadosAlgarve,
        VacinadosAçores,
        VacinadosCentro,
        VacinadosLisboa,
        VacinadosMadeira,
        VacinadosNorte
      )
      
    })
    output$plotRegiao <- renderPlot({
      
      res1 <- myReactiveData2()
      p1<- ggplot(data = res1$TV1, aes(x = as.numeric(1:length(res1$Datas1)))) +
        
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
              axis.title = element_text(face="bold", size=18),
              title = element_text(size = 20),
              legend.title = element_blank()) + 
        labs(title = "Quantidade de pessoas vacinadas em cada Região, por semana", 
             x = "Semanas", 
             y = "Pessoas Vacinadas") +
        scale_x_continuous(breaks = c(as.numeric(1:length(res1$Datas))) ,labels = res1$Datas1) 
      
      switch (input$radioOption1,
              "yup1" = {
                if(input$optionAlentejo)
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosAlentejo, colour="Alentejo", group = 1), size=3) +
                    geom_line(aes(y = res1$VacinadosAlentejo, colour="Alentejo", group = 1)) +
                    geom_label(aes(y = res1$VacinadosAlentejo, label= res1$VacinadosAlentejo))
                
                if(input$optionAlgarve)
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosAlgarve, colour="Algarve", group =2), size=3) +
                    geom_line(aes(y = res1$VacinadosAlgarve, colour="Algarve", group = 2))+
                    geom_label(aes(y = res1$VacinadosAlgarve, label= res1$VacinadosAlgarve))
                
                if(input$optionAçores)
                  p <- p + geom_point(aes(y = res1$VacinadosAçores, colour="Açores", group =3), size=3) +
                    geom_line(aes(y = res1$Vacinados50_59, colour="Açores", group = 3))+
                    geom_label(aes(y = res1$VacinadosAçores, label= res1$VacinadosAçores))
                
                if(input$optionCentro)
                  p <- p + geom_point(aes(y = res1$VacinadosCentro, colour="Centro", group =4), size=3) +
                    geom_line(aes(y = res1$VacinadosCentro, colour="Centro", group = 4)) +
                    geom_label(aes(y = res1$VacinadosCentro, label= res1$VacinadosCentro))
                
                if(input$optionLisboa)
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosLisboa, colour="Lisboa", group =5), size=3) +
                    geom_line(aes(y = res1$VacinadosLisboa, colour="Lisboa", group = 5)) +
                    geom_label(aes(y = res1$VacinadosLisboa, label= res1$VacinadosLisboa))
                
                if (input$optionMadeira)  
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosMadeira, colour="Madeira", group =6), size=3) +
                    geom_line(aes(y = res1$VacinadosMadeira, colour="Madeira", group = 6)) +
                    geom_label(aes(y = res1$VacinadosMadeira, label= res1$VacinadosMadeira))
                
                if (input$optionNorte)  
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosNorte, colour="Norte", group =6), size=3) +
                    geom_line(aes(y = res1$VacinadosNorte, colour="Norte", group = 6)) +
                    geom_label(aes(y = res1$VacinadosNorte, label= res1$VacinadosNorte))
              },
              "nop1" = {
                if(input$optionAlentejo)
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosAlentejo, colour="Alentejo", group = 1), size=3) +
                    geom_line(aes(y = res1$VacinadosAlentejo, colour="Alentejo", group = 1))
                
                if(input$optionAlgarve)
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosAlgarve, colour="Algarve", group =2), size=3) +
                    geom_line(aes(y = res1$VacinadosAlgarve, colour="Algarve", group = 2))
                
                if(input$optionAçores)
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosAçores, colour="Açores", group =3), size=3) +
                    geom_line(aes(y = res1$VacinadosAçores, colour="Açores", group = 3))
                
                if(input$optionCentro)
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosCentro, colour="Centro", group =4), size=3) +
                    geom_line(aes(y = res1$VacinadosCentro, colour="Centro", group = 4)) 
                
                if(input$optionLisboa)
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosLisboa, colour="Lisboa", group =5), size=3) +
                    geom_line(aes(y = res1$VacinadosLisboa, colour="Lisboa", group = 5)) 
                
                if (input$optionMadeira)  
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosMadeira, colour="Madeira", group =6), size=3) +
                    geom_line(aes(y = res1$VacinadosMadeira, colour="Madeira", group = 6))
                
                if (input$optionNorte)  
                  p1 <- p1 + geom_point(aes(y = res1$VacinadosNorte, colour="Norte", group =6), size=3) +
                    geom_line(aes(y = res1$VacinadosNorte, colour="Norte", group = 6))
                
                
              }
      )
      p1
      
    }  )
    
  }
)
