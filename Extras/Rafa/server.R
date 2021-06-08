library(dplyr)
library(ggplot2)
library(tidyr)

last_update <- Sys.Date()


shinyServer(
  function(input,output,session){
    
    
    loadedData <- reactiveVal()
    loadedData2 <- reactiveVal()
    
    
    # Se o last update ocorreu há mais de 7 dias:
    if(last_update + 7 >= Sys.Date()) {
      last_update <- Sys.Date()
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      write.csv(data, "Data/data.csv")
    }
    
    observe({
      loadedData(read.csv("Data/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")) 
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
        escolha <- loadedData2() %>%
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
        escolha <- loadedData2() %>%
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
        escolha <- loadedData2() %>%
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
        escolha <- loadedData2() %>%
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
        escolha <- loadedData2() %>%
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
        escolha <- loadedData2() %>%
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
        escolha <- loadedData2() %>%
          filter(Region == "PT")
      }
      
      

      
      
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
    
  }
)