library(dplyr)
library(ggplot2)
shinyServer(
  function(input,output,session){
    
    
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
      
      res <- myReactiveDat2()
      ggplot(res$df, aes(x=res$Data, y= res$Doses)) + 
          geom_bar(stat='identity')+ 
          theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                axis.title = element_text(face="bold", size=18),
                title = element_text(size = 20)) + 
          xlab('Week') +
          ylab('Doses') 

    })
    
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
    
  }
)
