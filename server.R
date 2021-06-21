library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape)
library(gganimate)
library(waffle)
last_update <- Sys.Date()

# Vamos usar como data inicial, a quinta-feira da semana de chegada do primeiro lote de vacinas a Portugal
date <- as.Date("24/12/2021", format = "%d/%m/%y")


shinyServer(
  function(input,output,session){
    
    loadedData <- reactiveVal()
    loadedData2 <- reactiveVal()
    
    loaded_Alentejo <- reactiveVal()
    loaded_Algarve <- reactiveVal()
    loaded_Acores <- reactiveVal()
    loaded_Centro <- reactiveVal()
    loaded_Lisboa <- reactiveVal()
    loaded_Madeira <- reactiveVal()
    loaded_Norte <- reactiveVal()
    loaded_PT <- reactiveVal()
    
  
    
    observe({
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      
      loadedData(data) 
      dadosapenasportugal <- loadedData() %>%
        filter(ReportingCountry == "PT")
      loadedData2(dadosapenasportugal)

      Alentejo <- loadedData2() %>%
        filter(Region == "PTCSR01")
      loaded_Alentejo(Alentejo)
      
      Algarve <- loadedData2() %>%
        filter(Region == "PTCSR02")
      loaded_Algarve(Algarve)
      
      Açores <- loadedData2() %>%
        filter(Region == "PTCSR03")
      loaded_Acores(Açores)
      
      Centro <- loadedData2() %>%
        filter(Region == "PTCSR04")
      loaded_Centro(Centro)
      
      Lisboa <- loadedData2() %>%
        filter(Region == "PTCSR05")
      loaded_Lisboa(Lisboa)
      
      Madeira <- loadedData2() %>%
        filter(Region == "PTCSR06")
      loaded_Madeira(Madeira)
      
      Norte <- loadedData2() %>%
        filter(Region == "PTCSR07")
      loaded_Norte(Norte)
      
      PT <- loadedData2() %>%
        filter(Region == "PT")
      loaded_PT(PT)
      
    })
    
    
    observeEvent(input$ajuda, {
      showModal(modalDialog(
        title = "Ajuda",
        "Uma pessoa vacinada é aquela que completou o plano de vacinação",
        easyClose = TRUE
      ))
    })
    
    source('Portugal/Dados.R', local = TRUE)$value
    source('Portugal/Plots.R', local = TRUE)$value
    source('Regiões/DadosReg.R', local = TRUE)$value
    source('Regiões/PlotsReg.R', local = TRUE)$value
    
    
    
    output$myEvolucao <- renderPlot({
      
      Alentejo <- loaded_Alentejo()
      
      Alentejo$FirstDoseRefused <- NULL
      Alentejo$UnknownDose <- NULL
      Alentejo$NumberDosesReceived <- NULL
      Alentejo$Population <- NULL
      Alentejo$ReportingCountry <- NULL
      Alentejo$TargetGroup <- NULL
      Alentejo$Denominator <- NULL
      Alentejo$Region <- NULL
      Alentejo$X <- NULL
      
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
      Algarve <- loaded_Algarve()
      
      Algarve$FirstDoseRefused <- NULL
      Algarve$UnknownDose <- NULL
      Algarve$NumberDosesReceived <- NULL
      Algarve$Population <- NULL
      Algarve$ReportingCountry <- NULL
      Algarve$TargetGroup <- NULL
      Algarve$Denominator <- NULL
      Algarve$Region <- NULL
      Algarve$X <- NULL
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
      Açores <- loaded_Acores()
      
      Açores$FirstDoseRefused <- NULL
      Açores$UnknownDose <- NULL
      Açores$NumberDosesReceived <- NULL
      Açores$Population <- NULL
      Açores$ReportingCountry <- NULL
      Açores$TargetGroup <- NULL
      Açores$Denominator <- NULL
      Açores$Region <- NULL
      Açores$X <- NULL
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
      Centro <- loaded_Centro()
      
      Centro$FirstDoseRefused <- NULL
      Centro$UnknownDose <- NULL
      Centro$NumberDosesReceived <- NULL
      Centro$Population <- NULL
      Centro$ReportingCountry <- NULL
      Centro$TargetGroup <- NULL
      Centro$Denominator <- NULL
      Centro$Region <- NULL
      Centro$X <- NULL
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
      Lisboa <- loaded_Lisboa()
      
      Lisboa$FirstDoseRefused <- NULL
      Lisboa$UnknownDose <- NULL
      Lisboa$NumberDosesReceived <- NULL
      Lisboa$Population <- NULL
      Lisboa$ReportingCountry <- NULL
      Lisboa$TargetGroup <- NULL
      Lisboa$Denominator <- NULL
      Lisboa$Region <- NULL
      Lisboa$X <- NULL
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
      Madeira <- loaded_Madeira()
      
      Madeira$FirstDoseRefused <- NULL
      Madeira$UnknownDose <- NULL
      Madeira$NumberDosesReceived <- NULL
      Madeira$Population <- NULL
      Madeira$ReportingCountry <- NULL
      Madeira$TargetGroup <- NULL
      Madeira$Denominator <- NULL
      Madeira$Region <- NULL
      Madeira$X <- NULL
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
      Norte <- loaded_Norte()
      
      Norte$FirstDoseRefused <- NULL
      Norte$UnknownDose <- NULL
      Norte$NumberDosesReceived <- NULL
      Norte$Population <- NULL
      Norte$ReportingCountry <- NULL
      Norte$TargetGroup <- NULL
      Norte$Denominator <- NULL
      Norte$Region <- NULL
      Norte$X <- NULL
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
      
      df$regiao <- as.character(df$regiao)
      df$regiao[df$regiao=="dosestotaisAlentejo"] <-"Alentejo"
      df$regiao[df$regiao=="dosestotaisAlgarve"] <-"Algarve"
      df$regiao[df$regiao=="dosestotaisMadeira"] <-"Madeira"
      df$regiao[df$regiao=="dosestotaisAçores"] <-"Açores"
      df$regiao[df$regiao=="dosestotaisCentro"] <-"Centro"
      df$regiao[df$regiao=="dosestotaisNorte"] <-"Norte"
      df$regiao[df$regiao=="dosestotaisLisboa"] <-"Área metropolitana de Lisboa"
      
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
              axis.text.y = element_text( vjust = 0.5,size=10),
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
    
    #Pedir ajuda à professora. Se passar como x = as.Date(res$Datas) não me aparece a data como queremos, mas apenas os meses.
    
    
  })
