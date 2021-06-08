library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)

myData -> reactive({  
  # Download data from website already filtered by Country
  #observe({
  #   loadedData(
  data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")  %>%
    filter(ReportingCountry == "PT")
  # ) 
  #})
  
  
  # See the data when it refears to "all" Portugal (not by regions)
  apenasportugal <- loadedData() %>%
    filter(Region == "PT")
  
  
  # Data filtered by ages
  ## In this case, we will filter the data by Ages in "all" Portugal (not by regions) 
  Age18_24 <- apenasportugal %>%
    filter(TargetGroup == "Age18_24")
  
  Age25_49 <- apenasportugal %>%
    filter(TargetGroup == "Age25_49")
  
  Age50_59 <- apenasportugal %>%
    filter(TargetGroup == "Age50_59")
  
  Age60_69 <- apenasportugal %>%
    filter(TargetGroup == "Age60_69")
  
  Age70_79 <- apenasportugal %>%
    filter(TargetGroup == "Age70_79")
  
  Age80 <- apenasportugal %>%
    filter(TargetGroup == "Age80+")
  
  
  # Data set filtered where we will only see the information of "all" Portugal
  allPortugal <- dadosapenasportugal %>%
    filter(TargetGroup == "ALL") %>%
    filter(grepl("PT$",Region))
  
  
  # Data set filtered where we will only see the information of "all" Portugal and a specific Vaccine
  ## read as "total_portugal_vaccine_MOD/COM/AZ" 
  tpv_MOD <- allPortugal %>%
    filter(Vaccine == "MOD")
  tpv_COM <- allPortugal %>%
    filter(Vaccine == "COM")
  tpv_AZ <- allPortugal %>%
    filter(Vaccine == "AZ")
  
  
  # Prepare information to create a data set
  ## As we know, a person is vaccinated, in this vaccines, if a second dose was administred. 
  ## In the future we can add more Vaccines and be aware of the dose that we must choose
  TotalVacinados = unlist(c(sum(tpv_MOD["SecondDose"])) + c(sum(tpv_COM["SecondDose"])) + c(sum(tpv_AZ["SecondDose"])))
  
  # Percentages of total of people vaccinated or not
  ## Since we don't have the number of population by ages or by regions, it's difficult to make more percentages values 
  Percentagem_tpv = (TotalVacinados/totalPopulacao)*100
  Percentagem_tpNv = ((totalPopulacao-TotalVacinados)/totalPopulacao)*100
  
  Percentagens = c(Percentagem_tpv, Percentagem_tpNv)
  
  dfPercentagens <-data.frame(
    Vac_NotVac,
    Percentagens
  )
  
  
  # Create a Data set to see, by ages, how many people had the first dose         
  Idades = unlist(c("18-24","25-49","50-59","60-69","70-79", "80+"))
  Doses=unlist(c(sum(Age18_24['FirstDose']),sum(Age25_49['FirstDose']),sum(Age50_59['FirstDose']),sum(Age60_69['FirstDose']),sum(Age70_79['FirstDose']),sum(Age80['FirstDose'])))
  
  dfFstDose_Ages <- data.frame(
    Idades,
    Doses
  )
  
  
  # Total of doses administred in every vaccine (Fst and Scn doses)              
  totPfizer = sum(Pfizer["FirstDose"]) + sum(Pfizer["SecondDose"])
  totAZ = sum(AZ["FirstDose"]) + sum(AZ["SecondDose"])
  totMOD = sum(MOD["FirstDose"]) + sum(MOD["SecondDose"])
  
  Totais = c(totpfizer,totAZ,totMOD)
  
  dfTotaisVacinas <- data.frame(
    Totais,
    Nomes_Vacinas
  )
  
  
  
  tab <- input$tabs
  fetaria <- input$fetaria
  tipo <- input$radiovac
  
  
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
  
  
  
  Data = unlist(marca["YearWeekISO"])
  Doses=unlist(marca["FirstDose"]+marca["SecondDose"])
  
  # Data frame that will use the given input and have the number of a specific vaccines administered by week       
  dfVaccineRegion <- data.frame(
    Data,
    Doses
  )
  
  
  
  
  # Filter by age and Vaccine.
  ## The result should be the number of people fully vaccinated with a specific vaccine within a specific age group
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
  
  
  
  #Join all group ages
  ## Also replace NA for 0
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
  
  
  
  #Sum all columns and remove columns that we don't need. 
  # The result should be the Total Vaccinated per Week in each age group
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
  #Bug- data set doesn't have the week "2020-W52" so we have to add it
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
  
  # Total vaccinated people by age group in every week
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


shinyServer(
  
    function(input,output,session){
      
      # Global variables
      {
      # Value manually scraped from the next external dataset
      totalPopulacao = 10295909
      
      # Vector with ages groups      
      c5 = c("18-24","25-49","50-59","60-69","70-79", "80+")
      
      # Vector with the names of the Vaccines 
      Nomes_Vacinas = c("Pfizer","AstraZeneca","Moderna")
      
      # Vector with Vaccinated/Not vaccinated strings
      Vac_NotVac = c("Vacinados","Nao Vacinados")
      }
      
      
      # This is the zone where we download and study the data

      #loadedData <- reactiveVal()
        
      
        
      # This is the zone where we will put the plots
      {

# PlotPais
        output$myPlotPais <- renderPlot({
          res <- myData()
          ggplot(data = res$dfFstDose_Ages, aes(y=as.factor(res$Idades), x= as.factor(res$Doses), fill = c5)) + 
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

        output$myPlotPais2 <- renderPlot({
          
          res <- myData()
          ggplot(res$dfTotaisVacinas, aes(x=res$Nomes_Vacinas, y= res$Totais, fill = c6)) + 
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
          
          resPie <- myData()
          
          ggplot(resPie$dfPercentagens, aes(x="", y=resPie$Percentagens, fill=resPie$Vac_NotVac)) +
            geom_bar(stat="identity", width=1, color="white") +
            coord_polar("y", start=0)+
            theme_void()+
            labs(title = "Percentagem pessoas vacinadas/não vacinadas ",
                 subtitle = "Falta por percentagens nesta cena")
          
        })
        
        #        Algarve          #
        output$myPlotB <- renderPlot({
          
          res <- myData()
          
          if(input$grafico=="Vacinação semanal")
            (ggplot(res$dfVaccineRegion, aes(x=res$Data, y= res$Doses)) + 
               geom_bar(stat='identity')+ 
               theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                     axis.title = element_text(face="bold", size=18),
                     title = element_text(size = 20)) + 
               xlab('Week') +
               ylab('Doses') +labs(title = "Para a Pfizer apenas. Fazer para as outras!!")
            )
          else if(input$grafico=="Acumulação de vacinas")
            ( ggplot(res$dfVaccineRegion, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
              + geom_line()  
              + geom_point() 
              + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                      axis.title = element_text(face="bold", size=18),
                      title = element_text(size = 20))  
              + xlab('Week') 
              + ylab('Doses') +labs(title = "Para a Pfizer apenas. Fazer para as outras!!")
            )
          
        })
        
        #        Alentejo          #
        output$myPlot2 <- renderPlot({
          
          res <- myData()
          
          if(input$grafico1=="Vacinação semanal")
            (ggplot(res$dfVaccineRegion, aes(x=res$Data, y= res$Doses)) + 
               geom_bar(stat='identity')+ 
               theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                     axis.title = element_text(face="bold", size=18),
                     title = element_text(size = 20)) + 
               xlab('Week') +
               ylab('Doses')
            )
          else if(input$grafico1=="Acumulação de vacinas")
            ( ggplot(res$dfVaccineRegion, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
              + geom_line()  
              + geom_point() 
              + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                      axis.title = element_text(face="bold", size=18),
                      title = element_text(size = 20))  
              + xlab('Week') 
              + ylab('Doses')
            )
        })
        
        #        Açores            #
        output$myPlot3 <- renderPlot({
          
          res <- myData()
          
          if(input$grafico2=="Vacinação semanal")
            (ggplot(res$dfVaccineRegion, aes(x=res$Data, y= res$Doses)) + 
               geom_bar(stat='identity')+ 
               theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                     axis.title = element_text(face="bold", size=18),
                     title = element_text(size = 20)) + 
               xlab('Week') +
               ylab('Doses') 
            )
          else if(input$grafico2=="Acumulação de vacinas")
            ( ggplot(res$dfVaccineRegion, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
        
        #        Centro            #
        output$myPlot4 <- renderPlot({
          
          res <- myData()
          
          if(input$grafico3=="Vacinação semanal")
            (ggplot(res$dfVaccineRegion, aes(x=res$Data, y= res$Doses)) + 
               geom_bar(stat='identity')+ 
               theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                     axis.title = element_text(face="bold", size=18),
                     title = element_text(size = 20)) + 
               xlab('Week') +
               ylab('Doses') 
            )
          else if(input$grafico3=="Acumulação de vacinas")
            ( ggplot(res$dfVaccineRegion, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
        
        #        Lisboa            #
        output$myPlot5 <- renderPlot({
          
          res <- myData()
          
          if(input$grafico4=="Vacinação semanal")
            (ggplot(res$dfVaccineRegion, aes(x=res$Data, y= res$Doses)) + 
               geom_bar(stat='identity')+ 
               theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                     axis.title = element_text(face="bold", size=18),
                     title = element_text(size = 20)) + 
               xlab('Week') +
               ylab('Doses') 
            )
          else if(input$grafico4=="Acumulação de vacinas")
            ( ggplot(res$dfVaccineRegion, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
        
        #        Madeira           #
        output$myPlot6 <- renderPlot({
          
          res <- myData()
          if(input$grafico5=="Vacinação semanal")
            (ggplot(res$dfVaccineRegion, aes(x=res$Data, y= res$Doses)) + 
               geom_bar(stat='identity')+ 
               theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                     axis.title = element_text(face="bold", size=18),
                     title = element_text(size = 20)) + 
               xlab('Week') +
               ylab('Doses') 
            )
          else if(input$grafico5=="Acumulação de vacinas")
            ( ggplot(res$dfVaccineRegion, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
        
        #        Norte             #
        output$myPlot7 <- renderPlot({
          
          res <- myData()
          
          if(input$grafico6=="Vacinação semanal")
            (ggplot(res$dfVaccineRegion, aes(x=res$Data, y= res$Doses)) + 
               geom_bar(stat='identity')+ 
               theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
                     axis.title = element_text(face="bold", size=18),
                     title = element_text(size = 20)) + 
               xlab('Week') +
               ylab('Doses') 
            )
          else if(input$grafico6=="Acumulação de vacinas")
            ( ggplot(res$dfVaccineRegion, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
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
      
    }
)