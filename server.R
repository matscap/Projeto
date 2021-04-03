shinyServer(
  
  function(input,output,session){
    output$myPlot <- renderPlot({
      regiao <- input$Regiao
      
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      #Dados para portugal apenas:
      
      dadosapenasportugal <- data %>%
        filter(ReportingCountry == "PT")
      View(dadosapenasportugal)
      #Dados por região:
      if(regiao=="ALENTEJO"){
        ALENTEJO <- dadosapenasportugal %>%
          filter(Region == "PTCSR01")
        
        weight=unlist(ALENTEJO["YearWeekISO"])
        
        set.seed(1234)
        df <- data.frame(
          sex=as.numeric(unlist(ALENTEJO["FirstDose"])),
          weight
        )
        ggplot(df, aes(x=weight)) + geom_bar()
        print("OLA");
        #hist(ALENTEJO['FirstDose']);
      }else if(regiao == "ALGARVE"){
        ALGARVE <- dadosapenasportugal %>%
          filter(Region == "PTCSR02")
        print("ALGARVE")
      }else if(regiao == "AZORES"){
        AZORES <- dadosapenasportugal %>%
          filter(Region == "PTCSR03")
        print("AZORES")
      }else if(regiao == "CENTRO"){
        CENTRO <- dadosapenasportugal %>%
          filter(Region == "PTCSR04")
        print("CENTRO")
      }else if(regiao == "LISBOA"){
        AREALISBOA <- dadosapenasportugal %>%
          filter(Region == "PTCSR05")
        print("LISBOA")
      }else if(regiao == "MADEIRA"){
        MADEIRA <- dadosapenasportugal %>%
          filter(Region == "PTCSR06")
        print("MADEIRA")
      }else if(regiao == "NORTE"){
        NORTE <- dadosapenasportugal %>%
          filter(Region == "PTCSR07")
        print("NORTE")
      }else{
        PT <- dadosapenasportugal %>%
          filter(Region == "PT")
      }
      
      #Dados por GRUPOS ETÁRIOS:
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
      
      
      #novo <- transform(dadosapenasportugal,dateRep = as.Date(dateRep,format = "%d/%m/%y"))
      #novo <- transform(novo,cases = as.numeric(cases))
      #str(novo)
      # plot(novo['dateRep'],novo['cases'],type="b")
      #View(novo['cases'])
      
      #hist(novo["cases"])
      
      
    })
    
  }
)