myReactiveDat <- reactive({
  
  tab <- input$tabs
  fetaria <- input$fetaria
  
  #Dados para portugal apenas:
  
  tipo <- input$radiovac
  if(tab=="Alentejo"){
    popul = sum(465701,118506,168034,911794,152758)
    escolha <- loaded_Alentejo()
    
  }else if(tab == "Algarve"){
    popul = 434023
    escolha <- loaded_Algarve()
    
  }else if(tab == "Acores"){
    popul = 246772
    escolha <- loaded_Acores()
    
  }else if(tab == "Centro"){
    popul = sum(714200,391215,168898,429714,196264,470895)
    escolha <- loaded_Centro()
    
  }else if(tab == "Lisboa"){
    popul = sum(2265832,911794)
    escolha <- loaded_Lisboa()
    
  }else if(tab == "Madeira"){
    popul = 258686
    escolha <- loaded_Madeira()
    
  }else if(tab == "Norte"){
    popul = sum(240133,956185,213775,136252,1778146)
    escolha <- loaded_Norte()
    
  }else{
    escolha <- loaded_PT()
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
  escolha$X <- NULL
  
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
  
  # Queremos alterar a informação que temos em Datas:
  d = as.Date(date, format = "%d/%m/%y")
  for (i in 1:length(Data)){
    Data[i] = toString(d)
    d = d + 7
  }
  
  df2 <- data.frame(
    Data,
    Doses
  )
})

myReactivePies <- reactive({
  
  tab <- input$tabs
  fetaria <- input$fetaria
  
  #Dados para portugal apenas:
  
  tipo <- input$radiovac
  if(tab=="Alentejo"){
    popul = sum(465701,118506,168034,911794,152758)
    escolha <- loaded_Alentejo()
    
  }else if(tab == "Algarve"){
    popul = 434023
    escolha <- loaded_Algarve()
    
  }else if(tab == "Acores"){
    popul = 246772
    escolha <- loaded_Acores()
    
  }else if(tab == "Centro"){
    popul = sum(714200,391215,168898,429714,196264,470895)
    escolha <- loaded_Centro()
    
  }else if(tab == "Lisboa"){
    popul = sum(2265832,911794)
    escolha <- loaded_Lisboa()
    
  }else if(tab == "Madeira"){
    popul = 258686
    escolha <- loaded_Madeira()
    
  }else if(tab == "Norte"){
    popul = sum(240133,956185,213775,136252,1778146)
    escolha <- loaded_Norte()
    
  }else{
    escolha <- loaded_PT()
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
  escolha$X <- NULL
  
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
  
  TotalVacinados = unlist(c(sum(escolha["SecondDose"])))
  Percentagem_tpv = (TotalVacinados/popul)*100
  Percentagem_tpNv = ((popul-TotalVacinados)/popul)*100
  
  Nomenclatura = c("Vacinados","Nao Vacinados")
  Percentagens = c(Percentagem_tpv, Percentagem_tpNv)
  
  dfPercentagens <-data.frame(
    Nomenclatura,
    Percentagens
  )
  
})
