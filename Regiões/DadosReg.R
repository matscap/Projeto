myReactiveDat <- reactive({
  
  tab <- input$tabs
  fetaria <- input$fetaria
  
  #Dados para portugal apenas:
  
  tipo <- input$radiovac
  if(tab=="Alentejo"){
    popul = 704558	
    escolha <- loaded_Alentejo()
    
  }else if(tab == "Algarve"){
    popul = 438406
    escolha <- loaded_Algarve()
    
  }else if(tab == "Acores"){
    popul = 242796
    escolha <- loaded_Acores()
    
  }else if(tab == "Centro"){
    popul = 2217285
    escolha <- loaded_Centro()
    
  }else if(tab == "Lisboa"){
    popul = 2863272
    escolha <- loaded_Lisboa()
    
  }else if(tab == "Madeira"){
    popul = 254254
    escolha <- loaded_Madeira()
    
  }else if(tab == "Norte"){
    popul = 3575338
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
    totalPopulacao = 704558
    escolha <- loaded_Alentejo()
    
  }else if(tab == "Algarve"){
    totalPopulacao =  438406
    escolha <- loaded_Algarve()
    
  }else if(tab == "Acores"){
    totalPopulacao = 242796
    escolha <- loaded_Acores()
    
  }else if(tab == "Centro"){
    totalPopulacao =2217285
    escolha <- loaded_Centro()
    
  }else if(tab == "Lisboa"){
    totalPopulacao = 2863272
    escolha <- loaded_Lisboa()
    
  }else if(tab == "Madeira"){
    totalPopulacao = 254254
    escolha <- loaded_Madeira()
    
  }else if(tab == "Norte"){
    totalPopulacao = 3575338
    escolha <- loaded_Norte()
    
  }else{
    totalPopulacao = 10295909	
    escolha <- loaded_PT()
  }
  
  #tpv -> total de pessoas vacinadas pela vacina _ ###
  tpv_MOD <- escolha %>%
    filter(Vaccine == "MOD")
  tpv_COM <- escolha %>%
    filter(Vaccine == "COM")
  tpv_AZ <- escolha %>%
    filter(Vaccine == "AZ")
  tpv_JJ <- escolha %>%
    filter(Vaccine == "JANSS")

  TotalVacinados1Dose = c(sum(tpv_COM["FirstDose"])) + c(sum(tpv_AZ["FirstDose"])) - c(sum(tpv_COM["SecondDose"])) - c(sum(tpv_AZ["SecondDose"]))
  TotalCompVacinados = c(sum(tpv_COM["SecondDose"])) + c(sum(tpv_AZ["SecondDose"]))
  if(nrow(tpv_MOD)!=0){
    TotalVacinados1Dose= TotalVacinados1Dose + c(sum(tpv_MOD["FirstDose"])) - c(sum(tpv_MOD["SecondDose"]))
    TotalCompVacinados = TotalCompVacinados + c(sum(tpv_MOD["SecondDose"]))
  }
  
  if(nrow(tpv_MOD)!=0){
    TotalCompVacinados = TotalCompVacinados + c(sum(tpv_JJ["FirstDose"]))
  }
  
  TotalCompVacinados = unlist(TotalCompVacinados)
  TotalVacinados1Dose = unlist(TotalVacinados1Dose)

  Percentagem_tpv2 = (TotalVacinados1Dose/totalPopulacao)*100
  Percentagem_tpv3 = (TotalCompVacinados/totalPopulacao)*100
  Percentagem_tpv4 = 100 - (Percentagem_tpv2+Percentagem_tpv3)
  Percentagem_tpNv = ((totalPopulacao-TotalCompVacinados)/totalPopulacao)*100
  

  vals <- c(round(Percentagem_tpv4), round(Percentagem_tpv3), round(Percentagem_tpv2))
  
})