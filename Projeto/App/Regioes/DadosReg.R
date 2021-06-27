VacinasReg <- reactive({
  
  #Dados para portugal apenas:
  
  tab <- input$tabs
  
  if(tab=="Alentejo"){
    escolha <- loaded_Alentejo()
  }else if(tab == "Algarve"){
    escolha <- loaded_Algarve()
  }else if(tab == "Acores"){
    escolha <- loaded_Acores()
  }else if(tab == "Centro"){
    escolha <- loaded_Centro()
  }else if(tab == "Lisboa"){
    escolha <- loaded_Lisboa()
  }else if(tab == "Madeira"){
    escolha <- loaded_Madeira()
  }else if(tab == "Norte"){
    escolha <- loaded_Norte()
  }else{
    escolha <- loaded_PT()
  }
  
  Pfizer <- escolha %>%
    filter(Vaccine == "COM")
  AZ <- escolha %>%
    filter(Vaccine == "AZ")
  MOD <- escolha %>%
    filter(Vaccine == "MOD")
  JJ <- escolha %>%
    filter(Vaccine == "JANSS")
  
  #Totais:
  totpfizer=0
  totAZ=0
  totMOD=0
  totJJ=0
  if(nrow(Pfizer)>0)
    totpfizer = sum(Pfizer["FirstDose"])+sum(Pfizer["SecondDose"])
  if(nrow(AZ)>0)
    totAZ = sum(AZ["FirstDose"])+sum(AZ["SecondDose"])
  if(nrow(MOD)>0)
    totMOD = sum(MOD["FirstDose"])+sum(MOD["SecondDose"])
  if(nrow(JJ)>0)
    totJJ = sum(JJ["FirstDose"])
  
  Totais = c(totpfizer,totMOD,totAZ, totJJ)
  Nomes = c("Comirnaty ","Moderna","AstraZeneca", "Janssen")
  
  
  df <- data.frame(
    Totais,
    Nomes
  )
})

myReactiveDat <- reactive({
  
  tab <- input$tabs
  fetaria <- input$fetaria
  
  #Dados para portugal apenas:
  
  tipo <- input$radiovac
  if(tab=="Alentejo"){
    escolha <- loaded_Alentejo()
  }else if(tab == "Algarve"){
    escolha <- loaded_Algarve()
  }else if(tab == "Acores"){
    escolha <- loaded_Acores()
  }else if(tab == "Centro"){
    escolha <- loaded_Centro()
  }else if(tab == "Lisboa"){
    escolha <- loaded_Lisboa()
  }else if(tab == "Madeira"){
    escolha <- loaded_Madeira()
  }else if(tab == "Norte"){
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
    totalPopulacao = pop_Alentejo()
    escolha <- loaded_Alentejo()
    
  }else if(tab == "Algarve"){
    totalPopulacao =  pop_Algarve()
    escolha <- loaded_Algarve()
    
  }else if(tab == "Acores"){
    totalPopulacao = pop_Acores()
    escolha <- loaded_Acores()
    
  }else if(tab == "Centro"){
    totalPopulacao =pop_Centro()
    escolha <- loaded_Centro()
    
  }else if(tab == "Lisboa"){
    totalPopulacao = pop_Lisboa()
    escolha <- loaded_Lisboa()
    
  }else if(tab == "Madeira"){
    totalPopulacao = pop_Madeira()
    escolha <- loaded_Madeira()
    
  }else if(tab == "Norte"){
    totalPopulacao = pop_Norte()
    escolha <- loaded_Norte()
    
  }else{
    totalPopulacao = pop_PT()	
    escolha <- loaded_PT()
  }
  #tpv -> total de pessoas vacinadas pela vacina _ ###
  teste = sum(escolha["SecondDose"])
  tpv_MOD <- escolha %>%
    filter(Vaccine == "MOD")
  tpv_COM <- escolha %>%
    filter(Vaccine == "COM")
  tpv_AZ <- escolha %>%
    filter(Vaccine == "AZ")
  tpv_JJ <- escolha %>%
    filter(Vaccine == "JANSS")
  
  TotalCompVacinados = c(sum(tpv_COM["SecondDose"])) + c(sum(tpv_AZ["SecondDose"]))
  if(nrow(tpv_MOD)!=0){
    TotalCompVacinados = TotalCompVacinados + c(sum(tpv_MOD["SecondDose"]))
  }
  
  if(nrow(tpv_JJ)!=0){
    TotalCompVacinados = TotalCompVacinados + c(sum(tpv_JJ["FirstDose"]))
  }
  TotalCompVacinados = unlist(TotalCompVacinados)
  
  Percentagem_tpv4 = (TotalCompVacinados/totalPopulacao)*100
  Percentagem_tpv3 = 100 - Percentagem_tpv4
  
  
  vals <- c( round(Percentagem_tpv3), round(Percentagem_tpv4))
  
})