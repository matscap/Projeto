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
  tpv_JJ <- totalGenteVacinada %>%
    filter(Vaccine == "JANSS")
  
  TotalVacinados = unlist(c(sum(tpv_MOD["SecondDose"])) + c(sum(tpv_COM["SecondDose"])) + c(sum(tpv_AZ["SecondDose"]))+ c(sum(tpv_AZ["SecondDose"])) + c(sum(tpv_JJ["SecondDose"])))
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
  Vac = unlist(rep(c("Comirnaty (BioNTech/Pfizer) ","Moderna (Moderna)","AstraZeneca (AstraZeneca/Oxford)", "Janssen (Janssen/Jonhson & Jonhson)"),6))
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
  
  # Queremos alterar a informação que temos em Datas:
  d = as.Date(date, format = "%d/%m/%y")
  for (i in 1:length(Datas)){
    Datas[i] = toString(d)
    d = d + 7
  }
  
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
  Nomes = c("Comirnaty (BioNTech/Pfizer) ","Moderna (Moderna)","AstraZeneca (AstraZeneca/Oxford)", "Janssen (Janssen/Jonhson & Jonhson)")
  
  
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
  tpv_JJ <- totalGenteVacinada %>%
    filter(Vaccine == "JANSS")
  
  TotalVacinados1Dose = unlist(c(sum(tpv_MOD["FirstDose"])) + c(sum(tpv_COM["FirstDose"])) + c(sum(tpv_AZ["FirstDose"])) - c(sum(tpv_MOD["SecondDose"])) - c(sum(tpv_COM["SecondDose"])) - c(sum(tpv_AZ["SecondDose"])))
  
  TotalCompVacinados = unlist(c(sum(tpv_MOD["SecondDose"])) + c(sum(tpv_COM["SecondDose"])) + c(sum(tpv_AZ["SecondDose"])) + c(sum(tpv_JJ["FirstDose"])))

  Percentagem_tpv2 = (TotalVacinados1Dose/totalPopulacao)*100
  Percentagem_tpv3 = (TotalCompVacinados/totalPopulacao)*100
  Percentagem_tpv4 = 100 - (Percentagem_tpv2+Percentagem_tpv3)
  Percentagem_tpNv = ((totalPopulacao-TotalCompVacinados)/totalPopulacao)*100
  
  vals <- c(round(Percentagem_tpv4), round(Percentagem_tpv3), round(Percentagem_tpv2))
  
})


myReactiveData2 <- reactive({
  
  
  Region1 <- loaded_Alentejo()
  
  Region2 <- loaded_Algarve()
  
  Region3 <- loaded_Acores()
  
  Region4 <- loaded_Centro()
  
  Region5 <- loaded_Lisboa()
  
  Region6 <- loaded_Madeira()
  
  Region7 <- loaded_Norte()
  
  
  
  MOD_AlentejoVacinados<- Region1 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
  COM_AlentejoVacinados<- Region1 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
  AZ_AlentejoVacinados<- Region1 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
  
  MOD_AlgarveVacinados<- Region2 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
  COM_AlgarveVacinados<- Region2 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
  AZ_AlgarveVacinados<- Region2 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
  
  MOD_AcoresVacinados<- Region3 %>% filter(Vaccine == "MOD") %>% select(YearWeekISO, SecondDose) 
  COM_AcoresVacinados<- Region3 %>% filter(Vaccine == "COM") %>% select(YearWeekISO, SecondDose)
  AZ_AcoresVacinados<- Region3 %>% filter(Vaccine == "AZ") %>% select(YearWeekISO, SecondDose)
  
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
  
  joinedDS_Acores <- left_join(COM_AcoresVacinados, left_join(MOD_AcoresVacinados, AZ_AcoresVacinados, by="YearWeekISO"), by="YearWeekISO") %>% 
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
  
  TVpS_Acores <-joinedDS_Acores %>% 
    mutate( TotalVacinados = rowSums(.[2:4])) %>% 
    select(YearWeekISO, TotalVacinados)
  
  TVpS_Centro <-joinedDS_Centro %>% 
    mutate( TotalVacinados = rowSums(.[2:4])) %>% 
    select(YearWeekISO, TotalVacinados)
  
  TVpS_Lisboa <-joinedDS_Lisboa %>% 
    mutate( TotalVacinados = rowSums(.[2:4])) %>% 
    select(YearWeekISO, TotalVacinados)
  
  TVpS_Madeira <-joinedDS_Madeira %>% 
    mutate( TotalVacinados = rowSums(.[2:4])) %>% 
    select(YearWeekISO, TotalVacinados)
  
  TVpS_Norte <-joinedDS_Norte %>% 
    mutate( TotalVacinados = rowSums(.[2:4])) %>% 
    select(YearWeekISO, TotalVacinados) 
  
  df <- merge(TVpS_Alentejo,TVpS_Algarve,by="YearWeekISO",all=TRUE)
  colnames(df) <- c("YearWeekISO","Alentejo", "Algarve")
  df <- merge(df,TVpS_Acores,by="YearWeekISO",all=TRUE)
  colnames(df) <- c("YearWeekISO","Alentejo", "Algarve","Açores")
  df <- merge(df,TVpS_Centro,by="YearWeekISO",all=TRUE)
  colnames(df) <- c("YearWeekISO","Alentejo", "Algarve","Açores","Centro")
  df <- merge(df,TVpS_Lisboa,by="YearWeekISO",all=TRUE)
  colnames(df) <- c("YearWeekISO","Alentejo", "Algarve","Açores","Centro","Lisboa")
  df <- merge(df,TVpS_Madeira,by="YearWeekISO",all=TRUE)
  colnames(df) <- c("YearWeekISO","Alentejo", "Algarve","Açores","Centro","Lisboa","Madeira")
  df <- merge(df,TVpS_Norte,by="YearWeekISO",all=TRUE)
  colnames(df) <- c("YearWeekISO","Alentejo", "Algarve","Açores","Centro","Lisboa","Madeira","Norte")
  
  df["Alentejo"][is.na(df["Alentejo"])] <- 0
  df["Algarve"][is.na(df["Algarve"])] <- 0
  df["Centro"][is.na(df["Centro"])] <- 0
  df["Lisboa"][is.na(df["Lisboa"])] <- 0
  df["Madeira"][is.na(df["Madeira"])] <- 0
  df["Norte"][is.na(df["Norte"])] <- 0
  df["Açores"][is.na(df["Açores"])] <- 0
  
  
  Datas1 = unlist(TVpS_Norte["YearWeekISO"])
  VacinadosAlentejo = unlist(df["Alentejo"])
  VacinadosAlgarve = unlist(df["Algarve"])
  VacinadosAcores = unlist(df["Centro"])
  VacinadosCentro = unlist(df["Centro"])
  VacinadosLisboa = unlist(df["Lisboa"])
  VacinadosMadeira = unlist(df["Madeira"])
  VacinadosNorte    = unlist(df["Açores"])
  
  d = as.Date(date, format = "%d/%m/%y")
  for (i in 1:length(Datas1)){
    Datas1[i] = toString(d)
    d = d + 7
  }
  
  TV1 <- data.frame(
    Datas1,
    VacinadosAlentejo,
    VacinadosAlgarve,
    VacinadosAcores,
    VacinadosCentro,
    VacinadosLisboa,
    VacinadosMadeira,
    VacinadosNorte
  )
  
})
