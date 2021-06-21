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