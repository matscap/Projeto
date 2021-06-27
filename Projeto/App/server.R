library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape)
library(waffle)
library(readxl)
library(httr)
library(markdown)


last_update <- Sys.Date()

# Vamos usar como data inicial, a quinta-feira da semana de chegada do primeiro lote de vacinas a Portugal
date <- as.Date("24/12/2021", format = "%d/%m/%y")


shinyServer(
  function(input,output,session){
    
    loadedData <- reactiveVal()
    loadedData2 <- reactiveVal()
    
    loaded_Alentejo <- reactiveVal()
    pop_Alentejo <- reactiveVal()
    
    loaded_Algarve <- reactiveVal()
    pop_Algarve <- reactiveVal()
    
    loaded_Acores <- reactiveVal()
    pop_Acores <- reactiveVal()
    
    loaded_Centro <- reactiveVal()
    pop_Centro <- reactiveVal()
    
    loaded_Lisboa <- reactiveVal()
    pop_Lisboa <- reactiveVal()
    
    loaded_Madeira <- reactiveVal()
    pop_Madeira <- reactiveVal()
    
    loaded_Norte <- reactiveVal()
    pop_Norte <- reactiveVal()
    
    loaded_PT <- reactiveVal()
    pop_PT <- reactiveVal()
    
    
    observe({
      data <- read.csv("https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv", na.strings = "", fileEncoding = "UTF-8-BOM")
      url <- "https://www.ecdc.europa.eu/sites/default/files/documents/Indicators_for_maps_week_45_1.xlsx"
      httr::GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
      data2 <- read_excel(tf, 1L)
      
      dadosapenasportugal2 <- data2 %>%
        filter(country_code == "PT")
      
      loadedData(data) 
      dadosapenasportugal <- loadedData() %>%
        filter(ReportingCountry == "PT")
      loadedData2(dadosapenasportugal)
      
      Alentejo <- loadedData2() %>%
        filter(Region == "PTCSR01")
      loaded_Alentejo(Alentejo)
      ApenasAlentejo <- dadosapenasportugal2 %>%
        filter(geo_id_final == "PTCSR01")
      pop_Alentejo(sum(ApenasAlentejo["subnational_population"]))
      
      Algarve <- loadedData2() %>%
        filter(Region == "PTCSR02")
      loaded_Algarve(Algarve)
      ApenasAlgarve <- dadosapenasportugal2 %>%
        filter(geo_id_final == "PTCSR02")
      pop_Algarve(sum(ApenasAlgarve["subnational_population"]))
      
      
      Açores <- loadedData2() %>%
        filter(Region == "PTCSR03")
      loaded_Acores(Açores)
      ApenasAcores <- dadosapenasportugal2 %>%
        filter(geo_id_final == "PTCSR03")
      pop_Acores(sum(ApenasAcores["subnational_population"]))
      
      Centro <- loadedData2() %>%
        filter(Region == "PTCSR04")
      loaded_Centro(Centro)
      ApenasCentro <- dadosapenasportugal2 %>%
        filter(geo_id_final == "PTCSR04")
      pop_Centro(sum(ApenasCentro["subnational_population"]))
      
      Lisboa <- loadedData2() %>%
        filter(Region == "PTCSR05")
      loaded_Lisboa(Lisboa)
      ApenasLisboa <- dadosapenasportugal2 %>%
        filter(geo_id_final == "PTCSR05")
      pop_Lisboa(sum(ApenasLisboa["subnational_population"]))
      
      Madeira <- loadedData2() %>%
        filter(Region == "PTCSR06")
      loaded_Madeira(Madeira)
      ApenasMadeira <- dadosapenasportugal2 %>%
        filter(geo_id_final == "PTCSR06")
      pop_Madeira(sum(ApenasMadeira["subnational_population"]))
      
      Norte <- loadedData2() %>%
        filter(Region == "PTCSR07")
      loaded_Norte(Norte)
      ApenasNorte <- dadosapenasportugal2 %>%
        filter(geo_id_final == "PTCSR07")
      pop_Norte(sum(ApenasNorte["subnational_population"]))
      
      PT <- loadedData2() %>%
        filter(Region == "PT")
      loaded_PT(PT)
      pop_PT(sum(ApenasNorte["national_population"]))
      
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
    source('Regioes/DadosReg.R', local = TRUE)$value
    source('Regioes/PlotsReg.R', local = TRUE)$value
    
  })
