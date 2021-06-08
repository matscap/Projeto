c5 = c("18-24","25-49","50-59","60-69","70-79", "80+")
output$myPlotPais <- renderPlot({
  
  res <- myReactiveDataPais()
  ggplot(data = res$dfpais, aes(x= res$Vac, y=as.factor(res$Doses), fill = res$Idades)) + 
    geom_bar(stat='identity', position = position_dodge())+
    geom_text(aes(label = as.factor(res$Doses)), vjust = -0.2,
              position = position_dodge(0.9), size = 3) +
    theme(
      axis.text.x = element_text(vjust = 1, size = 10,),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title = element_text(face="bold", size=18),
      title = element_text(size = 20),
      panel.grid = element_blank(),
      panel.background = element_rect(fill ="#ffffff")
    ) + 
    guides(fill= guide_legend(title = "Faixas etárias:")) +
    xlab('') + 
    ylab('') 
  
})



output$plot1824 <- renderPlot({
  
  res <- myReactiveData()
  p <- ggplot(data = res$TV, aes(x = as.numeric(1:length(res$Datas)))) +
    
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank()) + 
    labs(title = "", 
         x = "Semanas", 
         y = "Pessoas Vacinadas") +
    scale_x_continuous(breaks = c(as.numeric(1:length(res$Datas))) ,labels = res$Datas) 
  
  switch (input$radioOption,
          "yup" = {
            if(input$option18_24)
              p <- p + geom_point(aes(y = res$Vacinados18_24, colour="18-24", group = 1), size=3) +
                geom_line(aes(y = res$Vacinados18_24, colour="18-24", group = 1)) +
                geom_label(aes(y = res$Vacinados18_24, label= res$Vacinados18_24))
            
            if(input$option25_49)
              p <- p + geom_point(aes(y = res$Vacinados25_49, colour="25-49", group =2), size=3) +
                geom_line(aes(y = res$Vacinados25_49, colour="25-49", group = 2))+
                geom_label(aes(y = res$Vacinados25_49, label= res$Vacinados25_49))
            
            if(input$option50_59)
              p <- p + geom_point(aes(y = res$Vacinados50_59, colour="50-59", group =3), size=3) +
                geom_line(aes(y = res$Vacinados50_59, colour="50-59", group = 3))+
                geom_label(aes(y = res$Vacinados50_59, label= res$Vacinados50_59))
            
            if(input$option60_69)
              p <- p + geom_point(aes(y = res$Vacinados60_69, colour="60-69", group =4), size=3) +
                geom_line(aes(y = res$Vacinados60_69, colour="60-69", group = 4)) +
                geom_label(aes(y = res$Vacinados60_69, label= res$Vacinados60_69))
            
            if(input$option70_79)
              p <- p + geom_point(aes(y = res$Vacinados70_79, colour="70-79", group =5), size=3) +
                geom_line(aes(y = res$Vacinados70_79, colour="70-79", group = 5)) +
                geom_label(aes(y = res$Vacinados70_79, label= res$Vacinados70_79))
            
            if (input$option80)  
              p <- p + geom_point(aes(y = res$Vacinados80, colour="80+", group =6), size=3) +
                geom_line(aes(y = res$Vacinados80, colour="80+", group = 6)) +
                geom_label(aes(y = res$Vacinados80, label= res$Vacinados80))
          },
          "nop" = {
            if(input$option18_24)
              p <- p + geom_point(aes(y = res$Vacinados18_24, colour="18-24", group = 1), size=3) +
                geom_line(aes(y = res$Vacinados18_24, colour="18-24", group = 1))
            
            if(input$option25_49)
              p <- p + geom_point(aes(y = res$Vacinados25_49, colour="25-49", group =2), size=3) +
                geom_line(aes(y = res$Vacinados25_49, colour="25-49", group = 2))
            
            if(input$option50_59)
              p <- p + geom_point(aes(y = res$Vacinados50_59, colour="50-59", group =3), size=3) +
                geom_line(aes(y = res$Vacinados50_59, colour="50-59", group = 3))
            
            if(input$option60_69)
              p <- p + geom_point(aes(y = res$Vacinados60_69, colour="60-69", group =4), size=3) +
                geom_line(aes(y = res$Vacinados60_69, colour="60-69", group = 4)) 
            
            if(input$option70_79)
              p <- p + geom_point(aes(y = res$Vacinados70_79, colour="70-79", group =5), size=3) +
                geom_line(aes(y = res$Vacinados70_79, colour="70-79", group = 5)) 
            
            if (input$option80)  
              p <- p + geom_point(aes(y = res$Vacinados80, colour="80+", group =6), size=3) +
                geom_line(aes(y = res$Vacinados80, colour="80+", group = 6))
          }
  )
  p
})



c6 = c("AZ","MOD","COM","JJ")
output$myPlotPais2 <- renderPlot({
  
  res <- Vacinas()
  ggplot(res$df, aes(x=res$Nomes, y= as.factor(res$Doses_Administradas), fill = c6)) + 
    geom_bar(stat='identity')+ 
    scale_fill_manual(values = c("AZ"="#D8BFD8",
                                 "MOD"="#B0E0E6",
                                 "COM"="#FFDAB9",
                                 "JJ"="#c6ffb3")) +
    geom_text(aes(label = paste(round(res$Percentagens, digits=2),"%")), vjust = -0.6, size = 7)+
    theme(
      legend.position = "none",
      axis.text.x = element_text(vjust = 0.5, size = 12),
      axis.title = element_text(face="bold", size=18),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      title = element_text(vjust = 1, size = 20),
      panel.grid = element_blank(),
      panel.background = element_rect(fill ="#ffffff")
    ) + 
    xlab('') +
    ylab('') 
})

output$tablePerc <- renderDataTable(
  Vacinas()
)




output$myPie <- renderPlot({
  
  resPie <- myReactiveDataPais3()
  
  ggplot(resPie$dfPercentagens, aes(x="", y=resPie$Percentagens, fill=resPie$Nomenclatura)) +
    geom_bar( stat="identity", width=1, color="white") +
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(y=resPie$Percentagens, label = paste(round(resPie$Percentagens, digits=2),"%")), size = 7, color=c("black","white")) +
    guides(fill= guide_legend(title = ""))
})

output$plotRegiao <- renderPlot({
  
  res1 <- myReactiveData2()
  p1<- ggplot(data = res1$TV1, aes(x = as.numeric(1:length(res1$Datas1)))) +
    
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.title = element_blank()) + 
    labs(title = "Quantidade de pessoas vacinadas em cada Região, por semana", 
         x = "Semanas", 
         y = "Pessoas Vacinadas") +
    scale_x_continuous(breaks = c(as.numeric(1:length(res1$Datas1))) ,labels = res1$Datas1) 
  
  switch (input$radioOption1,
          "yup1" = {
            if(input$optionAlentejo)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosAlentejo, colour="Alentejo", group = 1), size=3) +
                geom_line(aes(y = res1$VacinadosAlentejo, colour="Alentejo", group = 1)) +
                geom_label(aes(y = res1$VacinadosAlentejo, label= res1$VacinadosAlentejo))
            
            if(input$optionAlgarve)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosAlgarve, colour="Algarve", group =2), size=3) +
                geom_line(aes(y = res1$VacinadosAlgarve, colour="Algarve", group = 2))+
                geom_label(aes(y = res1$VacinadosAlgarve, label= res1$VacinadosAlgarve))
            
            if(input$optionAcores)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosAcores, colour="Açores", group =3), size=3) +
                geom_line(aes(y = res1$VacinadosAcores, colour="Açores", group = 3))+
                geom_label(aes(y = res1$VacinadosAcores, label= res1$VacinadosAcores))
            
            if(input$optionCentro)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosCentro, colour="Centro", group =4), size=3) +
                geom_line(aes(y = res1$VacinadosCentro, colour="Centro", group = 4)) +
                geom_label(aes(y = res1$VacinadosCentro, label= res1$VacinadosCentro))
            
            if(input$optionLisboa)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosLisboa, colour="Lisboa", group =5), size=3) +
                geom_line(aes(y = res1$VacinadosLisboa, colour="Lisboa", group = 5)) +
                geom_label(aes(y = res1$VacinadosLisboa, label= res1$VacinadosLisboa))
            
            if (input$optionMadeira)  
              p1 <- p1 + geom_point(aes(y = res1$VacinadosMadeira, colour="Madeira", group =6), size=3) +
                geom_line(aes(y = res1$VacinadosMadeira, colour="Madeira", group = 6)) +
                geom_label(aes(y = res1$VacinadosMadeira, label= res1$VacinadosMadeira))
            
            if (input$optionNorte)  
              p1 <- p1 + geom_point(aes(y = res1$VacinadosNorte, colour="Norte", group =6), size=3) +
                geom_line(aes(y = res1$VacinadosNorte, colour="Norte", group = 6)) +
                geom_label(aes(y = res1$VacinadosNorte, label= res1$VacinadosNorte))
          },
          "nop1" = {
            if(input$optionAlentejo)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosAlentejo, colour="Alentejo", group = 1), size=3) +
                geom_line(aes(y = res1$VacinadosAlentejo, colour="Alentejo", group = 1))
            
            if(input$optionAlgarve)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosAlgarve, colour="Algarve", group =2), size=3) +
                geom_line(aes(y = res1$VacinadosAlgarve, colour="Algarve", group = 2))
            
            if(input$optionAcores)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosAcores, colour="Açores", group =3), size=3) +
                geom_line(aes(y = res1$VacinadosAcores, colour="Açores", group = 3))
            
            if(input$optionCentro)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosCentro, colour="Centro", group =4), size=3) +
                geom_line(aes(y = res1$VacinadosCentro, colour="Centro", group = 4)) 
            
            if(input$optionLisboa)
              p1 <- p1 + geom_point(aes(y = res1$VacinadosLisboa, colour="Lisboa", group =5), size=3) +
                geom_line(aes(y = res1$VacinadosLisboa, colour="Lisboa", group = 5)) 
            
            if (input$optionMadeira)  
              p1 <- p1 + geom_point(aes(y = res1$VacinadosMadeira, colour="Madeira", group =6), size=3) +
                geom_line(aes(y = res1$VacinadosMadeira, colour="Madeira", group = 6))
            
            if (input$optionNorte)  
              p1 <- p1 + geom_point(aes(y = res1$VacinadosNorte, colour="Norte", group =6), size=3) +
                geom_line(aes(y = res1$VacinadosNorte, colour="Norte", group = 6))
            
            
          }
  )
  p1
  
}  )


