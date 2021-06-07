##################################################################################
#        Algarve          #
##################################################################################
output$myPlotB3 <- renderPlot({
  
  res <- myReactivePies()
  ggplot(res$dfPercentagens, aes(x="", y=res$Percentagens, fill=res$Nomenclatura)) +
    geom_bar( stat="identity", width=1, color="white") +
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(y=res$Percentagens, label = paste(round(res$Percentagens, digits=2),"%")), size = 7, color=c("black","white")) +
    guides(fill= guide_legend(title = ""))
  
  
})
output$myPlotB2 <- renderPlot({
  
  res <- myReactiveDat()
  
  (ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line()  
    + geom_point() 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
    
  )
})

output$myPlotB1 <- renderPlot({
  
  res <- myReactiveDat()
  
  
  ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
    geom_bar(stat='identity')+ 
    
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Week') +
    ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
  
})

##################################################################################
#        Alentejo          #
##################################################################################

output$myPlot23 <- renderPlot({
  
  res <- myReactivePies()
  ggplot(res$dfPercentagens, aes(x="", y=res$Percentagens, fill=res$Nomenclatura)) +
    geom_bar( stat="identity", width=1, color="white") +
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(y=res$Percentagens, label = paste(round(res$Percentagens, digits=2),"%")), size = 7, color=c("black","white")) +
    guides(fill= guide_legend(title = ""))
})

output$myPlot22 <- renderPlot({
  
  res <- myReactiveDat()
  
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line()  
    + geom_point() 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses')+labs(title = "Acumulação de vacinas semana após semana")
  )
  
  ##########################################################################
  
})

output$myPlot21 <- renderPlot({
  
  res <- myReactiveDat()
  (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
      geom_bar(stat='identity')+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20)) + 
      xlab('Week') +
      ylab('Doses')+labs(title = "Número de vacinas administradas por semana")
  )
})


##############################################################################################################

output$myPlot33 <- renderPlot({
  res <- myReactivePies()
  ggplot(res$dfPercentagens, aes(x="", y=res$Percentagens, fill=res$Nomenclatura)) +
    geom_bar( stat="identity", width=1, color="white") +
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(y=res$Percentagens, label = paste(round(res$Percentagens, digits=2),"%")), size = 7, color=c("black","white")) +
    guides(fill= guide_legend(title = ""))
})

output$myPlot32 <- renderPlot({
  
  res <- myReactiveDat()
  
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line()  
    + geom_point() 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
  )
  
})

output$myPlot31 <- renderPlot({
  
  res <- myReactiveDat()
  (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
      geom_bar(stat='identity')+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20)) + 
      xlab('Week') +
      ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
  )
})

#################################################################################################################

output$myPlot43 <- renderPlot({
  
  res <- myReactivePies()
  ggplot(res$dfPercentagens, aes(x="", y=res$Percentagens, fill=res$Nomenclatura)) +
    geom_bar( stat="identity", width=1, color="white") +
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(y=res$Percentagens, label = paste(round(res$Percentagens, digits=2),"%")), size = 7, color=c("black","white")) +
    guides(fill= guide_legend(title = ""))
  
})

output$myPlot42 <- renderPlot({
  
  res <- myReactiveDat()
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line()  
    + geom_point() 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
  )
})

output$myPlot41 <- renderPlot({
  
  res <- myReactiveDat()
  (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
      geom_bar(stat='identity')+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20)) + 
      xlab('Week') +
      ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
  )
  
})

#################################################################################################################

output$myPlot53 <- renderPlot({
  
  res <- myReactivePies()
  ggplot(res$dfPercentagens, aes(x="", y=res$Percentagens, fill=res$Nomenclatura)) +
    geom_bar( stat="identity", width=1, color="white") +
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(y=res$Percentagens, label = paste(round(res$Percentagens, digits=2),"%")), size = 7, color=c("black","white")) +
    guides(fill= guide_legend(title = ""))
  
})

output$myPlot52 <- renderPlot({
  
  res <- myReactiveDat()
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line()  
    + geom_point() 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
  )
})

output$myPlot51 <- renderPlot({
  
  res <- myReactiveDat()
  
  (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
      geom_bar(stat='identity')+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20)) + 
      xlab('Week') +
      ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
  )
})

#################################################################################################################

output$myPlot63 <- renderPlot({
  
  res <- myReactivePies()
  ggplot(res$dfPercentagens, aes(x="", y=res$Percentagens, fill=res$Nomenclatura)) +
    geom_bar( stat="identity", width=1, color="white") +
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(y=res$Percentagens, label = paste(round(res$Percentagens, digits=2),"%")), size = 7, color=c("black","white")) +
    guides(fill= guide_legend(title = ""))
  
})

output$myPlot62 <- renderPlot({
  
  res <- myReactiveDat()
  
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line()  
    + geom_point() 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
  )
})

output$myPlot61 <- renderPlot({
  
  res <- myReactiveDat()
  (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
      geom_bar(stat='identity')+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20)) + 
      xlab('Week') +
      ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
  )
})

#################################################################################################################

output$myPlot73 <- renderPlot({
  
  res <- myReactivePies()
  ggplot(res$dfPercentagens, aes(x="", y=res$Percentagens, fill=res$Nomenclatura)) +
    geom_bar( stat="identity", width=1, color="white") +
    coord_polar("y", start=0)+
    theme_void()+
    geom_text(aes(y=res$Percentagens, label = paste(round(res$Percentagens, digits=2),"%")), size = 7, color=c("black","white")) +
    guides(fill= guide_legend(title = ""))
  
})

output$myPlot72 <- renderPlot({
  
  res <- myReactiveDat()
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line()  
    + geom_point() 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses') +labs(title = "Acumulação de vacinas semana após semana")
  )
})

output$myPlot71 <- renderPlot({
  
  res <- myReactiveDat()
  
  (ggplot(res$df2, aes(x=res$Data, y= res$Doses)) + 
      geom_bar(stat='identity')+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20)) + 
      xlab('Week') +
      ylab('Doses') +labs(title = "Número de vacinas administradas por semana")
  )
})
