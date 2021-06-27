##################################################################################
#        Alentejo          #
##################################################################################

output$myPlotB4 <- renderPlot({
  
  res <- VacinasReg()
  ggplot(res, aes(x=Nomes, y=Totais, fill = c6)) + 
    geom_bar(stat='identity')+ 
    scale_fill_manual(values = c("AZ"="#D8BFD8",
                                 "MOD"="#B0E0E6",
                                 "COM"="#FFDAB9",
                                 "JJ"="#c6ffb3")) +
    geom_text(aes(label=Totais), vjust = -0.6, size = 4)+
    theme(
      legend.position = "none",
      
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

output$myPlotB3 <- renderPlot({
  
  resPie <- myReactivePies()
  
  val_names <- sprintf("%s (%s%%)", c("Não: ", "Sim: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
  
})
output$myPlotB2 <- renderPlot({
  
  res <- myReactiveDat()
  (ggplot(res, aes(x=Data, y=cumsum(round(Doses/1000)),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Semanas') 
    + ylab('Doses')
    
  )
})

output$myPlotB1 <- renderPlot({
  
  res <- myReactiveDat()
  ##Doses <- res$Doses
  
  ggplot(res, aes(x=Data, y= Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Semanas') +
    ylab('Doses')
})

##################################################################################
#        Algarve          #
##################################################################################

output$myPlot24 <- renderPlot({
  
  res <- VacinasReg()
  ggplot(res, aes(x=Nomes, y= Totais, fill = c6)) + 
    geom_bar(stat='identity')+ 
    scale_fill_manual(values = c("AZ"="#D8BFD8",
                                 "MOD"="#B0E0E6",
                                 "COM"="#FFDAB9",
                                 "JJ"="#c6ffb3")) +
    geom_text(aes(label=Totais), vjust = -0.6, size = 4)+
    theme(
      legend.position = "none",
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

output$myPlot23 <- renderPlot({
  
  resPie <- myReactivePies()
  
  val_names <- sprintf("%s (%s%%)", c("Não: ", "Sim: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
})

output$myPlot22 <- renderPlot({
  
  res <- myReactiveDat()
  
  ( ggplot(res, aes(x=Data, y=cumsum(round(Doses/1000)),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Semanas') 
    + ylab('Doses')
  )
  
  ##########################################################################
  
})

output$myPlot21 <- renderPlot({
  
  res <- myReactiveDat()
  ##Doses <- res$Doses
  
  ggplot(res, aes(x=Data, y= Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Semanas') +
    ylab('Doses')
})


##############################################################################################################

output$myPlot34 <- renderPlot({
  
  res <- VacinasReg()
  ggplot(res, aes(x=Nomes, y= Totais, fill = c6)) + 
    geom_bar(stat='identity')+ 
    scale_fill_manual(values = c("AZ"="#D8BFD8",
                                 "MOD"="#B0E0E6",
                                 "COM"="#FFDAB9",
                                 "JJ"="#c6ffb3")) +
    geom_text(aes(label=Totais), vjust = -0.6, size = 4)+
    theme(
      legend.position = "none",
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

output$myPlot33 <- renderPlot({
  resPie <- myReactivePies()
  
  val_names <- sprintf("%s (%s%%)", c("Não: ", "Sim: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
})

output$myPlot32 <- renderPlot({
  
  res <- myReactiveDat()
  
  ( ggplot(res, aes(x=Data, y=cumsum(round(Doses/1000)),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Semanas') 
    + ylab('Doses')
  )
  
})

output$myPlot31 <- renderPlot({
  
  res <- myReactiveDat()
  ##Doses <- res$Doses
  
  ggplot(res, aes(x=Data, y= Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Semanas') +
    ylab('Doses')
})

#################################################################################################################

output$myPlot44 <- renderPlot({
  
  res <- VacinasReg()
  ggplot(res, aes(x=Nomes, y= Totais, fill = c6)) + 
    geom_bar(stat='identity')+ 
    scale_fill_manual(values = c("AZ"="#D8BFD8",
                                 "MOD"="#B0E0E6",
                                 "COM"="#FFDAB9",
                                 "JJ"="#c6ffb3")) +
    geom_text(aes(label=Totais), vjust = -0.6, size = 4)+
    theme(
      legend.position = "none",
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

output$myPlot43 <- renderPlot({
  
  resPie <- myReactivePies()
  
  val_names <- sprintf("%s (%s%%)", c("Não: ", "Sim: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
  
})

output$myPlot42 <- renderPlot({
  
  res <- myReactiveDat()
  ( ggplot(res, aes(x=Data, y=cumsum(round(Doses/1000)),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Semanas') 
    + ylab('Doses')
  )
})

output$myPlot41 <- renderPlot({
  
  res <- myReactiveDat()
  
  ggplot(res, aes(x=Data, y= Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Semanas') +
    ylab('Doses')
  
})

#################################################################################################################
output$myPlot54 <- renderPlot({
  
  res <- VacinasReg()
  ggplot(res, aes(x=Nomes, y= Totais, fill = c6)) + 
    geom_bar(stat='identity')+ 
    scale_fill_manual(values = c("AZ"="#D8BFD8",
                                 "MOD"="#B0E0E6",
                                 "COM"="#FFDAB9",
                                 "JJ"="#c6ffb3")) +
    geom_text(aes(label=Totais), vjust = -0.6, size = 4)+
    theme(
      legend.position = "none",
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

output$myPlot53 <- renderPlot({
  
  resPie <- myReactivePies()
  
  val_names <- sprintf("%s (%s%%)", c("Não: ", "Sim: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
  
})

output$myPlot52 <- renderPlot({
  
  res <- myReactiveDat()
  ( ggplot(res, aes(x=Data, y=cumsum(round(Doses/1000)),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Semanas') 
    + ylab('Doses')
  )
})

output$myPlot51 <- renderPlot({
  
  res <- myReactiveDat()
  
  ggplot(res, aes(x=Data, y= Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Semanas') +
    ylab('Doses')
})

#################################################################################################################
output$myPlot64 <- renderPlot({
  
  res <- VacinasReg()
  ggplot(res, aes(x=Nomes, y= Totais, fill = c6)) + 
    geom_bar(stat='identity')+ 
    scale_fill_manual(values = c("AZ"="#D8BFD8",
                                 "MOD"="#B0E0E6",
                                 "COM"="#FFDAB9",
                                 "JJ"="#c6ffb3")) +
    geom_text(aes(label=Totais), vjust = -0.6, size = 4)+
    theme(
      legend.position = "none",
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
output$myPlot63 <- renderPlot({
  
  resPie <- myReactivePies()
  
  val_names <- sprintf("%s (%s%%)", c("Não: ", "Sim: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
  
})

output$myPlot62 <- renderPlot({
  
  res <- myReactiveDat()
  
  ( ggplot(res, aes(x=Data, y=cumsum(round(Doses/1000)),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Semanas') 
    + ylab('Doses')
  )
})

output$myPlot61 <- renderPlot({
  
  res <- myReactiveDat()
  
  ggplot(res, aes(x=Data, y= Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Semanas') +
    ylab('Doses')
})

#################################################################################################################
output$myPlot74 <- renderPlot({
  
  res <- VacinasReg()
  ggplot(res, aes(x=Nomes, y= Totais, fill = c6)) + 
    geom_bar(stat='identity')+ 
    scale_fill_manual(values = c("AZ"="#D8BFD8",
                                 "MOD"="#B0E0E6",
                                 "COM"="#FFDAB9",
                                 "JJ"="#c6ffb3")) +
    geom_text(aes(label=Totais), vjust = -0.6, size = 4)+
    theme(
      legend.position = "none",
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
output$myPlot73 <- renderPlot({
  
  resPie <- myReactivePies()
  
  val_names <- sprintf("%s (%s%%)", c("Não: ", "Sim: "), resPie)
  names(resPie) <- val_names
  waffle::waffle(resPie)
  
})

output$myPlot72 <- renderPlot({
  
  res <- myReactiveDat()
  ( ggplot(res, aes(x=Data, y=cumsum(round(Doses/1000)),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Semanas') 
    + ylab('Doses')
  )
})

output$myPlot71 <- renderPlot({
  
  res <- myReactiveDat()
  
  ggplot(res, aes(x=Data, y= Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Semanas') +
    ylab('Doses')
})