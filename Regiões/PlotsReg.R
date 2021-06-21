##################################################################################
#        Alentejo          #
##################################################################################
output$myPlotB3 <- renderPlot({
  
  ##res <- myReactivePies()
  resPie <- myReactivePies()
  ##parts <- c(One=resPie['Percentagem_tpv4'], Two=resPie['Percentagem_tpv2'], Three=resPie['Percentagem_tpv3'])
  ##waffle(parts, rows=8)
  ##waffle(resPie, rows=5,keep=TRUE)
  ##waffle(resPie, rows = 5)
  
  val_names <- sprintf("%s (%s)", c("Por vacinar: ", "Completamente Vacinados: ", "Primeira dose apenas: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
  
})
output$myPlotB2 <- renderPlot({
  
  res <- myReactiveDat()
  
  (ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses')
    
  )
})

output$myPlotB1 <- renderPlot({
  
  res <- myReactiveDat()
  Doses <- res$Doses
  
  ggplot(res$df2, aes(x=res$Data, y= res$Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Week') +
    ylab('Doses')
})

##################################################################################
#        Algarve          #
##################################################################################

output$myPlot23 <- renderPlot({
  
  ##res <- myReactivePies()
  resPie <- myReactivePies()
  ##parts <- c(One=resPie['Percentagem_tpv4'], Two=resPie['Percentagem_tpv2'], Three=resPie['Percentagem_tpv3'])
  ##waffle(parts, rows=8)
  ##waffle(resPie, rows=5,keep=TRUE)
  ##waffle(resPie, rows = 5)
  
  val_names <- sprintf("%s (%s)", c("Por vacinar: ", "Completamente Vacinados: ", "Primeira dose apenas: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
})

output$myPlot22 <- renderPlot({
  
  res <- myReactiveDat()
  
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses')
  )
  
  ##########################################################################
  
})

output$myPlot21 <- renderPlot({
  
  res <- myReactiveDat()
  Doses <- res$Doses
  
  ggplot(res$df2, aes(x=res$Data, y= res$Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Week') +
    ylab('Doses')
})


##############################################################################################################

output$myPlot33 <- renderPlot({
  ##res <- myReactivePies()
  resPie <- myReactivePies()
  ##parts <- c(One=resPie['Percentagem_tpv4'], Two=resPie['Percentagem_tpv2'], Three=resPie['Percentagem_tpv3'])
  ##waffle(parts, rows=8)
  ##waffle(resPie, rows=5,keep=TRUE)
  ##waffle(resPie, rows = 5)
  
  val_names <- sprintf("%s (%s)", c("Por vacinar: ", "Completamente Vacinados: ", "Primeira dose apenas: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
})

output$myPlot32 <- renderPlot({
  
  res <- myReactiveDat()
  
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses')
  )
  
})

output$myPlot31 <- renderPlot({
  
  res <- myReactiveDat()
  Doses <- res$Doses
  
  ggplot(res$df2, aes(x=res$Data, y= res$Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Week') +
    ylab('Doses')
})

#################################################################################################################

output$myPlot43 <- renderPlot({
  
  ##res <- myReactivePies()
  resPie <- myReactivePies()
  ##parts <- c(One=resPie['Percentagem_tpv4'], Two=resPie['Percentagem_tpv2'], Three=resPie['Percentagem_tpv3'])
  ##waffle(parts, rows=8)
  ##waffle(resPie, rows=5,keep=TRUE)
  ##waffle(resPie, rows = 5)
  
  val_names <- sprintf("%s (%s)", c("Por vacinar: ", "Completamente Vacinados: ", "Primeira dose apenas: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
  
})

output$myPlot42 <- renderPlot({
  
  res <- myReactiveDat()
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses')
  )
})

output$myPlot41 <- renderPlot({
  
  res <- myReactiveDat()
  Doses <- res$Doses
  
  ggplot(res$df2, aes(x=res$Data, y= res$Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Week') +
    ylab('Doses')
  
})

#################################################################################################################

output$myPlot53 <- renderPlot({
  
  ##res <- myReactivePies()
  resPie <- myReactivePies()
  ##parts <- c(One=resPie['Percentagem_tpv4'], Two=resPie['Percentagem_tpv2'], Three=resPie['Percentagem_tpv3'])
  ##waffle(parts, rows=8)
  ##waffle(resPie, rows=5,keep=TRUE)
  ##waffle(resPie, rows = 5)
  
  val_names <- sprintf("%s (%s)", c("Por vacinar: ", "Completamente Vacinados: ", "Primeira dose apenas: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
  
})

output$myPlot52 <- renderPlot({
  
  res <- myReactiveDat()
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses')
  )
})

output$myPlot51 <- renderPlot({
  
  res <- myReactiveDat()
  Doses <- res$Doses
  
  ggplot(res$df2, aes(x=res$Data, y= res$Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Week') +
    ylab('Doses')
})

#################################################################################################################

output$myPlot63 <- renderPlot({
  
  ##res <- myReactivePies()
  resPie <- myReactivePies()
  ##parts <- c(One=resPie['Percentagem_tpv4'], Two=resPie['Percentagem_tpv2'], Three=resPie['Percentagem_tpv3'])
  ##waffle(parts, rows=8)
  ##waffle(resPie, rows=5,keep=TRUE)
  ##waffle(resPie, rows = 5)
  
  val_names <- sprintf("%s (%s)", c("Por vacinar: ", "Completamente Vacinados: ", "Primeira dose apenas: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
  
})

output$myPlot62 <- renderPlot({
  
  res <- myReactiveDat()
  
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses')
  )
})

output$myPlot61 <- renderPlot({
  
  res <- myReactiveDat()
  Doses <- res$Doses
  
  ggplot(res$df2, aes(x=res$Data, y= res$Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Week') +
    ylab('Doses')
})

#################################################################################################################

output$myPlot73 <- renderPlot({
  
  ##res <- myReactivePies()
  resPie <- myReactivePies()
  ##parts <- c(One=resPie['Percentagem_tpv4'], Two=resPie['Percentagem_tpv2'], Three=resPie['Percentagem_tpv3'])
  ##waffle(parts, rows=8)
  ##waffle(resPie, rows=5,keep=TRUE)
  ##waffle(resPie, rows = 5)
  
  val_names <- sprintf("%s (%s)", c("Por vacinar: ", "Completamente Vacinados: ", "Primeira dose apenas: "), resPie)
  names(resPie) <- val_names
  
  waffle::waffle(resPie)
  
})

output$myPlot72 <- renderPlot({
  
  res <- myReactiveDat()
  ( ggplot(res$df2, aes(x=res$Data, y=cumsum(res$Doses),group=1)) 
    + geom_line(color="#63ACAA")
    + geom_point(color="#507D94") 
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            axis.title = element_text(face="bold", size=18),
            title = element_text(size = 20))  
    + xlab('Week') 
    + ylab('Doses')
  )
})

output$myPlot71 <- renderPlot({
  
  res <- myReactiveDat()
  Doses <- res$Doses
  
  ggplot(res$df2, aes(x=res$Data, y= res$Doses,fill=Doses)) + 
    geom_bar(stat='identity')+ 
    scale_fill_gradient(low="#63ACAA",high="#507D94")+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
          axis.title = element_text(face="bold", size=18),
          title = element_text(size = 20)) + 
    xlab('Week') +
    ylab('Doses')
})