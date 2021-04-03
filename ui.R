shinyServer(
  fluidPage(
    pageWithSidebar(
      headerPanel("Vacinas"),
      
      sidebarPanel(
        selectInput("Regiao","Selecione a região:",
                    choices=c("ALENTEJO","ALGARVE","NORTE","MADEIRA","AZORES","LISBOA")),
      ),
      
      mainPanel(
        plotOutput("myPlot")
      )
      
    )
  )
)