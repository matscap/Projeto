shinyServer(
  fluidPage(
    pageWithSidebar(
      headerPanel("Vacinas"),
      
      sidebarPanel(
        selectInput("Regiao","Selecione a região:",
                    choices=c("ALENTEJO","ALGARVE","NORTE","MADEIRA","AZORES","LISBOA")),
        checkboxInput("CHECKBOX","Evolução de doses dadas/Doses por semana", FALSE), 
      ),
      
      mainPanel(
        plotOutput("myPlot")
      )
      
    )
  )
)
