shinyServer(
  fluidPage(
    
    tags$div(class="row;", id="yo",checked=NA,
             tags$div(class="col-md-3 col",checked=NA,
                      tags$h1("País"),tags$div(class="col",checked=NA,img(src = "mapaportugal.png",style="width:80%;height:80%"))
                      ),
             tags$div(class="col-md-9 col",checked=NA,
                      pageWithSidebar(
                          headerPanel(""),
                          
                          sidebarPanel(
                            selectInput("Regiao","Selecione a região:",
                                        choices=c("ALENTEJO","ALGARVE","NORTE","MADEIRA","AZORES","LISBOA")),
                          ),
                          
                          mainPanel(
                            tags$div(style="row", id="yo",checked=NA,
                                     tags$div(style="col-12",id="graph1",checked=NA,plotOutput("myPlot")),
                                     tags$div(style="col-12",id="graph2",checked=NA,plotOutput("myPlot2")))
                            #plotOutput("myPlot")
                          )
                      )
            )
    )
  )
)
