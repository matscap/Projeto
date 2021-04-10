library(shinythemes)
shinyServer(
  fluidPage(theme = shinytheme("superhero"),
    tags$div(class="row",tags$div(class="col",style="text-align:center",tags$h1("Vacinação em Portugal"))),
    
    tags$div(class="row;", id="yo",checked=NA,
             tags$div(class="col-md-3 col",checked=NA,
                      tags$h1(""),tags$div(class="col",checked=NA,img(src = "mapaportugal.png",style="width:80%;height:80%"))
                      ),
             tags$div(class="col-md-9 col",checked=NA,
                      pageWithSidebar(
                          headerPanel(""),
                          
                          sidebarPanel(
                            selectInput("opcao","Selecione o filtro:",c("Por região","Por idades")),
                            conditionalPanel(condition="input.opcao == 'Por região'",
                                selectInput("Regiao","Selecione a região:",
                                        choices=c("ALENTEJO","ALGARVE","NORTE","MADEIRA","AZORES","LISBOA")),
                                tags$h4("Opções"),
                                selectInput("grafico","Selecione o filtro:",c("Vacinação semanal","Acumulação de vacinas"))),
                            conditionalPanel(condition="input.opcao == 'Por idades'",
                                 selectInput("fetaria","Selecione a faixa etária:",
                                             choices=c("Age18_24","Age25_49","Age50_59","Age60_69","Age70_79","Age80+")),
                                 tags$h4("Opções"),
                                 selectInput("grafico","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas")))
                          ),
                          
                          mainPanel(
                            tags$div(style="row", id="yo",checked=NA,
                                     tags$div(style="col-12",id="graph1",checked=NA,plotOutput("myPlot"))
                                     )
                            #plotOutput("myPlot")
                          )
                      )
            )
    )
  )
)
