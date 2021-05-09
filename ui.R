
library(bslib)
library(thematic)
library(shinycssloaders)



thematic::thematic_shiny()

shinyServer(
  
  fluidPage(theme = bs_theme(version = 4, bootswatch = "united"),
            
            navbarPage("",
                       tabPanel("Portugal", fluid = TRUE,pageWithSidebar(
                         headerPanel(""),
                         
                         sidebarPanel(
                           checkboxInput('grafico11','Totalidade de pessoas vacinadas,tendo em conta a sua faixa etária e marca da vacina',TRUE),
                           checkboxInput('grafico22','Quantidade de pessoas totalmente vacinadas por semana, agrupadas por faixa etária',FALSE),
                           checkboxInput('grafico33','Número de doses de cada tipo de vacina administradas',FALSE),
                           checkboxInput('grafico44','Percentagem de pessoas vacinadas/não vacinadas',FALSE),
                           
                         ),
                         
                         mainPanel(
                           tags$div(style="row", id="yo",checked=NA,
                                    conditionalPanel(condition="input.grafico11 == 1",
                                                     tags$h3(style="col-12",'Quantidade de pessoas totalmente vacinadas por marca de vacina, tendo em conta a sua faixa etária'),
                                                     tags$div(style="col-12",checked=NA,plotOutput("myPlotPais")%>% withSpinner(color="#cccccc"))),
                                    conditionalPanel(condition="input.grafico22 == 1",
                                                        div(
                                                            fluidRow(
                                                                tags$h3(style="col-12", "Quantidade de pessoas totalmente vacinadas por semana, agrupadas por faixa etária"),
                                                                column(12, "Selecione que faixas etárias quer visualizar:"),
                                                                column(2,
                                                                    checkboxInput('option18_24', "18 - 24",TRUE),
                                                                    checkboxInput('option25_49', "25 - 49",FALSE)),
                                                                column(2,
                                                                    checkboxInput('option50_59', "50 - 59",FALSE),
                                                                    checkboxInput('option60_69', "60 - 69",FALSE)),
                                                                column(2,
                                                                    checkboxInput('option70_79', "70 - 79",FALSE),
                                                                    checkboxInput('option80', "80+",FALSE)),
                                                                column(2,""),
                                                                column(4, "Incluir contagem?",
                                                                       radioButtons("radioOption", "",
                                                                                    c("Sim" = "yup",
                                                                                      "Não" = "nop")))
                                                                
                                                            )
                                                        ),
                                                     tags$div(style="col-12",checked=NA,plotOutput("plot1824")%>% withSpinner(color="#cccccc")),
                                                    actionButton("ajuda", "Ajuda",style="color: #fff; background-color: #337ab7; border-color: #2e6da4;"),
                                    ),
                                    conditionalPanel(condition="input.grafico33 == 1",
                                                     tags$h3(style="col-12",'Quantidades de vacinas administradas, por marcas de vacinas'),
                                                     tags$div(style="col-12",checked=NA,plotOutput("myPlotPais2")%>% withSpinner(color="#cccccc"))),
                                    conditionalPanel(condition="input.grafico44 == 1",
                                                     tags$div(style="col-12",checked=NA,plotOutput("myPie")%>% withSpinner(color="#cccccc"))))
                           
                         )
                         #plotOutput("myPlot")
                       )
                       )   
                       ,
                       tabPanel("Regiões", fluid = TRUE,
                                tabsetPanel(id="tabs",
                                            tabPanel("Alentejo",fluid = TRUE,
                                                     pageWithSidebar(
                                                       headerPanel(""),
                                                       
                                                       sidebarPanel(
                                                         selectInput("grafico","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas")),
                                                         
                                                         
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$div(style="row",checked=NA,
                                                                  tags$div(style="col-12",checked=NA,plotOutput("myPlotB")%>% withSpinner(color="#cccccc"))
                                                         )
                                                         #plotOutput("myPlot")
                                                       )
                                                     )
                                                     
                                            ),
                                            tabPanel("Algarve", fluid = TRUE,
                                                     pageWithSidebar(
                                                       headerPanel(""),
                                                       
                                                       sidebarPanel(
                                                         selectInput("grafico1","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas")),
                                                        
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$div(style="row", id="yo",checked=NA,
                                                                  tags$div(style="col-12",checked=NA,plotOutput("myPlot2")%>% withSpinner(color="#cccccc"))
                                                         )
                                                         #plotOutput("myPlot")
                                                       )
                                                     )
                                            ),
                                            tabPanel("Lisboa", fluid = TRUE,
                                                     pageWithSidebar(
                                                       headerPanel(""),
                                                       
                                                       sidebarPanel(
                                                         selectInput("grafico2","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas")),
                                                         
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$div(style="row", id="yo",checked=NA,
                                                                  tags$div(style="col-12",checked=NA,plotOutput("myPlot3")%>% withSpinner(color="#cccccc"))
                                                         )
                                                         #plotOutput("myPlot")
                                                       )
                                                     )
                                            ),
                                            tabPanel("Centro", fluid = TRUE,
                                                     pageWithSidebar(
                                                       headerPanel(""),
                                                       
                                                       sidebarPanel(
                                                         selectInput("grafico3","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas")),
                                                         
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$div(style="row", id="yo",checked=NA,
                                                                  tags$div(style="col-12",checked=NA,plotOutput("myPlot4")%>% withSpinner(color="#cccccc"))
                                                         )
                                                         #plotOutput("myPlot")
                                                       )
                                                     )
                                            ),
                                            tabPanel("Açores", fluid = TRUE,
                                                     pageWithSidebar(
                                                       headerPanel(""),
                                                       
                                                       sidebarPanel(
                                                         selectInput("grafico4","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas")),
                                                         
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$div(style="row", id="yo",checked=NA,
                                                                  tags$div(style="col-12",checked=NA,plotOutput("myPlot5")%>% withSpinner(color="#cccccc"))
                                                         )
                                                         #plotOutput("myPlot")
                                                       )
                                                     )
                                            ),
                                            tabPanel("Madeira", fluid = TRUE,
                                                     pageWithSidebar(
                                                       headerPanel(""),
                                                       
                                                       sidebarPanel(
                                                         selectInput("grafico5","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas")),
                                                         
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$div(style="row", id="yo",checked=NA,
                                                                  tags$div(style="col-12",checked=NA,plotOutput("myPlot6")%>% withSpinner(color="#cccccc"))
                                                         )
                                                         #plotOutput("myPlot")
                                                       )
                                                     )
                                            ),
                                            tabPanel("Norte", fluid = TRUE,
                                                     pageWithSidebar(
                                                       headerPanel(""),
                                                       
                                                       sidebarPanel(
                                                         selectInput("grafico6","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas")),
                                                        
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$div(style="row", id="yo",checked=NA,
                                                                  tags$div(style="col-12",checked=NA,plotOutput("myPlot7")%>% withSpinner(color="#cccccc"))
                                                         )
                                                         #plotOutput("myPlot")
                                                       )
                                                     )
                                            )
                                )
                       )
            )
  )
)
