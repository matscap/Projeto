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
                        checkboxInput('grafico11','Total de pessoas vacinadas por faixa etária',TRUE),
                        checkboxInput('grafico22','Quantidade de pessoas vacinadas por faixas etárias, por semana',FALSE),
                        checkboxInput('grafico33','Número de doses de cada tipo de vacina administradas',FALSE),
                        checkboxInput('grafico44','Percentagem de pessoas vacinadas/não vacinadas',FALSE),
                        
                      ),
                      
                      mainPanel(
                        tags$div(style="row", id="yo",checked=NA,
                                 conditionalPanel(condition="input.grafico11 == 1",
                                                  tags$div(style="col-12",checked=NA,plotOutput("myPlotPais")%>% withSpinner(color="#cccccc"))),
                                 conditionalPanel(condition="input.grafico22 == 1",
                                                  actionButton("ajuda", "Ajuda",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                                  tags$div(style="col-12",checked=NA,plotOutput("plot1824")%>% withSpinner(color="#cccccc"))),
                                 conditionalPanel(condition="input.grafico33 == 1",
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
                                                       radioButtons("radiovac1", "Gráfico:",
                                                                    c("Moderna" = "mod",
                                                                      "AstraZeneca" = "az",
                                                                      "Pfizer"="pf"))
                                                  
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
                                                radioButtons("radiovac2", "Gráfico:",
                                                             c("Moderna" = "mod",
                                                               "AstraZeneca" = "az",
                                                               "Pfizer"="pf"))
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
                                                radioButtons("radiovac3", "Gráfico:",
                                                             c("Moderna" = "mod",
                                                               "AstraZeneca" = "az",
                                                               "Pfizer"="pf"))
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
                                                radioButtons("radiovac4", "Gráfico:",
                                                             c("Moderna" = "mod",
                                                               "AstraZeneca" = "az",
                                                               "Pfizer"="pf"))
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
                                                radioButtons("radiovac5", "Gráfico:",
                                                             c("AstraZeneca" = "az",
                                                               "Pfizer"="pf"))
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
                                                radioButtons("radiovac6", "Gráfico:",
                                                             c("AstraZeneca" = "az",
                                                               "Pfizer"="pf"))
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
                                                radioButtons("radiovac7", "Gráfico:",
                                                             c("Moderna" = "mod",
                                                               "AstraZeneca" = "az",
                                                               "Pfizer"="pf"))
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
