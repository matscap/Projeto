
library(bslib)
library(thematic)
library(shinycssloaders)

library(shinydashboard)

thematic::thematic_shiny()

shinyServer(
  
  fluidPage(theme = bs_theme(version = 4, bootswatch = "united"),
            
            includeCSS("www/styles.css"),
            
            navbarPage("",id="tabs",
                       
                       tabPanel("Portugal", fluid = TRUE, pageWithSidebar(
                         headerPanel(""),
                         
                         sidebarPanel( 
                           h3("Vacinação COVID-19 em Portugal"),
                           ("Clique nas Checkboxes para vizualizar os gráficos que quer analisar."),br(""),
                           checkboxInput('grafico11','Totalidade de pessoas vacinadas,tendo em conta a sua faixa etária e marca da vacina',TRUE),
                           checkboxInput('grafico22','Quantidade de pessoas totalmente vacinadas por semana, agrupadas por faixa etária',FALSE),
                           checkboxInput('grafico33','Número de doses de cada tipo de vacina administradas',FALSE),
                           checkboxInput('grafico44','Percentagem de pessoas vacinadas/não vacinadas',FALSE),
                           checkboxInput('grafico55','Evolução do número de vacinas administradas por região',FALSE),
                           checkboxInput('grafico66','Quantidade de pessoas totalmente vacinadas ,por semana, em cada Região',FALSE),
                           
                         ),
                         
                         mainPanel(
                           
                           tags$div(style="row", id="yo",checked=NA,
                                    conditionalPanel(condition="input.grafico66 == 1",
                                                     div(
                                                       fluidRow(
                                                         tags$h3(style="col-12", "Quantidade de pessoas totalmente vacinadas por semana, agrupadas por Região"),
                                                         column(12, "Selecione que Regiões quer visualizar:"),
                                                         column(2,
                                                                checkboxInput('optionAlentejo', "Alentejo",TRUE),
                                                                checkboxInput('optionAlgarve', "Algarve",FALSE)),
                                                         column(2,
                                                                checkboxInput('optionAçores', "Açores",FALSE),
                                                                checkboxInput('optionCentro', "Centro",FALSE)),
                                                         column(2,
                                                                checkboxInput('optionLisboa', "Lisboa",FALSE),
                                                                checkboxInput('optionMadeira', "Madeira",FALSE)),
                                                         column(2,
                                                                checkboxInput('optionNorte',"Norte",FALSE)),
                                                         column(4, "Incluir contagem?",
                                                                radioButtons("radioOption1", "",
                                                                             c("Sim" = "yup1",
                                                                               "Não" = "nop1")))
                                                         
                                                       )
                                                     ),
                                                     tags$div(style="col-12",checked=NA,plotOutput("plotRegiao")%>% withSpinner(color="#cccccc")),
                                                     
                                    ),
                                    conditionalPanel(condition="input.grafico55 == 1",
                                                     tags$h3(style="col-12",''),
                                                     tags$div(style="col-12",checked=NA,plotOutput("myEvolucao")%>% withSpinner(color="#cccccc"))),
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
                                                     tags$h3(style="col-12",'Percentagem de pessoas vacinadas/não vacinadas'),
                                                     tags$div(style="col-12",checked=NA,plotOutput("myPie")%>% withSpinner(color="#cccccc")))),
                           
                          )
                       )
                       )   
                       ,
                       navbarMenu("Regiões",
                                
                                            tabPanel("Alentejo",fluid = TRUE,
                                                     tags$div(class="row",checked=NA,
                                                       tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                tags$div(checked=NA,plotOutput("myPlotB1")%>% withSpinner(color="#cccccc"))
                                                       ),
                                                       tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                tags$div(checked=NA,plotOutput("myPlotB2")%>% withSpinner(color="#cccccc"))
                                                       ),
                                                       tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                tags$div(checked=NA,plotOutput("myPlotB3")%>% withSpinner(color="#cccccc"))
                                                       )
                                                     )    
                                            ),
                                            tabPanel("Algarve", fluid = TRUE,
                                                     tags$div(class="row",checked=NA,
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot21")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot22")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot23")%>% withSpinner(color="#cccccc"))
                                                              )
                                                     )  
                                            ),
                                            tabPanel("Lisboa", fluid = TRUE,
                                                     tags$div(class="row",checked=NA,
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot31")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot32")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot33")%>% withSpinner(color="#cccccc"))
                                                              )
                                                     )  
                                            ),
                                            tabPanel("Centro", fluid = TRUE,
                                                     tags$div(class="row",checked=NA,
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot41")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot42")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot43")%>% withSpinner(color="#cccccc"))
                                                              )
                                                     )
                                            ),
                                            tabPanel("Açores", fluid = TRUE,
                                                     tags$div(class="row",checked=NA,
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot51")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot52")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot53")%>% withSpinner(color="#cccccc"))
                                                              )
                                                     )
                                            ),
                                            tabPanel("Madeira", fluid = TRUE,
                                                     tags$div(class="row",checked=NA,
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot61")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot62")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot63")%>% withSpinner(color="#cccccc"))
                                                              )
                                                     )
                                            ),
                                            tabPanel("Norte", fluid = TRUE,
                                                     tags$div(class="row",checked=NA,
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot71")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot72")%>% withSpinner(color="#cccccc"))
                                                              ),
                                                              tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                                       tags$div(checked=NA,plotOutput("myPlot73")%>% withSpinner(color="#cccccc"))
                                                              )
                                                     )
                                            
                                )
                       ),
                       navbarMenu("About",
                          tabPanel("Informação",
                                   tags$div(class="row",checked=NA,
                                            tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                     includeMarkdown("www/info.md")
                                            ),
                                            tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                     img(src="pt.png", height=600, width=800)
                                            )
                                   )),
                          tabPanel("Autoria",
                                   fluidRow(
                                     column(12,
                                            includeMarkdown("www/autoria.md"))
                                   ))
                       )
            ),
            #tags$br(),
            #tags$div( class="info", "Licenciatura em Ciências da Computação, Universidade do Minho, 2021/21"),
  )
)
