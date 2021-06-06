
library(bslib)
library(thematic)
library(shinycssloaders)

library(shinydashboard)

thematic::thematic_shiny()

shinyServer(
  
  fluidPage(theme = bs_theme(version = 4, bootswatch = "united"),
            
            includeCSS("www/styles.css"),
            
            navbarPage("",
                       
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
                       tabPanel("Regiões", fluid = TRUE,
                                tabsetPanel(id="tabs",
                                            tabPanel("Alentejo",fluid = TRUE,
                                                     pageWithSidebar(
                                                       headerPanel(""),
                                                       
                                                       sidebarPanel(
                                                         selectInput("grafico","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas","Percentagem de pessoas vacinadas")),
                                                         
                                                         
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
                                                         selectInput("grafico1","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas","Percentagem de pessoas vacinadas")),
                                                         
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
                                                         selectInput("grafico2","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas","Percentagem de pessoas vacinadas")),
                                                         
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
                                                         selectInput("grafico3","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas","Percentagem de pessoas vacinadas")),
                                                         
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
                                                         selectInput("grafico4","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas","Percentagem de pessoas vacinadas")),
                                                         
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
                                                         selectInput("grafico5","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas","Percentagem de pessoas vacinadas")),
                                                         
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
                                                         selectInput("grafico6","Selecione o tipo de gráfico:",c("Vacinação semanal","Acumulação de vacinas","Percentagem de pessoas vacinadas")),
                                                         
                                                       ),
                                                       
                                                       mainPanel(
                                                         tags$div(style="row", id="yo",checked=NA,
                                                                  tags$div(style="col-12",checked=NA,plotOutput("myPlot7")%>% withSpinner(color="#cccccc"))
                                                         )
                                                       )
                                                     )
                                            )
                                )
                       ),
                       navbarMenu("About",
                          tabPanel("Informação",
                                   fluidRow(
                                     column(6,
                                            includeMarkdown("www/info.md")),
                                     img(src="pt.png", height=600, width=800),
                                     "Depois melhorar esta imagem. Andei a lutar com a posição dela."
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
