library(bslib)
library(thematic)
library(shinycssloaders)
library(shinydashboard)
library(markdown)
library(rsconnect)


thematic::thematic_shiny()

shinyServer(
  
  fluidPage(theme = bs_theme(version = 4, bootswatch = "sandstone"),
            
            includeCSS("www/styles.css"),
            
            navbarPage("",id="tabs",
                       
                       tabPanel("Portugal", fluid = TRUE, pageWithSidebar(
                         headerPanel(""),
                         
                         sidebarPanel( 
                           h3("Vacinação COVID-19 em Portugal"),
                           ("Clique nas Checkboxes para vizualizar os gráficos que quer analisar."),br(""),
                           checkboxInput('grafico11','Pessoas totalmente vacinadas por laboratório e por idade (*1000)',TRUE),
                           checkboxInput('grafico22','Pessoas totalmente vacinadas por semana e por faixa etária',FALSE),
                           checkboxInput('grafico33','Vacinas administradas por laboratório',FALSE),
                           checkboxInput('grafico44','Vacinados',FALSE),
                           checkboxInput('grafico66','Pessoas totalmente vacinadas por semana e por região',FALSE),
                           
                         ),
                         
                         mainPanel(
                           
                           tags$div(style="row", id="yo",checked=NA,
                                    conditionalPanel(condition="input.grafico11 == 1",
                                                     tags$h3(style="col-12",'Pessoas totalmente vacinadas por laboratório e por idade (*1000)'),
                                                     tags$div(style="col-12",checked=NA,plotOutput("myPlotPais")%>% withSpinner(color="#cccccc"))),
                                    conditionalPanel(condition="input.grafico22 == 1",
                                                     div(
                                                       fluidRow(
                                                         tags$h3(style="col-12", "Pessoas totalmente vacinadas por semana e por faixa etária"),
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
                                                     tags$h3(style="col-12",'Vacinas administradas por laboratório'),
                                                     tags$div(style="col-12",checked=NA,plotOutput("myPlotPais2")%>% withSpinner(color="#cccccc"))),
                                    conditionalPanel(condition="input.grafico44 == 1", 
                                                     tags$h3(style="col-12",'Vacinados'),
                                                     tags$div(style="col-12",checked=NA,plotOutput("myPie")%>% withSpinner(color="#cccccc"))),
                                    conditionalPanel(condition="input.grafico66 == 1",
                                                     div(
                                                       fluidRow(
                                                         tags$h3(style="col-12", "Pessoas totalmente vacinadas por semana e por região"),
                                                         column(12, "Selecione que Regiões quer visualizar:"),
                                                         column(2,
                                                                checkboxInput('optionAlentejo', "Alentejo",TRUE),
                                                                checkboxInput('optionAlgarve', "Algarve",FALSE)),
                                                         column(2,
                                                                checkboxInput('optionAcores', "Açores",FALSE),
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
                                                     
                                    )),
                           
                         )
                       )
                       )   
                       ,
                       navbarMenu("Regiões",
                                  
                                  tabPanel("Alentejo",fluid = TRUE,
                                           tags$div(class="row",checked=NA,
                                                    tags$div(class="col",checked=NA,      
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col",style="text-align:center; margin-bottom:30px; background-color:grey; color:white;border-radius: 5px;",checked=NA,
                                                                               tags$h3('Alentejo',style="margin-bottom:25px"))),
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por semana'),
                                                                               tags$div(checked=NA,plotOutput("myPlotB1")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Valor acumulado de vacinas por semana (*1000)'),
                                                                               tags$div(checked=NA,plotOutput("myPlotB2")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinados'),
                                                                               tags$div(checked=NA,plotOutput("myPlotB3")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por laboratório'),
                                                                               tags$div(checked=NA,plotOutput("myPlotB4")%>% withSpinner(color="#cccccc"))
                                                                      ))
                                                    )  )  
                                  ),
                                  tabPanel("Algarve", fluid = TRUE,
                                           tags$div(class="row",checked=NA,
                                                    tags$div(class="col",checked=NA,      
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col",style="text-align:center; margin-bottom:30px; background-color:grey; color:white;border-radius: 5px;",checked=NA,
                                                                               tags$h3('Algarve',style="margin-bottom:25px"))),
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por semana'),
                                                                               tags$div(checked=NA,plotOutput("myPlot21")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Valor acumulado de vacinas por semana (*1000)'),
                                                                               tags$div(checked=NA,plotOutput("myPlot22")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinados'),
                                                                               tags$div(checked=NA,plotOutput("myPlot23")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por laboratório'),
                                                                               tags$div(checked=NA,plotOutput("myPlot24")%>% withSpinner(color="#cccccc"))
                                                                      ))
                                                    ))  
                                  ),
                                  tabPanel("Lisboa", fluid = TRUE,
                                           tags$div(class="row",checked=NA,
                                                    tags$div(class="col",checked=NA,      
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col",style="text-align:center; margin-bottom:30px; background-color:grey; color:white;border-radius: 5px;",checked=NA,
                                                                               tags$h3('Lisboa e Vale do Tejo',style="margin-bottom:25px"))),
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por semana'),
                                                                               tags$div(checked=NA,plotOutput("myPlot31")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Valor acumulado de vacinas por semana (*1000)'),
                                                                               tags$div(checked=NA,plotOutput("myPlot32")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinados'),
                                                                               tags$div(checked=NA,plotOutput("myPlot33")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por laboratório'),
                                                                               tags$div(checked=NA,plotOutput("myPlot34")%>% withSpinner(color="#cccccc"))
                                                                      )))
                                           )  
                                  ),
                                  tabPanel("Centro", fluid = TRUE,
                                           tags$div(class="row",checked=NA,
                                                    tags$div(class="col",checked=NA,      
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col",style="text-align:center; margin-bottom:30px; background-color:grey; color:white;border-radius: 5px;",checked=NA,
                                                                               tags$h3('Centro',style="margin-bottom:25px"))),
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por semana'),
                                                                               tags$div(checked=NA,plotOutput("myPlot41")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Valor acumulado de vacinas por semana (*1000)'),
                                                                               tags$div(checked=NA,plotOutput("myPlot42")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinados'),
                                                                               tags$div(checked=NA,plotOutput("myPlot43")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por laboratório'),
                                                                               tags$div(checked=NA,plotOutput("myPlot44")%>% withSpinner(color="#cccccc"))
                                                                      )))
                                           )
                                  ),
                                  tabPanel("Acores", fluid = TRUE,
                                           tags$div(class="row",checked=NA,
                                                    tags$div(class="col",checked=NA,      
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col",style="text-align:center; margin-bottom:30px; background-color:grey; color:white;border-radius: 5px;",checked=NA,
                                                                               tags$h3('Região Autónoma dos Açores',style="margin-bottom:25px"))),
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por semana'),
                                                                               tags$div(checked=NA,plotOutput("myPlot51")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Valor acumulado de vacinas por semana (*1000)'),
                                                                               tags$div(checked=NA,plotOutput("myPlot52")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinados'),
                                                                               tags$div(checked=NA,plotOutput("myPlot53")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por laboratório'),
                                                                               tags$div(checked=NA,plotOutput("myPlot54")%>% withSpinner(color="#cccccc"))
                                                                      )))
                                           )
                                  ),
                                  tabPanel("Madeira", fluid = TRUE,
                                           tags$div(class="row",checked=NA,
                                                    tags$div(class="col",checked=NA,      
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col",style="text-align:center; margin-bottom:30px; background-color:grey; color:white;border-radius: 5px;",checked=NA,
                                                                               tags$h3('Região Autónoma da Madeira',style="margin-bottom:25px"))),
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por semana'),
                                                                               tags$div(checked=NA,plotOutput("myPlot61")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Valor acumulado de vacinas por semana (*1000)'),
                                                                               tags$div(checked=NA,plotOutput("myPlot62")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinados'),
                                                                               tags$div(checked=NA,plotOutput("myPlot63")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por laboratório'),
                                                                               tags$div(checked=NA,plotOutput("myPlot64")%>% withSpinner(color="#cccccc"))
                                                                      )))
                                           )
                                  ),
                                  tabPanel("Norte", fluid = TRUE,
                                           tags$div(class="row",checked=NA,
                                                    tags$div(class="col",checked=NA,      
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col",style="text-align:center; margin-bottom:30px; background-color:grey; color:white;border-radius: 5px;",checked=NA,
                                                                               tags$h3('Norte',style="margin-bottom:25px"))),
                                                             tags$div(class="row",checked=NA,
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por semana'),
                                                                               tags$div(checked=NA,plotOutput("myPlot71")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Valor acumulado de vacinas por semana (*1000)'),
                                                                               tags$div(checked=NA,plotOutput("myPlot72")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinados'),
                                                                               tags$div(checked=NA,plotOutput("myPlot73")%>% withSpinner(color="#cccccc"))
                                                                      ),
                                                                      tags$div(class="col-md-6 col-sm-6 col-12",checked=NA,tags$h3('Vacinas administradas por laboratório'),
                                                                               tags$div(checked=NA,plotOutput("myPlot74")%>% withSpinner(color="#cccccc"))
                                                                      )))
                                           )
                                           
                                  )
                       ),
                       navbarMenu("About",
                                  tabPanel("Informação",
                                           tags$div(class="row",checked=NA,
                                                    tags$div(class="col-md-4 col-sm-6 col-12",checked=NA,
                                                             includeMarkdown("www/info.md")
                                                    ),
                                                    tags$div(class="col-md-4 col-sm-6 col-12",style="margin-bottom: auto; margin-top: auto; margin-left: auto; margin-right: auto;",checked=NA,
                                                             img(src="pt.png", height=400, width=600)
                                                    )
                                           )),
                                  tabPanel("Autoria",
                                           fluidRow(
                                             column(12,
                                                    includeMarkdown("www/autoria.md"))
                                           ))
                       )
            ),
            tags$br(),
            tags$div(style="col-6", class="info", h4("Notas:"),
                     tags$b("Pessoa Vacinada: "), p("Individuo ao qual foi administrada a totalidade de doses."), 
                     tags$b("Doses por vacina: "), 
                     tags$ul(
                       tags$li("Comirnaty (BioNTech/Pfizer)- 2 doses"), 
                       tags$li("Moderna (Moderna)- 2 doses"),
                       tags$li("AstraZeneca (AstraZeneca/Oxford)- 2 doses"),
                       tags$li("Janssen (Janssen/Jonhson & Jonhson) - 1 dose"),
                     ),
                     tags$b("Dose Administrada:"), p("Dose de vacina administrada independentemente de ser a 1ª ou 2ª dose."),
                     
            ),
  )
)
