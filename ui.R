library(leaflet)
library(shiny)

load("data/eadata.RData")

vars <- c(
  "Nueva" = "NUEVA",
  "Rural" = "RURAL",
  "PIE" = "PIE"
)

navbarPage("Escuelas Arriba", id="nav",
           tabPanel("Mapa interactivo",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      h2("Explorador de Datos"),
                                      h2(),
                                      selectInput("color", "Seleccione variable", vars)
                        ),
                        tags$div(id="cite",
                                 'Los datos provienen de las bases públicas del', tags$em(''), 'Centro de Estudios del Ministerio de Educación y JUNAEB (2021) .'
                        )
                    )
           ),
           
           tabPanel("Análisis",
                    sidebarPanel(
                      h2("Establecimientos inscritos por región"),
                      p("- A la fecha se han inscrito", strong("1.724"), "escuelas."),
                      p("- Regiones Metropolitana, Valparapiso y Maule registran mayor número de inscritos.")
                    ),
                    mainPanel(
                      plotOutput("plot1"), 
                      hr()
                    ),
                    sidebarPanel(
                      h2("Establecimientos inscritos nuevos por región"),
                      p("- A la fecha son", strong("563"), "las escuelas que por primera vez participarán del programa Escuelas Arriba."),
                      p("- Las regiones de Los Lagos, el Maule y Metropolitana son las que mayor número de escuelas nuevas inscritas registran.")
                    ),
                    mainPanel(
                      plotOutput("plot2"),
                      hr()
                    ),
                    sidebarPanel(
                      h2("Establecimientos inscritos por dependencia"),
                      p("- Cerca del", strong("60%"), "de los establecimientos inscritos son municipales."),
                      p("- En segundo lugar están las escuelas particulares subvencionadas (",strong("32%"),".")
                    ),
                    mainPanel(
                      plotOutput("plot3"),
                      hr()
                    ),
                    sidebarPanel(
                      h2("Establecimientos inscritos urbanos y rurales"),
                      p("- El",strong("69%"),"de los establecimientos inscritos son urbanos, mientras que el resto son rurales.")
                         ),
                    mainPanel(
                      plotOutput("plot4"),
                      hr()
                    ),
                    sidebarPanel(
                      h2("Establecimientos inscritos por categoría de desempeño"),
                      p("- El",strong("14%"),"de los establecimientos inscritos tienen categoría de desempeño Insuficiente."),
                      p("- Hasta el momento hay más presencia de establecimientos con categoría Medio",strong("49%"),".")
                      ),
                    mainPanel(
                      plotOutput("plot5"),
                      hr()
                    ),
                    sidebarPanel(
                      h2("Establecimientos inscritos y participación en EA"),
                      p("- Del total de escuelas inscritas, el",strong("63%"),"también participó el año 2021.")
                        ),
                    mainPanel(
                      plotOutput("plot6"),
                      hr()
                    )
           ),
           
           tabPanel("Base de Datos",
                    fluidRow(
                      column(3,
                             selectInput("region", "REGIÓN", eadata$REGIÓN, multiple=TRUE)
                             
                      ),
                      column(3,
                             conditionalPanel("input.region",
                                              selectInput("comuna", "COMUNA", eadata$COMUNA, multiple=TRUE)
                                              
                             )
                      ),
                      column(3,
                             conditionalPanel("input.region",
                                              selectInput("escuela", "ESTABLECIMIENTO", eadata$ESTABLECIMIENTO, multiple=TRUE)
                             )
                      )),
                    hr(),
                    DT::dataTableOutput("eatabla")
           ),
           
           conditionalPanel("false", icon("crosshair"))
           
)

