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
                                      h5("Explorador de Datos"),
                                      selectInput("color", "Categoría", vars),
                                      selectizeInput(
                                        "region", label=h5("Seleccione región:",
                                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), choices=c("XV - Arica y Parinacota","I - Tarapaca","II - Antofagasta","III - Atacama","IV - Coquimbo",
                                       "V - Valparaiso","XIII - Metropolitana","VI - Libertador General Bernardo O'Higgins","VII - Maule",
                                       "XVI - Ñuble", "VIII - Biobio", "IX - Araucania","XIV - Los Rios","X - Los Lagos", 
                                       "XI - Aysen", "XII - Magallanes"), 
                                       selected="XIII - Metropolitana", width="90%"),
                                      h2(),
                                      h4("EE Inscritos: 1.724"),
                                      h2(),
                                      h4("EE Nuevos: 563"),
                                      h2(),
                                      h4("EE Antiguos: 1.146"),
                                      h2(),
                                      h4("EE Urbanos: 1.181"),
                                      h2(),
                                      h4("EE Rurales: 583"),
                                      h2(),
                                      h4("EE Convenio PIE: 1.361"),
                                      h2(),
                                      h4("Matrícula EA: 503.537")
                        ),
                        tags$div(id="cite",
                                 'Los datos provienen de las bases públicas del', tags$em(''), 'Centro de Estudios del Ministerio de Educación y JUNAEB (2021) .'
                        )
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
