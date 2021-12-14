library(leaflet)
library(shiny)

setwd("C:/Users/alvaro.romero/Escritorio/EA/EA")

# Choices for drop-downs
load("data/eadata.RData")

vars <- c(
  "Escuela Nueva" = "EA_2021",
  "Matrícula" = "MATRÍCULA",
  "Rural" = "RURAL"
)

ui <- navbarPage("Escuelas Arriba", id="nav",
           
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
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "EA_2021")
                        ),
                        
                        tags$div(id="cite",
                                 'Datos compilados a través del', tags$em(''), ' Ministerio de Educación, por Álvaro Romero (2021).'
                        )
                    )
           ),
           
           tabPanel("Explorador de Datos",
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
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("eatabla")
                     ),
           
           conditionalPanel("false", icon("crosshair"))
)
