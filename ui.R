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
                    titlePanel("My Shiny App"),
                    sidebarLayout(
                      sidebarPanel(
                        h2("Installation"),
                        p("Shiny is available on CRAN, so you can install it in the usual way from your R console:"),
                        code('install.packages("shiny")'),
                        br(),
                        br(),
                        br(),
                        br(),
                        img(src = "rstudio.png", height = 70, width = 200),
                        br(),
                        "Shiny is a product of ", 
                        span("RStudio", style = "color:blue")
                      ),
                      mainPanel(
                        h1("Introducing Shiny"),
                        p("Shiny is a new package from RStudio that makes it ", 
                          em("incredibly easy "), 
                          "to build interactive web applications with R."),
                        br(),
                        p("For an introduction and live examples, visit the ",
                          a("Shiny homepage.", 
                            href = "http://shiny.rstudio.com")),
                        br(),
                        h2("Features"),
                        p("- Build useful web applications with only a few lines of code—no JavaScript required."),
                        p("- Shiny applications are automatically 'live' in the same way that ", 
                          strong("spreadsheets"),
                          " are live. Outputs change instantly as users modify inputs, without requiring a reload of the browser.")
                      ),
                      # Application title
                      titlePanel("Ramen Reviews"),
                      
                      # Bar Chart
                      plotOutput("bargraph")
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
