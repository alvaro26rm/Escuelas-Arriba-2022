library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(tidyverse)


server <- function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()%>%
      setView(lng = -70.76293, lat = -33.52341, zoom = 11)
    
  })
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    
    if (colorBy == "EA_2021") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(eadata$EA_2021 == "SI", "SI", "NO")
      pal <- colorFactor(c("#1f78b4", "#e6550d"), colorData)
    }
     else {
      colorData <- eadata[[colorBy]]
      pal <- colorFactor(c("#1f78b4", "#e6550d"), colorData)
    }
  
    leafletProxy("map", data = eadata) %>%
      clearShapes() %>%
      addCircles(~LONGITUD, ~LATITUD, radius=500, layerId=~RBD,
                 stroke=FALSE, fillOpacity=0.7, fillColor=pal(colorData)) %>%
      addLegend("bottomright", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  observe({
    colorBy <- input$color
    
    if (colorBy == "RURAL") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(eadata$RURAL == "1", "SI", "NO")
      pal <- colorFactor(c("#1f78b4", "#e6550d"), colorData)
    }
    else {
      colorData <- eadata[[colorBy]]
      pal <- colorFactor(c("#1f78b4", "#e6550d"), colorData)
    }
    
    leafletProxy("map", data = eadata) %>%
      clearShapes() %>%
      addCircles(~LONGITUD, ~LATITUD, radius=500, layerId=~RBD,
                 stroke=FALSE, fillOpacity=0.7, fillColor=pal(colorData)) %>%
      addLegend("bottomright", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  observe({
    colorBy <- input$color
    eventExpr <- input$map_zoom
    
    if (colorBy == "PIE") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(eadata$PIE == "1", "SI", "NO")
      pal <- colorFactor(c("#1f78b4", "#e6550d"), colorData)
    }
    else {
      colorData <- eadata[[colorBy]]
      pal <- colorFactor(c("#1f78b4", "#e6550d"), colorData)
    }
    
  
    print(input$map_zoom)
    leafletProxy("map", data = eadata) %>%
      clearShapes() %>%
      addCircles(~LONGITUD, ~LATITUD, radius=500, layerId=~RBD,
                 stroke=FALSE, fillOpacity=0.7, fillColor=pal(colorData),
                 weight = case_when(input$map_zoom <=4 ~1, 
                                    input$map_zoom ==5 ~2, 
                                    input$map_zoom ==6 ~3, 
                                    input$map_zoom ==7 ~5, 
                                    input$map_zoom ==8 ~7, 
                                    input$map_zoom ==9 ~9, 
                                    input$map_zoom >9 ~11)) %>%
      addLegend("bottomright", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
    
    
  })
  
  # Show a popup at the given location
  showEAPopup <- function(RBD, lat, lng) {
    selectedEA <- eadata[eadata$RBD == RBD,]
    content <- as.character(tagList(
      tags$h4("", as.character(selectedEA$ESTABLECIMIENTO)),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedEA$COMUNA, selectedEA$REGIÓN
      ))), tags$br(),
      sprintf("RBD: %s", selectedEA$RBD), tags$br(),
      sprintf("Matrícula total: %s", selectedEA$MATRÍCULA), tags$br(),
      sprintf("Sostenedor: %s", as.character(selectedEA$SOSTENEDOR))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = RBD)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showEAPopup(event$id, event$lat, event$lng)
    })
  })
  
  ## Data Explorer ###########################################
  
  observe({
    comuna <- if (is.null(input$region)) character(0) else {
      filter(eadata, REGIÓN %in% input$region) %>%
        `$`('COMUNA') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$comuna[input$comuna %in% comuna])
    updateSelectizeInput(session, "comuna", choices = comuna,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    escuela <- if (is.null(input$region)) character(0) else {
      eadata %>%
        filter(REGIÓN %in% input$region,
               is.null(input$comuna) | COMUNA %in% input$comuna) %>%
        `$`('ESTABLECIMIENTO') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$escuela[input$escuela %in% escuela])
    updateSelectizeInput(session, "escuela", choices = escuela,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.1
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showEAPopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$eatabla <- DT::renderDataTable({
    df <- eadata %>%
      filter(
        is.null(input$region) | REGIÓN %in% input$region,
        is.null(input$comuna) | COMUNA %in% input$comuna,
        is.null(input$escuela) | ESTABLECIMIENTO %in% input$escuela
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', LATITUD, '" data-long="', LONGITUD, '" data-zip="', RBD , '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "eatabla")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    
  }  )
  
}


shinyApp(ui = ui, server = server)

