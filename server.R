library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
require(tidyverse)
require(dplyr)
library(ggplot2)
library(ggpubr)

load("data/eadata.RData")

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()%>%
      setView(lng = -70.56428186, lat = -33.44872712, zoom = 10)
    
  })
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  observe({
    colorBy <- input$color
    
    if (colorBy == "NUEVA") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(eadata$NUEVA == "SI", "SI", "NO")
      pal <- colorFactor( c("#377eb8", "#ff7f00"), colorData)
    }
    else {
      colorData <- eadata[[colorBy]]
      pal <- colorFactor( c("#377eb8", "#ff7f00"), colorData)
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
    
    if (colorBy == "PIE") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(eadata$PIE == "SI", "SI", "NO")
      pal <- colorFactor( c("#377eb8", "#ff7f00"), colorData)
    }
    else {
      colorData <- eadata[[colorBy]]
      pal <- colorFactor( c("#377eb8", "#ff7f00"), colorData)
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
      colorData <- ifelse(eadata$RURAL == "RURAL", "RURAL", "URBANO")
      pal <- colorFactor( c("#ff7f00", "#377eb8"), colorData)
    }
    else {
      colorData <- eadata[[colorBy]]
      pal <- colorFactor( c("#ff7f00", "#377eb8"), colorData)
    }
    
    leafletProxy("map", data = eadata) %>%
      clearShapes() %>%
      addCircles(~LONGITUD, ~LATITUD, radius=500, layerId=~RBD,
                 stroke=FALSE, fillOpacity=0.7, fillColor=pal(colorData)) %>%
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
      (sprintf("RBD: %s", selectedEA$RBD)), tags$br(),
      (sprintf("MATRÍCULA EA: %s", selectedEA$MATRÍCULA)), tags$br(),
      (sprintf("NIVEL: %s", selectedEA$NIVEL)), tags$br(),
      (sprintf("CATEGORÍA: %s", selectedEA$CATEGORÍA)), tags$br(),
      (sprintf("PARTICIPACIÓN EA: %s", selectedEA$PARTICIPA)), tags$br(),
      (sprintf("IVE BÁSICA: %s", selectedEA$IVE_BÁSICA)), tags$br(),
      (sprintf("IVE MEDIA: %s", selectedEA$IVE_MEDIA)), tags$br(),
      (sprintf("DEPENDENCIA: %s", selectedEA$DEPENDENCIA)), tags$br(),
      (sprintf("SOSTENEDOR: %s", as.character(selectedEA$SOSTENEDOR)))
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
  ## Análisis ###########################################
  output$plot1 <- renderPlot({
    
    total <- eadata %>% group_by(REGIÓN) %>%
      summarise(ee = table(REGIÓN))
    
    ggplot(total, aes(x = ee, y = REGIÓN)) +
      geom_col(fill = "#56B4E9") +
      xlab("Establecimientos Inscritos")+
      ylab("")+
      geom_text(
        aes(label = ee), 
        ## make labels left-aligned
        hjust = 1, nudge_x = -.5
      ) +
      theme_minimal()
  })
  
  output$plot2 <- renderPlot({
    
    regiones <- eadata %>% group_by(REGIÓN) %>%
      summarise(nuevos = sum(NUEVA=="SI")) 
    
    # Escuelas nuevas por región
    ggplot(regiones, aes(x = nuevos, y = REGIÓN)) +
      geom_col(fill = "#f1a340") +
      xlab("Establecimientos Nuevos")+
      ylab("")+
      geom_text(
        aes(label = nuevos), 
        ## make labels left-aligned
        hjust = 1, nudge_x = -.5
      ) +
      theme_minimal()
  })
  
  # Escuelas por dependencia 
  output$plot3 <- renderPlot({
    
    dependencia <- eadata %>% group_by(DEPENDENCIA) %>%
      summarise(dep = table(DEPENDENCIA))
    
    ggplot(dependencia, aes(x = dep, y = DEPENDENCIA)) +
      geom_col(fill = "#1a9850") +
      xlab("Establecimientos Inscritos")+
      ylab("")+
      geom_text(
        aes(label = dep), 
        ## make labels left-aligned
        hjust = 1, nudge_x = -.5
      ) +
      theme_minimal()
  })
  
  # Escuelas nuevas por dependencia 
  output$plot4 <- renderPlot({
    
    depen_nueva <- eadata %>% subset(NUEVA=="SI") %>% group_by(DEPENDENCIA) %>%
      summarise(dep = table(DEPENDENCIA))
    
    ggplot(depen_nueva, aes(x = dep, y = DEPENDENCIA)) +
      geom_col(fill = "#56B4E9") +
      xlab("Establecimientos Nuevos")+
      ylab("")+
      geom_text(
        aes(label = dep), 
        ## make labels left-aligned
        hjust = 1, nudge_x = -.5
      ) +
      theme_minimal()
  })
  
  # Escuelas urbanas y rurales
  output$plot5 <- renderPlot({
    rural <- eadata %>% group_by(RURAL) %>%
      summarise(ru = table(RURAL))
    
    ggplot(rural, aes(x = ru, y = RURAL)) +
      geom_col(fill = "#f1a340") +
      xlab("Establecimientos Inscritos")+
      ylab("")+
      geom_text(
        aes(label = ru), 
        ## make labels left-aligned
        hjust = 1, nudge_x = -.5
      ) +
      theme_minimal()
  })
  
  # Escuelas por categoría de desempeño
  output$plot6 <- renderPlot({
    categoria <- eadata %>% group_by(CATEGORÍA) %>%
      summarise(cat = table(CATEGORÍA))
    
    ggplot(categoria, aes(x = cat, y = CATEGORÍA)) +
      geom_col(fill = "#1a9850") +
      xlab("Establecimientos Inscritos")+
      ylab("")+
      geom_text(
        aes(label = cat), 
        ## make labels left-aligned
        hjust = 1, nudge_x = -.5
      ) +
      theme_minimal()
  })
  
  # Escuelas nuevas por categoría de desempeño
  output$plot7 <- renderPlot({
    cat_nueva <- eadata %>% subset(NUEVA=="SI") %>% group_by(CATEGORÍA) %>%
      summarise(cat = table(CATEGORÍA))
    
    ggplot(cat_nueva, aes(x = cat, y = CATEGORÍA)) +
      geom_col(fill = "#56B4E9") +
      xlab("Establecimientos Nuevos")+
      ylab("")+
      geom_text(
        aes(label = cat), 
        ## make labels left-aligned
        hjust = 1, nudge_x = -.5
      ) +
      theme_minimal()
  })
  
  # Escuelas por participación
  output$plot8 <- renderPlot({
    participa <- eadata %>% group_by(PARTICIPA) %>%
      summarise(par = table(PARTICIPA)) 
    
    ggplot(participa, aes(x = par, y = PARTICIPA)) +
      geom_col(fill = "#f1a340") +
      xlab("Establecimientos Inscritos")+
      ylab("")+
      geom_text(
        aes(label = par), 
        ## make labels left-aligned
        hjust = 1, nudge_x = -.5
      ) +
      theme_minimal()
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
      mutate(MAPA = paste('<a class="go-map" href="" data-lat="', LATITUD, '" data-long="', LONGITUD, '" data-zip="', RBD , '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "eatabla")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    
  }  )
  
}
