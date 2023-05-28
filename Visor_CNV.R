# PAQUETES
library(dplyr)
library(sf)
library(terra)
library(raster)
library(rgdal)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(shiny)
library(colorspace)
library(htmltools)
library(shinydashboard)

# DATOS
# Capa de polígonos: afectaciones en proyectos
afectaciones <-
  st_read(
    "https://raw.githubusercontent.com/ANALUGARITA/Visor_CNV/main/datos/CNV_afectaciones_WGS84.geojson",
    quiet = TRUE
  )
# Capa de puntos: inventario de proyectos
proyectos <-
  st_read(
    "https://raw.githubusercontent.com/ANALUGARITA/Visor_CNV/main/datos/CNV_Proyectos_con_afectaciones_WGS84.geojson",
    quiet = TRUE
  )

# LISTAS PARA FILTROS
# Lista ordenada de rutas + "Ninguna"
lista_rutas <- unique(afectaciones$id_ruta)
lista_rutas <- sort(lista_rutas)
lista_rutas <- c("Todas", lista_rutas)

# Lista ordenada de afectaciones + "Ninguna"
lista_afectaciones <- unique(afectaciones$id_afectacion)
lista_afectaciones <- sort(lista_afectaciones)
lista_afectaciones <- c("Todas", lista_afectaciones)

#ETIQUETADO
etiquetas <- paste(
  "<strong>Nombre: </strong>",proyectos$Nombre,
  "<br/><strong>Estructura: </strong>", proyectos$Estructura,
  "<br/><strong>Ruta Nacional: </strong>", proyectos$Ruta_Nacional,
  "<br/><strong>Requiere DV: </strong>", proyectos$Requiere_DV,
  "<br/><strong>Tiene planos: </strong>", proyectos$tiene_plano
  ) %>% lapply(htmltools::HTML)

# COMPONENTES DE LA APLICACIÓN SHINY
# Definición del objeto ui
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Afectaciones en la Red Vial Nacional",
                  titleWidth = 360, 
                  tags$li(class = "dropdown",
                          tags$style(".main-header {max-height: 50px}"),
                          tags$style(".main-header .logo {height: 70px;}"),
                          tags$style(".sidebar-toggle {height: 70px; padding-top: 25px !important;}"),
                          tags$style(".navbar {min-height: 50px !important}"),
                  tags$li(a(href = 'https://conavi.go.cr/cat%C3%A1logo-de-proyectos',
                            img(src = 'https://raw.githubusercontent.com/ANALUGARITA/Visor_CNV/main/www/logo.png',
                                title = "Proyectos CONAVI", height = "50px"),
                            style = "padding-top:10px; padding-bottom: 10px;"),
                          class = "dropdown" 
                          ))
                  ),
  dashboardSidebar(sidebarMenu( 
    tags$style(".left-side, .main-sidebar {padding-top: 70px}"),
    menuItem(
      text = "Filtros",
      selectInput(
        inputId = "id_ruta",
        label = "Ruta Nacional",
        choices = lista_rutas,
        selected = "Todas"),
      selectInput(
        inputId = "id_afectacion",
        label = "Id de afectación",
        choices = lista_afectaciones,
        selected = "Todas"),
      startExpanded = TRUE,
      icon = icon('filter')
    )
  )),
  dashboardBody(fluidRow(
            box(
                  title = div("Mapa de afectaciones en proyectos", style = 'font-size:22px;'),
                  solidHeader = F,
                  leafletOutput(outputId = "mapa"),
                  width = 9),
            box(
                  title = div("Estado de planos en el proyecto de la RN seleccionada", align = "center", style = 'font-size:17px;'), 
                  solidHeader = F,
                  plotlyOutput(outputId = "grafico_estado_proyecto"), 
                  width = 3)
            ),
        fluidRow(
            box(
                 title = "Tabla de información de afectaciones en la RN seleccionada",
                 solidHeader = F,
                 DTOutput(outputId = "tabla"),
                 width = 12)
    ))
)

# Definición del objeto server
server <- function(input, output, session) {
  filtrarAfectacion <- reactive({
    # Filtrado de geometrías - selección de columnas
    afectaciones_filtrado <-
      afectaciones %>%
      dplyr::select(id_afectacion, area_plano, no_plano, estado_plano, finca_generada, estado_proceso, ubicacion, id_provincia, id_canton, id_distrito, id_ruta, nombre_disenador, fecha_diseno, nombre_ingeniero_admi, no_contratacion, nombre_proyecto)
    
    # Filtrado de afectaciones por rutas
    if (input$id_ruta != "Todas") {
      afectaciones_filtrado <-
        afectaciones_filtrado %>%
        filter(id_ruta == input$id_ruta)
    }   
    # Filtrado de afectaciones por afectacion
    if (input$id_afectacion != "Todas") {
      afectaciones_filtrado <-
        afectaciones_filtrado %>%
        filter(id_afectacion == input$id_afectacion)
    }
    return(afectaciones_filtrado)
  })
  
  filtrarEstado <- reactive({
    # Filtrado de geometrías para estado para el proyecto en la ruta seleccionada
    estado_filtrado <-
      afectaciones %>%
      dplyr::select(id_ruta, estado_proceso, ubicacion)
    
    # Filtrado de afectaciones por rutas
    if (input$id_ruta != "Todas") {
      estado_filtrado <-
        estado_filtrado %>%
        filter(id_ruta == input$id_ruta)
    }
    return(estado_filtrado)
  }) 
  
  
   # Mapa ubicación de las afectaciones y proyectos
  
  output$mapa <- renderLeaflet({
    registros <- filtrarAfectacion()
    
    leaflet() %>%
      setView(-84.09, 9.84, 7) %>%
      addProviderTiles(providers$CartoDB.Voyager , group = "Voyager") %>%
      addTiles(group = "OSM") %>%
      addPolygons(
        data = afectaciones,
        color = "green",
        fillColor = "green",
        stroke = TRUE,
        weight = 3,
        opacity = 1,
        group = "Afectaciones",
        popup = paste0(
          "<strong>Ruta: </strong>", registros$id_ruta, "<br>",
          "<strong>No. Afectación: </strong>", registros$id_afectacion, "<br>",
          "<strong>No. Plano: </strong>", registros$no_plano, "<br>",
          "<strong>Área: </strong>", registros$area_plano, "<br>",
          "<strong>Estado: </strong>", registros$estado_proceso, "<br>",
          "<strong>Ubicación: </strong>", registros$ubicacion, "<br>"
        )
      ) %>%    
      addCircleMarkers(
        data = proyectos,
        stroke = F,
        radius = 5,
        fillColor = "orange",
        fillOpacity = 1,
        group = "Proyectos",
        label = etiquetas, 
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "11px",
          direction = "auto")
      ) %>%
      addLayersControl(
        overlayGroups = c("Afectaciones", "Proyectos"),
        baseGroups = c("Voyager", "OSM"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addMiniMap(toggleDisplay = TRUE, 
                 position = "bottomright") %>%
      addMeasure(position = "topleft",
                 primaryLengthUnit = "meters",
                 primaryAreaUnit = "sqmeters",
                 activeColor = "#3D535D",
                 completedColor = "#7D4479", 
                 localization = "es") %>%
      addScaleBar(position = "bottomleft", 
                  options = scaleBarOptions(metric = TRUE,
                                            imperial = FALSE)) %>%
      addFullscreenControl() %>%
      addMouseCoordinates() %>%
      addControlGPS() %>%
      addSearchOSM() %>%
      addResetMapButton() 
  })
  
  # Tabla de afectaciones
  output$tabla <- renderDT({
    registros <- filtrarAfectacion()
    registros %>%
      dplyr::select(id_ruta, id_afectacion, no_plano, area_plano, estado_proceso, ubicacion, nombre_proyecto, id_provincia, id_canton, id_distrito) %>%
      st_drop_geometry() %>%
      datatable(rownames = FALSE,
                colnames = c('Ruta', 'Id Afectación','No. Plano', 'Área [m2]', 'Estado', 'Ubicación', 'Proyecto', 'Provincia', 'Cantón', 'Distrito'),
                options = list(
                  pageLength = 10,
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                )
      )
  })
  
  # Gráfico de estado de proyecto seleccionado
  output$grafico_estado_proyecto <- renderPlotly({
    registrosEstado <- filtrarEstado()
    
    registrosEstado %>%
      st_drop_geometry() %>%
      group_by(estado_proceso) %>%
      summarize(suma_registros = n()) %>%
      filter(!is.na(estado_proceso))  %>%
      plot_ly(
        x = ~ estado_proceso,
        y = ~ suma_registros,
        type = "bar",
        color = ~ estado_proceso,
        colors = c("#FFCC00", "#FF9900", "#FF6600")
      ) %>%
      layout(
        xaxis = list(title = "Estado"),
        yaxis = list(title = "Cantidad de planos"),
        showlegend = FALSE
        )  %>%
      config(
        modeBarButtonsToRemove = c("zoom", "pan", "select", "zoomIn", "zoomOut", "autoScale", "resetScale", "zoomIn2d", "zoomOut2d", "lasso2d", "select2d"),
        displaylogo = FALSE
        )
  })
}

shinyApp(ui, server)