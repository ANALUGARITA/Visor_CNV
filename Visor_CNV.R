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
lista_rutas <- c("Ninguna", lista_rutas)
# Lista ordenada de afectaciones + "Ninguna"
lista_afectaciones <- unique(afectaciones$id_afectacion)
lista_afectaciones <- sort(lista_afectaciones)
lista_afectaciones <- c("Ninguna", lista_afectaciones)


# COMPONENTES DE LA APLICACIÓN SHINY
# Definición del objeto ui
ui <- dashboardPage(
  dashboardHeader(title = "Afectaciones tramitadas en la Red Vial Nacional"),
  #dashboardPage(skin = "skyblue"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      text = "Filtros",
      selectInput(
        inputId = "id_ruta",
        label = "Ruta Nacional",
        choices = lista_rutas,
        selected = "Ninguna"
      ),
      selectInput(
        inputId = "id_afectaciones",
        label = "Id de afectación",
        choices = lista_afectaciones,
        selected = "Ninguna"
      ),
      startExpanded = TRUE
    )
  )),
  dashboardBody(fluidRow(
    box(
      title = "Mapa de afectaciones",
      leafletOutput(outputId = "mapa"),
      width = 12
    )
  ),    
  fluidRow(
    box(
      title = "Tabla de información",
      DTOutput(outputId = "tabla"),
      width = 12
    )
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
    if (input$id_ruta == "Todas") {
      afectaciones_filtrado <-
        afectaciones_filtrado %>%
        filter(id_ruta == input$id_ruta)
    }   
    # Filtrado de afectaciones por afectacion
    if (input$id_afectacion= "Todas") {
      afectaciones_filtrado <-
        afectaciones_filtrado %>%
        filter(id_afectacion == input$id_afectacion)
    }
    return(afectaciones_filtrado)
  })
  
  
  # Mapa ubicación de las afectaciones
  
  output$mapa <- renderLeaflet({
    registros <- filtrarAfectacion()
    
    
    # Registro de afectaciones y proyectos
    leaflet() %>%
      setView(-84.09, 9.84, 8) %>%
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
        popup = (paste0(
          "<strong>Ruta: </strong>", registros$id_ruta,
          "<br>",
          "<strong>No. Afectación: </strong>", registros$id_afectacion,
          "<br>",
          "<strong>No. Plano: </strong>", registros$no_plano,
          "<br>",
          "<strong>Área: </strong>", registros$area_plano,
          "<br>",
          "<strong>Estado: </strong>", registros$estado_proceso,
          "<br>",
          "<strong>Ubicación: </strong>", registros$ubicacion,
          "<br>")
        )
      ) %>%    
      addCircleMarkers(
        data = proyectos,
        stroke = F,
        radius = 3,
        fillColor = "orange",
        fillOpacity = 1,
        group = "Proyectos",
        label = paste0(
          "Nombre: ", registros$Nombre,
          ", ",
          "Estructura: ", registros$Estructura,
          ", ",
          "Ruta Naciona: ", registros$Ruta_Nacional,
          ", ",
          "Requiere: ", proyectos$Requiere_DV,
          ", ",
          "Requiere: ", proyectos$tiene_plano
        )
      )  %>%
      addLayersControl(
        baseGroups = c("Voyager", "OSM"),
        overlayGroups = c("Afectaciones", "Proyectos"),
        options = layersControlOptions(collapsed = T)
      ) %>%
 
      addSearchOSM() %>%
      addResetMapButton() %>%
      addMouseCoordinates()
  })
  
  # Tabla de registro de daños
  
  output$tabla <- renderDT({
    registros <- filtrarAfectacion()
    registros %>%
      dplyr::select(id_ruta, id_afectacion, no_plano, area_plano, finca_generada, estado_proceso, ubicacion, nombre_proyecto, id_provincia, id_canton, id_distrito) %>%
      st_drop_geometry() %>%
      
      datatable(rownames = FALSE,
                colnames = c('Ruta', 'Proyecto','No. Plano', 'Área [m2]', 'Finca Generada','Estado', 'Ubicación', 'Proyecto', 'Provincia', 'Cantón', 'Distrito'),
                options = list(
                  pageLength = 7,
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                )
      )
  })
}

shinyApp(ui, server)