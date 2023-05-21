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
    "https://raw.githubusercontent.com/ANALUGARITA/visor_afectaciones/main/datos/CNV_afectaciones_WGS84.geojson",
    quiet = TRUE
  )

# Capa de puntos: inventario de proyectos
proyectos <-
  st_read(
    "https://raw.githubusercontent.com/ANALUGARITA/visor_afectaciones/main/datos/CNV_Proyectos_con_afectaciones_WGS84.geojson",
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
    ),
  ),    
  fluidRow(
    box(
      title = "Tabla de información",
      DTOutput(outputId = "tabla"),
      width = 12
    ),
  ))
)

# Definición del objeto server

server <- function(input, output, session) {
  filtrarAfectacion <- reactive({
    # Remoción de geometrías y selección de columnas
    afectaciones_filtrado <-
      afectaciones %>%
      dplyr::select(id, estructura, elemento, dano, severidad, servicio, ruta, seccion, zona, disparador, fecha, observaciones, usuario, fecha_reporte, fotos)
    
    # Filtrado de daños por estructura
    if (input$id_ruta= "Todas") {
      afectaciones_filtrado <-
        afectaciones_filtrado %>%
        filter(id_ruta == input$id_ruta)
    }   
    # Filtrado de daños por tipo
    if (input$id_afectacion= "Todas") {
      afectaciones_filtrado <-
        afectaciones_filtrado %>%
        filter(id_afectacion == input$id_afectacion)
    }
    return(afectaciones_filtrado)
  })
  
  
  # Mapa ubicación de los daños
  
  output$mapa <- renderLeaflet({
    registros <- filtrarAfectacion()
    
    
    # Registro de daños, zonas de conservación y ráster de zonas de conservación por cantidad
    leaflet() %>%
      setView(-84.09, 9.84, 8) %>%
      addProviderTiles(providers$CartoDB.Voyager , group = "Voyager") %>%
      addTiles(group = "OSM") %>%
      addPolygons(
        data = afectaciones,
        color = "#38302e",
        fillColor = "green",
        stroke = TRUE,
        weight = 3,
        opacity = 1,
        group = "Afectaciones"
      ) %>%    
      addCircleMarkers(
        data = proyectos,
        stroke = F,
        radius = 3,
        fillColor = '#d62828',
        fillOpacity = 1,
        group = "Proyectos",
        label = paste0(
          "Nombre: ", registros$Nombre,
          ", ",
          "Estructura: ", registros$Estructura,
          ", ",
          "Ruta Naciona: ", registros$Ruta_Nacional,
          ", "#,
          #"Requiere: ", proyectos$Requiere adquisicion de derecho de via,
          #", ",
          #"Requiere: ", proyectos$Requiere adquisicion de derecho de via,
        ),
        popup = (paste0(
          "<strong>Nombre: </strong>", registros$Nombre,
          "<br>",
          "<strong>Estructura: </strong>", registros$Estructura,
          "<br>",
          "<strong>Ruta Naciona: </strong>", registros$Ruta_Nacional,
          "<br>")
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
    registros <- filtrarDanos()
    registros %>%
      dplyr::select(id, estructura,elemento, dano, severidad, servicio, fecha, ruta, seccion) %>%
      st_drop_geometry() %>%
      
      datatable(rownames = FALSE,
                colnames = c('ID', 'Estructura','Elemento', 'Daño', 'Severidad','Servicio', 'Fecha', 'Ruta', 'Sección'),
                options = list(
                  pageLength = 7,
                  language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                )
      )
  })
}

shinyApp(ui, server)