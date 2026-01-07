# =============================================================================
# APP SHINY - ANALISIS DE PUNO (Distritos y Provincias)
# =============================================================================

# PASO 1: Instalar librerías (solo la primera vez)
# install.packages(c("shiny", "dplyr", "DT", "ggplot2", "readr"))

# PASO 2: Cargar librerías
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(readr)

# =============================================================================
# LECTURA DE DATOS
# =============================================================================
ruta_archivo <- "puno.csv"   # Asegúrate que el archivo esté en la misma carpeta
puno <- read.csv(ruta_archivo, header = TRUE, sep = ",")

# =============================================================================
# INTERFAZ DE USUARIO
# =============================================================================
ui <- fluidPage(
  titlePanel("📊 Análisis de Distritos y Provincias - Puno"),
  
  tabsetPanel(
    tabPanel("Distritos",
             DTOutput("tabla_distritos"),
             downloadButton("descargar_distritos", "⬇ Descargar Distritos")
    ),
    
    tabPanel("Provincias",
             DTOutput("tabla_provincias_detalle"),
             downloadButton("descargar_provincias", "⬇ Descargar Provincias")
    ),
    
    tabPanel("Resumen General",
             DTOutput("datos_completos"),
             downloadButton("descargar_resumen", "⬇ Descargar Resumen")
    )
  )
)

# =============================================================================
# LÓGICA DEL SERVIDOR
# =============================================================================
server <- function(input, output, session) {
  
  # DATOS DISTRITOS ----
  output$tabla_distritos <- renderDT({
    datos <- puno %>%
      mutate(
        superficie_total_ha = round(superficie_total_ha, 1),
        productores_total = round(productores_total, 1),
        superficie_promedio = round(superficie_promedio, 2),
        participacion_pct = round(participacion_pct, 2)
      )
    
    datatable(datos,
              options = list(pageLength = 10, scrollX = TRUE),
              colnames = c("Distrito", "Provincia", "Superficie (ha)",
                           "Productores", "Parcelas", "Promedio (ha)", "Participación (%)"),
              rownames = FALSE) %>%
      formatStyle(columns = 1:7, fontSize = '13px') %>%
      formatRound(columns = c(3, 4, 6, 7), digits = 2)
  })
  
  # DATOS PROVINCIAS ----
  output$tabla_provincias_detalle <- renderDT({
    datos <- puno %>%
      group_by(NOMBREPV) %>%
      summarise(
        distritos = n(),
        superficie_total_ha = sum(superficie_total_ha, na.rm = TRUE),
        productores_total = sum(productores_total, na.rm = TRUE),
        parcelas_total = sum(parcelas_total, na.rm = TRUE),
        superficie_promedio = mean(superficie_promedio, na.rm = TRUE)
      )
    
    datatable(datos,
              options = list(pageLength = 10, scrollX = TRUE),
              colnames = c("Provincia", "Distritos", "Superficie (ha)",
                           "Productores", "Parcelas", "Promedio (ha)"),
              rownames = FALSE) %>%
      formatStyle(columns = 1:6, fontSize = '13px') %>%
      formatRound(columns = c(3, 4, 6), digits = 1)
  })
  
  # RESUMEN GENERAL ----
  output$datos_completos <- renderDT({
    datos <- puno %>%
      select(ranking, NOMBREDI, NOMBREPV, superficie_total_ha, productores_total,
             parcelas_total, superficie_promedio, participacion_pct) %>%
      mutate(
        superficie_total_ha = round(superficie_total_ha, 1),
        productores_total = round(productores_total, 1),
        superficie_promedio = round(superficie_promedio, 2),
        participacion_pct = round(participacion_pct, 2)
      )
    
    datatable(datos,
              options = list(pageLength = 20, scrollX = TRUE),
              colnames = c("Ranking", "Distrito", "Provincia", "Superficie (ha)",
                           "Productores", "Parcelas", "Promedio (ha)", "Participación (%)"),
              rownames = FALSE) %>%
      formatStyle(columns = 1:8, fontSize = '13px') %>%
      formatRound(columns = c(4, 5, 7, 8), digits = 2)
  })
  
  # DESCARGAS ----
  output$descargar_distritos <- downloadHandler(
    filename = function() { paste0("analisis_distritos_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(puno, file, row.names = FALSE)
    }
  )
  
  output$descargar_provincias <- downloadHandler(
    filename = function() { paste0("analisis_provincias_", Sys.Date(), ".csv") },
    content = function(file) {
      datos <- puno %>%
        group_by(NOMBREPV) %>%
        summarise(
          distritos = n(),
          superficie_total_ha = sum(superficie_total_ha, na.rm = TRUE),
          productores_total = sum(productores_total, na.rm = TRUE),
          parcelas_total = sum(parcelas_total, na.rm = TRUE),
          superficie_promedio = mean(superficie_promedio, na.rm = TRUE)
        )
      write.csv(datos, file, row.names = FALSE)
    }
  )
  
  output$descargar_resumen <- downloadHandler(
    filename = function() { paste0("resumen_general_", Sys.Date(), ".csv") },
    content = function(file) {
      write.csv(puno, file, row.names = FALSE)
    }
  )
}

# =============================================================================
# LANZAR APLICACIÓN
# =============================================================================
shinyApp(ui = ui, server = server)
