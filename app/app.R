# app.R — Shiny dashboard del Índice de Carga Ambiental Injusta (CAI)
# Lee data/clean/* ; NO procesa datos crudos.
# Ejecutar desde la raíz del proyecto: shiny::runApp("app/")

library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(dplyr)
library(sf)
library(scales)
library(plotly)

# =========================================================================
# Datos
# =========================================================================
# Rutas: "data/" local para shinyapps.io, "../data/clean/" para desarrollo local
if (dir.exists("data")) {
  cai_v1     <- readRDS("data/cai.rds")
  cai_v2     <- readRDS("data/cai_v2.rds")
  comunas_sf <- readRDS("data/comunas_sf.rds")
} else {
  cai_v1     <- readRDS("../data/clean/cai.rds")
  cai_v2     <- readRDS("../data/clean/cai_v2.rds")
  comunas_sf <- readRDS("../data/clean/comunas_sf.rds")
}

# Unir geometría con CAI v2 (principal)
mapa_data <- comunas_sf |>
  left_join(
    cai_v2 |> select(codigo_comuna, nombre_comuna, region,
                     cai_v2, quintil_v2, quintil_v2_label,
                     expo_promedio_hab, tasa_pobreza_ing,
                     tasa_pobreza_mult, poblacion),
    by = "codigo_comuna"
  ) |>
  left_join(
    cai_v1 |> select(codigo_comuna,
                     cai_v1 = cai, ranking_v1 = ranking,
                     quintil_v1 = quintil_label,
                     emision_tox_pc),
    by = "codigo_comuna"
  ) |>
  mutate(
    ranking_v2 = rank(-cai_v2, ties.method = "min"),
    cambio = ranking_v1 - ranking_v2
  )

# Tabla para DT (sin geometría)
tabla_dt <- mapa_data |>
  st_drop_geometry() |>
  select(
    Comuna = nombre_comuna,
    `Region` = region,
    `Poblacion` = poblacion,
    `Pobreza Ing (%)` = tasa_pobreza_ing,
    `CAI v2` = cai_v2,
    `Quintil v2` = quintil_v2_label,
    `Ranking v2` = ranking_v2,
    `CAI v1` = cai_v1,
    `Ranking v1` = ranking_v1,
    `Cambio ranking` = cambio
  ) |>
  mutate(
    `Pobreza Ing (%)` = round(`Pobreza Ing (%)` * 100, 1),
    `CAI v2` = round(`CAI v2`, 1),
    `CAI v1` = round(`CAI v1`, 1)
  ) |>
  arrange(`Ranking v2`)

# Paleta de colores
colores_quintil <- c(
  "Q1 · Muy bajo" = "#2c7bb6",
  "Q2 · Bajo"     = "#abd9e9",
  "Q3 · Medio"    = "#ffffbf",
  "Q4 · Alto"     = "#fdae61",
  "Q5 · Muy alto" = "#d7191c"
)

pal <- colorFactor(
  palette = unname(colores_quintil),
  levels  = names(colores_quintil),
  na.color = "#cccccc"
)

# Popup HTML
mapa_data <- mapa_data |>
  mutate(
    popup_html = paste0(
      "<strong>", nombre_comuna, "</strong> (", region, ")<br>",
      "Poblacion: ", format(poblacion, big.mark = "."), "<br>",
      "Pobreza ingresos: ", round(tasa_pobreza_ing * 100, 1), "%<br>",
      "<hr style='margin:4px 0'>",
      "<strong>CAI v2:</strong> ", round(cai_v2, 1),
      " — ", quintil_v2_label, " (#", ranking_v2, ")<br>",
      "<strong>CAI v1:</strong> ", round(cai_v1, 1),
      " (#", ranking_v1, ")<br>",
      "Cambio ranking: ", ifelse(cambio > 0,
                                  paste0("+", cambio, " (sube)"),
                                  paste0(cambio, " (baja)")))
  )

# =========================================================================
# UI
# =========================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "CAI — Carga Ambiental Injusta · Chile 2024",
    titleWidth = 450
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Mapa", tabName = "mapa_tab", icon = icon("map")),
      menuItem("Tabla completa", tabName = "tabla_tab", icon = icon("table")),
      menuItem("Acerca de", tabName = "info_tab", icon = icon("info-circle"))
    ),
    hr(),
    div(style = "padding: 10px;",
      selectInput("version", "Version del indice:",
                  choices = c("v2 (proximidad)" = "v2",
                              "v1 (per capita)" = "v1"),
                  selected = "v2"),
      selectInput("region_filtro", "Filtrar por region:",
                  choices = c("Todas" = "todas",
                              sort(unique(na.omit(mapa_data$region)))),
                  selected = "todas")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box-header .box-title { font-weight: bold; }
    "))),
    tabItems(
      # --- Tab Mapa ---
      tabItem(tabName = "mapa_tab",
        fluidRow(
          valueBoxOutput("vb_comunas", width = 3),
          valueBoxOutput("vb_q5", width = 3),
          valueBoxOutput("vb_correl", width = 3),
          valueBoxOutput("vb_quintero", width = 3)
        ),
        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = "Mapa comunal — CAI por quintiles",
              leafletOutput("mapa", height = 550)
          )
        )
      ),
      # --- Tab Tabla ---
      tabItem(tabName = "tabla_tab",
        fluidRow(
          box(width = 12, solidHeader = TRUE, status = "primary",
              title = "Ranking completo de comunas",
              DTOutput("tabla_full")
          )
        )
      ),
      # --- Tab Info ---
      tabItem(tabName = "info_tab",
        fluidRow(
          box(width = 8, solidHeader = TRUE, status = "info",
              title = "Acerca del CAI",
              HTML("
                <h4>Indice de Carga Ambiental Injusta (CAI)</h4>
                <p>El CAI mide la coincidencia entre emisiones industriales toxicas
                y pobreza comunal en Chile, a escala de las 345 comunas.</p>
                <h4>Fuentes de datos</h4>
                <ul>
                  <li><strong>RETC 2024</strong> — Registro de Emisiones y Transferencias
                  de Contaminantes (Ministerio del Medio Ambiente). Emisiones al aire
                  de fuentes puntuales.</li>
                  <li><strong>Pobreza SAE CASEN 2024</strong> — Estimaciones comunales
                  de pobreza por ingresos (MDS).</li>
                  <li><strong>WorldPop 2020</strong> — Raster de poblacion ~100m para
                  calculo de exposicion por proximidad.</li>
                </ul>
                <h4>Versiones</h4>
                <ul>
                  <li><strong>v1:</strong> Emisiones per capita comunales ponderadas
                  por toxicidad.</li>
                  <li><strong>v2:</strong> Exposicion poblacional ponderada por
                  proximidad a fuentes (buffers 5km, decay 1/d2).</li>
                </ul>
                <h4>Autor</h4>
                <p>Renato Andres — Ingenieria Ambiental, Universidad de Concepcion,
                campus Chillan. Proyecto para el ramo Ciencia de Datos y Desarrollo
                Sostenible, 2026.</p>
              ")
          )
        )
      )
    )
  )
)

# =========================================================================
# Server
# =========================================================================
server <- function(input, output, session) {

  # Datos filtrados por region
  datos_filtrados <- reactive({
    if (input$region_filtro == "todas") {
      tabla_dt
    } else {
      tabla_dt |> filter(Region == input$region_filtro)
    }
  })

  # Value boxes
  output$vb_comunas <- renderValueBox({
    valueBox(345, "Comunas analizadas", icon = icon("city"), color = "blue")
  })

  output$vb_q5 <- renderValueBox({
    n_q5 <- sum(mapa_data$quintil_v2 == 5, na.rm = TRUE)
    valueBox(n_q5, "Comunas Q5 (Muy alto)", icon = icon("exclamation-triangle"),
             color = "red")
  })

  output$vb_correl <- renderValueBox({
    valueBox("-0.292", "Spearman (v2)", icon = icon("chart-line"),
             color = "yellow")
  })

  output$vb_quintero <- renderValueBox({
    valueBox("#139 -> #35", "Quintero (v1 -> v2)", icon = icon("arrow-up"),
             color = "green")
  })

  # Mapa leaflet
  output$mapa <- renderLeaflet({
    leaflet(mapa_data) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -71.5, lat = -35.5, zoom = 5) |>
      addPolygons(
        fillColor   = ~pal(quintil_v2_label),
        fillOpacity = 0.7,
        color       = "grey50",
        weight      = 0.5,
        popup       = ~popup_html,
        highlightOptions = highlightOptions(
          weight = 2, color = "#333", fillOpacity = 0.85,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = ~quintil_v2_label,
        title    = "CAI v2 (quintil)",
        opacity  = 0.8
      )
  })

  # Tabla DT
  output$tabla_full <- renderDT({
    datatable(
      datos_filtrados(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(6, "asc")),  # ordenar por ranking v2
        language = list(
          search = "Buscar comuna:",
          lengthMenu = "Mostrar _MENU_ comunas",
          info = "Mostrando _START_ a _END_ de _TOTAL_ comunas"
        )
      ),
      filter = "top",
      rownames = FALSE
    ) |>
      formatStyle(
        "Quintil v2",
        backgroundColor = styleEqual(
          names(colores_quintil),
          unname(colores_quintil)
        )
      )
  })
}

shinyApp(ui, server)
