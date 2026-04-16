# 08_viz_v2.R — Visualizaciones v2 y comparativas
# Entrada : data/clean/cai_v2.rds, data/clean/cai.rds, data/clean/comunas_sf.rds
# Salida  : outputs/04_mapa_cai_v2.png
#           outputs/05_comparacion_v1_v2.png
#           outputs/06_scatter_macrozona.png

source("R/00_setup.R")

if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(ggrepel)

cai_v1     <- readRDS("data/clean/cai.rds")
cai_v2     <- readRDS("data/clean/cai_v2.rds")
comunas_sf <- readRDS("data/clean/comunas_sf.rds")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

paleta_cai <- c(
  "Q1 · Muy bajo" = "#2c7bb6",
  "Q2 · Bajo"     = "#abd9e9",
  "Q3 · Medio"    = "#ffffbf",
  "Q4 · Alto"     = "#fdae61",
  "Q5 · Muy alto" = "#d7191c"
)

# =============================================================================
# 1) MAPA NACIONAL CAI V2
# =============================================================================
mapa_v2 <- comunas_sf |>
  dplyr::left_join(
    cai_v2 |> dplyr::select(codigo_comuna, nombre_comuna, cai_v2,
                            quintil_v2, quintil_v2_label,
                            expo_promedio_hab, tasa_pobreza_ing, poblacion),
    by = "codigo_comuna"
  )

p_mapa_v2 <- ggplot2::ggplot(mapa_v2) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = quintil_v2_label),
    color = "grey40", size = 0.05
  ) +
  ggplot2::scale_fill_manual(
    values = paleta_cai,
    na.value = "grey90",
    name = "CAI v2 (quintil)"
  ) +
  ggplot2::coord_sf(xlim = c(-76, -66), ylim = c(-56, -17)) +
  ggplot2::labs(
    title = "CAI v2 — Exposición poblacional por proximidad · Chile 2024",
    subtitle = "WorldPop 100m + buffers 5km + decay inverso al cuadrado × pobreza · escala log",
    caption = "Fuentes: RETC/MMA 2024 · Pobreza SAE CASEN 2024 · WorldPop 2020"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(face = "bold"),
    panel.grid      = ggplot2::element_blank(),
    axis.text       = ggplot2::element_blank(),
    axis.ticks      = ggplot2::element_blank(),
    axis.title      = ggplot2::element_blank(),
    legend.position = "right"
  )

ggplot2::ggsave("outputs/04_mapa_cai_v2.png",
                p_mapa_v2, width = 7, height = 11, dpi = 150, bg = "white")

message("✓ 04_mapa_cai_v2.png")

# =============================================================================
# 2) COMPARACIÓN V1 VS V2 — Top movers (mayor cambio de ranking)
# =============================================================================
comparacion <- cai_v1 |>
  dplyr::select(codigo_comuna, nombre_comuna, region,
                cai_v1 = cai, ranking_v1 = ranking,
                quintil_v1 = quintil_label) |>
  dplyr::inner_join(
    cai_v2 |> dplyr::select(codigo_comuna, cai_v2, ranking_v2,
                            quintil_v2 = quintil_v2_label),
    by = "codigo_comuna"
  ) |>
  dplyr::mutate(delta_ranking = ranking_v1 - ranking_v2)  # positivo = subió en v2

# Top 15 que más suben + top 15 que más bajan
top_sube <- comparacion |> dplyr::arrange(desc(delta_ranking)) |> head(15)
top_baja <- comparacion |> dplyr::arrange(delta_ranking) |> head(15)
movers <- dplyr::bind_rows(
  top_sube |> dplyr::mutate(tipo = "Sube en v2 (más expuesta)"),
  top_baja |> dplyr::mutate(tipo = "Baja en v2 (menos expuesta)")
)

p_movers <- ggplot2::ggplot(
  movers,
  ggplot2::aes(x = delta_ranking,
               y = forcats::fct_reorder(nombre_comuna, delta_ranking),
               fill = tipo)
) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(
    values = c("Sube en v2 (más expuesta)" = "#d7191c",
               "Baja en v2 (menos expuesta)" = "#2c7bb6"),
    name = NULL
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = delta_ranking,
                 hjust = ifelse(delta_ranking > 0, -0.15, 1.15)),
    size = 2.8
  ) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.15, 0.15))) +
  ggplot2::labs(
    title = "Mayores cambios de ranking: CAI v1 (per cápita) vs v2 (proximidad)",
    subtitle = "Delta positivo = comuna sube en v2 (fronteras administrativas ocultaban exposición real)",
    x = "Cambio de ranking (v1 - v2)", y = NULL,
    caption = "Caso emblemático: Quintero #139 → #35 (sube 104 posiciones)"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    legend.position = "top"
  )

ggplot2::ggsave("outputs/05_comparacion_v1_v2.png",
                p_movers, width = 10, height = 9, dpi = 150, bg = "white")

message("✓ 05_comparacion_v1_v2.png")

# =============================================================================
# 3) SCATTER POR MACROZONA — exposición v2 vs pobreza, facetado
# =============================================================================
asignar_macrozona <- function(codigo_comuna) {
  reg <- as.integer(substr(codigo_comuna, 1, 2))
  dplyr::case_when(
    reg %in% c(15, 1, 2)      ~ "Norte Grande",
    reg %in% c(3, 4)          ~ "Norte Chico",
    reg %in% c(5, 6, 13)      ~ "Centro",
    reg %in% c(7, 16, 8, 9)   ~ "Centro-Sur",
    reg %in% c(14, 10)        ~ "Sur",
    reg %in% c(11, 12)        ~ "Austral",
    TRUE                       ~ "Otro"
  )
}

datos_scatter <- cai_v2 |>
  dplyr::mutate(macrozona = asignar_macrozona(codigo_comuna)) |>
  dplyr::filter(!is.na(tasa_pobreza_ing), expo_promedio_hab > 0) |>
  dplyr::mutate(
    macrozona = factor(macrozona,
                       levels = c("Norte Grande","Norte Chico","Centro",
                                  "Centro-Sur","Sur","Austral"))
  )

# Zonas de sacrificio para destacar
zonas_destacadas <- c("Quintero","Puchuncaví","Coronel","Tocopilla","Tiltil",
                      "Hualpén","Talcahuano","Antofagasta","Calama",
                      "Mejillones","Sierra Gorda","Huasco",
                      "Llanquihue","Collipulli","Cunco","Ránquil")

marcar <- datos_scatter |>
  dplyr::filter(nombre_comuna %in% zonas_destacadas)

p_macro <- ggplot2::ggplot(
  datos_scatter,
  ggplot2::aes(x = tasa_pobreza_ing, y = expo_promedio_hab)
) +
  ggplot2::geom_point(ggplot2::aes(size = poblacion),
                      alpha = 0.3, color = "grey40") +
  ggplot2::geom_point(data = marcar, color = "#d7191c", size = 2) +
  ggrepel::geom_text_repel(
    data = marcar,
    ggplot2::aes(label = nombre_comuna),
    size = 2.5, max.overlaps = 15, segment.alpha = 0.4
  ) +
  ggplot2::scale_y_log10(labels = scales::label_comma()) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_size_area(max_size = 6, guide = "none") +
  ggplot2::facet_wrap(~macrozona, scales = "free_y", ncol = 3) +
  ggplot2::labs(
    title = "Exposición ambiental v2 vs. pobreza — por macrozona",
    subtitle = "Chile 2024 · Cada punto = una comuna · Tamaño = población",
    x = "Tasa de pobreza por ingresos",
    y = "Exposición promedio por habitante (log)",
    caption = "Fuentes: RETC/MMA 2024 · SAE CASEN 2024 · WorldPop 2020"
  ) +
  ggplot2::theme_minimal(base_size = 10) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    strip.text = ggplot2::element_text(face = "bold")
  )

ggplot2::ggsave("outputs/06_scatter_macrozona.png",
                p_macro, width = 12, height = 8, dpi = 150, bg = "white")

message("✓ 06_scatter_macrozona.png")
message("\n✓ Visualizaciones v2 completas.")
