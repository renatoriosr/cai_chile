# 04_viz.R — Visualizaciones estáticas del CAI v1
# Entrada : data/clean/cai.rds, data/clean/comunas_sf.rds
# Salida  : outputs/*.png

source("R/00_setup.R")

if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(ggrepel)

cai        <- readRDS("data/clean/cai.rds")
comunas_sf <- readRDS("data/clean/comunas_sf.rds")

# Unir geometría con CAI
mapa <- comunas_sf |>
  dplyr::left_join(
    cai |> dplyr::select(codigo_comuna, nombre_comuna, region,
                         cai, quintil, quintil_label,
                         emision_tox_pc, tasa_pobreza_ing, poblacion),
    by = "codigo_comuna"
  )

# =============================================================================
# 1) MAPA NACIONAL — CAI por quintiles
# =============================================================================
paleta_cai <- c(
  "Q1 · Muy bajo" = "#2c7bb6",
  "Q2 · Bajo"     = "#abd9e9",
  "Q3 · Medio"    = "#ffffbf",
  "Q4 · Alto"     = "#fdae61",
  "Q5 · Muy alto" = "#d7191c"
)

p_mapa <- ggplot2::ggplot(mapa) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = quintil_label),
    color = "grey40", size = 0.05
  ) +
  ggplot2::scale_fill_manual(
    values = paleta_cai,
    na.value = "grey90",
    name = "CAI (quintil)"
  ) +
  ggplot2::coord_sf(xlim = c(-76, -66), ylim = c(-56, -17)) +
  ggplot2::labs(
    title = "Índice de Carga Ambiental Injusta (CAI) por comuna — Chile 2024",
    subtitle = "Emisiones industriales ponderadas por toxicidad × pobreza por ingresos · escala log",
    caption = "Fuentes: RETC/MMA 2024 · Pobreza SAE CASEN 2024 · chilemapas"
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

ggplot2::ggsave("outputs/01_mapa_cai_quintiles.png",
                p_mapa, width = 7, height = 11, dpi = 150, bg = "white")

# =============================================================================
# 2) TOP 25 comunas con mayor CAI
# =============================================================================
top25 <- cai |> dplyr::arrange(desc(cai)) |> head(25)

p_top <- ggplot2::ggplot(
  top25,
  ggplot2::aes(x = cai,
               y = forcats::fct_reorder(nombre_comuna, cai),
               fill = quintil_label)
) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = paleta_cai, guide = "none") +
  ggplot2::geom_text(ggplot2::aes(label = round(cai, 1)),
                     hjust = -0.15, size = 3) +
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
  ggplot2::labs(
    title = "Top 25 comunas con mayor Carga Ambiental Injusta (CAI v1)",
    subtitle = "Escala log 0-100 · 2024",
    x = "CAI", y = NULL
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

ggplot2::ggsave("outputs/02_top25_cai.png",
                p_top, width = 9, height = 8, dpi = 150, bg = "white")

# =============================================================================
# 3) SCATTER emisión per cápita (log) vs pobreza — con zonas de sacrificio
# =============================================================================
zonas_destacadas <- c("Sierra Gorda","Mejillones","Huasco","Calama",
                      "Puchuncaví","Quintero","Coronel","Tocopilla","Tiltil",
                      "Hualpén","Talcahuano","Antofagasta","Chillán")

marcar <- cai |> dplyr::filter(nombre_comuna %in% zonas_destacadas)

p_scatter <- ggplot2::ggplot(
  cai |> dplyr::filter(emision_tox_pc > 0),
  ggplot2::aes(x = tasa_pobreza_ing, y = emision_tox_pc)
) +
  ggplot2::geom_point(ggplot2::aes(size = poblacion),
                      alpha = 0.35, color = "grey40") +
  ggplot2::geom_point(data = marcar, color = "#d7191c", size = 2.5) +
  ggrepel::geom_text_repel(
    data = marcar,
    ggplot2::aes(label = nombre_comuna),
    size = 3, max.overlaps = 20
  ) +
  ggplot2::scale_y_log10(labels = scales::label_comma()) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_size_area(max_size = 8, guide = "none") +
  ggplot2::labs(
    title = "Emisiones industriales per cápita vs. pobreza comunal",
    subtitle = "Chile 2024 · Cada punto = una comuna · Tamaño = población",
    x = "Tasa de pobreza por ingresos (SAE CASEN 2024)",
    y = "Emisión tox-eq PM2.5 per cápita (ton/hab, escala log)",
    caption = sprintf(
      "Pearson = %.3f · Spearman = %.3f",
      cor(cai$emision_tox_pc, cai$tasa_pobreza_ing,
          use = "pairwise.complete.obs", method = "pearson"),
      cor(cai$emision_tox_pc, cai$tasa_pobreza_ing,
          use = "pairwise.complete.obs", method = "spearman")
    )
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

ggplot2::ggsave("outputs/03_scatter_emisiones_pobreza.png",
                p_scatter, width = 9, height = 7, dpi = 150, bg = "white")

message("\n✓ Visualizaciones guardadas en outputs/:")
message("   01_mapa_cai_quintiles.png")
message("   02_top25_cai.png")
message("   03_scatter_emisiones_pobreza.png")
