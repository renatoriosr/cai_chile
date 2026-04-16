# 03_indice.R — Cálculo del Índice de Carga Ambiental Injusta (CAI)
# Entrada : data/clean/emisiones_long.rds
#           data/clean/pesos_toxicidad.rds
#           data/clean/tabla_maestra.rds
# Salida  : data/clean/cai.rds
#           outputs/cai_ranking.csv

source("R/00_setup.R")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

emisiones_long <- readRDS("data/clean/emisiones_long.rds")
pesos_tox      <- readRDS("data/clean/pesos_toxicidad.rds")
tabla          <- readRDS("data/clean/tabla_maestra.rds")

# =============================================================================
# 1) Emisiones ponderadas por toxicidad, agregadas por comuna
# =============================================================================
# Decisión C: cada contaminante se multiplica por su peso de toxicidad relativa
# (PM2.5 = 1). Unidad resultante: "toneladas equivalentes PM2.5".
emisiones_tox <- emisiones_long |>
  dplyr::inner_join(
    pesos_tox |> dplyr::select(contaminante, peso),
    by = "contaminante"
  ) |>
  dplyr::mutate(
    emision_tox_eq = emision_ton * peso
  ) |>
  dplyr::group_by(codigo_comuna) |>
  dplyr::summarise(
    emision_tox_total = sum(emision_tox_eq, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# 2) Unir con la tabla maestra y calcular per cápita
# =============================================================================
base_cai <- tabla |>
  dplyr::left_join(emisiones_tox, by = "codigo_comuna") |>
  dplyr::mutate(
    emision_tox_total  = tidyr::replace_na(emision_tox_total, 0),
    emision_tox_pc     = emision_tox_total / poblacion                    # ton-eq PM2.5 / hab
  )

# =============================================================================
# 3) Fórmula CAI (v0)
#    CAI_raw = (emision_tox_pc / media_nacional) * (1 + tasa_pobreza_ing)
#    Media nacional: media simple sobre comunas con población > 0
# =============================================================================
media_nac_pc <- base_cai |>
  dplyr::filter(poblacion > 0) |>
  dplyr::pull(emision_tox_pc) |>
  mean(na.rm = TRUE)

base_cai <- base_cai |>
  dplyr::mutate(
    cai_raw_ing  = (emision_tox_pc / media_nac_pc) * (1 + tasa_pobreza_ing),
    cai_raw_mult = (emision_tox_pc / media_nac_pc) * (1 + tasa_pobreza_mult)
  )

# =============================================================================
# 4) Normalización log (Decisión D-2) a escala 0-100
# =============================================================================
norm_log_100 <- function(x) {
  y  <- log1p(x)                    # log(x+1), preserva ceros
  lo <- min(y, na.rm = TRUE)
  hi <- max(y, na.rm = TRUE)
  100 * (y - lo) / (hi - lo)
}

cai <- base_cai |>
  dplyr::mutate(
    cai           = norm_log_100(cai_raw_ing),
    cai_mult      = norm_log_100(cai_raw_mult),     # versión de sensibilidad
    quintil       = dplyr::ntile(cai, 5),
    quintil_label = dplyr::case_when(
      quintil == 1 ~ "Q1 · Muy bajo",
      quintil == 2 ~ "Q2 · Bajo",
      quintil == 3 ~ "Q3 · Medio",
      quintil == 4 ~ "Q4 · Alto",
      quintil == 5 ~ "Q5 · Muy alto"
    ),
    ranking       = rank(-cai, ties.method = "min")
  )

# =============================================================================
# 5) Reportes de validación
# =============================================================================
top10 <- cai |>
  dplyr::arrange(desc(cai)) |>
  dplyr::select(ranking, nombre_comuna, region, cai, quintil_label,
                emision_tox_pc, tasa_pobreza_ing) |>
  head(10)

message("\nMedia nacional emisión tox-eq per cápita (ton-eq PM2.5 / hab): ",
        round(media_nac_pc, 6))
message("\nTop 10 comunas por CAI:")
print(top10, n = 10)

# --- Correlaciones con test de significancia ---
test_pearson <- cor.test(cai$emision_tox_pc, cai$tasa_pobreza_ing,
                         method = "pearson")
test_spearman <- cor.test(cai$emision_tox_pc, cai$tasa_pobreza_ing,
                          method = "spearman", exact = FALSE)

message("\nCorrelación emisión_pc ~ pobreza_ingresos (v1):")
message("  Pearson  r = ", round(test_pearson$estimate, 4),
        "  [IC 95%: ", round(test_pearson$conf.int[1], 4),
        ", ", round(test_pearson$conf.int[2], 4), "]",
        "  p = ", format.pval(test_pearson$p.value, digits = 3))
message("  Spearman ρ = ", round(test_spearman$estimate, 4),
        "  p = ", format.pval(test_spearman$p.value, digits = 3))

# =============================================================================
# 6) Guardar
# =============================================================================
saveRDS(cai, "data/clean/cai.rds")

cai |>
  dplyr::arrange(ranking) |>
  dplyr::select(ranking, codigo_comuna, nombre_comuna, region,
                poblacion, tasa_pobreza_ing, tasa_pobreza_mult,
                emision_tox_total, emision_tox_pc,
                cai, cai_mult, quintil, quintil_label) |>
  readr::write_csv("outputs/cai_ranking.csv")

message("\n✓ CAI calculado. cai.rds y outputs/cai_ranking.csv listos.")
