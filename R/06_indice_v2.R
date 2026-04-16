# 06_indice_v2.R — CAI v2: índice basado en exposición poblacional ponderada
# Entrada : data/clean/exposicion_comuna.rds, data/clean/tabla_maestra.rds,
#           data/clean/cai.rds  (para comparación)
# Salida  : data/clean/cai_v2.rds, outputs/cai_v2_ranking.csv,
#           outputs/comparacion_v1_v2.csv

source("R/00_setup.R")

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

expo   <- readRDS("data/clean/exposicion_comuna.rds")
tabla  <- readRDS("data/clean/tabla_maestra.rds")
cai_v1 <- readRDS("data/clean/cai.rds")

# =============================================================================
# 1) Construir base CAI v2
# =============================================================================
base_v2 <- tabla |>
  dplyr::left_join(expo, by = "codigo_comuna") |>
  dplyr::mutate(
    expo_promedio_hab = tidyr::replace_na(expo_promedio_hab, 0)
  )

media_nac_expo <- base_v2 |>
  dplyr::filter(poblacion > 0, expo_promedio_hab > 0) |>
  dplyr::pull(expo_promedio_hab) |>
  mean(na.rm = TRUE)

base_v2 <- base_v2 |>
  dplyr::mutate(
    cai_v2_raw_ing  = (expo_promedio_hab / media_nac_expo) * (1 + tasa_pobreza_ing),
    cai_v2_raw_mult = (expo_promedio_hab / media_nac_expo) * (1 + tasa_pobreza_mult)
  )

# Normalización log (decisión D-2)
norm_log_100 <- function(x) {
  y  <- log1p(x)
  lo <- min(y, na.rm = TRUE)
  hi <- max(y, na.rm = TRUE)
  100 * (y - lo) / (hi - lo)
}

cai_v2 <- base_v2 |>
  dplyr::mutate(
    cai_v2          = norm_log_100(cai_v2_raw_ing),
    cai_v2_mult     = norm_log_100(cai_v2_raw_mult),
    quintil_v2      = dplyr::ntile(cai_v2, 5),
    quintil_v2_label = dplyr::case_when(
      quintil_v2 == 1 ~ "Q1 · Muy bajo",
      quintil_v2 == 2 ~ "Q2 · Bajo",
      quintil_v2 == 3 ~ "Q3 · Medio",
      quintil_v2 == 4 ~ "Q4 · Alto",
      quintil_v2 == 5 ~ "Q5 · Muy alto"
    ),
    ranking_v2 = rank(-cai_v2, ties.method = "min")
  )

# =============================================================================
# 2) Reportes
# =============================================================================
top15_v2 <- cai_v2 |>
  dplyr::arrange(desc(cai_v2)) |>
  dplyr::select(ranking_v2, nombre_comuna, region, cai_v2,
                quintil_v2_label, expo_promedio_hab, tasa_pobreza_ing,
                poblacion) |>
  head(15)

message("\nTop 15 comunas — CAI v2 (exposición por proximidad):")
print(top15_v2, n = 15)

# --- Correlaciones con test de significancia ---
test_pearson_v2 <- cor.test(cai_v2$expo_promedio_hab, cai_v2$tasa_pobreza_ing,
                            method = "pearson")
test_spearman_v2 <- cor.test(cai_v2$expo_promedio_hab, cai_v2$tasa_pobreza_ing,
                             method = "spearman", exact = FALSE)

message("\nCorrelación exposición ~ pobreza_ingresos (v2):")
message("  Pearson  r = ", round(test_pearson_v2$estimate, 4),
        "  [IC 95%: ", round(test_pearson_v2$conf.int[1], 4),
        ", ", round(test_pearson_v2$conf.int[2], 4), "]",
        "  p = ", format.pval(test_pearson_v2$p.value, digits = 3))
message("  Spearman ρ = ", round(test_spearman_v2$estimate, 4),
        "  p = ", format.pval(test_spearman_v2$p.value, digits = 3))

# =============================================================================
# 3) Comparación v1 vs v2
# =============================================================================
comparacion <- cai_v1 |>
  dplyr::select(codigo_comuna, nombre_comuna, region,
                cai_v1 = cai, ranking_v1 = ranking,
                quintil_v1 = quintil_label) |>
  dplyr::inner_join(
    cai_v2 |> dplyr::select(codigo_comuna,
                            cai_v2, ranking_v2,
                            quintil_v2 = quintil_v2_label,
                            expo_promedio_hab),
    by = "codigo_comuna"
  ) |>
  dplyr::mutate(
    cambio_ranking = ranking_v1 - ranking_v2  # positivo = subió en v2
  )

# Movimientos significativos para zonas-ícono
zonas_destacadas <- c("Quintero","Puchuncaví","Coronel","Tocopilla","Tiltil",
                      "Lota","Talcahuano","Hualpén","Antofagasta",
                      "Sierra Gorda","Mejillones","Huasco","Calama",
                      "Río Verde","San Gregorio","Primavera",
                      "Chillán","Chillán Viejo")

mov <- comparacion |>
  dplyr::filter(nombre_comuna %in% zonas_destacadas) |>
  dplyr::arrange(ranking_v2) |>
  dplyr::select(nombre_comuna, region,
                ranking_v1, quintil_v1, cai_v1,
                ranking_v2, quintil_v2, cai_v2,
                cambio_ranking)

message("\n--- Movimientos v1 → v2 en zonas-ícono ---")
print(mov, n = Inf)

# =============================================================================
# 4) Guardar
# =============================================================================
saveRDS(cai_v2, "data/clean/cai_v2.rds")

cai_v2 |>
  dplyr::arrange(ranking_v2) |>
  dplyr::select(ranking_v2, codigo_comuna, nombre_comuna, region,
                poblacion, tasa_pobreza_ing, tasa_pobreza_mult,
                expo_promedio_hab,
                cai_v2, cai_v2_mult, quintil_v2, quintil_v2_label) |>
  readr::write_csv("outputs/cai_v2_ranking.csv")

readr::write_csv(comparacion, "outputs/comparacion_v1_v2.csv")

message("\n✓ CAI v2 calculado.")
message("  data/clean/cai_v2.rds")
message("  outputs/cai_v2_ranking.csv")
message("  outputs/comparacion_v1_v2.csv")
