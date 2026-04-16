# 07_analisis_avanzado.R — Análisis complementarios para discusión
# a) Identificar empresas detrás de comunas sorpresa top-15 v2
# b) Correlación desagregada por macrozona
# Entrada : data/clean/retc.rds, data/clean/cai_v2.rds, data/clean/cai.rds
# Salida  : outputs/empresas_sorpresas_v2.csv
#           outputs/correlaciones_macrozona.csv

source("R/00_setup.R")

retc   <- readRDS("data/clean/retc.rds")
cai_v2 <- readRDS("data/clean/cai_v2.rds")
cai_v1 <- readRDS("data/clean/cai.rds")

# =============================================================================
# A) EMPRESAS DETRÁS DE LAS SORPRESAS TOP-15 V2
# =============================================================================
# Comunas sorpresa: aparecen en top-15 v2 pero no en el discurso periodístico
sorpresas <- c("Llanquihue", "Collipulli", "Cunco", "Ránquil")

# También incluimos todo el top-15 para tener el cuadro completo
top15_codigos <- cai_v2 |>
  dplyr::arrange(ranking_v2) |>
  head(15) |>
  dplyr::pull(codigo_comuna)

# Para las sorpresas: qué empresas emiten y cuánto
empresas_top15 <- retc |>
  dplyr::filter(codigo_comuna %in% top15_codigos) |>
  dplyr::group_by(codigo_comuna, comuna_retc, establecimiento, rubro) |>
  dplyr::summarise(
    emision_ton_total = sum(emision_ton, na.rm = TRUE),
    contaminantes = paste(unique(contaminante), collapse = " | "),
    .groups = "drop"
  ) |>
  dplyr::left_join(
    cai_v2 |> dplyr::select(codigo_comuna, nombre_comuna, ranking_v2, cai_v2),
    by = "codigo_comuna"
  ) |>
  dplyr::arrange(ranking_v2, desc(emision_ton_total))

message("=== EMPRESAS EN TOP-15 CAI V2 ===")
for (cod in top15_codigos) {
  sub <- empresas_top15 |> dplyr::filter(codigo_comuna == cod)
  if (nrow(sub) > 0) {
    nom <- sub$nombre_comuna[1]
    rk  <- sub$ranking_v2[1]
    message("\n--- #", rk, " ", nom, " (", nrow(sub), " establecimientos) ---")
    for (i in seq_len(min(nrow(sub), 5))) {
      message("  ", sub$establecimiento[i], " | ", sub$rubro[i],
              " | ", round(sub$emision_ton_total[i], 1), " ton")
    }
  }
}

# Detalle específico de las 4 sorpresas
empresas_sorpresas <- empresas_top15 |>
  dplyr::filter(nombre_comuna %in% sorpresas)

message("\n=== DETALLE COMUNAS SORPRESA ===")
print(empresas_sorpresas, n = Inf)

readr::write_csv(empresas_top15, "outputs/empresas_top15_v2.csv")

# =============================================================================
# B) CORRELACIÓN POR MACROZONA
# =============================================================================
# Macrozonas basadas en la geografía económica chilena:
#   Norte Grande (Arica, Tarapacá, Antofagasta)
#   Norte Chico (Atacama, Coquimbo)
#   Centro (Valparaíso, O'Higgins, Metropolitana)
#   Centro-Sur (Maule, Ñuble, Biobío, Araucanía)
#   Sur (Los Ríos, Los Lagos)
#   Austral (Aysén, Magallanes)

# Mapeo por código de región (2 primeros dígitos del código comuna)
asignar_macrozona <- function(codigo_comuna) {
  reg <- as.integer(substr(codigo_comuna, 1, 2))
  dplyr::case_when(
    reg %in% c(15, 1, 2)           ~ "Norte Grande",
    reg %in% c(3, 4)               ~ "Norte Chico",
    reg %in% c(5, 6, 13)           ~ "Centro",
    reg %in% c(7, 16, 8, 9)        ~ "Centro-Sur",
    reg %in% c(14, 10)             ~ "Sur",
    reg %in% c(11, 12)             ~ "Austral",
    TRUE                            ~ "Otro"
  )
}

# Usar cai_v2 que tiene expo_promedio_hab y pobreza
datos_macro <- cai_v2 |>
  dplyr::mutate(macrozona = asignar_macrozona(codigo_comuna)) |>
  dplyr::filter(!is.na(tasa_pobreza_ing))

# También agregar emisión per cápita de v1
datos_macro <- datos_macro |>
  dplyr::left_join(
    cai_v1 |> dplyr::select(codigo_comuna, emision_tox_pc),
    by = "codigo_comuna"
  )

# Correlaciones por macrozona
resultados_macro <- datos_macro |>
  dplyr::group_by(macrozona) |>
  dplyr::summarise(
    n_comunas = dplyr::n(),
    # v1: emisión per cápita vs pobreza
    pearson_v1  = tryCatch(cor.test(emision_tox_pc, tasa_pobreza_ing,
                                    method = "pearson")$estimate, error = function(e) NA_real_),
    p_pearson_v1 = tryCatch(cor.test(emision_tox_pc, tasa_pobreza_ing,
                                     method = "pearson")$p.value, error = function(e) NA_real_),
    spearman_v1 = tryCatch(cor.test(emision_tox_pc, tasa_pobreza_ing,
                                    method = "spearman", exact = FALSE)$estimate, error = function(e) NA_real_),
    p_spearman_v1 = tryCatch(cor.test(emision_tox_pc, tasa_pobreza_ing,
                                      method = "spearman", exact = FALSE)$p.value, error = function(e) NA_real_),
    # v2: exposición por proximidad vs pobreza
    pearson_v2  = tryCatch(cor.test(expo_promedio_hab, tasa_pobreza_ing,
                                    method = "pearson")$estimate, error = function(e) NA_real_),
    p_pearson_v2 = tryCatch(cor.test(expo_promedio_hab, tasa_pobreza_ing,
                                     method = "pearson")$p.value, error = function(e) NA_real_),
    spearman_v2 = tryCatch(cor.test(expo_promedio_hab, tasa_pobreza_ing,
                                    method = "spearman", exact = FALSE)$estimate, error = function(e) NA_real_),
    p_spearman_v2 = tryCatch(cor.test(expo_promedio_hab, tasa_pobreza_ing,
                                      method = "spearman", exact = FALSE)$p.value, error = function(e) NA_real_),
    # Contexto
    media_pobreza = mean(tasa_pobreza_ing, na.rm = TRUE),
    media_expo_v2 = mean(expo_promedio_hab, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(match(macrozona, c("Norte Grande","Norte Chico","Centro",
                                     "Centro-Sur","Sur","Austral")))

message("\n=== CORRELACIONES POR MACROZONA ===")
print(resultados_macro, n = Inf, width = Inf)

readr::write_csv(resultados_macro, "outputs/correlaciones_macrozona.csv")

message("\n✓ Análisis avanzado OK.")
message("  outputs/empresas_top15_v2.csv")
message("  outputs/correlaciones_macrozona.csv")
