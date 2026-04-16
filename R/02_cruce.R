# 02_cruce.R — Construcción de la tabla maestra comuna (año base 2024)
# Entrada : data/clean/retc.rds, data/clean/pobreza.rds, data/clean/comunas_sf.rds
# Salida  : data/clean/emisiones_long.rds   (comuna × contaminante)
#           data/clean/tabla_maestra.rds    (1 fila por comuna, todo junto)

source("R/00_setup.R")

retc       <- readRDS("data/clean/retc.rds")
pobreza    <- readRDS("data/clean/pobreza.rds")
comunas_sf <- readRDS("data/clean/comunas_sf.rds")

# =============================================================================
# 1) Agregar emisiones RETC a nivel comuna × contaminante (suma en toneladas)
# =============================================================================
emisiones_long <- retc |>
  dplyr::group_by(codigo_comuna, contaminante) |>
  dplyr::summarise(
    emision_ton = sum(emision_ton, na.rm = TRUE),
    .groups = "drop"
  )

# N° establecimientos únicos contaminantes por comuna
establecimientos <- retc |>
  dplyr::group_by(codigo_comuna) |>
  dplyr::summarise(
    n_establecimientos = dplyr::n_distinct(establecimiento),
    .groups = "drop"
  )

# Suma simple (toneladas totales por comuna, sin ponderar por toxicidad)
# Se conserva como referencia / control; el CAI usará la versión ponderada.
emisiones_totales <- emisiones_long |>
  dplyr::group_by(codigo_comuna) |>
  dplyr::summarise(
    emision_ton_total = sum(emision_ton, na.rm = TRUE),
    .groups = "drop"
  )

# =============================================================================
# 2) Construir tabla maestra: 1 fila por comuna
#    Base = universo de comunas del shapefile (345), así no perdemos comunas
#    que simplemente no declararon emisiones (quedan con 0).
# =============================================================================
universo_comunas <- comunas_sf |>
  sf::st_drop_geometry() |>
  dplyr::select(codigo_comuna) |>
  dplyr::distinct()

tabla_maestra <- universo_comunas |>
  dplyr::left_join(pobreza,            by = "codigo_comuna") |>
  dplyr::left_join(emisiones_totales,  by = "codigo_comuna") |>
  dplyr::left_join(establecimientos,   by = "codigo_comuna") |>
  dplyr::mutate(
    emision_ton_total  = tidyr::replace_na(emision_ton_total, 0),
    n_establecimientos = tidyr::replace_na(n_establecimientos, 0L)
  )

# Reportes rápidos
message("Tabla maestra: ", nrow(tabla_maestra), " comunas.")
message("  Con población: ", sum(!is.na(tabla_maestra$poblacion)))
message("  Sin pobreza (posible match fallido): ",
        sum(is.na(tabla_maestra$tasa_pobreza_ing)))
message("  Comunas con 0 emisiones declaradas: ",
        sum(tabla_maestra$emision_ton_total == 0))
message("  Comunas con datos completos (pobreza + emisiones>0): ",
        sum(!is.na(tabla_maestra$tasa_pobreza_ing) & tabla_maestra$emision_ton_total > 0))

# =============================================================================
# Guardar
# =============================================================================
saveRDS(emisiones_long, "data/clean/emisiones_long.rds")
saveRDS(tabla_maestra,  "data/clean/tabla_maestra.rds")

message("\n✓ Cruce OK. Tabla maestra lista en data/clean/tabla_maestra.rds")
