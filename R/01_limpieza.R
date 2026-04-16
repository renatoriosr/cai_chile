# 01_limpieza.R — Carga y limpieza de fuentes crudas (año base: 2024)
# Entrada : data/raw/*
# Salida  : data/clean/retc.rds
#           data/clean/pobreza.rds
#           data/clean/comunas_sf.rds

source("R/00_setup.R")

dir.create("data/clean", showWarnings = FALSE, recursive = TRUE)

# Helper: normaliza nombres de comuna para matching robusto
#   - minúsculas, sin acentos, sin puntuación, sin paréntesis anidados, trimmed
normaliza_comuna <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_replace_all("\\(.*?\\)", " ") |>   # elimina "(Ex-Navarino)", etc.
    stringr::str_replace_all("[^a-z ]", " ") |>     # deja solo letras y espacios
    stringr::str_squish()
}

# Patches manuales para discrepancias reales de nombre entre RETC y pobreza
# (ya normalizadas). Lado izquierdo = lo que trae RETC; derecho = canon pobreza.
patches_nombre <- c(
  "coihaique"                = "coyhaique",
  "ohiggins"                 = "o higgins",
  "san pedro atacama"        = "san pedro de atacama"
)

# =============================================================================
# 1) POBREZA (CASEN SAE 2024) — ingresos + multidimensional + población
# =============================================================================

# Ambos archivos tienen la misma estructura: fila 1 es título, fila 2 cabecera.
# Saltar 1 fila con skip = 1.

pob_ing <- readxl::read_excel(
  "data/raw/pobreza_ingresos_2024.xlsx",
  skip = 1
) |>
  janitor::clean_names() |>
  dplyr::select(
    codigo_comuna  = codigo,
    region,
    nombre_comuna,
    poblacion      = numero_de_personas_segun_proyecciones_de_poblacion,
    n_pobres_ing   = numero_de_personas_en_situacion_de_pobreza_de_ingresos,
    tasa_pobreza_ing = porcentaje_de_personas_en_situacion_de_pobreza_de_ingresos_2024,
    tipo_estimacion_ing = tipo_de_estimacion_sae
  )

pob_mult <- readxl::read_excel(
  "data/raw/pobreza_multidimensional_2024.xlsx",
  skip = 1
) |>
  janitor::clean_names() |>
  dplyr::select(
    codigo_comuna = codigo,
    n_pobres_mult = numero_de_personas_en_situacion_de_pobreza_multidimensional,
    tasa_pobreza_mult = porcentaje_de_personas_en_situacion_de_pobreza_multidimensional_2024,
    tipo_estimacion_mult = tipo_de_estimacion_sae
  )

pobreza <- pob_ing |>
  dplyr::inner_join(pob_mult, by = "codigo_comuna") |>
  dplyr::mutate(
    codigo_comuna  = stringr::str_pad(codigo_comuna, width = 5, pad = "0"),
    nombre_comuna_norm = normaliza_comuna(nombre_comuna)
  ) |>
  dplyr::filter(!is.na(codigo_comuna), poblacion > 0)

message("Pobreza: ", nrow(pobreza), " comunas cargadas.")

# =============================================================================
# 2) RETC — emisiones puntuales al aire 2024
# =============================================================================

# Contaminantes priorizados (salud humana local, evita doble conteo SO2/SOx,
# excluye gases de efecto invernadero):
contaminantes_salud <- c(
  "PM10, primary",
  "PM2.5, primary",
  "Sulfur dioxide",
  "Nitrogen oxides (NOx)",
  "Carbon monoxide",
  "Arsenic",
  "Lead",
  "Mercury",
  "Cadmium"
)

retc_raw <- readxl::read_excel(
  "data/raw/retc/retc_puntuales_aire.xlsx",
  sheet = "Data"
)

retc_prelim <- retc_raw |>
  janitor::clean_names() |>
  dplyr::filter(
    tipo_outlier == "No es outlier",             # Decisión A-i
    contaminante %in% contaminantes_salud        # Decisión B
  ) |>
  dplyr::transmute(
    anio          = as.integer(ano),
    region,
    comuna_retc   = comuna,
    latitud       = as.numeric(latitud),
    longitud      = as.numeric(longitud),
    rubro,
    establecimiento = nombre_establecimiento,
    contaminante,
    emision_ton   = as.numeric(emision_retc)     # toneladas/año
  ) |>
  dplyr::filter(!is.na(emision_ton), emision_ton >= 0) |>
  dplyr::mutate(
    nombre_comuna_norm = normaliza_comuna(comuna_retc),
    # aplica patches manuales (Coihaique → Coyhaique, etc.)
    nombre_comuna_norm = dplyr::coalesce(patches_nombre[nombre_comuna_norm],
                                         nombre_comuna_norm)
  )

# Lookup canónico nombre_normalizado → código INE, desde pobreza
lookup_comuna <- pobreza |>
  dplyr::select(nombre_comuna_norm, codigo_comuna) |>
  dplyr::distinct()

retc <- retc_prelim |>
  dplyr::left_join(lookup_comuna, by = "nombre_comuna_norm")

# Diagnóstico de match
sin_match <- retc |> dplyr::filter(is.na(codigo_comuna)) |>
  dplyr::distinct(comuna_retc, nombre_comuna_norm)
if (nrow(sin_match) > 0) {
  warning("Comunas RETC sin match con pobreza:\n",
          paste(capture.output(print(sin_match, n = Inf)), collapse = "\n"))
} else {
  message("RETC: match 100% por nombre con pobreza.")
}

retc <- retc |> dplyr::filter(!is.na(codigo_comuna))

message("RETC: ", nrow(retc), " registros después de filtrar outliers + contaminantes salud.")
message("      Comunas con emisión registrada: ", dplyr::n_distinct(retc$codigo_comuna))

# =============================================================================
# 3) SHAPEFILES DE COMUNAS (chilemapas)
# =============================================================================

comunas_sf <- chilemapas::mapa_comunas |>
  sf::st_as_sf() |>
  dplyr::rename(codigo_comuna = codigo_comuna)

message("Shapefile: ", nrow(comunas_sf), " comunas.")

# =============================================================================
# Guardar
# =============================================================================
saveRDS(pobreza,    "data/clean/pobreza.rds")
saveRDS(retc,       "data/clean/retc.rds")
saveRDS(comunas_sf, "data/clean/comunas_sf.rds")

message("\n✓ Limpieza OK. Archivos guardados en data/clean/")
