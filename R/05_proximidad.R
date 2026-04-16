# 05_proximidad.R — Exposición poblacional ponderada por proximidad
#
# Para cada celda poblada (~100 m) dentro de una comuna, suma las emisiones
# tox-equivalentes de todas las fuentes RETC dentro de un radio R, ponderadas
# por inversa del cuadrado de la distancia. La exposición comunal es la
# media ponderada por población de la exposición celda a celda.
#
# Esto resuelve el problema Quintero-Puchuncaví: una comuna recibe exposición
# de fuentes que están fuera de sus fronteras administrativas pero dentro del
# radio de influencia.
#
# Entrada : data/clean/retc.rds, data/clean/pesos_toxicidad.rds,
#           data/clean/comunas_sf.rds, data/raw/chl_ppp_2020_constrained.tif
# Salida  : data/clean/exposicion_comuna.rds, outputs/exposicion_diagnostico.csv

source("R/00_setup.R")

retc       <- readRDS("data/clean/retc.rds")
pesos_tox  <- readRDS("data/clean/pesos_toxicidad.rds")
comunas_sf <- readRDS("data/clean/comunas_sf.rds")

# Parámetros (configurables)
RADIO_M <- 5000      # radio de influencia (metros)
D_MIN   <- 100       # distancia mínima (m), evita división por 0

# =============================================================================
# 1) Fuentes emisoras: 1 fila por (establecimiento, lat, lon) con tox-eq total
# =============================================================================
fuentes <- retc |>
  dplyr::filter(!is.na(latitud), !is.na(longitud)) |>
  dplyr::inner_join(pesos_tox |> dplyr::select(contaminante, peso),
                    by = "contaminante") |>
  dplyr::mutate(emision_tox_eq = emision_ton * peso) |>
  dplyr::group_by(establecimiento, latitud, longitud) |>
  dplyr::summarise(emision_tox_eq = sum(emision_tox_eq, na.rm = TRUE),
                   .groups = "drop") |>
  dplyr::filter(emision_tox_eq > 0)

message("Fuentes emisoras únicas: ", nrow(fuentes))

fuentes_sf <- sf::st_as_sf(fuentes, coords = c("longitud","latitud"), crs = 4326)

# =============================================================================
# 2) Cargar raster de población
# =============================================================================
pop <- terra::rast("data/raw/chl_ppp_2020_constrained.tif")
message("Raster pop: ", terra::ncol(pop), " x ", terra::nrow(pop))

# =============================================================================
# 3) Loop por comuna
# =============================================================================
n_com <- nrow(comunas_sf)
res <- vector("list", n_com)

# Para spatial join eficiente: extender cada comuna en RADIO_M y buscar fuentes
# que caen dentro. Hacemos primero un join único.
comunas_buff_m <- comunas_sf |>
  sf::st_transform(32719) |>
  sf::st_buffer(RADIO_M) |>
  sf::st_transform(4326)

# Para cada comuna, lista de índices de fuentes dentro del buffer extendido
fuentes_por_comuna <- sf::st_intersects(comunas_buff_m, fuentes_sf)

t0 <- Sys.time()

for (i in seq_len(n_com)) {
  com_geom <- comunas_sf[i, ]
  cod_com  <- com_geom$codigo_comuna
  src_idx  <- fuentes_por_comuna[[i]]

  # Extraer celdas pobladas dentro de la comuna
  vals <- terra::extract(pop, terra::vect(com_geom), cells = TRUE, xy = TRUE,
                         ID = FALSE, weights = FALSE)
  vals <- vals[!is.na(vals[[1]]) & vals[[1]] > 0, , drop = FALSE]

  pop_total <- sum(vals[[1]], na.rm = TRUE)

  if (nrow(vals) == 0 || length(src_idx) == 0) {
    res[[i]] <- tibble::tibble(
      codigo_comuna = cod_com,
      poblacion_grid = pop_total,
      expo_promedio_hab = 0
    )
    next
  }

  cell_pop <- vals[[1]]
  cell_xy  <- as.matrix(vals[, c("x","y")])

  src_xy   <- sf::st_coordinates(fuentes_sf[src_idx, ])
  E_src    <- fuentes_sf$emision_tox_eq[src_idx]

  # Distancias geodésicas (metros) entre cada celda y cada fuente
  # geosphere::distm devuelve matriz n_celdas × n_fuentes
  d <- geosphere::distm(cell_xy, src_xy)
  d[d < D_MIN] <- D_MIN

  # Exposición por celda = Σ_src E_src / d_src^2
  cell_expo <- as.numeric((1 / d^2) %*% E_src)

  expo_total_pob <- sum(cell_expo * cell_pop, na.rm = TRUE)
  expo_prom_hab  <- expo_total_pob / pop_total

  res[[i]] <- tibble::tibble(
    codigo_comuna = cod_com,
    poblacion_grid = pop_total,
    expo_promedio_hab = expo_prom_hab
  )

  if (i %% 25 == 0) {
    elapsed <- as.numeric(Sys.time() - t0, units = "mins")
    message(sprintf("  comuna %d/%d (%.1f min)", i, n_com, elapsed))
  }
}

exposicion_comuna <- dplyr::bind_rows(res)

saveRDS(exposicion_comuna, "data/clean/exposicion_comuna.rds")
readr::write_csv(exposicion_comuna, "outputs/exposicion_diagnostico.csv")

elapsed <- as.numeric(Sys.time() - t0, units = "mins")
message(sprintf("\n✓ Exposición calculada para %d comunas en %.1f min.",
                nrow(exposicion_comuna), elapsed))
message("  Resultado en data/clean/exposicion_comuna.rds")
