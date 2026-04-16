# toxicidad_pesos.R — Pesos de toxicidad relativa para el cálculo del CAI
#
# Método: peso = (valor_referencia_PM2.5) / (valor_referencia_contaminante)
# Es decir, peso inversamente proporcional al valor guía / nivel de referencia.
# A menor valor guía (más tóxico), mayor peso. Normalizado con PM2.5 = 1.
#
# Fuentes:
#   - OMS Air Quality Guidelines (2021): PM2.5, PM10, NO2, SO2, CO
#   - US EPA NAAQS: Lead
#   - ATSDR Minimal Risk Levels (MRL) crónicos inhalación: Arsenic, Mercury, Cadmium
#
# Todas las referencias expresadas en µg/m³ (o convertidas).
# Para gases (CO, NO2, SO2), se usa el valor anual/24h según OMS AQG 2021.
# Este archivo documenta la construcción de los pesos y los guarda para uso
# posterior en 03_indice.R.

library(tibble)

# Valor de referencia en µg/m³. Menor = más tóxico.
referencias <- tibble::tribble(
  ~contaminante,              ~ref_ug_m3,  ~fuente,
  "PM2.5, primary",                  5.0,  "OMS AQG 2021 anual",
  "PM10, primary",                  15.0,  "OMS AQG 2021 anual",
  "Nitrogen oxides (NOx)",          10.0,  "OMS AQG 2021 anual (como NO2)",
  "Sulfur dioxide",                 40.0,  "OMS AQG 2021 24h",
  "Carbon monoxide",              4000.0,  "OMS AQG 2021 24h (4 mg/m³)",
  "Lead",                            0.15, "EPA NAAQS trimestral",
  "Arsenic",                         0.015,"EPA IUR cancer 1e-6",
  "Mercury",                         0.3,  "ATSDR MRL crónico inhalación",
  "Cadmium",                         0.01, "ATSDR MRL crónico inhalación"
)

ref_pm25 <- referencias$ref_ug_m3[referencias$contaminante == "PM2.5, primary"]

pesos_tox <- referencias |>
  dplyr::mutate(
    peso = ref_pm25 / ref_ug_m3       # PM2.5 = 1 por construcción
  ) |>
  dplyr::arrange(dplyr::desc(peso))

print(pesos_tox)

saveRDS(pesos_tox, "data/clean/pesos_toxicidad.rds")
message("\n✓ Pesos guardados en data/clean/pesos_toxicidad.rds")
