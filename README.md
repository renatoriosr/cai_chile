# Indice de Carga Ambiental Injusta (CAI) — Chile 2024

> La contaminacion industrial en Chile sigue la geografia de la pobreza?

Analisis de datos que cruza emisiones industriales declaradas al RETC (Ministerio del Medio Ambiente) con pobreza comunal (SAE CASEN 2024) para las 345 comunas de Chile continental.

## Hallazgo principal

La correlacion nacional entre emisiones y pobreza es **debilmente negativa** (Spearman = -0.292, p < 0.001), refutando la hipotesis clasica de *environmental justice* trasladada desde la literatura norteamericana. Chile presenta un patron **extractivo-enclave**: la gran mineria del norte genera altas emisiones en comunas con poblaciones pequenas y salarios altos, invirtiendo la relacion esperada.

Sin embargo, el analisis por macrozona revela que la zona **Centro-Sur industrial** (Biobio, Nuble, Araucania) si presenta la relacion esperada entre contaminacion y vulnerabilidad.

## Dos versiones del indice

- **v1 (per capita):** Emisiones ponderadas por toxicidad / poblacion comunal. Sesgo hacia enclaves mineros de baja poblacion.
- **v2 (proximidad):** Exposicion poblacional real usando raster WorldPop 100m + buffers 5km + decay inverso al cuadrado. Corrige el sesgo de fronteras administrativas.

**Validacion clave:** Quintero paso de #139 (v1) a #35 (v2) — el complejo Ventanas esta en Puchuncavi pero afecta a Quintero por vientos. La v2 captura esto.

## Estructura

```
R/
  00_setup.R             # librerias
  01_limpieza.R          # RETC + pobreza + shapefiles
  02_cruce.R             # tabla maestra comuna-ano
  03_indice.R            # CAI v1 con cor.test() y p-values
  04_viz.R               # visualizaciones v1 (mapa, top25, scatter)
  05_proximidad.R        # exposicion por proximidad (WorldPop raster)
  06_indice_v2.R         # CAI v2
  07_analisis_avanzado.R # empresas por comuna + correlaciones macrozona
  08_viz_v2.R            # visualizaciones v2 (mapa, movers, scatter macrozona)
  toxicidad_pesos.R      # pesos OMS/EPA/ATSDR por contaminante
app/
  app.R                  # Shiny dashboard interactivo
informe/
  informe.qmd            # informe academico completo (Quarto)
outputs/
  *.png                  # 6 visualizaciones
  *.csv                  # rankings, comparaciones, correlaciones
```

## Fuentes de datos

| Fuente | Institucion | Ano |
|--------|-------------|-----|
| [RETC — Emisiones puntuales aire](https://datosretc.mma.gob.cl) | Ministerio del Medio Ambiente | 2024 |
| [Pobreza SAE por ingresos](https://observatorio.ministeriodesarrollosocial.gob.cl) | CASEN / MDS | 2024 |
| [Pobreza SAE multidimensional](https://observatorio.ministeriodesarrollosocial.gob.cl) | CASEN / MDS | 2024 |
| [chilemapas](https://pacha.dev/chilemapas/) | Paquete R | 2017 |
| [WorldPop Constrained](https://www.worldpop.org/) | University of Southampton | 2020 |

Los datos crudos no se incluyen en el repositorio por peso. Se pueden descargar desde los links anteriores.

## Reproduccion

1. Clonar el repo
2. Descargar los datos crudos en `data/raw/` (ver tabla anterior)
3. Abrir `proyecto_cai.Rproj` en RStudio
4. Correr los scripts en orden:

```r
source("R/00_setup.R")          # instala paquetes si faltan
source("R/01_limpieza.R")
source("R/toxicidad_pesos.R")
source("R/02_cruce.R")
source("R/03_indice.R")
source("R/04_viz.R")
source("R/05_proximidad.R")     # ~5 min (requiere WorldPop raster)
source("R/06_indice_v2.R")
source("R/07_analisis_avanzado.R")
source("R/08_viz_v2.R")
shiny::runApp("app/")
```

## Metodologia resumida

**Contaminantes:** PM10, PM2.5, SO2, NOx, CO, As, Pb, Hg, Cd — seleccionados por normas nacionales y guias OMS.

**Toxicidad:** Peso = inversa del valor guia de referencia, normalizado a PM2.5 = 1. Cadmio (500x), Arsenico (333x), Mercurio (167x), Plomo (33x).

**Formula CAI:**
```
CAI_raw = (E / E_media_nacional) * (1 + tasa_pobreza)
CAI = log(CAI_raw + 1), normalizado 0-100
```
Donde E = emision tox-eq per capita (v1) o exposicion promedio por habitante (v2).

## Stack

R 4.4 + tidyverse + sf + terra + geosphere + chilemapas + leaflet + Shiny + Quarto

## Autor

Renato Andres — Ingenieria Ambiental, Universidad de Concepcion, campus Chillan.

## Licencia

MIT
