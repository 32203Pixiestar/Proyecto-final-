####PRIMER PASO: Descargar Librerias necesarias.
#Librerias:

library(raster) #Maneja y analiza datos espaciales en formato de cuadrículas.
library(maps) #Dibuja mapas básicos
library(mapdata) #Proporciona mapas más detallados.
library(sp) #Trabaja con datos espaciales y vectoriales.
library(reshape2) #Reorganiza y transforma datos de formato ancho y largo.
library(mgcv) #Ajusta modelos generalizados para modelar datos complejos.
library(rgbif) #Proporciona acceso a la API del (GBIF), datos de biodiversidad
library(ggplot2) #Visualización de datos, permite generar gráficos
library(sf) #Para manejar y procesar datos geoespaciales y realizar análisis espaciales o crear mapas.
library(dplyr)# Para manipulación de datos tabulares.
library(tidyverse)#Manipulación, análisis y visualización de datos.
library(terra)#Leer, manipular y analizar grandes conjuntos de datos espaciales en formato ráster.
library(readr)#Importar datos en diferentes formatos de texto como CSV, TSV, y archivos delimitados.
library(readxl)#Paquete para leer archivos de Excel en R. 
library(ggspatial)#Agregar elementos específicos de los mapas como escalas, flechas del norte, anotaciones espaciales.
library(rnaturalearth)#para cargar archivos shp de mexico
library(rnaturalearthdata)#escalas mayores
library(viridis) #para paletas de colores.
library(patchwork)#Combinar graficos en R
library(grid)# Para utilizar el objeto grob
library(gridExtra)# Paquete necesario para combinar gráficos
library(paletteer) #para paletas de colores.
library(devtools) #Simplifica y agiliza el desarrollo de paquetes R.
library(readr) #Poder leer datos rectangulares de archivos delimitados, como valores separados por comas (CSV)
library(elevatr) #Acceso a los datos de elevaciÃ³n desde las API web.
library(rnaturalearthhires)#para hacer mapas de alta resolución (escala 10)
library(ggtext)


#SEGUNO PASO: LIMPIEZA Y DEPURACIÓN DE DATOS#############
# ---- 1. Parámetros de búsqueda ----
especie <- "Ursus americanus" #Creamos un valor llamado especie, a partir de nuestra especie.
pais1 <- "MX" #Creamos un valor llamado pais1, a partir de México (se pone con su código ISO).
pais2 <- "US" #Creamos un valor llamado pais2, a partir de Estados Unidos (se pone con su código ISO)
year_range <- c(1970, 2024) #Creamos un vector, en donde solo nos muestre 

# ---- 1.1 Obtener taxonKey ----
taxon <- name_backbone(name = especie, rank = "species")
taxon_key <- taxon$usageKey

# ---- 2. Descargar ocurrencias desde GBIF ----
# GBIF no acepta rangos directos como "1970,2024", así que se usa minYear y maxYear
res_mx <- occ_search(
  taxonKey = taxon_key,
  country = pais1,
  hasCoordinate = TRUE,
  basisOfRecord = "HUMAN_OBSERVATION",
  year = paste0(year_range[1], ",", year_range[2]),  # formato correcto
  limit = 20000
)

res_us <- occ_search(
  taxonKey = taxon_key,
  country = pais2,
  hasCoordinate = TRUE,
  basisOfRecord = "HUMAN_OBSERVATION",
  year = paste0(year_range[1], ",", year_range[2]),
  limit = 20000
)

# ---- 3. Unir dataframes ----
df_mx <- res_mx$data
df_us <- res_us$data

datos_ursus <- bind_rows(df_mx, df_us) %>%
  select(key, species, genus, decimalLongitude, decimalLatitude,
         year, stateProvince, country, countryCode)

# ---- 4. Limpieza y filtrado ----
datos_ursus <- datos_ursus %>%
  filter(!is.na(decimalLongitude),
         !is.na(decimalLatitude),
         !is.na(year)) %>%
  distinct(key, .keep_all = TRUE) %>%
  mutate(
    year = as.integer(year),
    countryCode = toupper(countryCode)
  ) %>%
  filter(
    year >= year_range[1],
    year <= year_range[2],
    countryCode %in% c("MX", "US")
  )

# ---- 5. Agrupar por décadas ----
datos_ursus <- datos_ursus %>%
  mutate(decada = paste0(floor(year / 10) * 10, "s"))

decadas_tabla <- datos_ursus %>%
  group_by(country = countryCode, decada) %>%
  summarise(abundancia = n(), .groups = "drop")

# ---- 6. Completar décadas faltantes ----
decadas_niveles <- c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s")
decadas_tabla <- decadas_tabla %>%
  complete(country = c("MX", "US"),
           decada = decadas_niveles,
           fill = list(abundancia = 0)) %>%
  mutate(decada = factor(decada, levels = decadas_niveles))

# ---- 7. Resultado final ----
print(decadas_tabla)

#Por falta de datos del 2010 para atrás en ambos países, el proyecto se redujo de intervalos de tiempo a las decádas de los 2010´s y 2020´s.
datos_recientes <- datos_ursus %>%
  filter(decada %in% c("2010s", "2020s")) #le pedimos al paquete que solo nos de los datos de los 2010´s y los 2020´s. 

##########GRÁFICOS QUE RESPONDEN A LA PREGUNTA DE INVESTIGACIÓN##############

#----1. Gráfico de Barras para ver la Abundancia del Ursus americanus a través de los años (México vs EEUU)------

abund_anual <- datos_recientes %>%
  group_by(year, countryCode) %>%
  summarise(abundancia = n(), .groups = "drop")

Grafico_Abundancia <- ggplot(abund_anual, aes(x = factor(year), y = abundancia, fill = countryCode)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "gray20") +
  scale_fill_manual(values = c("MX" = "#D55E00", "US" = "#0072B2"),
                    labels = c("México", "Estados Unidos")) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Abundancia anual de *Ursus americanus* (2010–2024)",
    subtitle = "Comparación entre México y Estados Unidos",
    x = "Año",
    y = "Número de observaciones (abundancia)",
    fill = "País"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
Grafico_Abundancia

ggsave("Gráfico_Abundancia.png", path = "Proyecto Final",
       Grafico_Abundancia, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)

#---- 2. Gráfico de dispersión sobre la latitud a la que están los individuos de Ursus Americanus (México vs EEUU)----
# ---- 1. Calcular distancia al muro (aprox con el paralelo 31°N) ----
# Diferencia en latitud respecto a la frontera
# Cada grado de latitud ≈ 111 km
datos_recientes <- datos_recientes %>%
  mutate(
    dist_muro_km = (decimalLatitude - 31) * 111,
    # Norte (EE.UU.) será positivo, Sur (México) negativo
    dist_muro_km = ifelse(countryCode == "MX", -abs(dist_muro_km), abs(dist_muro_km))
  )

# ---- 3. Agrupar por década, país y redondear distancias ----
datos_dist <- datos_recientes %>%
  mutate(dist_km_redondeada = round(dist_muro_km, -1)) %>%  # redondea cada 10 km
  group_by(decada, countryCode, dist_km_redondeada) %>%
  summarise(observaciones = n(), .groups = "drop")

# ---- 4. Gráfico de dispersión bilateral ----
Grafico_dispersion <- ggplot(datos_dist, aes(x = dist_km_redondeada, y = observaciones, color = countryCode)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  scale_color_manual(values = c("MX" = "#D55E00", "US" = "#0072B2")) +
  facet_wrap(~decada, ncol = 1) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribución del *Ursus americanus* respecto al muro fronterizo",
    subtitle = "Distancia aproximada (km) desde la frontera México–EE.UU.\nComparación entre décadas 2010s y 2020s",
    x = "Distancia al muro (km; 0 = frontera)",
    y = "Número de observaciones",
    color = "País"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid = element_line(color = "gray85")
  )
Grafico_dispersion

ggsave("Gráfico_Disperson.png", path = "Proyecto Final",
       Grafico_dispersion, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)

#########GRÁFICOS DE EXPLORACIÓN DE LOS DATOS########################
#----1. Gráfico de mapas comparativos en donde nos muestran la frecuencia de observaciones entre ambos paíse---------
mx <- ne_states(country = "Mexico", returnclass = "sf")
us <- ne_states(country = "United States of America", returnclass = "sf")
mapa_base <- rbind(mx, us) #Unimos ambos


Grafico_mapas <- ggplot() +
  geom_sf(data = mapa_base, aes(fill = admin), color = "gray40", size = 0.3, show.legend = FALSE) +
  geom_point(
    data = datos_recientes,
    aes(x = decimalLongitude, y = decimalLatitude, color = countryCode),
    size = 1.6, alpha = 0.7
  ) +
  scale_color_manual(values = c("MX" = "#E69F00", "US" = "#0072B2")) +
  coord_sf(xlim = c(-125, -90), ylim = c(15, 55), expand = FALSE) +
  facet_wrap(~decada, ncol = 2) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribución reciente del *Ursus americanus* (2010–2024)",
    subtitle = "Comparación entre México y Estados Unidos por década",
    color = "País",
    x = "Longitud",
    y = "Latitud"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid = element_line(color = "gray85", size = 0.2)
  )
Grafico_mapas

ggsave("Gráfico_mapas.png", path = "Proyecto Final",
       Grafico_mapas, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)

# ---- 2. Gráfico de boxplots ----
Grafico_Boxplots <- ggplot(datos_recientes, aes(x = factor(year), y = decimalLatitude, fill = countryCode)) +
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.6, outlier.size = 2, color = "gray30") +
  scale_fill_manual(values = c("MX" = "#D55E00", "US" = "#0072B2"),
                    labels = c("México", "Estados Unidos")) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribución latitudinal de *Ursus americanus* por año",
    subtitle = "Comparación entre México y Estados Unidos (2010–2024)",
    x = "Año",
    y = "Latitud (°N)",
    fill = "País"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
Grafico_Boxplots

ggsave("Gráfico_Boxplot.png", path = "Proyecto Final",
       Grafico_Boxplots, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)

#############MAPA DEL SITIO DE ESTUDIO################
# ---- 2. Cargar capa raster (.tif) ----
# Cambia el nombre por el de tu archivo real
raster_relieve <- terra::rast("SR_50M.tif")
print(terra::crs(raster_relieve)) ## Verifica que tenga sistema de coordenadas

# Verifica que tenga el CRS correcto (WGS84)
if (terra::crs(raster_relieve) != "EPSG:4326") {
  raster_relieve <- terra::project(raster_relieve, "EPSG:4326")
}

# ---- 3. Capa vectorial: división política ----
mx <- rnaturalearth::ne_states(country = "Mexico", returnclass = "sf")
us <-rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mapa_base <- rbind(mx, us)

# ---- 4. Definir zona de estudio ----
# Puedes modificar el cuadro para ampliar o reducir la zona del mapa
zona_estudio <- st_bbox(c(xmin = -120, xmax = -95, ymin = 20, ymax = 40), crs = st_crs(4326)) %>% 
  st_as_sfc()

# Recortar el raster a esa zona (para hacerlo más rápido y limpio)
raster_relieve_crop <- crop(raster_relieve, vect(zona_estudio))

# ---- 5. Cargar tus datos de Ursus americanus ----
# Asegúrate de tener cargado el objeto datos_ursus (de tu código previo)

datos_recientes <- datos_ursus %>%
  filter(decada %in% c("2010s", "2020s")) %>%
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude))

# Convertir a formato espacial
puntos_ursus <- st_as_sf(datos_recientes,
                         coords = c("decimalLongitude", "decimalLatitude"),
                         crs = 4326)

# ---- 6. Crear el mapa ----
raster_df <- as.data.frame(raster_relieve_crop, xy = TRUE)
names(raster_df)[3] <- "altitud"

MAPA_FINAL <- ggplot() +
  # Capa raster
  geom_raster(data = raster_df, aes(x = x, y = y, fill = altitud)) +
  scale_fill_gradientn(colours = terrain.colors(10), name = "Altitud (m)") +
  
  # Fronteras y divisiones políticas
  geom_sf(data = mapa_base, fill = NA, color = "gray40", size = 0.4) +
  
  # Puntos de Ursus
  geom_sf(data = puntos_ursus, aes(color = countryCode),
          size = 2, alpha = 0.8) +
  scale_color_manual(
    values = c("MX" = "#D55E00", "US" = "#0072B2"),
    labels = c("México", "Estados Unidos"),
    name = "País"
  ) +
  
  coord_sf(xlim = c(-120, -95), ylim = c(20, 40), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.35, text_cex = 0.8) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.6, "cm"), pad_y = unit(0.6, "cm"),
                         style = north_arrow_fancy_orienteering) +
  labs(
    title = "Mapa del sitio de estudio del *Ursus americanus*",
    subtitle = "Zona fronteriza México–Estados Unidos (2010–2024)",
    x = "Longitud",
    y = "Latitud"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid = element_line(color = "gray85", size = 0.2)
  )

MAPA_FINAL

ggsave("Mapa.png", path = "Proyecto Final",
       MAPA_FINAL, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)