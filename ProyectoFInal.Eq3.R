#########URSUS AMERICANUS A TRÁVES DE LOS AÑOS ENTRE LA FRONTERA DE MÉXICO Y ESTADOS UNIDOS.##########
#Equipo 3.
#Integrantes:
#Lemus Tello Alexa Glenn.
#Olvera Zarate Diego Alexis.
#Trejo Zarco Gisela.


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
library(ggtext)#Permite texto con formato (markdowmn/HTML) en ggplot.


#SEGUNO PASO: LIMPIEZA Y DEPURACIÓN DE DATOS#############
# ---- 1. Parámetros de búsqueda ----
especie <- "Ursus americanus" #Nombre científico de la especie buscada a partir de GBIF.
pais1 <- "MX" #Código ISO del país 1 (México)
pais2 <- "US" #Código ISO del país 1 (Estados Unidos)
year_range <- c(1970, 2024) #Rango de años que nos interesa, de la decada de los 70´s hasta los 2020´s 

# ---- 1.1 Obtener taxonKey ----
taxon <- name_backbone(name = especie, rank = "species") #name_backbone: Consulta la base de nombres de GBIFy nos devuelve los datos del taxón.
taxon_key <- taxon$usageKey #UsageKey: Identificador único que usará occ_search para descargar las ocurrencias del taxón.

# ---- 2. Descargar ocurrencias desde GBIF ----
#GBIF no acepta rangos directos como "1970,2024", así que se usa minYear y maxYear.
res_mx <- occ_search(#Le pedimos a occ_search que busque ocurrencias por país, coordenadas y de observaciones humanas.
  taxonKey = taxon_key, #le ponemos la clave de la especie obtenida arriba.
  country = pais1, #Filtrar por México.
  hasCoordinate = TRUE,#Pedimos solamente registros con coordenadas.
  basisOfRecord = "HUMAN_OBSERVATION", #El tipo de registros que queremos son observaciones humanas.
  year = paste0(year_range[1], ",", year_range[2]), #Rango de los años.
  limit = 20000 #Límite de registros que queremos que nos de.
)

res_us <- occ_search(#Le pedimos a occ_search que busque ocurrencias por país, coordenadas y de observaciones humanas.
  taxonKey = taxon_key,#le ponemos la clave de la especie obtenida arriba.
  country = pais2,#Filtrar por México.
  hasCoordinate = TRUE,#Pedimos solamente regsitros con coordenadas.
  basisOfRecord = "HUMAN_OBSERVATION",#El tipo de registros que queremos son observaciones humanas.
  year = paste0(year_range[1], ",", year_range[2]),#Rango de los años.
  limit = 20000#Límite de registros que queremos que nos de.
)

# ---- 3. Unir dataframes ----
df_mx <- res_mx$data #Extraer el data frame de resultados que se hizo para México.
df_us <- res_us$data#Extraer el data frame de resultados que se hizo para Etados Unidos.

datos_ursus <- bind_rows(df_mx, df_us) %>% #Combinamos ambas DF y seleccionamos las columnas relevbantes para el trabajo.
  select(key, species, genus, decimalLongitude, decimalLatitude, # Coordenadas y taxonomía.
         year, stateProvince, country, countryCode) # Año y ubicaciones.

# ---- 4. Limpieza y filtrado ----
datos_ursus <- datos_ursus %>% #filtramos la base de datos
  filter(!is.na(decimalLongitude), #Filter () nos ayuda a filtrar los datos que tenemos y eliminamos los registros sin longitud
         !is.na(decimalLatitude), #Filtramos y eliminiamos los registros sin latitud
         !is.na(year)) %>%       # Filtrmaos los datos y eliminamos los registros sin año.
  distinct(key, .keep_all = TRUE) %>% #Distinct () nos ayuda a seleccionar filas únicas y eliminar los duplicados por su clave única.
  mutate(  #mutate()se utiliza para crear nuevas variables o modificar las existentes dentro de un data frame.
    year = as.integer(year), #convertimos el año en una variable entera.
    countryCode = toupper(countryCode) #nos aseguramos de que el countrycode este en mayusculas.
  ) %>%
  filter( 
    year >= year_range[1],#Filtramos por años >= 1970
    year <= year_range[2], #Filtramos por años <=2024
    countryCode %in% c("MX", "US") #Mantener solo los países de México y Estados Unidos.
  )

# ---- 5. Agrupar por décadas ----
datos_ursus <- datos_ursus %>% #actualizamos el objeto de datos_ursus
  mutate(decada = paste0(floor(year / 10) * 10, "s")) #la ecuación dentro de paste0 nos ayuda a calcular la decada de cada año.
#Paste0 se utiliza para concatenar cadenas de texto y otros objetos sin ningún separador, en este caso se le pide concatener el año con la "s", o sea 2010s

decadas_tabla <- datos_ursus %>% #creamos un objeto a partir de nuestra base de datos
  group_by(country = countryCode, decada) %>% #group_by() se utiliza para agrupar un marco de datos por una o más variables
  summarise(abundancia = n(), .groups = "drop") #summarise reduce un conjunto de datos o un grupo de filas a un único valor mediante el cálculo de estadísticas descriptivas. 
#En estas lineas agrupamos por país y década y luego contamos cuantos registros hay (Abundancia)

# ---- 6. Completar décadas faltantes ----
decadas_niveles <- c("1970s", "1980s", "1990s", "2000s", "2010s", "2020s") #concatenamos décadas para crear un vector.
decadas_tabla <- decadas_tabla %>% 
  complete(country = c("MX", "US"), #Complete() genera combinaciones faltantes y aseguramos las filas para ambos países.
           decada = decadas_niveles, #incluimos las décadas definidas anteriormente.
           fill = list(abundancia = 0)) %>% #relleanmos con 0 por si no hay registros 
  mutate(decada = factor(decada, levels = decadas_niveles)) #mutate transforma las décadas como un factor en orden.

print(decadas_tabla) #visualizamos la tabla de decadas final.

###Por falta de datos del 2010 para atrás en ambos países, el proyecto se redujo de intervalos de tiempo a las decádas de los 2010´s y 2020´s.###
datos_recientes <- datos_ursus %>%
  filter(decada %in% c("2010s", "2020s")) #le pedimos que solo nos de los datos de los 2010´s y los 2020´s. 

##########GRÁFICOS QUE RESPONDEN A LA PREGUNTA DE INVESTIGACIÓN##############

#----1. Gráfico de Barras para ver la Abundancia del Ursus americanus a través de los años (México vs EEUU)------
abund_anual <- datos_recientes %>% #creamos una tabla de abundancia anual por país
  group_by(year, countryCode) %>% # agrupamos por año y y país
  summarise(abundancia = n(), .groups = "drop") # contamos las observaciones por cada grupo

Grafico_Abundancia <- ggplot(abund_anual, aes(x = factor(year), y = abundancia, fill = countryCode)) +#con ggplot creamos gráficos. le pedimos que ocupe la base de datos "abund_anual" y que en x este los años y en Y la abundancia, rellenado con el countrycode.
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "gray20") + #geom_bar nos creará una columna de barras, basada en la variable de abundancia directamente.
  scale_fill_manual(values = c("MX" = "#D55E00", "US" = "#0072B2"), #es para definir los colores.
                    labels = c("México", "Estados Unidos")) + #le damos etiquetas para identificar
  theme_minimal(base_size = 13) + # el tema es sencillo y minimalista.
  labs( #dentro de labs definimos todos los textos de la gráfica
    title = "Abundancia anual de *Ursus americanus* (2010–2024)",
    subtitle = "Comparación entre México y Estados Unidos",
    x = "Año",
    y = "Número de observaciones (abundancia)",
    fill = "País"
  ) +
  theme( #definimos tamaños, letras y angulos del texto.
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
Grafico_Abundancia #nos muestra la gráfica final.

ggsave("Gráfico_Abundancia.png", path = "Proyecto Final", #guarda el gráfico en PNG, con especificaciones y en una carpeta solicitada.
       Grafico_Abundancia, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)

#---- 2. Gráfico de dispersión sobre la latitud a la que están los individuos de Ursus Americanus (México vs EEUU)----
# ----Calcular distancia al muro ----
# Diferencia en latitud respecto a la frontera (Cada grado de latitud ≈ 111 km)
#
datos_recientes <- datos_recientes %>% #creamos una tabla de datos.
  mutate(
    dist_muro_km = (decimalLatitude - 31) * 111, #asumimos que la frontera está por la latitud 31°N y cada grado es de ~111km
    # Norte (EE.UU.) será positivo, Sur (México) negativo
    dist_muro_km = ifelse(countryCode == "MX", -abs(dist_muro_km), abs(dist_muro_km)) #si es México lo marcamos en negativo (sur) y si es Estados Unidos, como positivo (norte)
  )

# ----Agrupar por década, país y redondear distancias ----
datos_dist <- datos_recientes %>%
  mutate(dist_km_redondeada = round(dist_muro_km, -1)) %>%  # redondea cada 10 km 
  group_by(decada, countryCode, dist_km_redondeada) %>% # Agrupamos por decada, countrycode, y la distancia de kilometros redondeada.
  summarise(observaciones = n(), .groups = "drop") #contamos las observaciones por cada grupo

# ----Gráfico de dispersión comparativo (bilateral)----
Grafico_dispersion <- ggplot(datos_dist, aes(x = dist_km_redondeada, y = observaciones, color = countryCode)) + #creamos un gráfico con la base de datos de distribución, en X ponemos la distancia de km redondeada y en Y las observaciones.
  geom_point(size = 3, alpha = 0.8) + #creamos una gráfica de puntos de tamaño 3 y opacidad de 0.8
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") + # es la linea vertical de la frontera, en este caso es el 0
  scale_color_manual(values = c("MX" = "#D55E00", "US" = "#0072B2")) + # le damos los valores de color para los puntos,
  facet_wrap(~decada, ncol = 1) + #se hace un panel por década. Facet_wrap crea múltiples gráficos a partir de un conjunto de datos, organizándolos en una cuadrícula. basada en los niveles de una variable categórica
  theme_minimal(base_size = 13) +
  labs( # definimos todos los textos de la gráfica
    title = "Distribución del *Ursus americanus* respecto al muro fronterizo",
    subtitle = "Distancia aproximada (km) desde la frontera México–EE.UU.\nComparación entre décadas 2010s y 2020s",
    x = "Distancia al muro (km; 0 = frontera)",
    y = "Número de observaciones",
    color = "País"
  ) +
  theme(#definimos tamaños, letras y angulos del texto.
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid = element_line(color = "gray85")
  )
Grafico_dispersion #nos muestra nuesro mapa final

ggsave("Gráfico_Disperson.png", path = "Proyecto Final", #guarda el gráfico en PNG, con especificaciones y en una carpeta solicitada.
       Grafico_dispersion, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)

#########GRÁFICOS DE EXPLORACIÓN DE LOS DATOS########################

#----1. Gráfico de mapas comparativos en donde nos muestran la frecuencia de observaciones entre ambos países---------
mx <- ne_states(country = "Mexico", returnclass = "sf") #nos ayuda a obtener las divisiones políticas del país de méxico
us <- ne_states(country = "United States of America", returnclass = "sf")#nos ayuda a obtener las divisiones políticas del país del país EEUU
mapa_base <- rbind(mx, us) #Unimos ambos, mx y us, en un solo objeto.


Grafico_mapas <- ggplot() + #creamos un gráfico
  geom_sf(data = mapa_base, aes(fill = admin), color = "gray40", size = 0.3, show.legend = FALSE) + ## geom sf nos ayud a visualizar objetos espaciales (sf), en fill rellenamos por 'admin' si existe.
  geom_point( #superponemos puntos de ocurrencia
    data = datos_recientes, #se usa la base de datos "datos_recientes".
    aes(x = decimalLongitude, y = decimalLatitude, color = countryCode), #En X irá la longitud y en Y la latitud
    size = 1.6, alpha = 0.7 #tamaño y opacidad de los puntos.
  ) +
  scale_color_manual(values = c("MX" = "#E69F00", "US" = "#0072B2")) + #colores que se usaron.
  coord_sf(xlim = c(-125, -90), ylim = c(15, 55), expand = FALSE) + #los límites que definen al mapa.
  facet_wrap(~decada, ncol = 2) + # paneles por cada década
  theme_minimal(base_size = 12) + 
  labs( #definimos todos los textos de la gráfica
    title = "Distribución reciente del *Ursus americanus* (2010–2024)",
    subtitle = "Comparación entre México y Estados Unidos por década",
    color = "País",
    x = "Longitud",
    y = "Latitud"
  ) +
  theme( #definimos tamaños, letras y angulos del texto.
    strip.text = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid = element_line(color = "gray85", size = 0.2)
  )
Grafico_mapas #visualización del mapa.

ggsave("Gráfico_mapas.png", path = "Proyecto Final", #guarda el gráfico en PNG, con especificaciones y en una carpeta solicitada.
       Grafico_mapas, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)

# ---- 2. Gráfico de boxplots ----
Grafico_Boxplots <- ggplot(datos_recientes, aes(x = factor(year), y = decimalLatitude, fill = countryCode)) + #creamos un gráfico con la base de datos "datos_Recientes" con años en X y latitud en Y
  geom_boxplot(outlier.shape = 21, outlier.alpha = 0.6, outlier.size = 2, color = "gray30") + #definimos el gráfico como boxplot y le damos forma.
  scale_fill_manual(values = c("MX" = "#D55E00", "US" = "#0072B2"), #definimos los colores.
                    labels = c("México", "Estados Unidos")) +
  theme_minimal(base_size = 13) +
  labs(  #definimos todos los textos de la gráfica
    title = "Distribución latitudinal de *Ursus americanus* por año",
    subtitle = "Comparación entre México y Estados Unidos (2010–2024)",
    x = "Año",
    y = "Latitud (°N)",
    fill = "País"
  ) +
  theme( #definimos tamaños, letras y angulos del texto.
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )
Grafico_Boxplots #visualización del mapa.

ggsave("Gráfico_Boxplot.png", path = "Proyecto Final", #guarda el gráfico en PNG, con especificaciones y en una carpeta solicitada.
       Grafico_Boxplots, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)

#############MAPA DEL SITIO DE ESTUDIO################   

# ---- 1. Cargar capa raster (.tif) ----
# Cambia el nombre por el de tu archivo real
raster_relieve <- terra::rast("SR_50M.tif") #lee el archivo .tif como un objeto raster.
print(terra::crs(raster_relieve)) ## Verifica que tenga sistema de coordenadas

if (terra::crs(raster_relieve) != "EPSG:4326") { # Verifica que tenga el CRS correcto (WGS84), y si no lo cambia para que lo tenga.
  raster_relieve <- terra::project(raster_relieve, "EPSG:4326")
}

# ---- 2. Capa vectorial: división política ----
mx <- rnaturalearth::ne_states(country = "Mexico", returnclass = "sf") #nos ayuda a obtener las divisiones políticas del país de méxico
us <-rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") #nos ayuda a obtener las divisiones políticas del país de EEUU
mapa_base <- rbind(mx, us) #Unimos ambos, mx y us, en un solo objeto.

# ---- 3. Definir zona de estudio ----

zona_estudio <- st_bbox(c(xmin = -120, xmax = -95, ymin = 20, ymax = 40), crs = st_crs(4326)) %>% #Puedes modificar el cuadro para ampliar o reducir la zona del mapa
  st_as_sfc() #crea un poligono 


raster_relieve_crop <- crop(raster_relieve, vect(zona_estudio)) #cortar el raster a esa zona, para hacerlo más rápido y limpio.

# ---- 4. Cargarlos datos de la especie ----
  filter(decada %in% c("2010s", "2020s")) %>% # Filtrar solo las filas donde la década sea 2010s o 2020s
  filter(!is.na(decimalLongitude), !is.na(decimalLatitude)) # Filtrar solo las filas que tienen coordenadas válidas (no NA)

# Convertir a formato espacial
puntos_ursus <- st_as_sf(datos_recientes, #convetimos el data frame de puntos a objetos de tipo sf
                         coords = c("decimalLongitude", "decimalLatitude"),# Columnas que contienen las coordenadas
                         crs = 4326) #El sistema de referencia de coordenadas WGS84

# ---- 5. Crear el mapa ----
raster_df <- as.data.frame(raster_relieve_crop, xy = TRUE) #Convertimos el raster recortado en un data frame para usarlo en ggplot
names(raster_df)[3] <- "altitud" # Cambiamos el nombre de la columna 3 a "altitud"

MAPA_FINAL <- ggplot() + #creamos el gráfico

  geom_raster(data = raster_df, aes(x = x, y = y, fill = altitud)) + #la capa raster, nos muestra el relieve con colores según la latitud.
  scale_fill_gradientn(colours = terrain.colors(10), name = "Altitud (m)") + # la paleta de colores para la latitud
  

  geom_sf(data = mapa_base, fill = NA, color = "gray40", size = 0.4) +  # la capa vectorial para las fronteras y divisiones políticas
  

  geom_sf(data = puntos_ursus, aes(color = countryCode),  #Usamos los puntos de Ursus americanus
          size = 2, alpha = 0.8) + #tamaño y opacidad de los puntos
  scale_color_manual( #colores para México y Estados Unidos.
    values = c("MX" = "#D55E00", "US" = "#0072B2"),
    labels = c("México", "Estados Unidos"),
    name = "País"
  ) +
  
  coord_sf(xlim = c(-120, -95), ylim = c(20, 40), expand = FALSE) + #Ajuste de las coordenadas para marcar los limites del mapa
  annotation_scale(location = "bl", width_hint = 0.35, text_cex = 0.8) + #se agrega una escala para el mapa de forma gráfia
  annotation_north_arrow(location = "br", which_north = "true", #se agrega una rosa de los vientos para el mapa de forma visual.
                         pad_x = unit(0.6, "cm"), pad_y = unit(0.6, "cm"),
                         style = north_arrow_fancy_orienteering) +
  labs( #definimos todos los textos de la gráfica
    title = "Mapa del sitio de estudio del *Ursus americanus*",
    subtitle = "Zona fronteriza México–Estados Unidos (2010–2024)",
    x = "Longitud",
    y = "Latitud"
  ) +
  theme_minimal(base_size = 13) +
  theme( #definimos tamaños, letras y angulos del texto.
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid = element_line(color = "gray85", size = 0.2)
  )

MAPA_FINAL #visualización del mapa final.

ggsave("Mapa.png", path = "Proyecto Final", #guarda el mapa en PNG, con especificaciones y en una carpeta solicitada.
       MAPA_FINAL, width = 8, height = 6, units = "in",
       bg="white",
       dpi = 300)
