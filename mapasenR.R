install.packages("styler")
setwd("E:/OpenScience/mapas-article/departamentos_geo") # Redirecciona el directo actual a nuestro directorio de trabajo
styler::style_dir()
library(ggplot2)    # Librería para graficar
library(rgdal)      # Librería para abrir archivo de formato shapefiles (datos geográficos)
library(broom)      # Librería usada para extraer datos del archivo importado de shapefiles
library(tidyverse)  # Librería usada par juntar tablas
library(rio)        # Librería usada para importar datos de archivos csv
library(dplyr)

library(extrafont) # para importar fonts desde la carpteta de windows fonts


#install.packages()
#extrafont::font_import("C:/Windows/Fonts")

loadfonts(device = "win")
#fonts()

shapefile = readOGR(
  dsn = ".",
  layer = "departamentos_geo",
  encoding = 'utf-8',
  use_iconv = TRUE
)

View(shapefile)

geotable=tidy(shapefile)

head(geotable)

#Añadiendo una columna id para poder juntar las columnas de nuestro geotable con los datos correspondientes
shapefile$id<-row.names(shapefile)

#Añadiendo a geotable los datos que faltan de shapefile, lo junta por el id 
geotable<-left_join(geotable,shapefile@data, by = "id")

#Importando la tabla de datos de población
poblacion<-import("departamentospoblacion.csv", header=TRUE)

#Cambiando el nombre de la columna de DEPARTAMENTO POR DEPARTAMEN para poder juntar tablas segun nombre 
#Notesé que el nombre de los departamento dentro del shapefile es DEPARTAMEN
colnames(poblacion)[colnames(poblacion) == "DEPARTAMENTO"] <-
  "DEPARTAMEN"

#juntamos
datos <- left_join(geotable, poblacion, by = "DEPARTAMEN")


ggplot() +
  geom_polygon(data = datos, aes(
    x = long,
    y = lat,
    group = group,
    fill = Poblacion2022
  )) +
  coord_equal() + labs(fill = "POBLACION")

etiquetas_poblacion <- datos %>% group_by(DEPARTAMEN) %>%
  summarise(
    label_long = mean(range(long)),
    label_lat = mean(range(lat)),
    pob = mean(Poblacion2022)
  )

ggplot() +
  geom_polygon(data = datos, aes(
    x = long,
    y = lat,
    group = group,
    fill = Poblacion2022
  )) +
  coord_equal() + theme_void() +
  geom_text(
    size = 4.5,
    alpha = 0.9,
    fontface = "bold",
    data = etiquetas_poblacion,
    mapping = aes(
      x = label_long,
      y = label_lat,
      label = format(pob, big.mark = " "),
      color = pob
    )
  ) +
  labs(title = "Población estimada de Bolivia por departamentos para el año 2022",
       fill = "Habitantes",
       caption = "Datos:INE Boliva; GeoDatos: GeoBolivia") +
  scale_colour_gradientn(colours = c("black", "black", "white", "white", "white"),
                         guide = "none") +
  scale_fill_continuous(
    low = "#C4FFD1",
    high = "#05693E",
    guide = "colorbar",
    labels = scales::label_number(big.mark = " ")
  ) +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      family = "Helvetica",
      hjust = 0.5
    ),
    legend.title = element_text(size = 12, family = "Rubik"),
    plot.caption = element_text(family = "Helvetica")
  ) 


#Importamos la localización de las ciudades capitales
ciudades = import("ciudades.csv")
ggplot() +
  geom_polygon(
    data = datos,
    aes(
      x = long,
      y = lat,
      group = group,
      fill = DEPARTAMEN
    ),
    color = "gray",
    size = 0.5
  ) +
  geom_point(
    alpha = 0.7,
    data = ciudades,
    mapping = aes(x = lat, y = long, colour = Ciudad),
    size = 5
  ) +
  coord_equal() + labs(title = "Capitales de Departamento en Bolivia",
                       color = "Ciudades Capitales",
                       caption = "Fuente: geodatos.net, GeoBolivia") +
  scale_fill_brewer(palette = 'PuBuGn', guide = "none") +
  scale_color_manual(values = rainbow(9)) +
  theme_void() +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      family = "Helvetica",
      hjust = 0.5
    ),
    legend.title = element_text(size = 12, family = "Rubik"),
    plot.caption = element_text(family = "Helvetica")
  ) 

  

ggsave(
  filename = "grafica3.png",
  path = "E:/OpenScience/mapas-article/departamentos_geo/",
  scale = 1,
  device = "png",
  dpi = 320
)
