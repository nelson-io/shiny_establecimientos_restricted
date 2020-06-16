library(sf)
library(tidyverse)
library(rio)

# Descargamos datos

data <- st_read("comercio_crudo.shp")

data[which(data$actividadc == "Depósitos" & str_detect(data$observacio, "internet|web|Internet|INTERNET|Web|WEB|venta|Venta|VENTA|online|Online|ONLINE|ecommerce|ECOMMERCE|mail|email|telefonía|comercio")),"actividadc"] <- "Depósito con ventas web"

data <- data %>% filter(actividadc !=  "Depósitos")

# Definimos actividades
actividades <- list()

actividades$excluidas <- c("Industrias","Depósitos","DESOCUPADO",
                                       "Peluquerías, manicuras,  pedicuras, centros de belleza, spas y  depilación",
                                       "Hoteles, hoteles familiares, pensiones y geriátricos",
                                       "Gimnasios, canchas de alquiler y yoga",
                                       "Locales para esparcimiento (peloteros, teatros, cines, locales bailables)",
                                       "Locutorios y servicios de Internet","Tatuajes y Piercings",
                                       "Paseos de compras (puestos con estructura de metal)")

actividades$esenciales <- c("Restaurantes, bares, cafeterías, pizzerías, fast food",
                "Fruterías y verdulerías",
                "Carnicerías, fiambrerías, granjas, pescaderías",
                "Kioscos y golosinerías",
                "Ferreterías, cerrajerías, tornillos y bulonerías, alarmas, materiales eléctricos, iluminación",
                "Artículos limpieza, artículos de embalaje y pañaleras",
                "Supermercados de grandes cadenas e hipermercados (incluye locales Express)",
                "Bancos, financieras, tarjetas de crédito y casas de cambios",
                "Alimentos y bebidas en comercios especializados (dietéticas, herboristería, vinerías, etc.)",
                "Heladerías (elaboración y venta)",
                "Almacenes y supermercados chicos / Locales Express de cadenas",
                "Clínicas privadas, centros de diagnóstico por imágenes, emergencias, medicinas prepagas, laboratorios, médicos, dentistas, kinesiología (SALUD PRIVADA)",
                "Casas de comidas, casa de pastas y deliverys",
                "Veterinarias y alimentos para mascotas",
                "Combustibles, estaciones de servicio",
                "Panaderías, confiterías, bombonerías, chocolaterías y sandwicherías",
                "Farmacias, perfumerías, cosméticas y artículos de tocador",
                "Rapipago, Pago Fácil, Bapro Pago y lugares de pago de servicios diversos excepto Bancos",
                "Fotografía y ópticas"
)



# Definimos poligonos

poli_list <- list()

poli_list$zonas <- st_read("zonas_comerciales.geojson")
poli_list$corredores <- st_read("corredores_lineales_modif.shp") %>% 
  st_transform(crs = 5347) %>% 
  st_buffer(dist = 20) %>%
  st_transform(crs = 4326)

test_list <- map(poli_list, ~ st_intersects(data,.x, sparse = F) %>% tibble())

intersected_rows <- which(do.call(cbind, test_list) %>%  
  rowSums() > 0)

st_intersects(data , poli_list$corredores, sparse = F) %>% 
  rowSums() %>% 
  sum()

# Agregamos vector de comercios en zonas y corredores

data$intersected <- "Resto de la CABA"
data$intersected[intersected_rows] <- " Zonas comerciales y corredores estratégicos"

# Agregamos vector de comercios esenciales
data$esencial <- "No esencial"
data[data$actividadc %in% actividades$esenciales,'esencial'] <- "Esencial"

# Agregamos vector de comercios excluidos
data$excluido <- 0
data[data$actividadc %in% actividades$excluidas,'excluido'] <- 1
#agregamos vector de comercios permitidos
data$permitido <- "No permitido"

data[which((data$intersected == "Resto de la CABA" & data$excluido == 0) |
        (data$intersected == " Zonas comerciales y corredores estratégicos" & data$esencial == "Esencial")), 'permitido'] <- "Permitido"  


data[!data$actividadc %in% actividades$excluidas,] %>% nrow()

# pasamos de multipunto a punto
data <- data %>% st_cast('POINT')

#generamos labels

data$data_labels <- paste(sep = "<br/>",
                          paste0("<b>",data$nombrecome,"</b>"),
                          paste0("CUIT ", data$cuit),
                          data$actividadc)


export(data, "data.Rdata")

# 
# leaflet(data = data) %>% 
#   addTiles() %>% 
#   addMarkers(clusterOptions = markerClusterOptions())


