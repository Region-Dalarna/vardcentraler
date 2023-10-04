library(readxl)
library(sf)
library(mapview)
library(dplyr)
library(tidyverse)

# Utbudspunkter_kopia <- read_excel("Utbudspunkter_kopia.xlsx")
# head(Utbudspunkter_kopia)
# vardcentraler <- st_as_sf(Utbudspunkter_kopia,
#                           coords = c("Longitude", "Latitude"),
#                           crs = 4326)
#SWEREF istället
vardcentraler <- st_as_sf(Utbudspunkter_kopia,
                          coords = c("Sweref99Y", "Sweref99X"),
                          crs = 3006)


mapview(vardcentraler, label = "Populärnamn")

#hämta Regso från G:\Samhällsanalys\GIS\grundkartor\regso

regso_fil <- "G:/Samhällsanalys/GIS/grundkartor/regso/RegSO_2018_v1.gpkg"
regso <- st_read(regso_fil, crs = 3006) 

regso_dalarna <- regso %>%
  mutate(lan_kod = substr(kommun, 1, 2)) %>%
  filter(lan_kod == "20")

# Lägger till en col med id
regso_dalarna_id <- regso_dalarna %>%
  mutate(id = row_number())

#Spatial join, lägger till kolumnen regso med id från regso_dalarna_id
vardcentral_inter <- vardcentraler %>% 
  mutate(regso = as.integer(st_intersects(., regso_dalarna_id))) 

# Filtrerar på FGhKlass
vardcentraler_endast <- vardcentral_inter  %>% 
  filter(FghKlass == "Vårdcentral")

#Kommun gränser från regso

kommun <- regso_dalarna %>% 
  group_by(kommun, kommunnamn) %>% 
  summarize(geom = st_union(geom)) %>% 
  ungroup()

#från chatgpt
kommun <- regso_dalarna %>%
  group_by(kommun, kommunnamn) %>% 
  ungroup()

mapview(kommun, label = "kommunnamn", alpha.regions = 0)

mapview(vardcentraler_endast, label = "Populärnamn")+
  mapview(regso_dalarna_id, zcol = "kommun", legend = FALSE, hide = TRUE, alpha.regions = 0.5)+
  mapview(kommun, label = "kommunnamn", alpha.regions = 0)

library(sf)

# Assuming you have loaded your polygon and point layers into regso_dalarna_id and vardcentraler_endast

# Perform a spatial join to associate each point with its containing polygon
joined_data_regso <- st_join(vardcentraler_endast, regso_dalarna_id)

# Group by regso and count the vardcentral within each polygon
summary_data_regso <- joined_data %>%
  group_by(id) %>%  # Replace 'regso_dalarna_id_column' with the actual column name in regso_dalarna_id
  summarize(sum_vardcentral = n()) %>%
  ungroup()

# Add the point count data to the polygons layer
regso_dalarna_sum_vardcent <- st_join(regso_dalarna_id, summary_data, by = c("id" = "id"))

# Perform a spatial join to associate each point with its containing polygon
joined_data_kom <- st_join(vardcentraler_endast, kommun)

# Group by kommun and count the vardcentral within each polygon
summary_data_kom <- joined_data_kom %>%
  group_by(kommun) %>%  # Replace 'regso_dalarna_id_column' with the actual column name in regso_dalarna_id
  summarize(sum_vardcentral = n()) %>%
  ungroup()

# Add the point count data to the polygons layer
kommun_sum_vardcent <- st_join(kommun, summary_data_kom, by = c("kommun" = "kommun"))

mapview(vardcentraler_endast, label = "Populärnamn", layer.name = "Vardcentraler", homebutton = FALSE, legend = FALSE)+
  mapview(regso_dalarna_sum_vardcent, zcol = "kommun", legend = FALSE, hide = TRUE, alpha.regions = 0.5, homebutton = FALSE, layer.name = "regso")+
  mapview(kommun_sum_vardcent, label = "kommunnamn", alpha.regions = 0, homebutton = FALSE, legend = FALSE, layer.name = "kommun")
