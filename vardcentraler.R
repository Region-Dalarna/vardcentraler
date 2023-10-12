library(readxl)
library(sf)
library(mapview)
library(tidyverse)

source("G:/skript/func/func_postgis.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#Hämta vårdcentraler

utbudspunkter_fil <- "G:/skript/henrik/GitHub/Region-Atlas/vardcentraler/Utbudspunkter.xlsx" #denna fil är uppdaterad från orginalet med samma namn
  
utbudspunkter <- read_excel(utbudspunkter_fil)
X3kluster_namn <- read_excel("3kluster_namn.xlsx")
modell_2 <- read_excel("Kopia av Modell 2 - Dataset med 3 kluster utb_fodl_ek 2023-10-10.xlsx")

vardcentraler <- st_as_sf(utbudspunkter,
                          coords = c("Sweref99Y", "Sweref99X"),
                          crs = 3006)

# mapview(vardcentraler)+
#   mapview(regso_dalarna)

# hämta Regso från Geodatabasen
regso_dalarna <- hamta_karta(karttyp = "regso", regionkoder = "20")

# mapview(regso_dalarna)

# Lägger till en col med id
regso_dalarna_id <- regso_dalarna %>%
  mutate(id = row_number())

#Spatial join, lägger till kolumnen regso med id från regso_dalarna_id
vardcentral_inter <- vardcentraler %>% 
  mutate(regso = as.integer(st_intersects(., regso_dalarna_id))) 

# Filtrerar på FGhKlass, kanske mer lämpligt att filtrera på HSA_namnVC?
vardcentraler_endast <- vardcentral_inter  %>% 
  filter(FghKlass == "Vårdcentral")

#Kommungränser från regso
kommun <- regso_dalarna %>% 
  group_by(kommun, kommunnamn) %>% 
  summarize(geom = st_union(geometry)) %>% 
  ungroup()

# Chatgpt briljerar med följande som lägger till en kolumn i regso respektive kommun med antal vårdcentraler
#Perform a spatial join to associate each point with its containing polygon
joined_data_regso <- st_join(vardcentraler_endast, regso_dalarna_id)

# Group by regso and count the vardcentral within each polygon
summary_data_regso <- joined_data_regso %>%
  group_by(id) %>%  # Replace 'regso_dalarna_id_column' with the actual column name in regso_dalarna_id
  summarize(sum_vardcentral = n()) %>%
  ungroup()

# Add the point count data to the polygons layer
regso_dalarna_sum_vardcent <- st_join(regso_dalarna_id, summary_data_regso, by = c("id" = "id"))

# Perform a spatial join to associate each point with its containing polygon
joined_data_kom <- st_join(vardcentraler_endast, kommun)

# Group by kommun and count the vardcentral within each polygon
summary_data_kom <- joined_data_kom %>%
  group_by(kommun) %>%  # Replace 'regso_dalarna_id_column' with the actual column name in regso_dalarna_id
  summarize(sum_vardcentral = n()) %>%
  ungroup()

# Add the point count data to the polygons layer
kommun_sum_vardcent <- st_join(kommun, summary_data_kom, by = c("kommun" = "kommun"))

#joina kluster

regso_dalarna_sum_vardcent <- regso_dalarna_sum_vardcent %>% 
  rename(region_kod = regsokod)

regso_kluster <- left_join(regso_dalarna_sum_vardcent, X3kluster_namn)

modell_2_kluster <- left_join(regso_dalarna_sum_vardcent, modell_2)

mapview(vardcentraler_endast, label = "Populärnamn", layer.name = "Vardcentraler", homebutton = FALSE, legend = FALSE, hide = TRUE) +
  mapview(regso_dalarna_sum_vardcent, zcol = "kommun", legend = FALSE, hide = TRUE, alpha.regions = 0.5, homebutton = FALSE, layer.name = "regso", label = "regso") +
  mapview(kommun_sum_vardcent, label = "kommunnamn", alpha.regions = 0, homebutton = FALSE, legend = FALSE, layer.name = "kommun", hide = TRUE) +
  mapview(regso_kluster, zcol = "cmember", label = "regso_kluster", homebutton = FALSE, legend = FALSE, hide = TRUE) +
  mapview(modell_2_kluster, zcol = "cmember", label = "kluster_regso_folkmängd", homebutton = FALSE, legend = FALSE)

