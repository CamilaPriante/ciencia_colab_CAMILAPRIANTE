#Sotalia guianensis

setwd("C:/Users/CAMILA/Desktop/Cesar/aula_3")

## instalar pacotes
install.packages("tidyverse")
install.packages("rgbif")

## chamar os pacotes 
require(tidyverse)
require(rgbif)

## checar funcoes
?occ_data

## baixar ocorrencias
fliper_gbif <- occ_data(scientificName = "Sotalia guianensis", ##mudar nome da especie
                      hasCoordinate = TRUE,
                      hasGeospatialIssue=FALSE)

# dimensoes
dim(fliper_gbif)

dim(fliper_gbif$data)

# checar campos
fliper_gbif$data %>% names

# numero de ocorrencias

dim(fliper_gbif$data)

gbif_issues()

# checar problemas reportados
issues_gbif <- fliper_gbif$data$issues %>% 
  unique() %>% 
  strsplit(., "[,]") %>% 
  unlist()

gbif_issues() %>% 
  data.frame() %>% 
  filter(code %in% issues_gbif)

## selecionando as variaveis 
fliper_gbif1 <- fliper_gbif$data %>%
  dplyr::select(scientificName, acceptedScientificName, decimalLatitude, decimalLongitude,
                issues, waterBody, basisOfRecord, occurrenceStatus, rightsHolder, 
                datasetName, recordedBy, locality, habitat) 

# função distinc, se nao rodar, tem q instalar o pacote dplyr

fliper_gbif1 <- fliper_gbif1 %>% 
  distinct() 

# checar niveis dos fatores
lapply(fliper_gbif1, unique)

## chamar a tabela para ver as colunas disponiveis 
fliper_gbif1$datasetName

  #instalar pacotes
install.packages("CoordinateCleaner")
install.packages("bdc")  ### nome diferente do arquivo 

#chamar pacotes
library(bdc)
library(CoordinateCleaner)  

# checar coordenadas válidas
check_pf <- 
  bdc::bdc_coordinates_outOfRange(
    data = fliper_gbif1,
    lat = "decimalLatitude",
    lon = "decimalLongitude")

# chamar pacotes
library(tidyverse)

# checar coordenadas válidas e próximas a capitais (muitas vezes as coordenadas são erroneamente associadas a capitais dos países)

cl <- fliper_gbif1 %>%
  CoordinateCleaner::clean_coordinates(species = "acceptedScientificName",
                                       lat = "decimalLatitude",
                                       lon = "decimalLongitude",
                                       tests = c("capitals", 
                                                 "centroids","equal", 
                                                 "gbif", "institutions", 
                                                 "outliers", "seas", 
                                                 "zeros"))
# verificar coordenadas com flags

# capitais (padrão é um raio de 10km)
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.cap`)) +
  coord_quickmap() +
  theme_classic()


# pontos no mar
ggplot() +
  borders("world", fill = "lightgray") +
  geom_point(data = cl, aes(x = decimalLongitude, y = decimalLatitude, color = `.sea`)) +
  coord_quickmap() +
  theme_classic()

# investigar niveis suspeitos
fliper_gbif1 %>% 
  distinct(waterBody) %>% 
  pull()


# waterBody
fliper_gbif1 %>%
  group_by(waterBody) %>% 
  summarise(occ = length(scientificName)) %>% 
  ggplot(aes(occ, y=waterBody)) +
  geom_bar(stat = 'identity') 


# fonte das regioes erradas
fliper_gbif1 %>% 
  filter(waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire")) %>% 
  distinct(datasetName)

fliper_gbif1 <- fliper_gbif1 %>% 
  filter(!waterBody %in% c("Atlantic Ocean", "Carribean", "Royal Caribbean", "Carribean Sea", "Bonaire"))

# número X de ocorrencias
fliper_gbif1 %>% 
  filter(datasetName %in% c("Diveboard - Scuba diving citizen science")) %>% 
  data.frame()

# filtrar todas do dataset suspeito
fliper_gbif_noDiveboard <- fliper_gbif1 %>% 
  filter(!datasetName %in% c("Diveboard - Scuba diving citizen science"))

##cortes de outliers 
fliper_gbif_noDiveboard %>% 
  filter(decimalLatitude > 25) %>% 
  arrange(-decimalLatitude) %>% 
  data.frame()

fliper_gbif_ok <- fliper_gbif_noDiveboard %>% 
  filter(decimalLatitude < 31) 


#chamar pacotes
library(ggmap)
library(maps)
library(mapdata)

world <- map_data('world')

# checar pontos
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = fliper_gbif_ok, aes(x = decimalLongitude, y = decimalLatitude), color = "red") +
  labs(x = "longitude", y = "latitude", title = expression(italic("Sotalia guianensis")))


## OBIS

#instalar pacote + chamar pacote
install.packages("robis")
library(robis)


fliper_obis <- robis::occurrence("Sotalia guianensis") ##trocar nome da especie

# checar dados
names(fliper_obis)

#colunas 
fliper_obis1 <- fliper_obis %>% 
  dplyr::select(scientificName, decimalLatitude, decimalLongitude, bathymetry,
                flags, waterBody, basisOfRecord, occurrenceStatus, 
                datasetName, recordedBy, locality, habitat) %>% 
  distinct()

# check problemas reportados (flags)
fliper_obis1 %>% 
  distinct(flags)

# check NA em datasetName
fliper_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         is.na(datasetName)) %>% 
  distinct(waterBody)


# checar niveis
fliper_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique")) %>% 
  lapply(., unique)


# aplicar filtros
fliper_obis_ok <- fliper_obis1 %>% 
  filter(!flags %in% c("NO_DEPTH,ON_LAND", "ON_LAND", "DEPTH_EXCEEDS_BATH,ON_LAND"),
         !is.na(datasetName),
         !waterBody %in% c("Caribbean Sea", "atlantique"))

# plot ver o mapa 
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = fliper_obis_ok, aes(x = decimalLongitude, y = decimalLatitude, color = waterBody)) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Sotalia guianensis")))


# unir GBIF e OBIS

# ver diferencas
setdiff(names(fliper_gbif_ok), names(fliper_obis_ok))

setdiff(names(fliper_obis_final), names(fliper_obis_final))


all_data <- bind_rows(fliper_gbif_ok %>% 
                        mutate(repo = paste0("gbif", row.names(.))), 
                      fliper_obis_final %>% 
                        mutate(repo = paste0("obis", row.names(.)))) %>%
  column_to_rownames("repo") %>% 
  dplyr::select(decimalLongitude, decimalLatitude) %>% 
  distinct() %>% 
  rownames_to_column("occ") %>% 
  separate(col = "occ", into = c("datasetName", "rn"), sep = 4) %>%
  mutate(scientificName = "Sotalia guianensis") %>% 
  dplyr::select(-rn)

# mapear ocorrencias
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  theme_classic() +
  geom_point(data = all_data, aes(x = decimalLongitude, y = decimalLatitude, color = datasetName)) +
  #theme(legend.title = element_blank()) +
  labs(x = "longitude", y = "latitude", title = expression(italic("Sotalia guianensis")))


#salvar em modo csv

write.csv(all_data, "occ_GBIF-OBIS_par_hepa.csv", row.names = FALSE)

#cheguei viva e com o dado .export
