library(tidyverse)
library(osmdata)
library(sf)
library(nominatim)
library(ggsflabel)
library(magrittr)
# library(ggmap)
# register_google(key = "REDACTED_GOOGLE_MAPS_KEY")
# ggmap_credentials()

theme_set(theme_bw())

# references --------------------------------------------------------------
#' https://www.dshkol.com/2018/better-maps-with-vector-tiles/
#' https://dominicroye.github.io/en/2018/accessing-openstreetmap-data-with-r/
#' https://www.r-spatial.org/r/2019/09/26/spatial-networks.html
#' http://varianceexplained.org/r/seven-fav-packages/

# shapefiles --------------------------------------------------------------

rute <- "<path-to-local-gis-data>/"
# importar shapes ---------------------------------------------------------

per2 <- st_read(paste0(rute,"data-raw/gis/PER_adm1.shp")) %>% 
  filter(is_in(NAME_1,c("Lambayeque","Cajamarca","Piura",
                         "La Libertad","Amazonas")))

per4 <- st_read(paste0(rute,"data-raw/gis/Distritos.shp")) %>% 
  filter(NM_DEPA=="LAMBAYEQUE")

farms <- st_read("../ubicaciones/Granjas/Granjas_INEI.shp") %>% 
  filter(NOMBREDD=="LAMBAYEQUE") %>% 
  mutate(name="Granja") %>% 
  filter(is_in(field_1,c(237,236)))

slauther_disa <- st_read("../ubicaciones/Mataderos/Mataderos_DISA.shp") %>% 
  mutate(name="Matadero DISA")

slauther_senasa <- st_read("../ubicaciones/Mataderos/Mataderos_SENASA.shp") %>% 
  filter(provincia=="CHICLAYO") %>% 
  mutate(name="Matadero SENASA")

cases_geo <- st_read("../ubicaciones/Casos/Brote_39-47.shp")

# peru roads --------------------------------------------------------------

panam_lamba <- getbb("Lambayeque Peru") %>%
  opq()%>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  osmdata_sf()
panam_cajam <- getbb("Cajamarca Peru") %>%
  opq()%>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  osmdata_sf()
panam_piur <- getbb("Piura Peru") %>%
  opq()%>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  osmdata_sf()
panam_lalib <- getbb("La Libertad Peru") %>%
  opq()%>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  osmdata_sf()
panam_amaz <- getbb("Amazonas Peru") %>%
  opq()%>%
  add_osm_feature(key = "highway", value = "trunk") %>%
  osmdata_sf()


ggplot() +
  geom_sf(data = per4,alpha=0) +
  geom_sf(data = panam_lamba$osm_lines,size=1.5) +
  # geom_sf(data = panam_cajam$osm_lines,size=1.5) +
  # geom_sf(data = panam_piur$osm_lines,size=1.5) +
  # geom_sf(data = panam_lalib$osm_lines,size=1.5) +
  # geom_sf(data = panam_amaz$osm_lines,size=1.5)
ggsave("figure/fig02-lamabayeque.png",width = 9,height = 8)

ggplot() +
  geom_sf(data = per2,alpha=0) +
  geom_sf(data = panam_lamba$osm_lines,size=1.5) +
  geom_sf(data = panam_cajam$osm_lines,size=1.5) +
  geom_sf(data = panam_piur$osm_lines,size=1.5) +
  geom_sf(data = panam_lalib$osm_lines,size=1.5) +
  geom_sf(data = panam_amaz$osm_lines,size=1.5)
ggsave("figure/fig03-norte.png",width = 9,height = 8)



# osm tutorial ------------------------------------------------------------

# _extract features -------------------------------------------------------

# available_tags("highway")
# available_features()

chiclayo <- getbb("Chiclayo Lambayeque Peru")

large_streets <- getbb("Chiclayo Lambayeque Peru") %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "trunk")) %>%
  osmdata_sf()

streets <- getbb("Lambayeque Peru") %>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("primary", "secondary", 
                            "tertiary")) %>%
  osmdata_sf()

small_streets <- getbb("Lambayeque Peru")%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()

# river <- getbb("Lambayeque Peru")%>%
#   opq()%>%
#   add_osm_feature(key = "waterway", value = "river") %>%
#   osmdata_sf()

# markets <- chiclayo %>%
#   opq() %>%
#   add_osm_feature("name","market")%>%
#   #add_osm_feature(key = "shop",value = "supermarket") %>%
#   osmdata_sf()
# markets %>% str()

# _nominatim --------------------------------------------------------------

# sí replica buscador: https://nominatim.openstreetmap.org/search.php?q=mercado+chiclayo&polygon_geojson=1&exclude_place_ids=267181192&accept-language=en-US%2Cen%3Bq%3D0.5
markets <- osm_search(query = "mercado+chiclayo+lambayeque+peru",
                      key = "REDACTED_NOMINATIM_KEY",limit = 10) %>% 
  as_tibble() %>% 
  mutate(new_name=str_replace(display_name,"\\,.*$",""),
         place_size=case_when(place_id=="44747738"~4,
                              place_id=="118720478"~2,
                              TRUE~1)) %>% 
  st_as_sf( coords = c("lon", "lat")) %>% 
  st_set_crs("+init=epsg:4326") %>% 
  st_transform(crs="+proj=longlat +datum=WGS84")

markets %>% select(place_id,new_name,place_size)

market_network <- tibble(from=c(4,2,4),
                         to=c(2,1,1),
                         width=c(4,2,1))

# osm_search(query = "supermarket+chiclayo+lambayeque+peru",
#            key = "REDACTED_NOMINATIM_KEY",limit = 10) %>% 
#   as_tibble() %>% 
#   select(display_name)

# _make osm map -----------------------------------------------------------

extencion <- 0.02
ggplot() +
  geom_sf(data = per4,alpha=0) +
  geom_sf(data = large_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 1,
          alpha = .8) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .4,
          alpha = .8) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "gray",
          size = .2,
          alpha = .5) +
  geom_sf(data=farms,shape=2) +
  geom_sf_text_repel(data = farms,
                     mapping = aes(label=name),
                     size = 2.5,
                     hjust = 1,direction="y") +
  geom_sf(data=slauther_disa,shape=3) +
  geom_sf_text_repel(data = slauther_disa,
                     mapping = aes(label=name),
                     size = 2,
                     hjust = 1,direction="y") +
  geom_sf(data=slauther_senasa,shape=3) +
  geom_sf_text_repel(data = slauther_senasa,
                     mapping = aes(label=name),
                     size = 2,
                     hjust = 1,direction="y") +
  geom_sf(data = markets,aes(size=place_size),
          color="red",show.legend = FALSE) +
  geom_sf_label_repel(data = markets,
                      mapping = aes(label=new_name),
                      hjust = 0,
                      direction="y",
                      size = 2.5) +
  geom_sf(data = cases_geo,colour="blue",show.legend = FALSE) +
  coord_sf(xlim = c(chiclayo[1,1]-extencion, chiclayo[1,2]+extencion),
           ylim = c(chiclayo[2,1]+extencion, chiclayo[2,2]+extencion),
           expand = FALSE) + 
  theme_void()
ggsave("figure/fig01-markets.png",width = 9,height = 8)

# osm map -----------------------------------------------------------------

# get_stamenmap(bbox = chiclayo,
#               zoom = 15, 
#               maptype = "toner-background") %>% ggmap()

# library(ggraph)
# library(tidygraph)
# as_tibble(highschool)
# as_tbl_graph(highschool)
# graph <- as_tbl_graph(market_network)
# ggraph(graph, layout = 'kk') + 
#   geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) + 
#   geom_node_point(aes(size = Popularity)) 
