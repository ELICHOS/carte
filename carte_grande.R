library(osmdata)
library(ggplot2)

coord<-c(5.02817943675312, 44.74928157272365, 5.559643062465317, 45.18795355761796)
q <- opq(bbox = coord)


routes<-q%>%add_osm_feature(key = 'highway', value = c('motorway',
                                                  'trunk', 'primary',
                                                  'secondary'))%>%osmdata_sf()

routes2<-q%>%add_osm_feature(key = 'highway', value = c('tertiary', "unclassified"))%>%osmdata_sf() #,'residential'))%>%osmdata_sf()


chemin<-q%>%add_osm_feature(key = 'highway', value = c('path'))%>%osmdata_sf()


grotte<-q%>%add_osm_feature(key = 'natural', value = c('cave_entrance'))%>%osmdata_sf()

rivieres<-q%>%add_osm_feature(key = 'waterway', value = c('river'))%>%osmdata_sf()
city<-opq(bbox=coord, nodes_only = TRUE) %>%
  add_osm_feature (key = "place", value = "town") %>% osmdata_sf()
village<-opq(bbox=coord, nodes_only = TRUE) %>%
  add_osm_feature (key = "place", value = "village") %>% osmdata_sf()


com<-sf::read_sf("C:/Users/024176/Downloads/ADMIN-EXPRESS-COG_3-1__SHP_WGS84G_FRA_2022-04-15/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2022-04-15/ADECOG_3-1_SHP_WGS84G_FRA/COMMUNE.shp")%>%
  dplyr::filter(INSEE_DEP%in%c("26", "38"))%>%
  dplyr::filter(POPULATION>5000)

forest<-opq(bbox=coord) %>%
  add_osm_feature (key = "landuse", value = "forest") %>% osmdata_sf()

pasture<-opq(bbox=coord) %>%
  add_osm_feature (key = "landuse", value = "meadow") %>% osmdata_sf()

agricol<-opq(bbox=coord) %>%
  add_osm_feature (key = "landuse", value = c("ordchard", "farmland", "allotments", "greenhouse_horticulture", "plant_nursery", "vineyard ")) %>% osmdata_sf()


residential<-opq(bbox=coord) %>%
  add_osm_feature (key = "landuse", value = "residential") %>% osmdata_sf()

montagne<-opq(bbox=coord) %>%
  add_osm_feature (key = "natural", value = "sadle") %>% osmdata_sf()

####
pal.polyg<-c("forest" = "#00cc44", #as.matrix(c("red"=51, "green"=97, "blue"=49)),  #"#00cc44"
             "pasture"= "#77ff33",  #"#77ff33"
             "agricol"= "#b3e6cc",  #"#b3e6cc"
             "residential" = gray(0.9) #gray(0.5)
             )



####


carte<-ggplot() +
  geom_sf(data=forest$osm_polygons, fill=pal.polyg["forest"], color=NA)+
  geom_sf(data=pasture$osm_polygons, fill=pal.polyg["pasture"], color=NA)+
  geom_sf(data=agricol$osm_polygons, fill=pal.polyg["agricol"], color=NA)+
  geom_sf(data=residential$osm_polygons, fill=pal.polyg["residential"], color=NA)+
  geom_sf(data = rivieres$osm_lines,
          inherit.aes = FALSE,
          color = "#006699", linewidth=0.1)+
  geom_sf(data = rivieres$osm_lines%>%dplyr::filter(name=="L'Isère"|name=="La Drôme"),
          inherit.aes = FALSE,
          color = "#006699", linewidth=0.7, alpha=0.8)+
  geom_sf(data = routes2$osm_lines,
          inherit.aes = FALSE,
          color = gray(0.5), linewidth=0)+
  geom_sf(data = routes$osm_lines,
          inherit.aes = FALSE,
          color = "black", linewidth=0)+
  geom_sf(data=grotte$osm_points,shape=21, color="black",
          fill=gray(0.8), inherit.aes = F, size=0.2)+
  coord_sf(xlim = coord[c(1, 3)], ylim = coord[c(2, 4)])+
  theme_void()
ggsave(plot = carte, device = "pdf", filename = "C:/Users/024176/Desktop/PERSO/carte1.pdf", dpi = 1000)

