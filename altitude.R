library(sf)
library(ggplot2)
library(tidyverse)
# ISO 38
func_load_sf <- function(pat){
  li <- list.files(pat)
  li <- li[grepl(".shp", li, fixed=TRUE)]
  iso_curves<- bind_rows(
  lapply(li, function(x){
    print(x)
    res <- sf::read_sf(paste0(pat, x)) |> 
      mutate(TILE= x)
    #
    res <- bind_cols(
      res,
      (matrix(sf::st_bbox(res), ncol = 4, nrow = nrow(res), byrow = T)  |> 
        as.data.frame()  |> setNames(c("xmin", "ymin", "xmax", "ymax")))
    )
    return(res)
  })
)
  return(iso_curves)
}

iso_curves_38 <- func_load_sf(pat = "COURBE_1-0__SHP_LAMB93_D038_2021-01-01(1)/COURBE_1-0__SHP_LAMB93_D038_2021-01-01/COURBE/1_DONNEES_LIVRAISON_2021-01-01/COURBE_1-0_SHP_LAMB93_D038_2021/") |> 
  filter(IMPORTANCE == "1") |> ungroup()
iso_curves_26 <- func_load_sf(pat = "COURBE_1-0__SHP_LAMB93_D026_2021-01-01(1)/COURBE_1-0__SHP_LAMB93_D026_2021-01-01/COURBE/1_DONNEES_LIVRAISON_2021-01-01/COURBE_1-0_SHP_LAMB93_D026_2021/") |> 
  filter(IMPORTANCE == "1") |> ungroup()

iso_curves <- bind_rows(
  iso_curves_38,
  iso_curves_26 |> 
    filter(!TILE %in% unique(iso_curves_38$TILE))
  ) 
iso_curves_10 <- iso_curves #|> 
  #filter(endsWith(x = as.character(ALTITUDE), suffix = "0"))
iso_curves_10 <- iso_curves_10 |> 
  ungroup() |> 
  mutate(easten = dplyr::dense_rank(xmin)) |> 
  mutate(northern = dplyr::dense_rank(ymin)) |> 
  filter(easten < max(easten) & 
    northern < max(northern) & 
    northern > 2) 

p_altitude <- ggplot(iso_curves_10 |> slice_sample(prop = 1))+
  geom_sf(color = gray(0.2), alpha=0.9, linewidth=0.1) + 
  theme_light(base_family = "Ubuntu-L")+
  theme()
ggsave(plot = p_altitude, filename = "alti_plot2.pdf", device = cairo_pdf, 
        dpi = 1200, bg = "white",
        width=20, height=10.5)

p_altitude <- ggplot(iso_curves_10 |> slice_sample(prop = 1))+
          geom_sf(color = gray(0), alpha=0.9, linewidth=0.1) + 
          theme_light(base_family = "Ubuntu-L")+
          theme()
ggsave(plot = p_altitude, filename = "alti_plot2.png", 
        dpi = 1200, bg = "white",
        width=20, height=10.5)
