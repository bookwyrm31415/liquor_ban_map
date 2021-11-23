library(tidyverse)
library(dtplyr)
library(sf)
library(osmdata)
library(osmplotr)
library(sysfonts)
library(viridis)
library(showtext)

#Fonts
showtext_auto()
font_add_google("Fira Sans")
font1 <- "Fira Sans"

#bbox

bbox <- get_bbox(c(174.730994,-36.868309,174.793204,-36.836179))


#street data from osm
streets <- extract_osm_objects(key = "highway", bbox = bbox) %>% st_transform(crs = 3857)

#gets rid of extraneous long road
streets <- streets[!(streets$osm_id == "808232612"),]

streets <- streets[!grepl("Tamaki",streets$name),]



#data from https://data-aucklandcouncil.opendata.arcgis.com/datasets/alcohol-control-area/explore
liquor <- st_read(dsn = "C:/Users/Ryan/Downloads/Alcohol_Control_Area") 

#crop liquor ban area to streets
liquor <- st_intersection(st_as_sfc(st_bbox(streets)), liquor)

#500m buffer
liq_buffer <- st_buffer(liquor, 500) %>% st_sf()

#grid over street area
grid <- st_as_sfc(st_bbox(streets)) %>% st_make_grid(n = 40) %>% st_sf() %>% mutate(id = row_number())

#grid gets count of liquor ban areas over it
test_grid <- grid %>% st_join(liq_buffer) %>% group_by(id) %>% summarise(bans = n())

#give street data the ban count
street_intersect <- st_intersection(test_grid, streets)

#plot
auck_plot <- ggplot() + 
  geom_sf(data = street_intersect, aes(col = bans)) +
  scale_color_viridis(option = "D", name = "# Bans") + 
  theme_void()+
  ggtitle("Auckland", subtitle = "Number of unique Liquor Ban Areas within 500m")+
  theme(panel.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        plot.background = element_rect(color="#fdf9f5", fill="#fdf9f5"),
        text = element_text(family = font1, size = 35, hjust = 0.5),
        plot.margin=margin(0.5, 0.5, 0.5, 0.5, "cm"))

# show plot
auck_plot


#save
ggsave("D:/r_mess_around/Auck_Liq.png")

