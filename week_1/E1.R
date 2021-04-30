library(readr)       
library(dplyr)       
library(ggplot2)
wildschwein_BE <- read_delim("wildschwein_BE.csv",",")
attr(wildschwein_BE$DatetimeUTC,"tzone")
ggplot(wildschwein_BE, aes(Long,Lat, colour = TierID)) +
geom_point() +
coord_map() +
theme(legend.position = "none")
wildschwein_BE <- st_transform(wildschwein_BE, 2056)
ggplot(mcp,aes(fill = TierID)) +
geom_sf(alpha = 0.4)
ggplot(mcp,aes(fill = TierID)) +
geom_sf(alpha = 0.4) +
coord_sf(datum = 2056)
library(tmap)
tm_shape(pk100_BE) + 
tm_rgb() 
tm_shape(pk100_BE) + 
tm_rgb() +
tm_shape(mcp) +
tm_polygons(col = "TierID",alpha = 0.4,border.col = "red") +
tm_legend(bg.color = "white")
tmap_mode("view")
tm_shape(mcp) +
tm_polygons(col = "TierID",alpha = 0.4,border.col = "red") +
tm_legend(bg.color = "white")
  
  