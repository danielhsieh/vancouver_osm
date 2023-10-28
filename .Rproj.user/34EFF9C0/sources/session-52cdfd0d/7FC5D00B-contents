require(osmdata)
require(dplyr)
require(sf)
require(leaflet)
require(tmap)
require(sfnetworks)
require(plotly)

lon=ridership_vs_walkscore$lon[49]
lat=ridership_vs_walkscore$lat[49]

get_far_score<-function(lon,lat){

#create centre point as an sf_point    
  lat_long <- st_as_sf(
    data.frame(lon=lon, lat =lat), 
    coords = c("lon", "lat"), 
    crs = 4326
  )

#query OSM for buildings within 1km
  building_density <- opq_around (lon, lat, 
                             1000,
                             key="building",
                             timeout=5000) %>%
    osmdata_sf ()

#extra only polygons into a data.frame
  building_density<-building_density$osm_polygons
#subset to remove unnecessary columns
#  building_density<-subset(building_density,select=c(osm_id,name,building:levels,geometry))
#calculate area of each polygon
  building_density$area <- sf::st_area(building_density)
#create a buffer 1km radius from the centre point
  buffer<-st_buffer(lat_long,1000)
#remove objects that overlap with the buffer
  building_density<-st_intersection(building_density,buffer)
#convert building levels into a numeric instead of character
  building_density$building.levels<-as.numeric(building_density$building.levels)
#if levels=NA, make it 1 
  building_density$building.levels <- ifelse(is.na(building_density$building.levels), 1, building_density$building.levels)

# Calculate the total floor area of all buildings
  total_floor_area <- sum(st_area(building_density) * building_density$building.levels)

# Calculate the total area of the area of interest
  total_area <- st_area(buffer)

# Calculate the floor area ratio (FAR)
  far <- total_floor_area / total_area * 100
  
#finish
  result=far
  return(result)
}

get_residential_area<-function(lon,lat){
  
  #create centre point as an sf_point    
  lat_long <- st_as_sf(
    data.frame(lon=lon, lat =lat), 
    coords = c("lon", "lat"), 
    crs = 4326
  )
  
  #query OSM for buildings within 1km
  residential_freq <- opq_around (lon, lat, 
                                  1000,
                                  key="building",
                                  timeout=5000) %>%
    osmdata_sf ()
  
  #extra only polygons into a data.frame
  residential_freq<-residential_freq$osm_polygons
  #subset to remove unnecessary columns
  #calculate area of each polygon
  residential_freq$area <- sf::st_area(residential_freq)
  #create a buffer 1km radius from the centre point
  buffer<-st_buffer(lat_long,1000)
  #remove objects that overlap with the buffer
  residential_freq<-st_intersection(residential_freq,buffer)
  residential_freq$touches<-as.character(st_touches(residential_freq))
  
  #if area is more or less than the threshold, consider it residential
  residential_freq$residential <- ifelse(as.numeric(residential_freq$area) < 300 & residential_freq$touches == "integer(0)", TRUE, FALSE)
  
  # Calculate the total floor area of all buildings
  total_residential_area <- sum(residential_freq$area[residential_freq$residential == TRUE])
  total_building_area <- sum(residential_freq$area)
  
  # Calculate the area ratio of detached homes to all other buildings in the area
  residential_area <- total_residential_area / total_building_area * 100
  
  #finish
  result=residential_area
  return(result)
}

result<-get_far_score(vancouver_ridership$lon[1],vancouver_ridership$lat[1])
ridership_vs_walkscore$floor_area_ratio<-mapply(get_far_score,
                                                ridership_vs_walkscore$lon,
                                                ridership_vs_walkscore$lat)

ridership_vs_walkscore$residential_area<-mapply(get_residential_area,
                                            ridership_vs_walkscore$lon,
                                            ridership_vs_walkscore$lat)


tmap_mode("view")
tm_shape(ridership_vs_walkscore) +
  tm_polygons("residential", palette = "Blues", title = "Area")

tm_shape(subset(ridership_vs_walkscore,city == "Vancouver")) +
  tm_dots(col="Line",size="residential_area") 


ggplotly(ggplot() +
  geom_sf(data = building_density, aes(fill=as.numeric(area))) +
  #scale_fill_manual(values = c("#999999", "#777777", "#555555", "#333333", "#111111")) +
  labs(title = "Floor Area Ratio of Buildings", x = "Longitude", y = "Latitude"))





