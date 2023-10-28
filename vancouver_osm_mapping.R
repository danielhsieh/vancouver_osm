require(osmdata)
require(dplyr)
require(sf)
require(leaflet)
require(tmap)
require(caret)
require(class)

#bounding box for vancouver
vancouver_bb <- getbb("vancouver")
  available_features()
  available_tags("highway")
  #retrieve rail stations within the bbox
  van_landuse<- vancouver_bb %>%
    opq() %>%
    add_osm_feature(key = 'landuse',#, value = 'station',
                    value_exact = F, key_exact = F) %>% 
    osmdata_sf()
  #convert the osmdata points into a dataframe 
  van_stations<-data.frame(van_stations$osm_points)
  #convert the sfc_point data.class into numeric values in a matrix
  van_stations_coords<-st_coordinates(van_stations$geometry)
  #append the matrix columns as new variables in the data.frame
  van_stations$lon <- van_stations_coords[,1]
  van_stations$lat <- van_stations_coords[,2]


###create a function that iterates across all the layers
  get_osm_data <- function(df,i) { 
  lon_min <<- df$lon[i]-0.015060
  lat_min <<- df$lat[i]-0.008983
  lon_max <<- df$lon[i]+0.015060
  lat_max <<- df$lat[i]+0.008983
  bbox=c(lon_min,lat_min,lon_max,lat_max)
  
    buildings <<- bbox %>%
      opq() %>%
      add_osm_feature(key="building") %>%
      osmdata_sf()
    retail <<- bbox %>%
      opq() %>%
      add_osm_feature(key="landuse",value="retail") %>%
      osmdata_sf()
    residential <<- bbox %>%
      opq() %>%
      add_osm_feature(key="landuse",value="residential") %>%
      osmdata_sf()    
    greenspace <<- bbox %>%
      opq() %>%
      add_osm_feature(key="leisure",value=c("park","golf_course","pitch","playground")) %>%
      osmdata_sf()    
    institute <<- bbox %>%
      opq() %>%
      add_osm_feature(key="amenity",value=c("school","college")) %>%
      osmdata_sf()
    highway <<- bbox %>%
      opq() %>%
      add_osm_feature(key="highway",value=c("motorway","primary","motorway_link","primary_link")) %>%
      osmdata_sf()
    medium_streets <<- bbox %>%
      opq() %>%
      add_osm_feature(key="highway",value=c("secondary","tertiary","secondary_link","tertiary_link")) %>%
      osmdata_sf()
    small_streets <<- bbox %>%
      opq() %>%
      add_osm_feature(key="highway",value=c("residential","living_street","unclassified","service")) %>%
      osmdata_sf()
    cycleway <<- bbox %>%
      opq() %>%
      add_osm_feature(key="highway",value="cycleway") %>%
      osmdata_sf()   
    cycleway2 <<- bbox %>%
      opq() %>%
      add_osm_feature(key="route",value="bicycle") %>%
      osmdata_sf()   
    footpath <<- bbox %>%
      opq() %>%
      add_osm_feature(key="highway",value="footpath") %>%
      osmdata_sf()   
    waterways <<- bbox %>%
      opq() %>%
      add_osm_feature(key="natural",value=c("water","bay")) %>%
      osmdata_sf()  
}

#buildings only by levels
ggplot() +
  geom_sf(data = buildings$osm_polygons,
          aes(fill=as.numeric(buildings$osm_polygons$`building:levels`)),color=NA) +
  labs(title = "")+
  #theme_void()+
  xlim(-123.125,-123.112)+
  ylim(49.222,49.231)

#leaflet plot
tmap_mode("view")
tm_shape(buildings$osm_polygons)+
  tm_polygons("building",palette="RdYlGn")
# tm_sf() #view full sf object
# tm_borders() #view polygon edges
# tm_dots() #view points
# tm_fill() 
# tm_polygons("building:levels",palette="RdYlGn", style="cont")


#buildings only by type
filter(buildings$osm_polygons,
       building == "residential" | 
         building == "apartment" | 
         building == "garage" | 
         building == "townhouse" | 
         building == "apartments" | 
         building == "mixed_use" | 
         building == "train_station" 
         ) %>%
      filter_sf(xmin=-123.120,xmax=-123.110,ymin=49.223,ymax=49.229) %>%
ggplot(aes(fill=building)) +
  geom_sf() 
  

#plot
ggplot() +
  geom_sf(data = waterways$osm_polygons,
          inherit.aes = FALSE,
          fill = "steelblue",
          color=NA) +
  geom_sf(data = greenspace$osm_polygons,
          inherit.aes = FALSE,
          fill = "#C1E1C1",
          color=NA) +
  geom_sf(data = institute$osm_polygons,
          inherit.aes = FALSE,
          fill = "#D3C7A2",
          color=NA) +
  geom_sf(data = retail$osm_polygons,
          inherit.aes = FALSE,
          fill = "#F8C8DC",
          color=NA) +
  geom_sf(data = residential$osm_polygons,
          inherit.aes = FALSE,
          color = "#e6e6e6",
          color=NA) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          lwd = 1, alpha=0.1) +
  geom_sf(data = medium_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          lwd = 1, alpha=0.1) +
  geom_sf(data = highways$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          lwd = 1, alpha=0.1) +
  geom_sf(data = footpath$osm_lines,
          inherit.aes = FALSE,
          color = "#ffa852",
          linetype="dotdash",
          lwd = 1, alpha=0.1) +
  geom_sf(data = cycleway$osm_lines,
          inherit.aes = FALSE,
          color = "#FF0000",
          lwd = 1, alpha=0.8) +
  geom_sf(data = cycleway2$osm_lines,
          inherit.aes = FALSE,
          color = "#FF0000",
          linetype="dotdash",
          lwd = 1, alpha=0.3) +
  geom_sf(data = buildings$osm_polygons,
          aes(fill=buildings$osm_polygons$`building:levels`),
          color=NA) +
  coord_sf(xlim = c(lon_min, lon_max), 
         ylim = c(lat_min, lat_max),
         expand = FALSE) +
  labs(title = "")+
  theme_void()+
  xlim(123.110,123.120)+
  ylim(49.222,49.232)

#analyze buildings
#count of buildings where the level is NA
count(buildings$osm_polygons,is.na(buildings$osm_polygons$`building:levels`))
count(buildings$osm_polygons,is.na(buildings$osm_polygons$amenity))
count(buildings$osm_polygons,factor(buildings$osm_polygons$building))
colnames(buildings$osm_polygons)

#calculate surface area of buildings



#amenities
amenity <- opq_around (-123.0907677, 49.2254735, 500,key = "amenity") %>%
  osmdata_sf ()
amenity <- amenity$osm_points
amenity <- amenity %>%
  filter(!is.na(amenity))
amenity <- amenity %>%
  filter(!(amenity=='yes'),
         !(amenity=='bench'),
         !(amenity=='bicycle_parking'),
         !(amenity=='post_box'),
         !(amenity=='vending_machine'),
         !(amenity=='waste_basket'),
         !(amenity=='parking_entrance'),
         !(amenity=='bicycle_rental'),
         !(amenity=='public_bookcase'),
         !(amenity=='waste_disposal')
         )

#store
store <- opq_around (-123.0907677, 49.2254735, 500,key = "shop") %>%
  osmdata_sf ()
store <- store$osm_points
store <- store %>%
  filter(!is.na(shop))
store <- store %>%
  filter(!(shop=='yes'))

#healthcare providers
hcp <- opq_around (-123.0907677, 49.2254735, 500,key = "healthcare") %>%
  osmdata_sf ()
hcp <- hcp$osm_points
hcp <- hcp %>%
  filter(!is.na(healthcare))

#add a type column
amenity <- amenity %>% mutate(type = amenity)
store <- store %>% mutate(type = paste("shop - ", shop, sep = ""))
hcp <- hcp %>% mutate(type = "hcp")
# Combine the data frames and retain all rows and columns
points <- bind_rows(amenity, store, hcp)

#landuse
landuse <- opq_around (-123.0907677, 49.2254735, 500,key = "landuse") %>%
  osmdata_sf ()


#plot residential building area
ggplot(buildings_df[buildings_df$building=="residential",], aes(x = building, y = as.numeric(area))) +
  geom_boxplot() +
  labs(x = "Building Type", y = "Building Area m^2") +
  theme_bw()

#subset data into separate df
buildings_residential <- buildings$osm_polygons %>% filter(building == "residential")
buildings_garage <- buildings$osm_polygons %>% filter(building == "garage")
buildings_house <- buildings$osm_polygons %>% filter(building == "house")

#t-test between the two
t.test(as.numeric(buildings_residential$area), as.numeric(buildings_garage$area), alternative = "two.sided", var.equal = TRUE)


#analysis between the 3; p<0.05 means there is difference between the 3 types
buildings_accommodations <-buildings$osm_polygons %>% 
  filter(building == "residential" |
         building == "garage" | 
         building == "house"| 
           building == "apartments")

model <- aov(as.numeric(area) ~ building, data = buildings$osm_polygons)
summary(model)
#pairwise test 
TukeyHSD(model)

#plot pairwise differences

boxplot(area ~ building, data = buildings_accommodations, 
        main = "Building Area by Type", 
        xlab = "Building Type", ylab = "Area", col = "lightblue")


#classification algorithm to tell building type by area

# split the data into a training set (80%) and a testing set (20%)

#randomize 80% of the data set
trainIndex <- createDataPartition(buildings_accommodations$building, p = 0.8, list = FALSE)

training <- buildings_accommodations[trainIndex, ]
testing <- buildings_accommodations[-trainIndex, ]

#training <- buildings_accommodations[1:620, ]
#testing <- buildings_accommodations[621:773, ]

# Train a K Nearest Neighbour (KNN) classification model
model <- train(building ~ area, data = training, method = "knn")
model <- train(building ~ area, data = buildings$osm_polygons, method = "knn")

# predict which building type based on the area, using the model
predictions <- predict(model, newdata = testing)
# predict probabilities for each class, with a 0-1 probability
predictions_prob <- predict(model, newdata = testing,type="prob")


#performance of the prediction matrix
#note that this only works if there are the same number of building classes between testing and training datasets
confusionMatrix(predictions, factor(testing$building))


#full script
#find everything around a lat lon based on radius in m
buildings <- opq_around (-123.0907677, 49.2254735, 500,key = "building") %>%
  osmdata_sf ()
#calc area of the polygon
buildings$osm_polygons$area <- sf::st_area(buildings$osm_polygons)
# predict probabilities for each class, with a 0-1 probability
predictions_prob <- predict(model, newdata = buildings$osm_polygons,type="prob")

#add the predicted building type and probability score to the shapes
buildings$osm_polygons$prediction<-colnames(predictions_prob)[apply(predictions_prob, 1, which.max)]
buildings$osm_polygons$prob_score <- apply(predictions_prob, 1, max)

#check if the building polygon is stand-alone or not
#st_precision(buildings$osm_polygons) <- 0.1 
#buildings$osm_polygons$touches<-st_touches(st_simplify(buildings$osm_polygons, dTolerance = 0.1))
buildings$osm_polygons$touches<-as.character(st_touches(buildings$osm_polygons))


#replace building type based on the prediction and score

# replace buidling='yes' with 'residential' your actual data frame name
buildings$osm_polygons <- buildings$osm_polygons %>%
  mutate(building = ifelse(
    building == "yes" 
    & prediction == "residential" 
    & prob_score > 0.95 
    & touches == "integer(0)", 
    prediction, 
    building))

#summary stat
buildings$osm_polygons %>% 
  group_by(building) %>%
  summarise(count=n(),
            freq=n()/nrow(buildings$osm_polygons),
            sum_area=sum(area),
            avg_area=mean(area),
            prcnt_area=100*sum(area)/sum(buildings$osm_polygons$area),
            tot_living_space=sum(area*as.numeric(`building:levels`)),
            avg_lviing_space=mean(area*as.numeric(`building:levels`))
  )

#see it
tmap_mode("view")
tm_shape(buildings$osm_polygons) +
  tm_polygons("building")      #+,palette="RdYlGn") +
  tm_shape(amenity) + 
    tm_dots(col="amenity")
  
#https://ttc-cdn.azureedge.net/-/media/Project/TTC/DevProto/Documents/Home/About-the-TTC/Projects-Landing-Page/Transit-Planning/TTCca---Subway-Ridership-2019.pdf?rev=ca0f80af88b24890aec3b77b6a69a009