#Steps
#1. query osm_data around a station with boundary of 2414m
#2. Copy osm_points to a data.frame - keep only osm_id, geometry, shop, and amenity columns
#3, filter osm_points the amenity categories below
#4. Add a distance column by calculating distance to the station. Calculate distance-weight 
# score by plugging into the formula and into a new column
#5. Calculate amenity-scores by multiplying distance-weight by amenity-weight score
#6. Sum up scores and multiply by 6.67 to get score out of 100

#http://pubs.cedeus.cl/omeka/files/original/b6fa690993d59007784a7a26804d42be.pdf

#multiplied by approximately 6.67 to get to score out of 100

require(osmdata)
require(dplyr)
require(sf)
require(leaflet)
require(tmap)
require(sfnetworks)
require(plotly)

#query

get_walk_score<-function(lon,lat,full){
  
  #put the lon and lat of the input into a st_point object
  #lat_long<-st_transform(st_point(c(lon, lat)),crs=4326)
  #ensure then proper projection 
  #st_crs(lat_long)
  #query osm around the point of interest by 1-mile radius
  walkability <- opq_around (lon, lat, 1609,timeout=5000) %>%
    osmdata_sf ()
  
#  walkability <- opq ("portsmouth usa") %>%
#    add_osm_features (features = list (
#      "amenity" = "restaurant",
#      "amenity" = "pub"
#    ))
#  
##  bbox = c(lon-1609, lat-1609, lon+1609, lat+1609)
#  walkability<- opq(bbox = c(lon-2000, lat-2000, lon+2000, lat+2000)) %>%
#    add_osm_features (features = list (
#      "shop",
#      "amenity",
#      "highway",
#      "building"))
 
  #subset points and get important columns
  osm_points_subset<-walkability$osm_points[(which(!is.na(walkability$osm_points$amenity) | !is.na(walkability$osm_points$shop) | !is.na(walkability$osm_points$leisure))), ]
  #osm_points_subset<-subset(osm_points_subset,select=c(osm_id,name,geometry,amenity,shop,leisure))
  #subset polygons and get important columns
  osm_polygons_subset <- walkability$osm_polygons[(which(!is.na(walkability$osm_polygons$amenity) | !is.na(walkability$osm_polygons$shop) | !is.na(walkability$osm_polygons$leisure))), ]
  #osm_polygons_subset<-subset(osm_polygons_subset,select=c(osm_id,name,geometry,amenity,shop,leisure))
  
  #combine points and polygons into a single df containing all POI
  walkability_df <- bind_rows(osm_points_subset,osm_polygons_subset) %>%
    distinct(osm_id, .keep_all = TRUE)
  
  #Calculate Distance 
    # Define the function
    f <- function(x) -2.104949 + (99.35195 - -2.104949)/(1 + (x/1082.286)^4.185825)
    
    #convert centre point into an sf data.frame
    lat_long <- st_as_sf(
      data.frame(lon=lon, lat =lat), 
      coords = c("lon", "lat"), 
      crs = 4326
      )
    #calculate distance from each POI to the centre
    walkability_df$distance <- as.numeric(st_distance(walkability_df$geometry, lat_long))
    walkability_df$distance_score <- f(walkability_df$distance)
  
  #Calculate ranked weight for POIs  
    #add weight column scores for each category
    walkability_df$weight <-NA
    
    #function used to add weight for each category of POI based on distance
    weight_scoring<-function(df,category,x){
    
    if (category == "supermarket") {
      subset_df<-subset(walkability_df,shop=="supermarket")
    }
    if (category == "bank") {
      subset_df<-subset(walkability_df,amenity=="bank")
    }
    if (category == "park") {
      subset_df<-subset(walkability_df,leisure=="park")
    }
    if (category == "education") {
      subset_df<-subset(walkability_df,amenity=="kindergarden" | amenity=="school" | amenity=="childcare")
    }
    if (category == "books") {
      subset_df<-subset(walkability_df,amenity=="library" | amenity=="public_bookcase" | shop=="books")
    }
    if (category == "entertainment") {
      subset_df<-subset(walkability_df, amenity=="conference_centre" | amenity=="exhibition_centre" | amenity=="social_centre" | amenity=="arts_centre" | amenity=="casino" | amenity=="gambling" | amenity=="nightclub" | amenity=="theatre" | amenity=="cinema" | amenity=="music_venue" | amenity=="community_centre" )
    }
    
    if(nrow(subset_df) > 0) {
      i <- as.numeric(arrange(subset_df, desc(distance_score))[1,1])
      df[df$osm_id==i[1],"weight"] <- x
    } else {
      i <- NA
    }
    return(df)
    }
    
    #pooulate the POIs with weight score
    walkability_df<-weight_scoring(walkability_df,"supermarket",3)
    walkability_df<-weight_scoring(walkability_df,"bank",1)
    walkability_df<-weight_scoring(walkability_df,"park",1)
    walkability_df<-weight_scoring(walkability_df,"education",1)
    walkability_df<-weight_scoring(walkability_df,"books",1)
    walkability_df<-weight_scoring(walkability_df,"entertainment",1)
    
    #restaurants [.75, .45, .25, .25, .225, .225, .225, .225, .2, .2]
    weights_restaurant <- c(.75, .45, .25, .25, .225, .225, .225, .225, .2, .2)
    
    restaurant_df<-subset(walkability_df, amenity == "restaurant" & is.na(weight))
    row_count<-nrow(restaurant_df)
    if (row_count<10) {
      restaurant_df<- restaurant_df %>%
        arrange(desc(distance_score)) %>% 
        top_n(row_count, distance_score) %>% 
        mutate(weight = weights_restaurant[1:row_count])      
    } else {
      restaurant_df<- restaurant_df %>%
        arrange(desc(distance_score)) %>% 
        top_n(10, distance_score) %>% 
        mutate(weight = weights_restaurant)      
    }

    #coffee [1.25, .75]
    weights_coffee<-c(1.25,.75)
    
    coffee_df<-subset(walkability_df, (amenity == "cafe" | shop=="coffee") & is.na(weight))
    row_count<-nrow(coffee_df)
    if (row_count<2) {
      coffee_df<- coffee_df %>%
        arrange(desc(distance_score)) %>% 
        top_n(row_count, distance_score) %>% 
        mutate(weight = weights_coffee[1:row_count])      
    } else {
      coffee_df<- coffee_df %>%
        arrange(desc(distance_score)) %>% 
        top_n(2, distance_score) %>% 
        mutate(weight = weights_coffee)      
    }
    
    #shops [.5, .45, .4, .35, .3]
    weights_shopping<-c(.5,.45,.4,.35,.3)
    
    shop_df <- subset(walkability_df, !is.na(shop) & is.na(weight))
    row_count<-nrow(shop_df)
    if (row_count<5) {
      shop_df<- shop_df %>%
        arrange(desc(distance_score)) %>% 
        top_n(row_count, distance_score) %>% 
        mutate(weight = weights_shopping[1:row_count])      
    } else {
      shop_df<- shop_df %>%
        arrange(desc(distance_score)) %>% 
        top_n(5, distance_score) %>% 
        mutate(weight = weights_shopping)      
    }
  
  #Calculate final POI score
    sf_all <- rbind(walkability_df, restaurant_df, coffee_df, shop_df)
    sf_all<-subset(sf_all,!is.na(weight))
    sf_all$score<-(sf_all$distance_score/100) * sf_all$weight

  #Calculate penalty for number of crossings available to pedestrians  
    intersections<-walkability$osm_points[(which(walkability$osm_points$highway=="crossing" | !is.na(walkability$osm_points$crossing))),]
    intersections<-subset(intersections,select=c(osm_id,name,geometry,highway,crossing))
    intersections$distance <- as.numeric(st_distance(intersections$geometry, lat_long))
    intersections<-subset(intersections,intersections$distance < 2000)
    intersections_per_mile <- nrow(intersections)/(pi * 1.242742^2)
    
  #Calculate penalty for longer blocks without intersections  
    #find average block length
    ways<-walkability$osm_lines
    ways<-subset(ways,
                 ways$highway=="residential" | 
                   ways$highway=="tertiary" | 
                   ways$highway=="secondary" |
                   ways$highway=="primary")
    buffer<-st_buffer(lat_long,1609)
    roadways<-st_intersection(ways,buffer)
    roadways<-subset(roadways,select=c(osm_id,name,geometry,highway))
    roadways <- roadways[st_geometry_type(roadways) == "LINESTRING", ]
    
    #create sfnetwork using the roadways sf object
      network <- as_sfnetwork(roadways,length_as_weight=TRUE)
    #calculate the lengths between nodes
      network<-network %>%
        activate("edges") %>%
        mutate(length = edge_length())
    #convert the sfnetwork object back to sf
      network<-network %>%
        activate("edges") %>%
        st_as_sf()
    #calc mean length
      avg_block_length<-as.numeric(mean(network$length))
      #print(avg_block_length)
      if (avg_block_length <= 120) {
        penalty_block_length <- 0
      } else if (avg_block_length > 120 & avg_block_length <= 150) {
        penalty_block_length <- 1
      } else if (avg_block_length > 150 & avg_block_length <= 165) {
        penalty_block_length <- 2
      } else if (avg_block_length > 165 & avg_block_length <= 180) {
        penalty_block_length <- 3
      } else if (avg_block_length > 180 & avg_block_length <= 195) {
        penalty_block_length <- 4
      } else {
        penalty_block_length <- 5
      }
      #print(intersections_per_mile)
      if (intersections_per_mile > 200) {
        penalty_crossings <- 0
      } else if (intersections_per_mile > 150 & intersections_per_mile <= 200) {
        penalty_crossings <- 1
      } else if (intersections_per_mile > 120 & intersections_per_mile <= 150) {
        penalty_crossings <- 2
      } else if (intersections_per_mile > 90 & intersections_per_mile <= 120) {
        penalty_crossings <- 3
      } else if (intersections_per_mile > 60 & intersections_per_mile <= 90) {
        penalty_crossings <- 4
      } else {
        penalty_crossings <- 5
      }
      
  #Calculate final Walk Score
      score<-(sum(sf_all$score)*6.67)-penalty_crossings-penalty_block_length
      #print(sf_all)
      #print(penalty_crossings)
      #print(penalty_block_length)
      print(score)
  if (full==TRUE) {
      plot<-tm_shape(sf_all$geometry) + 
        tm_dots(col = "red", size = 0.1, title = "POIs")
      crossings<-tm_shape(intersections) + 
        tm_dots(col = "red", size = 0.1, title = "Crossing")
      result=list(poi=sf_all,
                score = score, 
                plot = plot,
                crossings = crossings,
                network = network)
      }
      else if (full==FALSE) {
        result=score
      }
  #return the results     
    return(result)
}

vancouver_skytrain_sf <- st_as_sf(vancouver_skytrain, coords = c("lon", "lat"), crs = 4326)
vancouver_skytrain$lon<-st_coordinates(vancouver_skytrain$geometry)[,1]
vancouver_skytrain$lat<-st_coordinates(vancouver_skytrain$geometry)[,2]

vancouver_skytrain$walk_score <- mapply(get_walk_score, vancouver_skytrain$lon, vancouver_skytrain$lat,FALSE)

van_landuse<- c(-122.77394608035578,49.15432655668457,-123.27481153687995,49.300660648361216) %>%
  opq() %>%
  add_osm_feature(key = 'landuse',value_exact = F, key_exact = F) %>%
  osmdata_sf()

van_landuse_df<-subset(van_landuse$osm_polygons,
                                landuse == "commercial" | 
                                landuse == "construction" | 
                                landuse == "industrial" |
                                landuse == "institution" | 
                                landuse == "residential" | 
                                landuse == "recreation_ground" | 
                                landuse == "retail" 
                      )

tmap_mode("view")
tm_shape(van_landuse_df)+
  tm_polygons("landuse")+ #
tm_shape(st_as_sf(vancouver_skytrain)) +
       tm_dots(col="walk_score",size="TotalBoardings",palette="RG") 

plot(vancouver_skytrain$walk_score,vancouver_skytrain$TotalBoardings)



toronto_ridership$walk_score <- mapply(get_walk_score, toronto_ridership$lon, toronto_ridership$lat,FALSE)

ridership_vs_walkscore<-rbind(vancouver_ridership,toronto_ridership)

p <-ggplot(ridership_vs_walkscore,aes(walk_score, 
                                  residential_area,
                                  col=Line,
                                  text=paste("Station:",Station))) +
                                geom_point() +
                                theme_bw()
ggplotly(p)

