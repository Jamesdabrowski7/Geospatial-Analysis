# James Dabrowski Project 1: Geospatial Data

# installing packages necessary for project 1
library(sf)
library(dplyr)
library(ggplot2)
# reading in necessary data
zip_codes <- st_read("zip_codes.shp")
crime_data <- read.csv("crimes1.csv")
communities <- st_read("communities.shp")
schools <- st_read("schools.shp")
# showing column names 
colnames(crime_data)
colnames(zip_codes)
# showing median / mean 
mean_crime <- 2841  
median_crime <- 2767  
# projecting zip codes
st_crs(zip_codes)
zip_codes_proj <- st_transform(zip_codes, crs = 3435)
# change csv to shp
crime_to_sf <- st_as_sf(x = crime_data,
                        coords= c("Longitude", "Latitude"), crs = 4326)
# checking and projecting 
st_is_longlat(crime_to_sf)
crime_to_sf_proj = st_transform(crime_to_sf, crs = 3435)                                                                  
# using st within
crimes_zip <- st_within(crime_to_sf_proj, zip_codes_proj, sparse = FALSE)
#summing number
sum_crimes <- apply(X = crimes_zip , MARGIN=2, FUN=sum)
# rebinding
crime_zip_bind <- cbind(zip_codes_proj, sum_crimes)
head(crime_zip_bind)
# filter based on median 
highrisk_crime <- crime_zip_bind %>% filter(sum_crimes > 2767)
lowrisk_crime <- crime_zip_bind %>% filter(sum_crimes < 2767)
# view filtered data
head(lowrisk_crime)
head(highrisk_crime)
  
# displaying map 
tm_shape(zip_codes_proj) + 
  tm_polygons(alpha = 0.4) +
  
  tm_shape(highrisk_crime) +
  tm_polygons(col = "red", alpha = 0.5) +
  tm_text('zip') +  
  
  tm_shape(lowrisk_crime) + 
  tm_polygons(col = "green", alpha = 0.2) +
  tm_text('zip') + 
  
  tm_layout(title = "Crime Incidents by Zip Code",
            title.position = "center",
            frame = FALSE)
# doing same thing but for communities
st_crs(communities)
communities_proj <- st_transform(communities, crs = 3435)
# using st within
crimes_by_community <- st_within(crime_to_sf_proj, communities_proj, sparse = FALSE)
# using apply
#summing number
community_crime <- apply(X = crimes_by_community , MARGIN=2, FUN=sum)
# rebinding
community_rebind <- cbind(communities_proj, community_crime)
# checking data 
head(community_rebind)
# filter based on mean
highrisk_community <- community_rebind %>% filter(community_crime > 2250)
lowrisk_community <- community_rebind %>% filter(community_crime < 2250)
# finding highest and lowest values
lowest <- community_rebind %>% arrange(community_crime) %>% head(1)
highest <- community_rebind %>% arrange(community_crime) %>% tail(1)
# making map
tm_shape(community_rebind) + 
  tm_polygons(alpha = 0.4) +
  
  tm_shape(highest) +
  tm_polygons(col = "red", alpha = 0.5) +
  tm_text('community') +  
  
  tm_shape(lowest) + 
  tm_polygons(col = "green", alpha = 0.2) +
  tm_text('community') + 
  
  tm_layout(title = "Highest and Lowest Crime Inc By community",
            title.position = "center",
            frame = FALSE)
###### Done with Task 1 ###########


#####################Task 2################################### 
# Task 2 school exposure to crime incidents
# making first map showing baseline visulation
tm_shape(schools) +
  tm_dots(col = "blue", size = 0.05, title = "Schools") +
  tm_text("short_name", size = 0.3, col = "black", shadow = TRUE, remove.overlap = TRUE) +  
  tm_layout(title = "Locations of Schools in the City",
            title.position = c("center", "top"),
            frame = FALSE)
# filtering crime_sf for criminal damage to property
property_damage <- crime_to_sf %>%
  filter(Primary.Type == "CRIMINAL DAMAGE", Description == "TO PROPERTY")

# creating map 1 of 2
#checking crs and long/lat of schools and crime data
class(property_damage)
st_crs(property_damage)
st_is_longlat(property_damage)
st_crs(schools)
st_is_longlat(schools)

#projecting both data types to match CRS
property_damage_proj <- st_transform(property_damage, crs = 3435)
schools_proj <- st_transform(schools, crs = 3435)

#buffers for second map .5 and 1 mile
st_crs(schools_proj)$units 
school_half_buffer <- st_buffer(schools_proj, dist = 2640)
schools_one_buff <- st_buffer(schools_proj, dist = 5280)
#create second map including buffers and concentration of crimes in the area 
tm_shape(communities_proj) + tm_polygons(alpha = .8) +
  tm_shape(schools_proj) + tm_dots(col = 'blue', size = 0.1) + 
  tm_shape(school_half_buffer) + tm_borders(col = "red") + tm_fill(col = "red", alpha = 0.2) +
  tm_shape(schools_one_buff) + tm_borders(col = "orange") + tm_fill(col = "orange", alpha = 0.25) +
  tm_shape(property_damage_proj) + tm_dots(col = 'green', size = .001) +
  tm_layout(main.title = "Schools with Buffer Zones and Criminal Damage to Property Incidences",
            main.title.position = "center",
            main.title.size = .6,
            frame = FALSE)
################# creating bar charts using ggplot ######################
library(ggplot2)


# create the mile buffer around the schools
crime_and_mile <- st_within(property_damage_proj, schools_one_buff, sparse = FALSE)
crime_and_mile_sum <- apply(X = crime_and_mile, MARGIN = 2, FUN = sum)
crime_within_a_mile <- cbind(schools_one_buff, crime_and_mile_sum)

# Filter for top 10 and least 10 schools
top_10_schools <- crime_within_a_mile %>%
  arrange(desc(crime_and_mile_sum)) %>%
  head(10)

least_10_schools <- crime_within_a_mile %>%
  arrange(crime_and_mile_sum) %>%
  head(10)

# Create bar plot for top 10 schools
ggplot(top_10_schools, aes(x = reorder(short_name, crime_and_mile_sum), y = crime_and_mile_sum)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  labs(title = "Top 10 Schools with Highest Criminal Damage Incidents",
       x = "School",
       y = "Number of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))

# Create bar plot for least 10 schools
ggplot(least_10_schools, aes(x = reorder(short_name, crime_and_mile_sum), y = crime_and_mile_sum)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  labs(title = "Least 10 Schools with Lowest Criminal Damage Incidents",
       x = "School",
       y = "Number of Incidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))


