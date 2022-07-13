# Overlap of Fulmers and fishery bycatch in the UK

# Date created: 09/06/2022

# Author: Adam McGregor Laskey

# Last edited: 09/06/2022


install.packages("tmap")
install.packages("sp")
install.packages("sf")
install.packages("ggmap")
install.packages("RColorBrewer")
install.packages("classInt")
install.packages("tidyr")


library("tmap")
library("sp")
library("sf")
library("ggmap")
library("RColorBrewer")
library("classInt")
library("tidyr")
library("ggplot2")
library("dplyr")
library("stringr")


raw_df <- read.csv("C:/Users/adamm/Desktop/University/Masters/R code/Fulmers/Fulmer.csv",header = TRUE)
str(raw_df)
raw_df$bird_no <- as.character(raw_df$bird_no)
raw_df$twl_type <- as.character(raw_df$twl_type)
summary(raw_df)
str(raw_df)

fulmer_df <- raw_df[!is.na(raw_df$"lon_smooth2"),]


#remove values within the breeding season (keep september 15th - march 31st)

# transform the dates into julian day form

#create a usable date variable
fulmer_df <- fulmer_df %>% 
  separate(date_time, c("date", "time"), " ")


# use the same functions below to filter out breeding seasons days
twl_1 <- fulmer_df %>% filter(twl_type == "1")
twl_2 <- fulmer_df %>% filter(twl_type == "2")

#twl_type:
# 1 = day
# 2 = night



# make a base table, by merging twl type 1 & 2, this will be the data frame used for calculating indexes and average distances


# add the columns

calc_df <- inner_join(twl_1, twl_2, by = c("bird_no", "date"))

# x = 1 =  day
# y = 2 = night

# delete redundant columns for now (for future, show as "deleted by name")
calc_df <- calc_df[,-c()]







# average distance

calc_df <- mutate(calc_df, (lon_smooth2.x + lon_smooth2.y)/2)
calc_df <- mutate(calc_df, (lat_smooth2.x + lat_smooth2.y)/2)

rename(calc_df, "lon_smooth2.x + lon_smooth2.y)/2 = average_x",
"lat_smooth2.x + lat_smooth2.y)/2 = average_y")


names(calc_df)[18] <- "average_x"
names(calc_df)[19] <- "average_y"


ggplot(data = calc_df, mapping = aes(average_x, average_y))



str(map_sf)
summary(calc_df)



map_sf <- st_as_sf(calc_df, coords = c("average_x", "average_y"))


ggplot(data = map_sf) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude")


st_crs(map_sf) <- 4326



#to save:
st_write(map_sf, "C:/Users/adamm/Desktop/University/Masters/R code/Fulmers", driver = "ESRI Shapefile")
# to force the save: 
st_write(map_sf, "C:/Users/adamm/Desktop/University/Masters/R code/Fulmers", driver = "ESRI Shapefile", delete_layer = TRUE)

map_sf <- st_read("C:/Users/adamm/Desktop/University/Masters/R code/Fulmers")

register_google(key = "AIzaSyBq3lUlY0ixJDGYGgTk6JJjofgnMROLQwo")# sort out "write= TRUE"

basemap_sea <- get_map(location=c(lon = -30, lat = 30),zoom = 3, maptype = 'terrain-background', source = 'stamen')

str(map_sf)

ggmap(basemap_sea) +
  geom_sf(data = map_sf,aes(), inherit.aes = FALSE) +
  coord_sf(crs = st_crs(4326))
