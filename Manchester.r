# Load the required libraries

library(rgdal)
library(ggplot2)
library(wesanderson)
library(RColorBrewer)
library(rgeos)

# Change to the required directory

setwd("C:/Users/Kaustubh/Desktop/Neogeography_Lab")

# Load the datasets

lcp_dir = 'epa1315_19_data/Manchester/Manchester/'
los_dir = 'epa1315_19_data/Manchester/E08000003/'
lsoas_link = paste(lcp_dir, "shapefiles/Manchester_lsoa11.shp", sep = "")
lsoas = readOGR(lsoas_link)

# plot the shapefile
plot(lsoas, col = rgb(0, 0.2, 0.4, alpha = 0.3), main = "Manchester Polygons",axes = FALSE)

# Plot with x and y as longitude and latitude
ggplot() +
  geom_path(data = lsoas, aes(x = long, y = lat, group = group)) +
  coord_fixed() +
  labs(title = "GGPLOT map of Manchester",
       subtitle = "A city with rich diversity",
       x = "", y = "") 
theme_void()


proj4string(lsoas)

#Load railwayTrack shapefile and plot it

los_dir_rwy = paste(los_dir, "RailwayTrack.shp", sep = "")
rwy_tun = readOGR(los_dir_rwy)

plot(rwy_tun)
proj4string(rwy_tun)

# Plot normal city map with colored boundaries for counties
ggl = ggplot(data = lsoas) +                                 # normal city map
  geom_path(aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color =rgb(0.9, 0, 0.3, alpha = 0.4)) + # to make background white and show county boundaries in color!
  coord_fixed() +
  labs(title = "GGPLOT map of Manchester",
       subtitle = "A city with rich diversity",
       x = "", y = "") 

theme_void()
plot(ggl)
# save it as a png
ggsave("Manchester_white_redlines_county.png")

# add railway tracks on top of the map and save it
ggl +
  geom_polygon(data = rwy_tun, aes(x = long, y = lat, group = group), color = "black")        # added railway Track on top! 
theme_void()
ggsave("Manchester_railwaytrackontop_all.png")

#Load different shapes for the city of manchester#

impB = readOGR(paste(los_dir, "Importantbuilding.shp", sep = ""))
woodL = readOGR(paste(los_dir, 'Woodland.shp', sep = ""))
surW = readOGR(paste(los_dir, 'SurfaceWater_Area.shp', sep = ""))
raiT = readOGR(paste(los_dir, 'RailwayTrack.shp', sep = ""))


ggplot() +
  geom_polygon(data = lsoas, aes(x = long, y = lat, group = group), fill =
                 NA, color = "black", size = 0,alpha=0.1) +
  geom_polygon(data = impB, aes(x = long, y = lat, group = group), fill =
                 "red", size = 0.8,alpha=0.8) +
  
  geom_polygon(data = woodL, aes(x = long, y = lat, group = group), color =
                 "green",alpha=0.5) +
   geom_polygon(data = raiT, aes(x = long, y = lat, group = group), color =
                   "yellow", size = 0.1,alpha=1) +
  
  geom_polygon(data = surW, aes(x = long, y = lat, group = group), color =
                 "blue", size = 0.1,alpha =0.3) +
  # Impose same size for units across axes
  coord_fixed() +
  
  labs(title = "Manchester woodland,waterways,Important Buildings and railway tracks") + 
  theme_void()


###################### Save to files###############
ggsave("Manchester_polygons_all.png")                       # save image as png format
# Save it as a PDF file
ggsave("Manchester_polygons_all.pdf")

  # fortify transforms this list of polygons into a data.frame
  lsoas_df1 = fortify(lsoas, region = "LSOA11CD")     # as this contains the common entries     
  head(lsoas_df1)
  
  tab_path = 'epa1315_19_data/Manchester/Manchester/'            # set the path
  
  tab_path_file1 = paste(tab_path, "manchester_pop_religion.csv", sep = "")      # set path to csv
  lsoa_orig_sub1 = read.csv(tab_path_file1, header = TRUE, sep = ",") # read csv
  
  tab_path_file2 = paste(tab_path, "manchester_pop_occupation.csv", sep = "")      # set path to csv
  lsoa_orig_sub2 = read.csv(tab_path_file2, header = TRUE, sep = ",") # read csv
  
  head(lsoa_orig_sub1)
  head(lsoa_orig_sub2)
  
  lsoa_orig_sub1$Total = rowSums(lsoa_orig_sub1[,-1])
  
  head(lsoas@data)   # column name =LSOA11CD
  
  # merge data
  merged_Data1 = merge(lsoas_df1, lsoa_orig_sub1, by.x = "id", by.y ="GeographyCode") # merge polygons, csv, 
  merged_Data2 = merge(lsoas_df1, lsoa_orig_sub2, by.x = "id", by.y ="GeographyCode") # merge polygons, csv, 


  # notice the names have been appended

head(merged_Data1)
head(merged_Data2)

# add a column which has the most popular religion and occupation for a county in the merged data
merged_Data1[, "Popular_religion"] <- colnames(merged_Data1[,8:16])[apply(merged_Data1[,8:16],1,which.max)]
merged_Data2[, "Popular_occupation"] <- colnames(merged_Data2[,8:16])[apply(merged_Data2[,8:16],1,which.max)]

#choropleths

# coropleth with popular religion per county

ggplot() +
  # Add surface water
  #geom_polygon(data = surW, aes(x = long, y = lat, group = group), fill ="#618A98", size = 0) +
  # Add woodland
  #geom_polygon(data = impB, aes(x = long, y = lat, group = group), color ="black") +
  # Add LSOAs but merged with population this time
  geom_polygon(data = merged_Data1, aes(x = long, y = lat, group = group, fill =Popular_religion), color = "white", size = 0.25) +
  # Impose same size for units across axes
  coord_fixed() +
  # Add your titles
  labs(title = "Most practiced religion per county in the city of Manchester",
       
       x = "", y = "") + 
  theme_void()
ggsave("Manchester_religion.png")

ggplot() +
  # Add surface water
  #geom_polygon(data = surW, aes(x = long, y = lat, group = group), fill ="#618A98", size = 0) +
  # Add woodland
  #geom_polygon(data = impB, aes(x = long, y = lat, group = group), color ="black") +
  # Add LSOAs but merged with population this time
  geom_polygon(data = merged_Data2, aes(x = long, y = lat, group = group, fill =Popular_occupation), color = "white", size = 0.25) +
  # Impose same size for units across axes
  coord_fixed() +
  # Add your titles
  labs(title = "Disribution of prevalent occupation per county in the city of Manchester",
      
       x = "", y = "") + 
  theme_void()
ggsave("Manchester_occupation.png")

cent = gCentroid(lsoas, byid=TRUE)

plot(cent)

los_dir_namp = paste(los_dir, "NamedPlace.shp", sep = "")
namp = readOGR(los_dir_namp)


head(impB@data)
proj4string(namp)

buf = gBuffer(namp, width = 450, byid = TRUE)
head(buf@data)
namp_df = data.frame(namp)
head(namp_df)



ggplot() +
  # Add wood land shown in green 
  geom_polygon(data = woodL, aes(x = long, y = lat, group = group), fill ="green", size = 0) +
  # Add surface water shown in blue
  geom_polygon(data = surW, aes(x = long, y = lat, group = group), color ="blue") +
  # Add LSOAs but merged with population this time
  geom_polygon(data = buf, aes(x = long, y = lat, group = group), fill ="#F9DA95", color = "white", size = 0.25, alpha = 0.4) +
  geom_point(data = namp_df, aes(x = coords.x1, y = coords.x2, fill =classifica), color = "black", size = 0.1) +
  # Impose same size for units across axes
  coord_fixed() +
  # Add your titles
  labs(title = "Woodland,surfaceWater corresponding to population of the city of Manchester",
       x = "", y = "") + 
  theme_void()
