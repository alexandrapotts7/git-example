#Black backed jackal assessment

wants <-c("adehabitatHR", "adehabitatHS", "lubridate", "here", "rgeos","dplyr", "ggplot2", "ggmap", "sf", "scales", "rgdal")

has   <- wants%in% rownames(installed.packages())
if(any(!has))install.packages(wants[!has])

library(adehabitatHR)
library(adehabitatHS)
library(lubridate)
library(here)
library(ggplot2)
library(dplyr)
library(ggmap)
library(sf)
library(scales)
library(rgdal)

install.packages("maptools")
library(maptools)


fulldata_jackal_gps<-read.csv(here::here("Data", "black-backed-jackal-Namibia.csv"))

#looking at the full dataset for the 22 individuals
head(fulldata_jackal_gps)

str(fulldata_jackal_gps)

fulldata_jackal_gps <- fulldata_jackal_gps %>%
  transform(timestamp = dmy_hm(timestamp))

#check how many fixes - i assume this is how many individuals/ photos for each one

fixes<- table(fulldata_jackal_gps$local_identifier)
fixes

#useful to see this on a map

#preparing the map for output later by set the boundary
jackals_bbox <- make_bbox(lon = location_long, lat = location_lat, data = fulldata_jackal_gps, f = 0.1)

#collect map background
jackals_map <- ggmap(get_stamenmap(jackals_bbox), zoom = 11)

#plot the map
plot(jackals_map)+
  geom_point(data=fulldata_jackal_gps, aes(x=location_long, y=location_lat)) +
  ggtitle("The Jackals")


#we can colour code the animals

plot(jackals_map) + geom_point(data=fulldata_jackal_gps, aes(x=location_long, y=location_lat,
                                                color=local_identifier), alpha = 0.2)


#changing the data frame into a spatial points dataframe - assigning spatial coordinates

jackal.sp<- fulldata_jackal_gps%>%
  dplyr::select(location_lat, location_long,local_identifier)%>%
  as.data.frame()

coordinates(jackal.sp)<- ~location_long+location_lat

#r now knows jackal.sp is a spatial object
str(jackal.sp)


#then we assign the correct coordinate system

# this is the coordinate reference system for lat long data
crs.ll <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

#the proj4string function allows us to assign this to the spatial points data frome.
proj4string(jackal.sp) <- crs.ll


#convex polygon method

#We wish to estimate the Minimum Convex Polygon (MCP) for all the animals, which is a measure of the smallest area in which the animals were recorded, or a measure of the home range

jackal.sp_m<- spTransform(jackal.sp, CRS("+init=epsg:29375"))

#enter the data in metres and have the output in hectares

jackal_mcp <- mcp(jackal.sp_m, percent=100, unin="m", unout = "h")

#we can look at home range of each jackal
jackal_mcp.data<- as.data.frame(jackal_mcp)

jackal_mcp.data

#This gives the home range area (mcp is in hectares) for each animal (id). 1000 hectares is one square kilometre.

#lets also add number of fixes to this data
jackal_mcp.data$relocs<- fixes

#calculate mean MCP of all the jackals
mean(jackal_mcp.data$area)

#standard deviation
sd(jackal_mcp.data$area)

#min value
min(jackal_mcp.data$area)

#max value which is useful with a large dataset
max(jackal_mcp.data$area)

#graphically presented as a boxplot
boxplot(jackal_mcp.data$area)

#The boxplot tells us that the home ranges vary in size between individual jackals and that there are two unusually large home ranges 

ggplot(jackal_mcp.data, aes(area, relocs))+geom_point()



#plot the home ranges
plot(jackal_mcp)

plot(jackal.sp_m, col = as.numeric(jackal.sp_m$local_identifier), pch = 16)
plot(jackal_mcp, col = alpha(1:22, 0.3), add = TRUE)

jackal_mcp.sf<-st_as_sf(jackal_mcp)
jackal_mcpgeo <- spTransform(jackal_mcp, CRS("+proj=longlat"))

plot(jackals_map) +
  geom_polygon(data = fortify(jackal_mcpgeo,region = "id"),
               aes(long, lat, colour = id, fill = id, group=group),alpha = 0.3)+
  geom_point(data=fulldata_jackal_gps, aes(x=location_long, y=location_lat,
                              color=local_identifier), alpha = 0.2)

#The minimum convex polygon (mcp) shows the minimum area that the jackal was recorded. This may over-estimate the true range of the animal, for example it may only spend a small proportion of its time at some points ( e.g. as it travels through an area), we should take this into account when considering a true measure of home range.


#kernel estimation
#This method takes account of the distribution of relocation points in spcae, as a measure of how much time an animal spends, and applies a weighting, as a way to reduce the overestimation of space use inherent with the mcp method.

ud <- kernelUD(jackal.sp, grid = 100)
ver95 <- getverticeshr(ud, 95)

#plot the data
plot(ver95, col=rainbow(10))

ver75 <- getverticeshr(ud, 75)
plot(ver75, add =TRUE, col=rainbow(10))

ver50 <- getverticeshr(ud, 50)
plot(ver50, add = TRUE, col=rainbow(10))

#extract the estimates for kernel
hruds<-kernel.area(ud, percent=seq(50, 95, by=5))
hruds

plot(jackals_map) + geom_polygon(data=fortify(ver75, add = TRUE, col=rainbow(10), region = "id"), aes(long, lat, colour = id, fill = id, group=group), alpha = 0.3)


#trajectory analysis
jackal.spdf<- fulldata_jackal_gps %>%
  dplyr::distinct(local_identifier, timestamp, .keep_all = TRUE)%>%
  as.data.frame()
coordinates(jackal.spdf)<- ~location_long+location_lat


proj4string(jackal.spdf) <- crs.ll
jackal.sp_m<- spTransform(jackal.spdf, CRS("+init=epsg:29375"))

jackal.ltraj <- as.ltraj(coordinates(jackal.sp_m),
                         date = jackal.sp_m$timestamp, id =jackal.sp_m$local_identifier)


#shows summary information for each of the animals trajectories (referred to as a burst)
jackal.ltraj

#plot all 22 trajectories
plot(jackal.ltraj)

total.path.df <- data.frame(jackal.ltraj[[1]], id = attr(jackal.ltraj[[1]], "id"))

for(i in 2:length(jackal.ltraj)) {
  total.path.df <- rbind(total.path.df,
                         data.frame(jackal.ltraj[[i]], id = attr(jackal.ltraj[[i]], "id")))
}


#calculate the mean distance travelled per day (in metres) and the total distance travelled by each animal

jackal.path<-total.path.df %>%
  group_by(id)%>%
  summarise (path.total= sum(dist, na.rm=TRUE)/1000,
             time.total=sum(dt,na.rm=TRUE))%>%
  mutate (dailydist= path.total / (time.total/60/60/24)) %>%
  data.frame

jackal.path

#merge this dataframe with our data on the MCP and number of fixes/relocations.

jackal.data<- left_join(jackal_mcp.data, jackal.path, by="id")

#visualise the data to see if there is a relationship between a total home range area and distance travelled.


dist.area.plot <- ggplot(data = jackal.data, aes(x = area/1000 , y = path.total,
                                                 colour = id)) + geom_point(size = 3) + labs(x = "MCP area (km2)", y = "Total distance travelled (km)" ) + theme_classic() 

dist.area.plot

plot(jackals_map) +geom_polygon(data=fortify(ver75, add = TRUE, col=rainbow(10), region="id"), aes(long, lat, colour = id, fill = id, group=group), alpha = 0.3)












