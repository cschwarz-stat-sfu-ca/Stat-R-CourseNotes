# R code for spatial data processing


library(ggplot2)
library(sf)
library(spData)

# Look at the world dataset
library(spData)
head(world)
str(world)


plot1 <- ggplot()+
   geom_sf(data=world, aes(fill=lifeExp))+
   ggtitle("Life Expectancy")
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","Spatial","world-001.png"), h=4, w=6, units="in",dpi=300)

# select only Africa
africa <- world[ world$continent=="Africa",]
plot2 <- ggplot()+
   geom_sf(data=africa, aes(fill=lifeExp))+
   ggtitle("Life Expectancy in Africa")
plot2
ggsave(plot2, 
       file=file.path("..","..","MyStuff","Images","Spatial","world-002.png"), h=4, w=6, units="in",dpi=300)


plot3 <- ggplot()+
   geom_sf(data=africa, aes(fill=lifeExp))+
   ggtitle("Life Expectancy in part of Africa")+
   xlim(0,40)+ylim(0,40)
plot3
ggsave(plot3, 
       file=file.path("..","..","MyStuff","Images","Spatial","world-003.png"), h=4, w=6, units="in",dpi=300)


# Create your own spatial features
# First location of SFU
SFU.sf <- sf::st_point(  c(-122.917957, 49.276765 ))
str(SFU.sf)

my.drive.csv <- textConnection("
long, lat
-122.84378900000002, 49.29009199999999
-122.82799615332033, 49.28426960031931
-122.82696618505861, 49.27755059244836
-122.86679162451173, 49.27676664856581
-122.88790597387697, 49.26276555269492
-122.90833367773439, 49.26534205263451
-122.92532815405275, 49.273518748310764
-122.91434182592775, 49.27766258341439")
my.drive <- read.csv(my.drive.csv, header=TRUE, as.is=TRUE, strip.white=TRUE)

my.drive.sf <- sf::st_linestring(as.matrix(my.drive[, c("long","lat")]))
str(my.drive.sf)

plot1 <- ggplot() +
         ggtitle("My drive to work")+
         geom_sf(data=SFU.sf, color="red", size=4)+
         geom_sf(data=my.drive.sf, color="black", size=2, inherits.aes=FALSE)+
         ylab("Latitude")+xlab("Longitude")
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","Spatial","my-drive-001.png"),
       h=4, w=6, units="in", dpi=300)



### Adding a background map
library(ggmap)
sfu.coord <- c(-122.917957, 49.276765 )
# get the map from stamen. You can fiddle with the zoom to get the right scale
my.commute.map.dl <- ggmap::get_map(c(left=sfu.coord[1]-.02, bottom=sfu.coord[2]-.02, right=sfu.coord[1]+.12, top=sfu.coord[2]+.03), 
               maptype="watercolor",  source="stamen")
my.commute.map <- ggmap(my.commute.map.dl)

# careful, ggmap uses lon/lat but sf uses long/lat 
# you need to ignore the aee from the ggmap by using inherit.aes=FALSE
plot1 <- my.commute.map +
         ggtitle("My drive to work")+
         geom_sf(data=SFU.sf, color="red", size=4, inherit.aes=FALSE)+
         geom_sf(data=my.drive.sf, color="black", size=2, inherit.aes=FALSE)+
         ylab("Latitude")+xlab("Longitude")
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","Spatial","my-drive-002.png"),
       h=4, w=6, units="in", dpi=300)

library(leaflet)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=sfu.coord[1], lat=sfu.coord[2], popup="SFU") %>%
  addPolylines(lng=my.drive$long, lat=my.drive$lat)
m  # P

#################
#  Exercise I
# Plot the mean location of accidents over time for fatal accidents
# Read in the accident data and get the date and fatality variables set

accidents <- read.csv(file.path("..","sampledata","Accidents","road-accidents-2010.csv"), header=TRUE,
             as.is=TRUE, strip.white=TRUE)
# Convert date to internal date format
accidents$mydate <- as.Date(accidents$Date, format="%d/%m/%Y")
# Create the fatality variable
accidents$Fatality <- accidents$Accident_Severity == 1
accidents[1:5,]

accidents$month <- lubridate::month(accidents$mydate)

# Find the mean location by month
library(dplyr)
mean.fatal.location <- 
  accidents %>%
    filter( Fatality==1) %>%
      group_by(month) %>%
        summarize( mean.long=mean(Longitude), mean.lat=mean(Latitude))
mean.fatal.location  

mean.fatal.location.path.sf <- sf::st_linestring( as.matrix(mean.fatal.location[,c("mean.long","mean.lat")]))

plot0 <- ggplot()+
  geom_sf(data=mean.fatal.location.sf.path, color="red")+
  geom_text(data=mean.fatal.location, aes(x=mean.long, y=mean.lat, label=month))
plot0
ggsave(plot0, 
       file=file.path("..","..","MyStuff","Images","Spatial","mean-accident-location-000.png"),
       h=4, w=6, units="in", dpi=300)

# notice that is is SILLY to compute the mean lat/long, but
# for simplicity we will do this here.

mean.lat <- mean(accidents$Latitude)
mean.long<- mean(accidents$Longitude)
my.map.dl <- ggmap::get_map(c(left  =min(accidents$Longitude), bottom=min(accidents$Latitude), 
                              right =max(accidents$Longitude), top   =max(accidents$Latitude)), 
                             maptype="watercolor",  source="stamen", zoom=6)

my.map <- ggmap(my.map.dl)


plot1 <- my.map +
         ggtitle("Mean location of fatal accidents by month")+
         geom_sf(data=mean.fatal.location.path.sf, color="red", inherit.aes=FALSE)+
         geom_text(data=mean.fatal.location, aes(x=mean.long, y=mean.lat, label=month))+
         ylab("Latitude")+xlab("Longitude")
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","Spatial","mean-accident-location-001.png"),
       h=4, w=6, units="in", dpi=300)

library(leaflet)
m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addLabelOnlyMarkers(lng=mean.fatal.location$mean.long, 
             lat=mean.fatal.location$mean.lat, 
             label=as.character(mean.fatal.location$month), 
                 labelOptions = labelOptions(noHide = T, textOnly=T,
                 style=list("font-size"="25pt"))) %>%
  addPolylines(lng=mean.fatal.location$mean.long, lat=mean.fatal.location$mean.lat)
m  # P


##################
# What is the CRS/

library(spData)
head(world)
str(world)

sf::st_crs(world)


luxembourg = world[world$name_long == "Luxembourg", ]
st_area(luxembourg)

# careful about setting units
# right number but wrong units
st_area(luxembourg) / 1000000
#> 2414 [m^2]

# right number with right units
units::set_units(st_area(luxembourg), km^2)
#> 2414 [km^2]


# Setting and converting CRS
st_crs(my.commute.map)
st_crs(my.drive.sf)
st_crs(4326)

my.drive.sf2 <- st_sf(geometry=st_sfc(my.drive.sf, crs=4326))
st_crs(my.drive.sf2)
st_length(my.drive.sf2)
st_length(my.drive.sf)

######################################################
############################################################################################################
######################################################
# Attribute operations
names(world)

my.world <- world
my.world$pop.density <- my.world$pop / my.world$area_km2

plot1 <- ggplot()+
   geom_sf(data=my.world, aes(fill=pop.density))+
   ggtitle("Population density")+
   scale_fill_gradient(na.value="white", trans="reverse")
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","Spatial","world-pop-density.png"), h=4, w=6, units="in",dpi=300)


library(spData)
names(coffee_data)

setdiff(my.world$name_long, coffee_data$name_long)
setdiff(coffee_data$name_long, my.world$name_long)

coffee_data$name_long[ coffee_data$name_long=="Congo, Dem. Rep. of"] <- "Democratic Republic of the Congo"
setdiff(coffee_data$name_long, my.world$name_long)

my.world2 <- merge(my.world, coffee_data, all.x=TRUE)
plot1 <- ggplot()+
   geom_sf(data=my.world2, aes(fill=coffee_production_2016))+
   scale_fill_gradient(trans="reverse", na.value="white")+
   ggtitle("Coffee production 2016")
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","Spatial","world-coffee-2016.png"), h=4, w=6, units="in",dpi=300)

# Show what happens if forget to use all.x=TRUE
my.world3 <- merge(my.world, coffee_data)
plot1 <- ggplot()+
   geom_sf(data=my.world3, aes(fill=coffee_production_2016))+
   ggtitle("Coffee production 2016")
plot1


#########################################################
#  Aggregate populations to continent and compute density of resulting continents

names(world)

cont <- 
  world %>%
    group_by(continent) %>%
      summarize(
         total.pop =sum(pop,na.rm=TRUE),
         total.area=sum(area_km2, na.rm=TRUE),
         density = total.pop / total.area)
str(cont)
cont

plot1 <- ggplot()+
  geom_sf(data=cont, aes(fill=continent))
  scale_fill_gradient(trans="reverse", na.value="white")
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","Spatial","world-cont.png"), h=4, w=6, units="in",dpi=300)

plot1 <- ggplot()+
  geom_sf(data=cont, aes(fill=density))+
  scale_fill_gradient(trans="reverse", na.value="white")
plot1
ggsave(plot1, 
       file=file.path("..","..","MyStuff","Images","Spatial","world-cont-density.png"), h=4, w=6, units="in",dpi=300)



#################################################################################
#################################################################################
#################################################################################
#################################################################################
# Look at proportion never married from the 2016 census at the FSA level

library(ggplot2)
library(sf)


# Get the FSA 
# We unzip the compressed file to a temporary directory, and then point to the shape file
fsa.dir <- tempdir()
unzip(file.path("..","sampledata","2016-census","FSA","lfsa000b16a_e.zip"),  exdir=fsa.dir)
fsa <- sf::st_read(file.path(fsa.dir,"lfsa000b16a_e.shp"), stringsAsFactors=FALSE)
head(fsa)

# Extract only bc in the lower mainland 
xtabs(~PRNAME, data=fsa, exclude=NULL, na.action=na.pass)
fsa <- fsa[ substr(fsa$CFSAUID,1,2) %in% c("V1","V2","V3","V4","V5","V6","V7"),]

st_is_valid(fsa)

fsa <- sf::st_simplify(fsa, dTolerance=200)
st_is_valid(fsa)


plot1 <- ggplot()+
  geom_sf(data=fsa, aes(fill="red"))
plot1
ggsave(plot1,
       file=file.path("..","..","MyStuff","Images","Spatial","p-not-married-001.png"),
       h=6, w=6, units="in",dpi=300)

# Use a bounding box to select the polygons of interest
my.bbox <- data.frame( long=c(-122, -122, -124, -124, -122),
                       lat =c(  49,  49.5, 49.5,  49,   49))
my.bbox.sf <- st_sfc(st_polygon(list(as.matrix(my.bbox[,c("long","lat")]))), crs=4326)

plot2 <- ggplot()+
  geom_sf(data=my.bbox.sf, color="blue", size=4, aes(fill=NULL))+
  geom_sf(data=fsa, aes(fill=NULL))

plot2
ggsave(plot2,
       file=file.path("..","..","MyStuff","Images","Spatial","p-not-married-002.png"),
       h=6, w=6, units="in",dpi=300)


# Truncate the dataset to only use fsa within my bounding box
# change the CRS to UTM
my.bbox.sf <- sf::st_transform(my.bbox.sf, crs="+proj=utm +zone=10 ellps=WGS84")
fsa        <- sf::st_transform(fsa,        crs="+proj=utm +zone=10 ellps=WGS84")

fsa <- sf::st_intersection(fsa, my.bbox.sf)
st_is_valid(fsa)

plot3 <- ggplot()+
  geom_sf(data=fsa,aes(fill="red"))
plot3
ggsave(plot3,
       file=file.path("..","..","MyStuff","Images","Spatial","p-not-married-003.png"),
       h=6, w=6, units="in",dpi=300)




# Get in the distribution of marital status and select only those in the selected FSA
m.status <- read.csv(file.path("..","sampledata","2016-census","MaritalStatus","98-400-X2016039_English_CSV_data.csv"), header=TRUE, as.is=TRUE)
m.status <- m.status[ m.status$GEO_NAME %in% fsa$CFSAUID,]
m.status <- m.status[ m.status$DIM..Sex..3. == 'Total - Sex',]
names(m.status)

# get the proportion who are "Not Married and not common law
library(dplyr)
p.not.married <- 
  m.status %>%
    group_by(GEO_NAME) %>%
      do(
        (function(x){
           #browser()
           p.not.married = x$Dim..Age..16...Member.ID...1...Total...Age[x$Member.ID..Marital.status..13.==9] /
                           x$Dim..Age..16...Member.ID...1...Total...Age[x$Member.ID..Marital.status..13.==1]
           data.frame(p.not.married)
          })(.)
      )
head(p.not.married)

# add p.not.married to the fsa
# are there any code missing or extra
setdiff(fsa$CFSAUID, p.not.married$GEO_NAME)
setdiff(p.not.married$GEO_NAME, fsa$CFSAUID)

fsa <- merge(fsa, p.not.married, by.x="CFSAUID", by.y="GEO_NAME", all.x=TRUE)

final.plot <- ggplot()+
  ggtitle("Proportion not married")+
  geom_sf(data=fsa,aes(fill=p.not.married), color=NA)+
  scale_fill_gradient2(low='darkred', mid="white", high='darkblue', midpoint=0.43)

final.plot
ggsave(final.plot,
       file=file.path("..","..","MyStuff","Images","Spatial","p-not-married-final.png"),
       h=4, w=6, units="in",dpi=300)


# Put this into leaflet

library(leaflet)

### Create five colors for fill
mypal <- colorQuantile(palette = "RdYlBu", 
                       domain = fsa$p.not.married, 
                       n = 5, reverse = TRUE)

# we need to transform the FSA inot WGS84
fsa2 <- st_transform(fsa, '+proj=longlat +datum=WGS84')

# Feed the fsa dataset with proper projection into leaflet
m <-fsa2 %>% 
     leaflet() %>%
      addTiles() %>%
      addPolygons(data = fsa2,
            stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.6,
            fillColor = ~mypal(fsa2$p.not.married)) %>%
      addLegend(position = "bottomright", pal = mypal, fsa$p.not.married,
          title = "P(not married)",
          opacity = 1)
m  # display the map 


library(mapview)

## 'leaflet' objects (image above)
mapview::mapshot(m, 
   file = paste0(normalizePath(file.path("..","..","MyStuff","Images","Spatial")),"/p-not-married-final-leaflet.png"))

