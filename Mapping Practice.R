#SC 7/12/19
#practice with mapping!!

setwd("C:/Users/scardarelli/Documents/research/siberia_lai")

library(c(devtools, dplyr, ggmap, ggplot2, mapdata, maps, raster, sf, sp, stringr, usethis))
#don't think the above works but these are all the ones i have on rn at least

#let's start by making a map of the five largest cities in italy!
#get the data for the italy map like so
italy<-map_data("italy")

#now plot it
italy_map<-ggplot() + geom_polygon(data = italy, 
                            aes(x=long, y = lat, group = group), fill="ivory", 
                                  color="springgreen4") + coord_fixed(1.3)

#i googled the coordinates for the five largest cities and input them here!
points<-data.frame(long=c(12.4964,9.1900,14.2681,7.6869,13.3615), 
                   lat=c(41.9028,45.4642,40.8518,45.0703,38.1157),
                   names=c("Rome", "Milan","Naples","Turin", "Palermo"), 
                   stringAsFactors=FALSE)

#and now we plot the points!
italy_map+geom_point(data=points,aes(x=long,y=lat), color="red3", size=3)

#in the future: how to add a title? how to label the points? hm!

#now let's do the one in the tutorial site i found for population density in california
#the link is here: https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html

states<-map_data("state")

cali<-subset(states,region=="california")

#map of california
ca_base<-ggplot(data=cali, mapping=aes(x=long, y=lat, group=group))+
  coord_fixed(1.3)+
  geom_polygon(color="black", fill="grey")
ca_base+theme_nothing()

#adding in the counties
counties<-map_data("county")
ca_county<-subset(counties, region== "california")

ca_base + theme_nothing() + 
  geom_polygon(data=ca_county, fill=NA, color="white")+
  geom_polygon(color="black", fill=NA)

#now reading in population data
poparea<-read.csv("California Counties - Sheet1.csv")

#we have to combine the two data frames
countypop<-inner_join(ca_county,poparea,by="subregion")
countypop$Population<-as.numeric(countypop$Population)
countypop$area<-as.numeric(countypop$area)

#adding a column for people per mile to map by population density
countypop$'People Per Mile'<-countypop$Population/countypop$area

#making a new theme i guess. thanks website
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank())

calipop<-ca_base + 
  geom_polygon(data = countypop, aes(fill=`People Per Mile`), color="white")+
  geom_polygon(color="black", fill=NA)+
  theme_bw()+
  ditch_the_axes
  
calipop<-calipop+ scale_fill_gradientn(colours=rev(rainbow(7)),
                      trans="log10")
calipop

#not quite sure why there isn't a scale. a lil upset about that
