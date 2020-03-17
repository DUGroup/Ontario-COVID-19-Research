#install.packages("googlesheets4")
library(googlesheets4)
library(data.table)
library(dplyr)
library(lubridate)
library(janitor)
library(RODBC)
library(tidyr)
library(stringr)
library(ggplot2)
library(kableExtra)

library(rgeos)
library(maptools)
library(ggmap)
library(ggiraph)
library(plotly)

#read from google sheet
data <- read_sheet("https://docs.google.com/spreadsheets/d/1Y3_qYkTJK6Vnhw3RCOhOiwafm4-PQxd5l0Hxk4x1ZxE/edit?ts=5e6f7e06#gid=0", 
                   sheet = 1)

data$Date <- as.Date(now())
data$Cases <- as.numeric(data$Cases)

#bar plot, count by region
ggplot(data = data[data$Cases >= 0,], aes(x = reorder(Region, Cases), y = Cases, na.rm = T)) +
  geom_col() +
  geom_text(aes(label = Cases), hjust = -0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3)) + 
  labs(title = "Number of Confirmed (COVID-19) Cases by Ontario Region",
       x = "Region", y = "Cases") +  coord_flip()


ontario.data = data

#read in shape file (census divisions) from stats canada (https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2006-eng.cfm)
sp.Canada <- readShapeSpatial('C:/Users/mn209073/Documents/March Break/Coronavirus Analysis/gcd_000a06a_e/gcd_000a07a_e.shp', 
                              proj4string = CRS("+proj=longlat +datum=WGS84") )
slotNames(sp.Canada)
names(sp.Canada@data)
head(sp.Canada@data, 5)
sp.Ontario = sp.Canada[sp.Canada@data$PRNAME == "Ontario",]

sp.Ontario.df <- fortify(sp.Ontario, region ="CDNAME")

#join cases count to the shapefile by region name
sp.Ontario.COVID = left_join(x = sp.Ontario.df, y = ontario.data, by = c("id" = "Region"))

#sp.Ontario.COVID$breaks <- cut(sp.Ontario.COVID$Cases, 5)

table(sp.Ontario.COVID$Cases, exclude = NULL)

#plot map
p <- ggplot(data = sp.Ontario.COVID, aes(x=long, y=lat, group = group)) + 
  geom_polygon(aes(fill = Cases, group = id), color = 'black') + scale_fill_gradient(low = "yellow", high = "red")+
  labs(title = "(COVID-19) Cases by Ontario Region")

fig <- ggplotly(p)
fig
