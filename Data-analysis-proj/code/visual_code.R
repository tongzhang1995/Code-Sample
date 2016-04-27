# =============================================================================
# Stat 133: Final Project
# Part 3 Data Visualization
# Description: work with two csv files to produce images of the trajectory 
# of storms from 1980 to 2010 in both East Pacific and North Atlantic

# Data: International Best Track Archive for Climate Stewardship (IBTrACS)
# ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/
# Basin.EP.ibtracs_wmo.v03r06.csv
# ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/
# Basin.NA.ibtracs_wmo.v03r06.csv

# =============================================================================

# Name:Tong Zhang 24540972, Zhiqiang Liao 24229382

# =============================================================================
# Load packages
# =============================================================================
library(dplyr)
library(maps)
library(ggplot2)
library(stringr)

# =============================================================================
# Loading
# =============================================================================
EastP<-read.csv('rawdata/EastP.csv',skip = 1, header = TRUE)
NorthA<-read.csv('rawdata/NorthA.csv', skip=1, header=TRUE)


# =============================================================================
# Preprocessing Data
# =============================================================================

cleanEP<-EastP[(as.character(EastP$Season)>='1980' & 
                as.character(EastP$Season)<= '2010'),]
cleanNA<-NorthA[(as.character(NorthA$Season)>='1980' & 
                as.character(NorthA$Season)<= '2010'),]


# =============================================================================
# Change the format in each column
# =============================================================================
cleanEP$Latitude<-as.numeric(str_trim(as.character(cleanEP$Latitude), side='left'))
cleanEP$Longitude<-as.numeric(str_trim(as.character(cleanEP$Longitude), side='left'))
cleanEP$Wind.WMO.<-as.numeric(str_trim(as.character(cleanEP$Wind.WMO.), side='left'))


# =============================================================================
# Change the format in each column in North America Basin
# =============================================================================
cleanNA$Latitude<-as.numeric(str_trim(as.character(cleanNA$Latitude), side='left'))
cleanNA$Longitude<-as.numeric(str_trim(as.character(cleanNA$Longitude), side='left'))
cleanNA$Wind.WMO.<-as.numeric(str_trim(as.character(cleanNA$Wind.WMO.), side='left'))

# =============================================================================
# Combine the two data frame
# =============================================================================
clean<- full_join(cleanEP, cleanNA)

# =============================================================================
# Change Serial_Num column to factor
# =============================================================================
clean$Serial_Num<-as.factor(clean$Serial_Num)

# =============================================================================
# Extract Month
# =============================================================================
clean$Month<-str_replace_all(str_extract(as.character(clean$ISO_time),
            '-[0-9]{2}-'),'-','')
clean$Month<-factor(clean$Month, labels=month.name)

# =============================================================================
# gather world map data from maps package
# =============================================================================
mapData<-map_data('world')

# =============================================================================
# Visualization 1: Trajectory of storms from 1980 to 2010
# =============================================================================

globalMap<- ggplot(clean, aes(x=Longitude, y=Latitude, group=Serial_Num)) +
  geom_path(data=clean, aes(group=Serial_Num, colour=Wind.WMO.), alpha=0.5,size=0.5) +
  geom_polygon(data=mapData, aes(x=long, y=lat, group=group),
               fill='#000050', colour='#000050', size=0.5) +
  ggtitle('Trajectory of storms from 1980 to 2010')+
  scale_color_continuous(name='Wind (knot)',low = '#E68A00',high = '#4C2E00',
                         space = 'Lab',guide = 'colourbar')+
  xlim(-150, -25) + ylim(5, 60)+
  theme(panel.background = element_rect(fill='#00006BDD',colour='#00006BDD'),
        panel.grid = element_blank(),
        panel.border = element_blank())
globalMap

pdf("images/globalMap.pdf") # open device
globalMap # plot something 
dev.off() # close device

png("images/globalMap.png", width = 1024, height = 900, res= 120) # open device
globalMap # plot something 
dev.off() # close device
# =============================================================================
# Visualization 2 : Trajectory of storms per month from 1980 to 2010
# (one facet per month)
# =============================================================================

stormByMonth <- ggplot(clean, aes(x=Longitude, y=Latitude, group=Serial_Num)) +
  geom_path(data=clean, aes(group=Serial_Num, colour=Wind.WMO.), alpha=0.2,size=0.5) +
  geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group),
               fill='#000050', colour='#000050', size=0.5) +
  ggtitle('Trajectory of storms per month from 1980 to 2010')+
  scale_color_continuous(name='Wind (knot)',low = '#E68A00',high = '#4C2E00',
                         space = 'Lab',guide = 'colourbar')+
  xlim(-150, -25) + ylim(5, 60)+facet_wrap(~ Month)+
  theme(panel.background = element_rect(fill='#00006BDD',colour='#00006BDD'),
        panel.grid = element_blank(),
        panel.border = element_blank())

stormByMonth

pdf("images/stormByMonth.pdf") # open device
stormByMonth # plot something 
dev.off() # close device

png("images/stormByMonth.png", width = 1024, height = 900, res= 120) # open device
stormByMonth # plot something 
dev.off() # close device

# =============================================================================
# Visualization 3: trajectory of storms in decade 1980s 
# (one facet per year)
# =============================================================================

# extract storms from 1980 to 1989
traj1980<-clean[as.character(clean$Season)>='1980' & 
                as.character(clean$Season)<='1989',]

# As. factor Season column in the data frame
traj1980$Season<-as.factor(traj1980$Season)

# plot map
map1980 <- ggplot(traj1980, aes(x=Longitude, y=Latitude, group=Serial_Num)) +
  geom_path(data=traj1980, aes(group=Serial_Num, colour=Wind.WMO.),alpha=0.2, size=0.5) +
  geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group),
               fill='#000050', colour='#000050', size=0.5) +
  ggtitle('Trajectory of storms per month from 1980 to 1989')+
  scale_color_continuous(name='Wind (knot)',low = '#E68A00',high = '#4C2E00',
                         space = 'Lab',guide = 'colourbar')+
  xlim(-150, -25) + ylim(5, 60)+facet_wrap(~ Season)+
  theme(panel.background = element_rect(fill='#00006BDD',colour='#00006BDD'),
        panel.grid = element_blank(),
        panel.border = element_blank())

map1980

pdf("images/stormsIn1980s.pdf") # open device
map1980 # plot something 
dev.off() # close device


png("images/stormsIn1980s.png", width = 1024, height = 900, res= 120) # open device
map1980 # plot something 
dev.off() # close device

# =============================================================================
# Visualization 4: trajectory of storms in decade 1990s 
# (one facet per year)
# =============================================================================

# Extract storms from 1990 to 1999
traj1990<-clean[as.character(clean$Season)>='1990' & 
                as.character(clean$Season)<='1999',]

# As. factor Season column in the storm data frame
traj1990$Season<-as.factor(traj1990$Season)

# plot map
map1990 <- ggplot(traj1990, aes(x=Longitude, y=Latitude, group=Serial_Num)) +
  geom_path(data=traj1990, aes(group=Serial_Num, colour=Wind.WMO.), 
            alpha=0.2, size=0.5)+
  geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group),
               fill='#000050', colour='#000050', size=0.5) +
  ggtitle('Trajectory of storms per month from 1990 to 1999')+
  scale_color_continuous(name='Wind (knot)',low = '#E68A00',high = '#4C2E00',
                         space = 'Lab',guide = 'colourbar')+
  xlim(-150, -25) + ylim(5, 60)+facet_wrap(~ Season)+
  theme(panel.background = element_rect(fill='#00006BDD',colour='#00006BDD'),
        panel.grid = element_blank(),
        panel.border = element_blank())

map1990

pdf("images/stormsIn1990s.pdf") # open device
map1990 # plot something 
dev.off() # close device

png("images/stormsIn1990s.png", width = 1024, height = 900, res= 120) # open device
map1990 # plot something 
dev.off() # close device

# =============================================================================
# Visualization 5: trajectory of storms in decade 2000s 
# (one facet per year)
# =============================================================================

# Extract storms from 2000 to 2009
traj2000<-clean[as.character(clean$Season)>='2000' & 
                as.character(clean$Season)<='2009',]

# As. factor Season column in the storm data frame
traj2000$Season<-as.factor(traj2000$Season)

# plot map
map2000 <- ggplot(traj2000, aes(x=Longitude, y=Latitude, group=Serial_Num)) +
  geom_path(data=traj2000, aes(group=Serial_Num, colour=Wind.WMO.),
            alpha=0.2, size=0.5) +
  geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group),
               fill='#000050', colour='#000050', size=0.5) +
  ggtitle('Trajectory of storms per month from 2000 to 2009')+
  scale_color_continuous(name='Wind (knot)',low = '#E68A00',high = '#4C2E00',
                         space = 'Lab',guide = 'colourbar')+
  xlim(-150, -25) + ylim(5, 60)+facet_wrap(~ Season)+
  theme(panel.background = element_rect(fill='#00006BDD',colour='#00006BDD'),
        panel.grid = element_blank(),
        panel.border = element_blank())

map2000

pdf("images/stormsIn2000s.pdf") # open device
map2000 # plot something 
dev.off() # close device

png("images/stormsIn2000s.png", width = 1024, height = 900, res= 120) # open device
map2000 # plot something 
dev.off() # close device

