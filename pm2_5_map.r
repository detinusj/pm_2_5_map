library(tidyverse)
library(sf)
library(ggspatial)
library(jsonlite)
library(maps)

# get mexico, to be added to plot for completeness
world <- st_as_sf(map("world", plot = F,fill = T))
mx = world %>% filter(ID == 'Mexico')

# get California and some neighbours, to get nicely filled out plot
states <- st_as_sf(map("state", plot = F, fill = T))
cali = states %>% filter(ID %in% c('california','nevada','arizona','utah'))

# create the hex grid in the above defined states, having a size of 30km (~20 miles)
cali_grid = cali %>%
  st_transform(3857) %>%
  st_make_grid(cellsize = 30000, square=F) %>%
  st_transform(st_crs(cali)) %>%
  st_sf() %>%
  mutate(cell_id = row_number())

# get sensor data from Purple Air's JSON API
json_data = fromJSON("https://www.purpleair.com/json")

# write to file for replayability
# add a timestamp for uniqueness
ts = format(Sys.time(), "%Y-%m-%d-%H%M")
write_csv(json_data$results,paste('purple_air_',ts,'.csv',sep=''))

# read just written file (because we can)
# drop a few columns (we don't use all the ones we keep atm as well, future work)
# filter data to the plot area
# make it a sf object
data.sf = read_csv(paste('purple_air_',ts,'.csv',sep='')) %>%
  dplyr::select(
    id=ID, label=Label, location_type=DEVICE_LOCATIONTYPE,
    latitude=Lat, longitude=Lon, pm_25 = PM2_5Value,
    ts=LastSeen, type=Type, temp_f, humidity, pressure, age=AGE,
    stats=Stats
  ) %>%
  filter(
    latitude > 32, latitude < 42,
    longitude > -130, longitude < -113,
    !is.na(latitude), !is.na(longitude)
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"), 
    crs = st_crs(cali)
  )

# aggregate point data in the hex tiles
# calculate the median measured PM 2.5 in the tile
# compare this value to the WHO and US standards, and create a discretized version (from Wikipedia)
cell_stats = data.sf %>%
  dplyr::select(pm_25) %>%
  aggregate(
    cali_grid,
    FUN= function(x) quantile(x,0.5, na.rm=T)
  ) %>% 
  filter(!is.na(pm_25)) %>%
  mutate(
    who = ifelse(pm_25 > 25, 'over WHO limit', 'under WHO limit'),
    us = ifelse(pm_25 > 35, 'over US limit', 'under US limit'),
    scale = case_when(
      pm_25 < 30 ~ "Good",
      pm_25 < 60 ~ "Satisfactory",
      pm_25 < 90 ~ "Moderately Polluted",
      pm_25 < 120 ~ "Poor",
      pm_25 < 250 ~ "Very Poor",
      pm_25 > 250 ~ "Severe"
    ),
    scale = factor(scale, levels=c("Good","Satisfactory","Moderately Polluted","Poor", "Very Poor", "Severe"))
  )

# Base plot
# mexico, US states on the map
# setting up the theme and the scale and arrow.
base_plot = ggplot()+
  geom_sf(data=mx)+
  geom_sf(data=cali)+
  theme_minimal()+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(
    location = "bl", which_north = "true", 
    pad_x = unit(0.25, "in"), pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering
  )

# plot the median PM 2.5 valuese
# I add the location of the measurement stations to get a sense of density
pm_plot = base_plot + 
  geom_sf(data=cell_stats, aes(fill=pm_25))+
  geom_sf(data=data.sf,size=0.0001,alpha=0.25)+
  labs(x='',y='',title='Median Measured PM 2.5',fill='ug/m^3')+
  scale_fill_distiller(palette='OrRd',direction=1)+
  coord_sf(xlim = c(-130, -113), ylim = c(32, 42), expand = FALSE)

# plot the tiles with discretized scale
scale_plot = base_plot+
  geom_sf(data=cell_stats, aes(fill=scale))+
  geom_sf(data=data.sf,size=0.0001,alpha=0.25)+
  labs(x='',y='',title='Median Measured PM 2.5 Discretized',fill='')+
  scale_fill_brewer(palette='RdYlGn',direction=-1)+
  coord_sf(xlim = c(-130, -113), ylim = c(32, 42), expand = FALSE)

# plot the WHO standard comparison  
who_plot = base_plot+
  geom_sf(data=cell_stats, aes(fill=who))+
  geom_sf(data=data.sf,size=0.0001,alpha=0.25)+
  labs(x='',y='',title='Median Measured PM 2.5 vs WHO 24hr Standard (25ug)',fill='')+
  coord_sf(xlim = c(-130, -113), ylim = c(32, 42), expand = FALSE)

# plot the US standard comparison
us_plot = base_plot+
  geom_sf(data=cell_stats, aes(fill=us))+
  geom_sf(data=data.sf,size=0.0001, alpha=0.25)+
  labs(x='',y='',title='Median Measured PM 2.5 vs US 24hr standard (35ug)',fill='')+
  theme_minimal()+
  coord_sf(xlim = c(-130, -113), ylim = c(32, 42), expand = FALSE)

# write the 4 plots to a PDF file
pdf(paste('pm2_5_',ts,'.pdf',sep=''),height=8.5,width=11)
print(pm_plot)
print(scale_plot)
print(who_plot)
print(us_plot)
dev.off()
