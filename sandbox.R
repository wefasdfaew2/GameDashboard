library(readr)
library(DT)
library(leaflet)
library(ggplot2)
library(magrittr)
library(dplyr)

enc <- tbl(src, 'data_Capture') %>% 
  filter(CapHuntUnit %in% hu) %>% 
  select(EncounterID, AnimalKey, CapDate, Status, Age, 
         capE, capN, CapMtnRange, CapHuntUnit, CapLocDesc) %>% 
  inner_join(tbl(src, 'data_Animal')) %>% 
  collect()

df <- xyConv(enc, c('capE', 'capN'), '+init=epsg:26911', '+init=epsg:4326')

leaflet() %>% 
  addProviderTiles('Esri.WorldTopoMap', 
                   options = providerTileOptions(attribution = NA)) %>% 
  addCircleMarkers(lng = df$x, lat = df$y, stroke = FALSE, radius = 4)

frq <- enc %>% group_by(Species, Sex) %>% 
  summarize(Total = n()) %>% 
  arrange(Species, Sex)
frq  
frq$Sex[frq$Sex == ''] <- 'Unk'

year(as_date(enc$CapDate))
ts <- enc %>% 
  group_by(Species, Year = year(as_date(CapDate))) %>% 
  summarize(Total = n())
ts
ggplot(ts, aes(x = Year, y = Total, group = Species, fill = Species)) +
  geom_bar(stat = 'identity') +
  ggthemes::scale_fill_gdocs() +
  scale_x_continuous(breaks = seq(min(ts$Year), max(ts$Year), 1),
                     labels = seq(min(ts$Year), max(ts$Year), 1)) +
  theme_bw()

### dropdowns for page one
bios <- read_csv('bioareas.csv')
vBios <- bios %>% select(Biologist) %>% extract2(1) %>% unique() %>% sort()
vArea <- bios %>% filter(Biologist == 'Cooper Munson') %>% extract2('MGMT') %>% unique %>% sort()
vUnit <- bios %>% filter(Biologist == 'Pat Cummings') %>% extract2('HuntUnit') %>% unique %>% sort()
vRange <- bios %>% filter(HuntUnit %in% vUnit()) # need ths data from the table...

x <- 'bar'
y <- switch (x,
  'foo' = 'MGMT',
  'bar' = 'HuntUnit'
)
y
