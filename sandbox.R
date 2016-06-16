library(readr)
library(DT)
library(leaflet)
library(ggplot2)
library(magrittr)
library(dplyr)
library(lazyeval)

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
vArea <- bios %>% filter(Biologist == 'Cooper Munson') %>% extract2('MGMT') %>% unique() %>% sort()
vUnit <- bios %>% filter(Biologist == 'Pat Cummings') %>% extract2('HuntUnit') %>% unique()
vRange <- vUnit %>% inner_join(ranges) %>% extract2('Range') %>% unique() %>% sort()
#vRange <- bios %>% filter(Biologist == 'Cooper Munson') %>% extract('HuntUnit') %>% unique() %>% 
#  inner_join(ranges, by = ('HuntUnit' = 'HuntUnit')) %>% extract2('Range') %>% unique() %>% sort()

vLkp <- 'Range'

Bios <- bios %>% filter(Biologist == 'Pat Cummings') %>% 
  inner_join(ranges, by = ('HuntUnit' = 'HuntUnit')) %>% unique()

vDat <- Bios %>% extract2(vLkp) %>% unique() %>% sort()

## database stuff
source('db_config.R')
enc <- tbl(src, 'data_Animal') %>% 
  inner_join(tbl(src, 'data_Capture'), by = c('AnimalKey' = 'AnimalKey')) %>% 
  select(ndowID, Species, Sex, EncounterID, CapDate, Status, Age, 
         capE, capN, CapMtnRange, CapHuntUnit) %>% 
  collect()

## trying to get data
dat <- tbl(src, 'data_Animal') %>% 
  inner_join(tbl(src, 'data_Capture'), by = c('AnimalKey' = 'AnimalKey')) %>% 
  select(ndowID, Species, Sex, CapDate, Status, Age, 
         capE, capN, CapMtnRange, CapHuntUnit, EncounterID) %>% 
  filter(Species == 'MULD') %>% 
  collect()

clmn <- switch('Hunt Unit',
               'Hunt Unit' = 'CapHuntUnit',
               'Mountain Range' = 'CapMtnRange')

val <- '262'
dots <- list(interp(~x == y, .values = list(x = as.name(clmn), y = val)))
dat <- dat %>% filter_(.dots = dots)



## playing with non standard evaluation
downloader::download("https://github.com/cboettig/2015/raw/fc0d9185659e7976927d0ec91981912537ac6018/assets/data/2015-02-06-taxa.csv", "taxa.csv")
all_taxa <- read.csv('taxa.csv')
# standard non lazy eval
x <- filter(all_taxa, Family == 'Scaridae')

# SE in NSE ..etc
.dots <- list(~Family == 'Scaridae')
x1 <- filter_(all_taxa, .dots = .dots)
identical(x, x1)

## arguments for the filter variable
family <- 'Scaridae'
.dots <- list(~Family == family)
x2 <- filter_(all_taxa, .dots = .dots)

## vary column and filter variable
family <- 'Scaridae'
field <- 'Family'
dots <- list(interp(~y == x,
                     .values = list(y = as.name(field), x = family)))
x3 <- filter_(all_taxa, .dots = .dots)
identical(x, x3)

query <- list(Family = 'Scaridae')
dots <- lapply(names(query), function(level){
  value <- query[[level]]
  interp(~y == x, 
         .values = list(y = as.name(level), x = value))
})
x3 <- filter_(all_taxa, .dots = dots)

## leaflet color palette
df <- xyConv(dat, c('capE', 'capN'), '+init=epsg:26911', '+init=epsg:4326')
pal <- colorFactor(palette = gdocs_pal()(length(vSpp)),
                   domain = vSpp)
previewColors(pal, vSpp)
leaflet() %>% addTiles() %>% 
  addCircleMarkers(lng = df$x, lat = df$y, stroke = FALSE, radius = 4, color = pal(df$Species)) %>% 
  addLegend('bottomright', pal = pal, values = c('DBHS', 'RBHS'), title = 'Species')
