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

################################
# GET DATA, AND ALL OTHER DATA #
################################
source('db_config.R')
dat <- tbl(src, 'data_Animal') %>% 
  inner_join(tbl(src, 'data_Capture'), by = c('AnimalKey' = 'AnimalKey')) %>% 
  select(ndowID, Species, Sex, CapDate, Status, Age, 
         capE, capN, CapMtnRange, CapHuntUnit, EncounterID) %>% 
  filter(Species == 'MULD' & CapHuntUnit %in% c('101', '102', '103')) %>% 
  collect()

# type conversions
dat$CapDate <- ymd_hms(dat$CapDate)
dat$capE <- as.numeric(dat$capE)
dat$capN <- as.numeric(dat$capN)


source('db_config.R')
biometric <- dat %>% 
  left_join(tbl(src, 'data_Biometric'), copy = TRUE) %>% 
  select(EncounterID, Sex, Age, Biometric, Measurement, Units) %>% 
  collect()
# or (below may be faster...double check later)
source('db_config.R')
biometric <- tbl(src, 'data_Biometric') %>% 
  filter(EncounterID %in% dat$EncounterID) %>% 
  collect() %>% 
  left_join(dat, by = c('EncounterID' = 'EncounterID')) %>% 
  select(ndowID, Sex, Age, Biometric, Measurement, Units, CapMtnRange, CapHuntUnit, CapDate, EncounterID)

## query without data to return, work around error when no data returned
source('db_config.R')
biom <- tbl(src, 'data_Biometric') %>% 
  collect() %>% 
  filter(EncounterID %in% dat$'EncounterID') %>% 
  left_join(dat)

dat <- tbl(src, 'data_Animal') %>% 
  
  inner_join(tbl(src, 'data_Capture'), by = c('AnimalKey' = 'AnimalKey')) %>% 
  select(ndowID, Species, Sex, CapDate, Status, Age, 
         capE, capN, CapMtnRange, CapHuntUnit, EncounterID) %>% 
  filter(Species == 'desert bighorn') %>% 
  collect()
  

# type conversion
biometric$

## graphing this data with ggplot2
biometric$CapDate <- ymd_hms(biometric$CapDate)
biometric$CapYear <- year(biometric$CapDate)
# basic
ggplot(biometric, aes(x = Biometric, y = Measurement)) +
  geom_boxplot()
# group
ggplot(biometric, aes(x = Biometric, y = Measurement)) +
  geom_boxplot() +
  facet_wrap(~CapMtnRange)
# with plotly
library(plotly)
p <- ggplot(biometric, aes(x = Biometric, y = Measurement, fill = CapMtnRange)) +
  geom_boxplot(outlier.shape = 95, outlier.size = 7) +
  scale_fill_gdocs() +
  theme_bw() 
p

ggplot(filter(biometric, Biometric == 'Weight'),
       aes(x = Biometric, y = Measurement, fill = CapMtnRange)) +
  geom_boxplot(outlier.shape = 95, outlier.size = 7) +
  scale_fill_gdocs() +
  facet_wrap(~CapYear) +
  theme_bw() 

################
# SUMMARY DATA #
################
bio_gp <- biometric %>% 
  select(ndowID, EncounterID, Biometric, Measurement)
bio_tidy <- tidyr::spread(bio_gp, Biometric, Measurement)
bio_tidy
bio_sm <- xda::numSummary(data.frame(bio_tidy[, 3:8]))
bio_sm[, c(1, 2, 4, 3, 6, 12:14, 5)]

## get data for summary
src <- RSQLServer::src_sqlserver("GAMEDB", database = "encounterdb")
dat <- tbl(src, 'data_Animal') %>% 
  inner_join(tbl(src, 'data_Capture'), by = c('AnimalKey' = 'AnimalKey')) %>% 
  select(ndowID, Species, Sex, CapDate, Status, Age, 
         capE, capN, CapMtnRange, CapHuntUnit, EncounterID) %>% 
  filter(Species == 'MULD' & CapHuntUnit %in% c('101', '102', '103')) %>% 
  collect()

dat$CapDate <- ymd_hms(dat$CapDate)
dat$capE <- as.numeric(dat$capE)
dat$capN <- as.numeric(dat$capN)
dat$CapYear <- year(dat$CapDate)

## get biometric data
biometric <- tbl(src, 'data_Biometric') %>% 
  filter(EncounterID %in% dat$EncounterID) %>% 
  collect() %>% 
  left_join(dat, by = c('EncounterID' = 'EncounterID')) %>% 
  select(ndowID, Sex, Age, Biometric, Measurement, Units, CapMtnRange, 
         CapHuntUnit, CapDate, CapYear, EncounterID) 

## bio summary
dim(biometric)
head(biometric, 5)
bio_smry <- biometric %>% 
  select(ndowID, EncounterID, Biometric, Measurement) %>% 
  tidyr::spread(Biometric, Measurement)
bio_smry <- xda::numSummary(data.frame(bio_smry[, 3:8]))[, c(1, 2, 4, 3, 6, 12:14, 5)]
DT::datatable(bio_smry)

## enc summary
dim(dat)
head(dat, 5)
as_date(c(min(dat$CapDate), max(dat$CapDate)))
xda::numSummary(data.frame(dat))
xda::charSummary(data.frame(dat))

##################################
# INTERACTIVE DATA VISUALIZATION #
##################################
## get data from database
source('db_config.R')
dat <- tbl(src, 'data_Animal') %>% 
  inner_join(tbl(src, 'data_Capture'), by = c('AnimalKey' = 'AnimalKey')) %>% 
  select(ndowID, Species, Sex, CapDate, Status, Age, 
         capE, capN, CapMtnRange, CapHuntUnit, EncounterID) %>% 
  filter(Species == 'MULD' & CapHuntUnit %in% c('101', '102', '103')) %>% 
  collect()
# type conversions
dat$CapDate <- as_date(dat$CapDate)
dat$CapYear <- year(dat$CapDate)
dat$capE <- as.numeric(dat$capE)
dat$capN <- as.numeric(dat$capN)
source('db_config.R')
biometric <- tbl(src, 'data_Biometric') %>% 
  filter(EncounterID %in% dat$EncounterID) %>% 
  collect() %>% 
  left_join(dat, by = c('EncounterID' = 'EncounterID')) %>% 
  select(ndowID, Sex, Age, Biometric, Measurement, Units, CapMtnRange, 
         CapHuntUnit, CapDate, CapYear, EncounterID)
messyBiom <- biometric %>% 
  select(ndowID, EncounterID, Biometric, Measurement) %>% 
  tidyr::spread(Biometric, Measurement)

## making figures
### boxplot
ggplot(biometric, aes(x = Biometric, y = Measurement, fill = CapMtnRange)) +
  geom_boxplot(outlier.shape = 95, outlier.size = 7) +
  scale_fill_gdocs(drop = F) +
  theme_bw() 
### scatter with biometric data
ggplot(messyBiom, aes(x = HindLeg, y = Weight)) +
  geom_point() +
  theme_bw()
### bar charts, ..count..
ggplot(dat, aes_string(x = 'CapMtnRange', y = '..count..')) +
  geom_bar(stat = 'count')

## figure types
### distributions
ggplot(filter(biometric, Biometric == 'ChestGirth'), aes(x = Measurement, fill = factor(CapYear))) +
  geom_histogram(binwidth = 2.5)
ggplot(filter(biometric, Biometric == 'ChestGirth'), aes(x = Measurement, fill = factor(CapYear))) +
  geom_density()

### boxplot
ggplot(biometric, aes(x = Biometric, y = Measurement)) +
  geom_boxplot()
ggplot(filter(biometric, Biometric == 'Weight'), aes(x = CapMtnRange, y = Measurement, color = Sex)) +
  geom_boxplot() +
  scale_color_gdocs(drop = T)

### simulating shiny app
xval <- 'ChestGirth'
yval <- 'Weight'
colval <- 'Sex'
filval <- colval
group <- NA
facetval <- 'Age'
ggplot(messyBiom, aes_string(x = xval, y = yval, color = colval)) +
  geom_point() +
  theme_bw() 
  





### writing a function for interactive data analysis
intPlot <- function(dat, xval, yval, colval, type, filval = colval,
                    groupval = NULL, facetval = NULL) {
  gg <- ggplot(dat, aes_string(x = xval, y = yval))
  
  if (type == 'scatter') {
    gg <- gg + geom_point(aes_string(color = colval)) +
      scale_color_gdocs()
  } else if (type == 'box') {
    gg <- gg + geom_boxplot(aes_string(fill = filval)) +
      scale_fill_gdocs()
  } else if (type == 'bar') {
    gg <- gg + geom_bar(aes_string(color = NULL, fill = colval)) +
      scale_fill_gdocs()
  }
  gg <- gg + theme_bw()
  return(gg)
}

plotData <- function(xval, yval, type) {
  if(type == 'scatter') {
    dat <- messyBiom
  } else if {type == }
}
### testing scatter plot
intPlot(messyBiom, xval, yval, colval, type = 'scatter')
### testing box plot
intPlot(biometric, 'Biometric', 'Measurement', 'Sex', type = 'box')
### testing bar plot
intPlot(dat, 'CapYear', '..count..', 'CapMtnRange', type = 'bar')
