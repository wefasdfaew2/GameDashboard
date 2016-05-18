library(readr)
library(DT)
library(leaflet)
library(ggplot2)
library(dplyr)

## loading data
animal <- read_csv('data/animal.csv')
capture <- read_csv('data/capture.csv')

# xyConv on tbl_df

