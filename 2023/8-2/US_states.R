library(tidyverse)
library(rgdal)
library(rgeos)
library(dplyr)
library(broom)
library(rgeos)
library(rgdal)
library(maptools)
library(ggplot2)
devtools::install_github("hrbrmstr/ggalt", force=TRUE)
library(ggalt) 
library(ggthemes)
library(viridis)
library(maptools)

states <- readr::read_csv('/Users/dainson/Downloads/states.csv')
state_name_etymology <- readr::read_csv('/Users/dainson/Downloads/state_name_etymology.csv')
us_map_hex <- readOGR('/Users/dainson/Downloads/us_states_hexgrid.geojson')

tot_us_pop <- sum(states$population_2020)

# 미국 전체 인구를 각 주의 면적으로 나눴을 때 각 가정마다 사용할 수 있는 땅의 면적. 
# 1 square mile = 640 acre <= acrs_per_us_cap
# 1가구당 평균 가구원 수 2.6명 <= acre_per_us_hld
states <- states |>
  mutate(pop_dens = population_2020 / land_area_mi2,
         pop_dens_km = population_2020/total_area_km2,
         us_pop = tot_us_pop,
         mls_per_us_cap = land_area_mi2 / us_pop,
         acrs_per_us_cap = mls_per_us_cap * 640,
         acre_per_us_hld = acrs_per_us_cap * 2.6,
         id = postal_abbreviation)

us_sts <- us_map_hex |>
  fortify("iso3166_2") |>
  # transform into tibble format
  as_tibble() |>
  # select columns
  select(id, long, lat) |>
  filter(id != "DC") 

centres <- gCentroid(us_map_hex, byid = TRUE)|>
  as_tibble(id= us_map_hex@data$iso3166_2)

labels <- us_map_hex@data$iso3166_2

acre_sts <- states |>
  select(id, acre_per_us_hld)

hex_labels <- tibble(id = labels, centres) |>
  filter(id != "DC") |>
  left_join(acre_sts)

plt <- ggplot() +
  geom_polygon(data = us_sts, aes(x = long, y = lat, group = id ), fill = "#1E90FF" , colour = "#183059") +
  geom_text(data = hex_labels, aes(x = x, y = y+0.75, label = id) ,  colour = "#F6F4F3", size = 3) +
  geom_text(data = hex_labels, aes(x = x, y = y-0.75, label = round( acre_per_us_hld, 2)) ,  
            colour = "#F6F4F3", size = 3) +
  coord_map() +
  theme_void() +
  labs(title="If Everyone in the US Moved to a Single State, \n\nHow Many Acres of Land Would Each Household Get?")+
  theme(
    text=element_text() ,
    plot.title = element_text(size = 13, margin = margin(0,0,20,0), hjust=0.5),
    plot.margin = margin(10,10,10,10)
  )

plt

