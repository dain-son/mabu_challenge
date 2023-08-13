#한글 폰트
install.packages('extrafont')
library(extrafont)

font_import()
install.packages('maps')
install.packages('mapproj')
install.packages('nord')
install.packages('ggiraphExtra')
library(ggiraphExtra)
library(tidyverse)
library(maps)
library(mapproj)
library(nord)
library(ggplot2)
theme_set(theme_grey(base_family='NanumBarunGothic'))

states <- readr::read_csv('/Users/dainson/Desktop/dataviz/viz_challenge/2023-8-2/states.csv')

usa_state <- map_data(map='state')

# states data의 state변수와 map data의 region 변수 mapping
# state 변수를 대문자 -> 소문자
states$state <- tolower(states$state)

# 주별 인구/면적, 주별 인구/땅면적 column추가
states <- states |>
  mutate(pop_dens = population_2020 / total_area_km2,
         pop_dens_land = population_2020 / land_area_km2)

# 인구 수
ggChoropleth(data=states,
             mapping=aes(map_id = state, fill=population_2020),
             map=usa_state,
             interactive=TRUE,
             title="how many people live in each US state?")

# 인구 밀도(total area)
ggChoropleth(data=states,
             map=usa_state,
             mapping=aes(map_id = state, fill=pop_dens),
             interactive=T,
             title="How many people live km2 in each US state?")
??ggChoropleth
# 인구 밀도(land area)
ggChoropleth(data=states,
             map=usa_state,
             mapping=aes(map_id = state, fill=pop_dens_land),
             interactive=T,
             title="How many people live km2 in each US state? (land only)")


#한국도 해보자
install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014",force=TRUE)

library(kormaps2014)
library(dplyr)
korpop1 <- rename(korpop1,
                  pop=총인구_명,
                  name=행정구역별_읍면동)

#korpop1 = changeCode(korpop1)
class(korpop1) # "data.frame"
str(korpop1)

# 면적 데이터 구하기
korea<- readr::read_csv('/Users/dainson/Desktop/dataviz/viz_challenge/2023-8-2/korea.csv')
korea <- korea |> group_by(광역시도) |>
  summarise(area = sum(면적))
korea <- rename(korea, name=광역시도)

options(scipen=999)

ggChoropleth(data=korpop1, # 지도에 표시할 데이터 
             aes(fill= pop, # 색깔로 나타낼 변수 
                 map_id = code, # 지역 기준 변수 
                 tooltip = name), 
             map = kormap1,
             interactive=T,
             title="우리나라 인구 수")

#인구 밀도

dens<- merge(x = korea, y=korpop1,
             by='name')
dens <- dens |> mutate(dens= pop/area)

ggChoropleth(data=dens,
             map=kormap1,
             aes(fill= dens, # 색깔로 나타낼 변수 
                 map_id = code,# 지역 기준 변수 
                 tooltip = name),
             interactive=T,
             title= "우리나라 인구 밀도 (1km2당)")
