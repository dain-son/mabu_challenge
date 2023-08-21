library(ggmap)
library(ggplot2)
library(dplyr)
library(extrafont)
font_import()
theme_set(theme_minimal(base_family='NanumGothic'))

# 데이터 불러오기
tp <- readr::read_csv('/Users/dainson/Downloads/ibtracs.WP.list.v04r00.csv')

# 변수 제거
tp <- tp[-1,]

# dplyr 패키지 이용하기 위해 tibble dataframe으로 변환
tp <- tibble::as_tibble(tp)

# google API key 받아오기
mykey = 'AIzaSyDAL3TmsrHJcFWDBNktit0ycSp8jxIQ6gw'
register_google(key=mykey)

# google_map 불러오기
gc<- geocode("south korea", source="google")
center <- as.numeric(gc)
map<- ggmap(get_googlemap(center = center,zoom=5, color = "bw"), extent = "device")

# 충청권과 남해안을 지나간 태풍 리스트 -혜민 선배 참고
tp_list_df <- tp %>%
  filter(SID %in% c("1945211N23129",
                    "1946222N15152",
                    "1957223N08145",
                    "1961198N18134",
                    "1961204N12147",
                    "1987188N10151",
                    "1989201N11145",
                    "1995193N06156",
                    "2000245N14157",
                    "2002234N14164",
                    "2006180N06140",
                    "2012196N19144",
                    "2012254N09135",
                    "2018227N11145",
                    "2023208N13140"))

# season, latitude, logitude 숫자 데이터로 바꾸기
# wind.WMO. 숫자로 바꾸고 초속 단위로 바꾸기
# ISO_time에서 월 정보 추출
tp_list_df <- tp_list_df%>% mutate(Season = as.numeric(SEASON),
                   Latitude = as.numeric(gsub("^ ", "", LAT)),
                   Longitude = as.numeric(gsub("^ ", "", LON)),
                   Wind.WMO. = as.numeric(gsub("^ ", "", WMO_WIND)) * 0.5144,
                   ISO_time = as.POSIXct(ISO_TIME, tz = "UTC"))

# google_map 지도 시각화
map + geom_path(data=tp_list_df, 
                aes(x= Longitude, y=Latitude, group=SID, color=Wind.WMO.),
                alpha = 0.5, linewidth=0.6) +
  labs(x="", y="", colour="Wind \n(m/sec)",
       title="Typhoon Trajectories that penetrated the Korea") +
  theme(plot.title = element_text(size=15,hjust=0.5),
        panel.background = element_rect(fill="grey10", colour ="gray30"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor=element_blank())

