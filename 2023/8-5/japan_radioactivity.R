library(readxl)
library(ggplot2)
library(magrittr)
library(dplyr)
library(readr)
library(tidyr)

data_T01<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=1, skip=1)
data_T01A<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=2, skip=1)
data_T02<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=3, skip=1)
data_T03<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=4, skip=1)
data_T03A<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=5, skip=1)
data_A1<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=9, skip=1)
data_A2<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=10, skip=1)
data_A3<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=11, skip=1)

data_T1<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=6, skip=1)
data_T2<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=7, skip=1)
data_T21<- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-31/close1F_water.xlsx', sheet=8, skip=1)


data_A1$'Sampling date'<- as.Date(as.numeric(data_A1$`Sampling date`), origin='1899-12-30')
data_A2$'Sampling date'<- as.Date(as.numeric(data_A2$`Sampling date`), origin='1899-12-30')
data_A3$'Sampling date'<- as.Date(as.numeric(data_A3$`Sampling date`), origin='1899-12-30')
data_T01A$'Sampling date'<- as.Date(as.numeric(data_T01A$`Sampling date`), origin='1899-12-30')
data_T03A$'Sampling date'<- as.Date(as.numeric(data_T03A$`Sampling date`), origin='1899-12-30')

#data_T01 <- data_T01 %>% mutate(cs134 = parse_number(`134Cs detection limit (Bq/L)`))
data_T01<- data_T01 %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_T01A<- data_T01A %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_T02<- data_T02 %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_T03<- data_T03 %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_T03A<- data_T03A %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_T1<- data_T1 %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_T2<- data_T2 %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_T21<- data_T21 %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_A1<- data_A1 %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_A2<- data_A2 %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')
data_A3<- data_A3 %>% gather('134Cs detection limit (Bq/L)','137Cs detection limit (Bq/L)',key='type',value='detection limit (Bq/L)')

ggplot(data_T01)+
  geom_point(aes(x=`Sampling date`, y=`detection limit (Bq/L)`, color=type), size=1)+
  ggtitle("T01")+
  scale_color_manual(values=c('red','blue'))+
  xlab("")+
  ylab("")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, size=10))
  
ggplot(data_T01A)+
  geom_point(aes(x=`Sampling date`, y=`detection limit (Bq/L)`, color=type), size=1)+
  ggtitle("T01A")+
  scale_color_manual(values=c('red','blue'))+
  xlab("")+
  ylab("")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, size=10))

ggplot(data_T02)+
  geom_point(aes(x=`Sampling date`, y=`detection limit (Bq/L)`, color=type), size=1)+
  ggtitle("T02")+
  scale_color_manual(values=c('red','blue'))+
  xlab("")+
  ylab("")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, size=10))

ggplot(subset(data_T1, !is.na(data_T1$`detection limit (Bq/L)`)),aes(x=`Sampling date`, y=`detection limit (Bq/L)`,color=type))+
  geom_point(aes(x=`Sampling date`, y=`detection limit (Bq/L)`, color=type), size=1)+
  ggtitle("T1")+
  scale_color_manual(values=c('red','blue'))+
  xlab("")+
  ylab("")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, size=10))+
  scale_y_discrete(breaks=c(0.01,0.1,1,10,100))

ggplot(subset(data_T2, !is.na(data_T2$`detection limit (Bq/L)`)),aes(x=`Sampling date`, y=`detection limit (Bq/L)`,color=type))+
  geom_point(aes(x=`Sampling date`, y=`detection limit (Bq/L)`, color=type), size=1)+
  ggtitle("T2")+
  scale_color_manual(values=c('red','blue'))+
  xlab("")+
  ylab("")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, size=10))+
  scale_y_discrete(breaks=c(0.01,0.1,1,10,100,1000))

ggplot(subset(data_T21, !is.na(data_T21$`detection limit (Bq/L)`)),aes(x=`Sampling date`, y=`detection limit (Bq/L)`,color=type))+
  geom_point(aes(x=`Sampling date`, y=`detection limit (Bq/L)`, color=type), size=1)+
  ggtitle("T21")+
  scale_color_manual(values=c('red','blue'))+
  xlab("")+
  ylab("")+
  theme_minimal()+
  theme(plot.title = element_text(hjust=0.5, size=10))+
  scale_y_discrete(breaks=c(0.001,0.01,0.1,1,10,100,1000))

