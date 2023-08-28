#라이브러리 설치 및 폰트 설정
install.packages('readxl')
library(readxl)
install.packages('glimpse')
library(dplyr)library(ggplot2)
library(extrafont)
font_import()
theme_set(theme_minimal(base_family='AppleGothic'))

#데이터 불러오기
mass <- read_excel('/Users/dainson/Desktop/dataviz/viz_challenge/2023-08-24/Violence Project Mass Shooter Database - Version 7 5.28.23.xlsx',                   sheet='Full Database')
mass <- mass[-1,]

# 연도별 총기 난사 사건 발생 추이 시각화
year <- mass %>% group_by(mass$Year) %>% summarise(n=n())
year$n <- as.integer(year$n)
ggplot(data=year, aes(x=`mass$Year`, y=n)) +  geom_bar(stat='identity')+  geom_smooth()+  xlab('year')+  ylab('count')+  ggtitle('How many US mass shootings \n have there been since 1966')+  theme(plot.title=element_text(face="bold", hjust=0.5, size=12))  

# 총기 난사 사건 간단한 분포 시각화
#1 연령
Age_group<- mass %>%    mutate(age_group =case_when(Age < 10 ~ 'Under 10s', Age < 20 ~ '10s',Age < 30 ~ '20s',Age < 40 ~ '30s',Age < 50 ~ '40s',                Age < 60 ~ '50s',Age < 70 ~ '60s',Age >= 70 ~ '70+'))
View(Age_group)
Age_group1<- Age_group %>% group_by(Age_group$age_group) %>%  summarise(n=n())
Age_group1<- Age_group1[-8,]
age_plot<- ggplot(data=Age_group1, aes(x=`Age_group$age_group`, y=n))+  
					geom_bar(stat='identity',fill = "#FF6666")+  
					ggtitle("age")+  
					theme(plot.title=element_text(face="bold", hjust=0.5, size=12))+  
					xlab('')+  ylab('')

#2 성별
mass2 <- mass%>%group_by(Gender)%>%summarise(n=n())%>%  na.omit()mass2['Gender'] = c("Male","Female","Non-Binary","Transgender")
gender_plot<- mass2%>%ggplot(aes(x=Gender, y=n))+  
						geom_bar(stat='identity', fill="#FF6666")+  
						ggtitle("Gender")+  
						theme(plot.title=element_text(face="bold", hjust=0.5, size=12))+  
						xlab("")+  ylab("")

#3 이민
mass1<- mass%>%group_by(Immigrant) %>% summarise(n=n())%>%  na.omit()
mass1["Immigrant"] = c('No', 'Yes')
immigrant_plot<- mass1%>%ggplot(aes(x=Immigrant, y=n))+  
								geom_bar(stat='identity',fill="#FF6666")+  
								ggtitle("Immigrant")+  
								theme(plot.title=element_text(face="bold", hjust=0.5, size=12))+
							  xlab("")+  ylab("")

#4 다른 총격 사건과의 관계
mass3<- mass%>%group_by(mass$`Relationship with Other Shooting(s)`)%>% summarise(n=n())%>%  na.omit()
mass3[,1] = c("No evidence","Yes")
colnames(mass3)[1] = "Relationship with other shooting"
mass3other_shooting_plot<- mass3%>%
									ggplot(aes(x=mass3$`Relationship with other shooting`, y=n))+  
									geom_bar(stat='identity', fill="#FF6666")+  
									ggtitle("Relationship with other shooting")+  
								theme(plot.title=element_text(face="bold", hjust=0.5, size=12))+
							  xlab("")+  ylab("")

# 그래프 한 번에 그리기
library(gridExtra)
grid.arrange(age_plot, gender_plot, immigrant_plot, other_shooting_plot, ncol=2)
