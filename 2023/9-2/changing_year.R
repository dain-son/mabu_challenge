munwha$지정일 <- as.Date(munwha$지정일)
gg_year<- gg %>% mutate(year=format(지정일, "%Y")) 
	%>%  group_by(year) 
	%>%  summarise(total=sum(n))

gg_year$cumulative<- cumsum(gg_year$total)
gg_year %>% ggplot(aes(x=year)) +
  geom_bar(aes(y=total), fill="skyblue", stat='identity')+
  geom_point(aes(y=cumulative), color=rgb(0,0,1), pch=16, size=2)+
  geom_path(aes(y=cumulative, group=1), color="slateblue1", lty=3, size=1)+
  theme_minimal(base_family='NanumGothic')+
  labs(title='연도별 등록문화재 건수 변화',
       x="연도",
       y='건수')+
  theme(plot.title=element_text(size=15,hjust=0.5,face='bold'))
