umul<- read.delim(pipe("pbpaste"), header=TRUE)
library(treemap)
library(treemapify)

ggplot(umul, aes(area=합계, fill=분야, label=분야))+
  geom_treemap()+
  geom_treemap_text(face="bold",family="NanumGothic")+
  theme_minimal(base_family="NanumGothic")+
  labs(title="근현대 시기 주요 분야별 유물 목록화 ")+
  theme(legend.position="none",
        plot.title=element_text(size=15,hjust=0.5,vjust=2,face='bold'))
