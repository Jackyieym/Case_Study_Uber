library(readr)
ProjectRawData <- read_csv("BU/Y2 Sem2/BUSI2045/ProjectRawData.csv", 
  col_types = cols(Importance_Price = col_number(), 
     Importance_Safety = col_number(), 
     Importance_Conv = col_number(), Importance_Cus = col_number()))
View(ProjectRawData)
library(ggplot2)
library(ggcorrplot)
library(nFactors)
library(GGally)

library(dplyr)
s1<-ProjectRawData %>% 
  filter(OnlineTaxiService=="Yes") %>% 
  select(2,11,12,13,14) %>% 
  arrange(Gender)


#Frequency bar chart
bar_Price<-ggplot(s1, aes(x=Importance_Price, fill=Gender))+
  geom_bar(position = position_dodge(preserve = "single"))
bar_Safety<-ggplot(s1, aes(x=Importance_Safety,fill=Gender))+
  geom_bar(position = position_dodge(preserve = "single"))
bar_Conv<-ggplot(s1, aes(x=Importance_Conv,fill=Gender))+
  geom_bar(position = position_dodge(preserve = "single"))
bar_Cus<-ggplot(s1, aes(x=Importance_Cus, fill=Gender))+
  geom_bar(position = position_dodge(preserve = "single"))
library(patchwork)
bar_Price+bar_Safety+bar_Conv+bar_Cus+
  plot_annotation("Votes on score of different factors per gender (frequency)", 
                  theme=theme(plot.title=element_text(hjust=0.5)))

#Proportion bar chart 
port1<-as.data.frame(with(s1, prop.table(table(Gender, Importance_Price), margin=1)))
port2<-as.data.frame(with(s1, prop.table(table(Gender, Importance_Safety), margin=1)))
port3<-as.data.frame(with(s1, prop.table(table(Gender, Importance_Conv), margin=1)))
port4<-as.data.frame(with(s1, prop.table(table(Gender, Importance_Cus), margin=1)))

porp_bar_Price<-ggplot(port1, aes(x=Importance_Price, y=Freq, fill=Gender))+
  geom_col(position="dodge")+
  labs(y="Proportion of count")
porp_bar_Safety<-ggplot(port2, aes(x=Importance_Safety, y=Freq, fill=Gender))+
  geom_col(position="dodge")+
  labs(y="Proportion of count")
porp_bar_Conv<-ggplot(port3, aes(x=Importance_Conv, y=Freq, fill=Gender))+
  geom_col(position="dodge")+
  labs(y="Proportion of count")
porp_bar_Cus<-ggplot(port4, aes(x=Importance_Cus, y=Freq, fill=Gender))+
  geom_col(position="dodge")+
  labs(y="Proportion of count")
porp_bar_Price+porp_bar_Safety+porp_bar_Conv+porp_bar_Cus+
  plot_annotation("Votes on score of different factors per gender (proportion)", 
                  theme=theme(plot.title=element_text(hjust=0.5)))


s1$Gender<-ifelse(s1$Gender=="Male", 1, ifelse(s1$Gender=="Female",0,2))

ggcorr(s1, label = T, label_round = 2)

lm(Gender~Importance_Price+Importance_Safety+Importance_Conv+Importance_Cus, s1)
