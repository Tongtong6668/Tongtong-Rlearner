setwd('F:/Onedrive/OneDrive - stu.shzu.edu.cn/Desk') ##设置工作路径
library(ggmap)
library(rgdal)
library(ggplot2)
library(maps)
library(dplyr)
library(ggThemeAssist)
library(ggsci)
library(gridExtra)

##地图
EC <- read.csv('苗苗地图num.csv',header = T)  ## 读取我们的数据
Num_2019 <- subset(EC,EC$year==2019 & 
                     EC$age=='All ages' & 
                     EC$metric== 'Number' &
                     EC$measure=='Deaths') ## 获取2019年EC年龄校正后发病率
Num_2019 <- Num_2019[,c(2,9,10,11)]
Num_2019$val <- round(Num_2019$val,2) ###保留一位小数点
Num_2019$lower <- round(Num_2019$lower,2) ###保留一位小数点
Num_2019$upper <- round(Num_2019$upper,2) ###保留一位小数点

####  map for ASR
worldData <- map_data('world')
worldData <- subset(worldData, region != 'Antarctica')
country_asmr <- Num_2019
country_asmr$location <- as.character(country_asmr$location) 
###以下代码的目的是让country_asr$location的国家名称与worldData的国家名称一致
### 这样才能让数据映射到地图上
country_asmr$location[country_asmr$location == 'United States of America'] = 'USA'
country_asmr$location[country_asmr$location == 'Russian Federation'] = 'Russia'
country_asmr$location[country_asmr$location == 'United Kingdom'] = 'UK'
country_asmr$location[country_asmr$location == 'Congo'] = 'Republic of Congo'
country_asmr$location[country_asmr$location == "Iran (Islamic Republic of)"] = 'Iran'
country_asmr$location[country_asmr$location == "Democratic People's Republic of Korea"] = 'North Korea'
country_asmr$location[country_asmr$location == "Taiwan (Province of China)"] = 'Taiwan'
country_asmr$location[country_asmr$location == "Republic of Korea"] = 'South Korea'
country_asmr$location[country_asmr$location == "United Republic of Tanzania"] = 'Tanzania'
country_asmr$location[country_asmr$location == "Bolivia (Plurinational State of)"] = 'Bolivia'
country_asmr$location[country_asmr$location == "Venezuela (Bolivarian Republic of)"] = 'Venezuela'
country_asmr$location[country_asmr$location == "Czechia"] = 'Czech Republic'
country_asmr$location[country_asmr$location == "Republic of Moldova"] = 'Moldova'
country_asmr$location[country_asmr$location == "Viet Nam"] = 'Vietnam'
country_asmr$location[country_asmr$location == "Lao People's Democratic Republic"] = 'Laos'
country_asmr$location[country_asmr$location == "Syrian Arab Republic"] = 'Syria'
country_asmr$location[country_asmr$location == "North Macedonia"] = 'Macedonia'
country_asmr$location[country_asmr$location == "Micronesia (Federated States of)"] = 'Micronesia'
country_asmr$location[country_asmr$location == "Macedonia"] = 'North Macedonia'
country_asmr$location[country_asmr$location == "Trinidad and Tobago"] = 'Trinidad'
a <- country_asmr[country_asmr$location == "Trinidad",]
a$location <- 'Tobago'
country_asmr <- rbind(country_asmr,a)
country_asmr$location[country_asmr$location == "Cabo Verde"] = 'Cape Verde'
country_asmr$location[country_asmr$location == "United States Virgin Islands"] = 'Virgin Islands'
country_asmr$location[country_asmr$location == "Antigua and Barbuda"] = 'Antigu'
a <- country_asmr[country_asmr$location == "Antigu",]
a$location <- 'Barbuda'
country_asmr <- rbind(country_asmr,a)
country_asmr$location[country_asmr$location == "Saint Kitts and Nevis"] = 'Saint Kitts'
a <- country_asmr[country_asmr$location == "Saint Kitts",]
a$location <- 'Nevis'
country_asmr <- rbind(country_asmr,a)
country_asmr$location[country_asmr$location == "Côte d'Ivoire"] = 'Ivory Coast'
country_asmr$location[country_asmr$location == "Saint Vincent and the Grenadines"] = 'Saint Vincent'
a <- country_asmr[country_asmr$location == "Saint Vincent",]
a$location <- 'Grenadines'
country_asmr <- rbind(country_asmr,a)
country_asmr$location[country_asmr$location == "Eswatini"] = 'Swaziland'
country_asmr$location[country_asmr$location == "Brunei Darussalam"] = 'Brunei'

Num_quan <- quantile(Num_2019$val,probs = seq(0,1,0.125))
for (z in 1:(length(Num_quan)-1)) {
  if (z==1) {
    Num_group <- paste0('<',round(Num_quan[z+1],2))
  } else {
    Num_group <- c(Num_group,paste(round(Num_quan[z],2),round(Num_quan[z+1],2),sep = ' to <'))
  }
}
total <- full_join(worldData,country_asmr,by = c('region'='location'))

total <- total %>% mutate(val2 = cut(val, breaks = Num_quan,
                                     labels = Num_group,  ### breaks需要根据自己的实际结果来调整
                                     include.lowest = T,right = T))
library('RColorBrewer')
display.brewer.all()

colorpalette=rev(c("#F4695E","#FEB176","#FFECA8","#ECECB3","#AFD3AF","#35ACBB","#6F8DB2","#7264A9"))
p1 <- ggplot()+
  geom_polygon(data=total, 
               aes(x=long, y=lat, group = group,fill=val2),
               colour="white",size = .2)+ 
  scale_fill_manual(values = colorpalette)+
  labs(x="", y="")+
  guides(fill = guide_legend(title='Number of deaths'))+
  theme_void()+
  
  theme(text=element_text(family = 'sans'),   ##全文字体
        legend.position = c(0.1,0.25),
        legend.title = element_text(size=19,face = 'bold'), ##图例标题
        legend.text = element_text(size = 15)  ##图例文字
  ) 
p1

plot_grid(p1, p2, p3, labels = c("A", "B", "C"),label_size = 19, ncol = 1)

