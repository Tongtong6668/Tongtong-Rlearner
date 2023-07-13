library(tidyverse)
library(vegan)
library(scico)


data("varechem")

df <- varechem %>% select(1:14) %>% select(-c(Humdepth,Baresoil))



# 使用Min-Max标准化方法
df_normalized <- as.data.frame(apply(df, 2, function(x) (x - min(x)) / (max(x) - min(x))))

varechem %>% rownames_to_column(var="id") %>% #给数据框 varechem 添加一列名为 id 的行号列
  select(id) %>% 
  bind_cols(.,df_normalized) %>%  #将选择的 id 列与数据框 df_normalized 进行列绑定操作
  pivot_longer(-id) %>%   #将除了 id 列之外的所有列进行长格式转换，即将宽表转换为长表
  ggplot(aes(name,id,fill=value))+
  geom_tile()+ #添加一个矩形图层，用于创建热力图
  labs(x=NULL,y=NULL)+
  scale_fill_scico(palette="vik")+
  scale_y_discrete(expand=c(0,0),position = 'left')+
  scale_x_discrete(expand=c(0,0))+
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(color="black",size=8),
        axis.ticks = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(color="black"),
        legend.title = element_blank(),
        legend.spacing.x = unit(0.1,"cm"))+
  guides(fill=guide_colorbar(direction="vertical",reverse=F,barwidth=unit(.5,"cm"),
                             barheight=unit(11,"cm")))
