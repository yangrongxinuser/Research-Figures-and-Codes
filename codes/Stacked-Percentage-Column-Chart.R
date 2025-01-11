library('readxl')


##导入相关包
library(tidyverse)
library(patchwork)


m1 <- read_excel("验证集病人结局指标.xlsx")


##将className跟rlCodes变量转换为因子类型，主要为了跟论文中的顺序保持一致
m1 %>% 
  mutate(cluster_label = factor(cluster_label, levels = c("Validation-1-GROUP","Validation-2-GROUP","Validation-3-GROUP","Validation-4-GROUP") ),
         Outcome = factor(Outcome, levels = rev(c("Non-Endpoint","Endpoint")))) -> df

##作图
##配色方案，直接中原图中吸取过来的
pal = c("Endpoint"="#98d09d",
        "Non-Endpoint"="#f7a895")

ggplot() +
  ##百分比堆叠柱状图
  geom_col(data = df,
           aes(x=cluster_label,y=n,fill=Outcome),
           position = position_fill(),
           color = "grey20", linewidth=0.25) +
  scale_fill_manual(values = pal,
                    limits=c("Endpoint","Non-Endpoint"),
                    name = NULL,
                    guide = guide_legend(nrow=1,
                                         label.theme = element_text(size=14)))+
  scale_y_continuous(expand = expansion(mult = c(0.05, .12)),
                     breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c(0, 0.25, 0.5, 0.75, 1)*100) +
  labs(x = NULL,
       y = "Outcome distribution (%)"
       ) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"),
        axis.title = element_text(size=18),
        axis.text = element_text(size=16),
        plot.title = element_text(size=24),
        plot.title.position ="plot") -> p1

p1