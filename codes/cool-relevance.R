# 加载R包
library(tidyverse)
library(vegan)
library(ggcor)



library('readxl')

m1 <- read_excel("肠道菌群作图数据.xlsx")
m2 <- read_excel("临床分类数据.xlsx")




# mantel test
mantel <- mantel_test(m2, m1,  # 传入需要检测的两个矩阵
                      # 这里很多人不明白，其实意思就是把临床数据分成5类：
                      spec.select = list(Demographic_Characteristics = 1:2,
                                         
                                         
                                         Proteinuria_Hematuria = 12:13,
                                         Pathological_Lesions = 3:9,
                                         Lipid_Metabolism = 10:11,
                                         Renal_Function = 14
                      )) %>% 
  # 下面就是检测的一些阈值，这个保持不变即可：
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

#################### 花式修改 ####################
quickcor(m1, type = "lower", show.diag = FALSE) +  # 传入的第一个数据是菌群相关数据，type参数也可以是upper，
  geom_star(n=5) +  # 把正方形改成五角星，n表示多边形角的个数，同样也可以改成其它形状
  # 这里的data传入mantel检验的结果：方块的颜色根据pd变化，大小根据rd变化；
  anno_link(aes(colour = pd, size = rd), data = mantel,
            curvature = -0.2) +  # 变曲线
  scale_size_manual(values = c(0.5, 1, 2))+  # 大小变化范围
  # 修改颜色属性：gradientn -- 渐变色；manul -- 分类变量颜色
  scale_fill_gradientn(values = seq(0,1,0.2),
                       colors = c("#0b3b71","#569cc7", "#f5f4f4", "#d05646","#610214")) +
  scale_colour_manual(values = c("#d85c01", "#29d300", "#A2A2A288")) +
  # 图例
  guides(size = guide_legend(title = "Mantel's r",
                             order = 2),  # 图例排序
         colour = guide_legend(title = "Mantel's p", 
                               order = 1),  # 图例排序
         fill = guide_colorbar(title = "Pearson's r", order = 3))

