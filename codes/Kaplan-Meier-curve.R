if(!requireNamespace("survival",quietly = T))
  install.packages("survival")
install.packages("survminer")
library(survival)
library(survminer)
install.packages("survminer")


install.packages("readxl")

library('readxl')

lung<- read_excel("第四组.xlsx")

lung

#下面开始进行数据的检验
fit <- survfit(Surv(time, event) ~ cluster_label, data = lung)
print(fit)


ggsurvplot(fit,
           data = lung,  # 指定变量数据来源
           #fun = "cumhaz",# "event"绘制累积事件(f(y)=1-y)，# "cumhaz"绘制累积危害函数(f(y)=-log(y));# "pct"绘制生存概率(百分比)。
           linetype = 1, # 根据分层更改线型c(0,1) or  c("solid", "dashed") or "strata"
           #surv.median.line = "hv", # 同时显示垂直和水平参考线 即增加中位生存时间   可选 "none"、"hv"、"h"、"v"
           palette = "lancet" ,#定义颜色 可选调色板有 "hue" "grey","npg","aaas","lancet","jco", "ucscgb","uchicago","simpsons"和"rickandmorty".c("red","blue")
           #add.all = TRUE, # 添加总患者生存曲线
           
           #图标题和坐标轴标签 图例标题和位置
           xlab = "Time(Months)", # 指定x轴标签
           ylab = "Survival probability", # 指定y轴标签
           title = "Cluster 4",# 指定title标签
           # 指定图例位置 "top"(默认),"bottom","left","right","none"
           # 设置图例标题
           legend.labs = c("SC","GS","II") ,# 指定图例分组标签
           #legend.labs = c("SC","IST"),
           
           
           #坐标轴范围、刻度间距
           break.x.by = 24,# 设置x轴刻度间距
           #xlim = c(0, 1180),#设置x轴范围
           ylim = c(0.0, 1),#设置y轴范围
           break.y.by = .25,# 设置y轴刻度间距
           axes.offset = T, # 逻辑词，默认为TRUE。为FALSE，则生存曲线图的坐标轴从原点开始。
           
           
           
           #P值文本大小和位置
           pval = TRUE, #log 秩检验  看两个曲线之间有无显著区别
           pval.size = 4,# 指定p值文本大小的数字，默认为 5。
           pval.coord = c(0.1,.1),# 长度为2的数字向量，指定p值位置x、y，如pval.coord=c(x,y)。
           
           
           
           
           #生存表
           risk.table = T, # 添加风险表""absolute" 显示处于风险中的绝对数量；# "percentage" 显示处于风险中的百分比数量# "abs_pct" 显示处于风险中的绝对数量和百分比
           risk.table.col = c(1),#"strata", # 根据分层更改风险表颜色 c(1, 2)  or c("solid", "dashed").
           fontsize = 4,# 指定风险表和累积事件表的字体大小。
           tables.y.text = T,# 逻辑词，默认显示生存表的y轴刻度标签；为FALSE则刻度标签被隐藏
           tables.y.text.col = F, # 逻辑词，默认FALSE；为TRUE，则表的y刻度标签将按strata着色。
           tables.height = .25,# 指定所有生存表的高度，数值在0-1之间，默认为0.25.
           
           #累积事件表
           #cumevents = T,#累计死亡人数
           #cumevents.height = .25,
           
           #累积删失表
           #cumcensor =T,#累计删失人数
           #cumcensor.height = .3,
           
           ggtheme = theme_survminer(), #图的主题
           tables.theme = theme_bw()#下面图的主题
)
