
install.packages("ggforce")

library(ggforce)

install.packages("readxl")

library('readxl')


library(ggplot2)


library(ggalluvial)

install.packages("ggalluvial")







df <- read_excel("验证集病人桑葚图.xlsx")

df<-as.data.frame(df)

df

ggplot(as.data.frame(df),
       aes(axis1 = cluster_label, axis2 = Outcome)) +
  geom_alluvium(aes(fill = Outcome))+
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Group", "Outcome"),expand = c(.2, .05))+
  xlab("Alluvia across strata")+
  theme_minimal() +
  ggtitle("Alluvium Plot:",
          "Validation Group")