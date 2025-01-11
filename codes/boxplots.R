library(gridExtra)
library(ggplot2)
library(reshape2)

install.packages("wesanderson")
library(wesanderson)

install.packages("MetBrewer")
library(MetBrewer)

library('readxl')

data <- read_excel("k-means-ÍøÂç.xlsx")
names(data)

for (i in (1:length(data$cluster_label))) {
  if (data[i, 'cluster_label'] == '1'){
    data[i,'clust_dom'] = "Cluster 1"
  }
  else if (data[i, 'cluster_label'] == '2'){
    data[i,'clust_dom'] = "Cluster 2"
  }
  else if (data[i, 'cluster_label'] == '3'){
    data[i,'clust_dom'] = "Cluster 3"
  }
  else if (data[i, 'cluster_label'] == '4'){
    data[i,'clust_dom'] = "Cluster 4"
  }
}

data$clust_dom = as.factor(data$clust_dom)
data$clust_dom <- factor( data$clust_dom,levels = c('Cluster 1','Cluster 2','Cluster 3','Cluster 4'));

# define colors
pink = c('#980043',"#DD1C77","#C994C7","#E7E1Ef")
blue = c('#08519c',"#3182bd","#9ecae1","#deebf7")
green = c('#006d2c','#31a354','#a1d99b','#e5f5e0')
yellow = c('#ff9900','#ffcc33','#ffff00','#ffff99')
col_list = list(pink,blue,green,yellow)

dom_list = c('Cluster 1','Cluster 2','Cluster 3','Cluster 4')
var_list = c('uPro', 'uRBC', 'eGFR')
title_list = c('uPro', 'uRBC', 'eGFR')

plt <- list()
# looping on VABS variables to 
for (j in (1:3)){
  #define varible, color to use and titels to the plots
  var2use = var_list[j]
  col_name = col_list[j]
  color = col_list[[j]] 
  title = title_list[j]
  
  p = ggplot(data = data, 
             aes_string(x = 'clust_dom',
                        y = var2use,
                        color='clust_dom')) 
  p = p + geom_jitter() + geom_boxplot(fill = NA, colour = "#000000", outlier.shape = NA)
  p = p + scale_colour_manual(values = color)
  p = p + guides(colour = FALSE)
  p = p + xlab("") + ylab("")
  p = p + ggtitle(title_list[j]) 
  p = p + theme_bw() 
  p = p + theme(plot.title = element_text(hjust = 0.5))
  ## changed here
  p = p + theme(axis.text.x = element_text(face="bold", size=15))
  p = p + theme(axis.text.y = element_text(face="bold", size=15))
  p = p + scale_y_continuous( )
  p = p + theme(axis.text=element_text(size=10),
                axis.title=element_text(size=8,face="bold"))
  print(p)
  # evantually save the plot
  #ggsave(paste(plot_path,'/',db_type,'_total_',var2use,".pdf", sep=""))
  plt[[j]] = p
}
  
  
print(plt[[1]])