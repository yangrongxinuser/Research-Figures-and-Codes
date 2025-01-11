library(gridExtra)
library(ggplot2)
library(reshape2)

install.packages("wesanderson")
library(wesanderson)

install.packages("MetBrewer")
library(MetBrewer)

install.packages("effsize")
library(effsize)

install.packages("here")
library(here)

library('readxl')

install.packages("matrixStats")
library(matrixStats)

cohens_d <- function(x, y, DIM=1, SIGN=TRUE, na.rm=TRUE) {
  #
  # Function will compute cohen's d effect size.
  # Generalized to work on either matrices, data frames, vectors
  #
  # INPUT
  #	x <- matrix or numeric vector
  #	y <- matrix or numeric vector
  #	DIM <- specify the dimension which samples are along
  #
  # Example usage
  #
  # x <- cbind(rnorm(100,0,1), rnorm(100,0,1), rnorm(100,0,1))
  # y <- cbind(rnorm(100,1,1), rnorm(100,2,1), rnorm(100,3,1))
  # d <- cohens_d(x, y, 1)
  #
  # written by mvlombardo - 28.08.2015
  #
  
  library(matrixStats)
  

  x <- as.matrix(x)
 
  
  
  y <- as.matrix(y)
  # if
  
  if (na.rm==TRUE){
    missingValDecision = TRUE
  } else {
    missingValDecision = FALSE
  }
  
  # n-1 for x and y
  lx <- dim(x)[DIM]-1
  ly <- dim(y)[DIM]-1
  
  # if samples are along the rows
  if (DIM==1){
    if (SIGN){
      # mean difference (numerator)
      md <- colMeans(x, na.rm = missingValDecision) - colMeans(y, na.rm = missingValDecision)
    } else{
      # mean difference (numerator)
      md <- abs(colMeans(x, na.rm = missingValDecision) - colMeans(y, na.rm = missingValDecision))
    }# if (SIGN)
    # pooled variance (denominator), but before any sqrt is done
    csd <- (lx * rowVars(t(x),na.rm = missingValDecision)) + (ly * rowVars(t(y), na.rm = missingValDecision))
    
    # else if samples are along the columns
  } else if (DIM==2){
    if (SIGN){
      # mean difference (numerator)
      md <- rowMeans(x, na.rm = missingValDecision) - rowMeans(y, na.rm = missingValDecision)
    } else{
      # mean difference (numerator)
      md <- abs(rowMeans(x, na.rm = missingValDecision) - rowMeans(y, na.rm = missingValDecision))
    }# if (SIGN)
    # pooled variance (denominator), but before any sqrt is done
    csd <- lx * rowVars(x, na.rm = missingValDecision) + ly * rowVars(y, na.rm = missingValDecision)
  }# end if
  
  # divide pooled variance by sum of n-1 for x and y and then square root it
  csd <- sqrt(csd/(lx + ly))
  # compute cohen's d
  cd  <- md/csd
}# end cohens_d <- function(x, y, DIM)

data <- read_excel("ÏäµãÍ¼Êý¾Ý.xlsx")

for (i in (1:length(data$cluster_label))) {
  if (data[i, 'cluster_label'] == '1'){
    data[i,'clust_dom'] = "Excellent"
  }
  else if (data[i, 'cluster_label'] == '2'){
    data[i,'clust_dom'] = "Fine"
  }
  else if (data[i, 'cluster_label'] == '3'){
    data[i,'clust_dom'] = "Poor"
  }
  else if (data[i, 'cluster_label'] == '4'){
    data[i,'clust_dom'] = "Terrible"
  }
}

data$clust_dom = as.factor(data$clust_dom)
data$clust_dom <- factor( data$clust_dom,levels = c('Excellent','Fine','Poor','Terrible'));

# define colors
pink = c('#980043',"#DD1C77","#C994C7","#E7E1Ef")
blue = c('#08519c',"#3182bd","#9ecae1","#deebf7")
green = c('#006d2c','#31a354','#a1d99b','#e5f5e0')
yellow = c('#ff9900','#ffcc33','#ffff00','#ffff99')
col_list = list(pink,blue,green,yellow)

dom_list = c('Excellent','Fine','Poor','Terrible')
var_list = c('uPro', 'uRBC', 'eGFR')
title_list = c('uPro', 'uRBC', 'eGFR')

heat = list()

for (i in (1:3) ){
  var2use = var_list[i]
  #print(var2use)
  color = col_list[[i]]
  title = title_list[i]
  data2use <- data
  vector <- c()
  index <- 0
  group_order <- c('Excellent','Fine','Poor','Terrible')
  # compute cohens'd
  for (A in group_order){
    for (B in group_order){
      index <- index + 1
      C_d <- cohens_d(data2use[data2use$clust_dom==A,var2use],
                      data2use[data2use$clust_dom==B,var2use], 
                      SIGN=FALSE)
      vector[index] <- round(C_d,digits=2)
    }}
  
  m = matrix(vector,4)
  print(m)
  
  ggheatmap = ggplot(melt(m), aes(Var1,Var2, fill=value)) + 
    scale_x_discrete(name="cluster_label1",labels=c('Excellent','Fine','Poor','Terrible')) + 
    scale_y_discrete(name="cluster_label1",labels=c('Excellent','Fine','Poor','Terrible')) + 
    geom_raster() +
    scale_fill_gradient2(low = color[4], high = color[1],   guide = "colourbar",name=NULL) +
    theme_minimal()#+
  theme(axis.text.x = element_blank())#element_text(angle = 45, vjust = 1, 
  #size = 14, hjust = 1)) +
  #coord_fixed()
  
  ggheatmap = ggheatmap +
    geom_text(aes(Var1, Var2, label = value), color = "black", size = 10) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank())+
    guides(fill = guide_colorbar(barwidth = 1, barheight = 10,
                                 title.position = "top", title.hjust = 0, title="d"))
  
  # show here the heatmaps to check them
  #print(ggheatmap)
  # eventually save them on a pdf file
  #ggsave(paste(plot_path,'/',db_type,'_heatmap_',var2use,".pdf", sep=""),width = 10,height = 9)
  heat[[i]]=ggheatmap
}
heat[[3]]