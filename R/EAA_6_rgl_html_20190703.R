# ---
# title: "EAA_6_run_graph_program"
# author: "wangbinzjcc@qq.com"
# date: "2019/7/3"
# output: html_document
# ---


#```{r}
rm(list = ls())
#```

## Read R Code
#```{r}
setwd("F:\\porjects-wangbin\\EAA_pnas")

require(rgl)

require(mgcv)

source("EAA_4_graph_program_gam_ggplot_persp_rgl_20190703_1616.R")

#```



## dat_0
#```{r}
setwd("F:\\porjects-wangbin\\EAA_pnas\\EAA_results")

load("dat_0_pnas_20190703.save")

dat_0 <- dat_0

#```



# Parameters setting
#```{r}
setwd("F:\\porjects-wangbin\\EAA_pnas")

plot_name = "bci"

k_set = c(born = 7,  clust = 7, diedS = 7, diedL = 7, growth = 7)

#```



# Run "rgl_gam_graph_FUN"
#```{r}

data_type <- as.character(unique(dat_0[, "data_type"]))

for(i in data_type) {
  
  condition <- switch(EXPR = i, growth = c(1, 5), c(1, 4))
  
  gam_k <- switch(EXPR = i,
    born = k_set[1],  clust = k_set[2],
    diedL = k_set[3], diedS = k_set[4], 
    growth = k_set[5])
    
  for(j in condition) {
  
  dat_1 <- dat_1_FUN(
    dat_0 = dat_0, 
    plot_name = plot_name,
    census_interval = 1,
    data_type = i,
    condition = j)
    
  dat_1[, "annul_dist"] <- as.numeric(as.character(dat_1[, "annul_dist"]))
    
  gam_0 <- mgcv::gam(
    formula = graph_value ~ s(annul_dist, k = gam_k) + 
      s(phyl_dist, k = gam_k),
    family = gaussian(link = "identity"),
    data = dat_1)
    
  predict_values <- 
    predict_gam_FUN(gam_0 = gam_0, ngrid = 20)
    
  ###
    
  main <- sprintf(
      fmt = "%s, %s, condition %s",
      plot_name, i, j)
    
  require(rgl)
    
  rgl_gam_graph_FUN(
    predict_values = predict_values,
    main = main,
    xlab = "Phys distance (m)",
    ylab = "phylo distance (Ma)",
    zlab = "z-values",
    axes_type = c(1, 2)[1])
    
  url <- writeWebGL(
    filename = sprintf(fmt = "pdf_save//rgl_%s.html", main),
    width = 900, height = 900)
    
  browseURL(url = url)

    } 
  }
#```
















