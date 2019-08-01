# ---
# title: "EAA_6_run_graph_program"
# author: "wangbinzjcc@qq.com"
# date: "2019/7/3"
# output: html_document
# ---








#```{r}
rm(list = ls())
#```




## save_persp_pdf_FUN

#```{r}

save_persp_pdf_FUN <- function(
  dat_0 = dat_0,
  plot_name = "bci",
  theta_set = c(born = 320,  clust = 310, diedS = 310, diedL = 310, growth = 320),
  phi_set = c(born = 30,  clust = 30, diedS = 30, diedL = 30, growth = 30),
  k_set = c(born = 12,  clust = 12, diedS = 12, diedL = 12, growth = 5)
  ) {
  
  
  data_type <- as.character(unique(dat_0[, "data_type"]))
  
  p_list <- vector(mode = "list", length = length(data_type))
  
  names(p_list) <- data_type
  
  #
  
  for(i in data_type) {
    
    condition <- switch(EXPR = i, growth = c(1, 5), c(1, 4))
    
    theta <- switch(EXPR = i,
                    born = theta_set[1],  clust = theta_set[2],
                    diedL = theta_set[3], diedS = theta_set[4],
                    growth = theta_set[5])
    
    phi <- switch(EXPR = i,
                  born = phi_set[1],  clust = phi_set[2],
                  diedL = phi_set[3], diedS = phi_set[4], 
                  growth = phi_set[5])
    
    gam_k <- switch(EXPR = i,
                    born = k_set[1],  clust = k_set[2],
                    diedL = k_set[3], diedS = k_set[4], 
                    growth = k_set[5])
    
    
    p_j <- vector(mode = "list", length = length(condition))
    
    names(p_j) <- condition
    
    for(j in condition) {
      
      dat_1 <- dat_1_FUN(
        dat_0 = dat_0, 
        plot_name = plot_name,
        data_type = i,
        condition = j)
      
      dat_1[, "annul_dist"] <- 
        as.numeric(as.character(dat_1[, "annul_dist"]))
      
      gam_0 <- mgcv::gam(
        formula = graph_value ~ s(annul_dist, k = gam_k) + 
          s(phyl_dist, k = gam_k),
        family = gaussian(link = "identity"),
        data = dat_1)
      
      predict_values <- predict_gam_FUN(
        gam_0 = gam_0, ngrid = 50)
      
      main <- sprintf(
        fmt = "%s, %s, condition %s",
        plot_name, i, j)
      
      #
      pdf(file = sprintf(fmt = "pdf_save//persp_%s.pdf", main),
          width = 16 / 2.54, height = 16 / 2.54)
      
      par(mar = c(3, 2, 2, 0))
      
      persp_gam_graph_FUN(
        predict_values = predict_values, 
        theta = theta, phi = phi,
        main = main,
        xlab = "Phys distance (m)",
        ylab = "phylo distance (Ma)", 
        zlab = paste("\n", "z-values",  sep = "")
      )
      
      dev.off()
      #
    }
  }
}


#```











## Read R Code
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")
# dir()
source("EAA_4_graph_program_gam_ggplot_persp_rgl_20190703_1616.R")

#```



## dat_0
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas\\EAA_results")

load("dat_0_pnas_20190703.save")

dat_0 <- dat_0

#```



##  Run "save_persp_pdf_FUN"   "bci"

#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")

plot_name = "bci"

theta_set = c(born = 320, clust = 310, diedS = 310, diedL = 310, growth = 320)

phi_set = c(born = 30, clust = 30, diedS = 30, diedL = 30, growth = 30)

k_set = c(born = 12, clust = 12, diedS = 12, diedL = 12, growth = 5)

#
save_persp_pdf_FUN(dat_0, plot_name, theta_set, phi_set, k_set)

#

#```














