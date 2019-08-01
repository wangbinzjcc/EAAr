# ---
# title: "all_plots_patchwork_EAA_20190629"
# author: "wangbinzjcc@qq.com"
# date: "2019/6/29"
# output: html_document
# ---



#```{r}
rm(list = ls())
#```


#```{r}
require(ggplot2)

# devtools::install_github("thomasp85/patchwork")
require(patchwork)


setwd("F:\\porjects-wangbin\\EAA_pnas")

source("EAA_4_graph_program_gam_ggplot_persp_rgl_20190703_1616.R")

#```


# dat_0_new
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas\\EAA_results")

load(file = "dat_0_pnas_20190703.save")

dat_0 <- dat_0

# unique(dat_0[, "plot_name"])
#```





# patchwork_graph_FUN -------------------------

#```{r}

patchwork_graph_FUN <- function(
  dat_0, plot_name,
  gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)) {
  #
  
  data_type <- unique(dat_0[, "data_type"])
  
  p_list <- vector(mode = "list", length = length(data_type))
  
  names(p_list) <- data_type
  
  #---
  
  for(i in data_type) {
  
    condition <- switch(EXPR = i, growth = 1:5, 1:4)
    
    dat_1 <- dat_1_FUN(
      dat_0 = dat_0, plot_name = plot_name,
      data_type = i,
      condition = condition,
      annul_interval = 1)
    
    k_0 <- switch(
      EXPR = i,
      born = gam_k[1], clust = gam_k[2], diedS = gam_k[3],
      diedL = gam_k[4], growth = gam_k[5])
     
    p_list[[i]] <- ggplot_graph_FUN(dat_1 = dat_1,  gam_k = k_0)
  
  }
  
  # return
  p_list

}
#```





# print graph "mudumalai" 
#```{r}
require(patchwork)

plot_name = "mudumalai"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 4)

p_list <- patchwork_graph_FUN(dat_0, plot_name,  gam_k)

p_mudumalai <- p_list[["born"]] +  p_list[["clust"]] +  p_list[["diedS"]]+  p_list[["diedL"]] + p_list[["growth"]] +  plot_layout(ncol = 5)

# print(p_mudumalai)
#```


# print graph "windriver" 
#```{r}
require(patchwork)

plot_name = "windriver"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)

p_list <- patchwork_graph_FUN(dat_0, plot_name,  gam_k)

p_windriver <- p_list[["born"]] +  p_list[["clust"]] +  p_list[["diedS"]]+  p_list[["diedL"]] + p_list[["growth"]] +  plot_layout(ncol = 5)

# print(p_windriver)
#```


# print graph "wytham"
#```{r}
require(patchwork)

plot_name = "wytham"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)

p_list <- patchwork_graph_FUN(dat_0, plot_name,  gam_k)

p_wytham <- p_list[["born"]] +  p_list[["clust"]] +  p_list[["diedS"]]+  p_list[["diedL"]] + p_list[["growth"]] +  plot_layout(ncol = 5)

# print(p_wytham)
#```



# print graph nonggang
#```{r}
require(patchwork)

plot_name = "nonggang"
dat_0 = dat_0
gam_k = c(born = 7,  clust = 7, diedL = 5, diedS = 5, growth = 4)

p_list <- patchwork_graph_FUN(dat_0, plot_name,  gam_k)

p_nonggang <- p_list[["born"]] +  p_list[["clust"]] +  p_list[["diedS"]]+  p_list[["diedL"]] + p_list[["growth"]] +  plot_layout(ncol = 5)

# print(p_nonggang)
#```


# print graph heishiding
#```{r}
require(patchwork)

plot_name = "heishiding"
dat_0 = dat_0
gam_k = c(born = 7,  clust = 7, diedL = 5, diedS = 5, growth = 5)

p_list <- patchwork_graph_FUN(dat_0, plot_name,  gam_k)

p_heishiding <- p_list[["born"]] +  p_list[["clust"]] +  p_list[["diedS"]]+  p_list[["diedL"]] + p_list[["growth"]] +  plot_layout(ncol = 5)

# print(p_heishiding)
#```



# print graph pasoh
#```{r}
require(patchwork)

plot_name = "pasoh"
dat_0 = dat_0
gam_k = c(born = 7,  clust = 7, diedL = 5, diedS = 5, growth = 5)

p_list <- patchwork_graph_FUN(dat_0, plot_name,  gam_k)
p_list[["growth"]] <-  p_list[["growth"]] + ylim(-20, 5)

p_pasoh <- p_list[["born"]] +  p_list[["clust"]] +  p_list[["diedS"]]+  p_list[["diedL"]] + p_list[["growth"]] +  plot_layout(ncol = 5)

# print(p_pasoh)
#```

# print graph "bci"
#```{r}
require(patchwork)

plot_name = "bci"
dat_0 = dat_0
gam_k = c(born = 7,  clust = 7, diedL = 5, diedS = 5, growth = 5)

p_list <- patchwork_graph_FUN(dat_0, plot_name,  gam_k)
p_list[["growth"]] <-  p_list[["growth"]] + ylim(-20, 5)

p_bci <- p_list[["born"]] +  p_list[["clust"]] +  p_list[["diedS"]]+  p_list[["diedL"]] + p_list[["growth"]] +  plot_layout(ncol = 5)

# print(p_bci)
#```



# save all graphs
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")

require(patchwork)

p_all_plot <- p_mudumalai / p_windriver / p_wytham / p_nonggang /
  p_heishiding / p_pasoh / p_bci

ggsave(
  filename = "pdf_save//all_plots_patchwork_EAA_graphs.pdf",
  plot = p_all_plot, device = "pdf",
  width = 8 * 5, height = 8 * 7, units = "cm")

#```




