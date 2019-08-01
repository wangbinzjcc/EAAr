# ---
# title: "Untitled"
# author: "wangbinzjcc@qq.com"
# date: "2019/06/27"
# output: html_document
# editor_options: 
#   chunk_output_type: console
# ---


#```{r}
rm(list = ls())
#```


## package
#```{r}
setwd("F:\\porjects-wangbin\\EAA_pnas")

require(ggplot2)

# devtools::install_github("thomasp85/patchwork")
require(patchwork)

source("EAA_4_graph_program_gam_ggplot_persp_rgl_20190703_1616.R")
#```


# dat_0
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas\\EAA_results")

load(file = "dat_0_pnas_20190703.save")

dat_0 <- dat_0
#```




## patchwork_detailed_values_FUN
#```{r}

patchwork_detailed_values_FUN <- function(
  dat_0, plot_name,
  gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)) {
  
  #
    data_types <- unique(dat_0[, "data_type"])
    
    p_j_list <- vector(mode = "list", length = length(data_types))
    
    names(p_j_list) <- data_types
    
    for(j in data_types) {
    
      k_0 <- switch(EXPR = j, born = gam_k[1],  clust = gam_k[2], diedL = gam_k[3], diedS = gam_k[4], growth = gam_k[5])
        
      value_names <- c("real_value", "relative_value", "mean_shuffled", "sd_shuffled", "graph_value")
      
      p_i_list <- vector(mode = "list", length = length(value_names))
    
      names(p_i_list) <- value_names
     
    for(i in value_names) {
      
      condition <- switch(EXPR = j, growth = 0:5, 1:4)
      
      dat_1 <- dat_1_FUN(
        dat_0 = dat_0, plot_name = plot_name,
        data_type = j, condition = condition,
        annul_interval = 1)
       
      p_i_list[[i]] <- ggplot_graph_FUN(
        dat_1 = dat_1,  gam_k = k_0,
        graph_name = i)
    
      p_i_list[[i]] <- p_i_list[[i]] + annotate("text", label = i, x = 100, y = 0, size = 6, colour = "black")
    
    }
    
    p_j_list[[j]] <- p_i_list
    }
    
  # return
  p_j_list
  }

#```




## graph_rbind_FUN
#```{r}
require(patchwork)

graph_rbind_FUN <- function(p_j_list) {
      
    # 
    p_list <- p_j_list[["born"]]
    
    p_born <-  p_list[["real_value"]] + p_list[["mean_shuffled"]] + p_list[["relative_value"]] + p_list[["sd_shuffled"]] + p_list[["graph_value"]] + plot_layout(ncol = 5)
    
    #
    
    p_list <- p_j_list[["clust"]]
    
    p_clust <-  p_list[["real_value"]] + p_list[["mean_shuffled"]] + p_list[["relative_value"]] + p_list[["sd_shuffled"]] + p_list[["graph_value"]] + plot_layout(ncol = 5)
    
    #
    
    p_list <- p_j_list[["diedS"]]
    
    p_diedS <-  p_list[["real_value"]] + p_list[["mean_shuffled"]] + p_list[["relative_value"]] + p_list[["sd_shuffled"]] + p_list[["graph_value"]] + plot_layout(ncol = 5)
    
    #
    
    p_list <- p_j_list[["diedL"]]
    
    p_diedL <-  p_list[["real_value"]] + p_list[["mean_shuffled"]] + p_list[["relative_value"]] + p_list[["sd_shuffled"]] + p_list[["graph_value"]] + plot_layout(ncol = 5)
    
    #
    
    p_list <- p_j_list[["growth"]]
    
    p_growth <-  p_list[["real_value"]] + p_list[["mean_shuffled"]] + p_list[["relative_value"]] + p_list[["sd_shuffled"]] + p_list[["graph_value"]] + plot_layout(ncol = 5)
    
    #
    
    p_all <- p_born / p_clust / p_diedS / p_diedL / p_growth
    
    # return
    p_all
}

#```













## detailed graphs "windriver"
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")
plot_name = "windriver"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)

#

p_j_list <- patchwork_detailed_values_FUN(dat_0, plot_name, gam_k)


p_all <- graph_rbind_FUN(p_j_list)

filename = sprintf(
  fmt = "pdf_save//patchwork_detailed_values_%s.pdf", plot_name)

ggsave(
  filename = filename,
  plot = p_all, device = "pdf",
  width = 8 * 5, height = 8 * 5, units = "cm")


#```



## detailed graphs "mudumalai"
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")
plot_name = "mudumalai"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)

#

p_j_list <- patchwork_detailed_values_FUN(dat_0, plot_name, gam_k)


p_all <- graph_rbind_FUN(p_j_list)

filename = sprintf(
  fmt = "pdf_save//patchwork_detailed_values_%s.pdf", plot_name)

ggsave(
  filename = filename,
  plot = p_all, device = "pdf",
  width = 8 * 5, height = 8 * 5, units = "cm")


#```





## detailed graphs wytham
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")
plot_name = "wytham"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)

#

p_j_list <- patchwork_detailed_values_FUN(dat_0, plot_name, gam_k)

p_all <- graph_rbind_FUN(p_j_list)

filename = sprintf(
  fmt = "pdf_save//patchwork_detailed_values_%s.pdf", plot_name)

ggsave(
  filename = filename,
  plot = p_all, device = "pdf",
  width = 8 * 5, height = 8 * 5, units = "cm")


#```



## detailed graphs gutianshan
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")
plot_name = "gutianshan"
dat_0 = dat_0
gam_k = c(born = 6,  clust = 6, diedL = 5, diedS = 5, growth = 5)

#

p_j_list <- patchwork_detailed_values_FUN(dat_0, plot_name, gam_k)

p_all <- graph_rbind_FUN(p_j_list)

filename = sprintf(
  fmt = "pdf_save//patchwork_detailed_values_%s.pdf", plot_name)

ggsave(
  filename = filename,
  plot = p_all, device = "pdf",
  width = 8 * 5, height = 8 * 5, units = "cm")


#```



## detailed graphs nonggang
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")
plot_name = "nonggang"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)

#

p_j_list <- patchwork_detailed_values_FUN(dat_0, plot_name, gam_k)

p_all <- graph_rbind_FUN(p_j_list)

filename = sprintf(
  fmt = "pdf_save//patchwork_detailed_values_%s.pdf", plot_name)

ggsave(
  filename = filename,
  plot = p_all, device = "pdf",
  width = 8 * 5, height = 8 * 5, units = "cm")


#```


## detailed graphs heishiding
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")
plot_name = "heishiding"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)

#

p_j_list <- patchwork_detailed_values_FUN(dat_0, plot_name, gam_k)

p_all <- graph_rbind_FUN(p_j_list)

filename = sprintf(
  fmt = "pdf_save//patchwork_detailed_values_%s.pdf", plot_name)

ggsave(
  filename = filename,
  plot = p_all, device = "pdf",
  width = 8 * 5, height = 8 * 5, units = "cm")


#```



## detailed graphs "bci"
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")
plot_name = "bci"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)

#

p_j_list <- patchwork_detailed_values_FUN(dat_0, plot_name, gam_k)

p_all <- graph_rbind_FUN(p_j_list)

filename = sprintf(
  fmt = "pdf_save//patchwork_detailed_values_%s.pdf", plot_name)

ggsave(
  filename = filename,
  plot = p_all, device = "pdf",
  width = 8 * 5, height = 8 * 5, units = "cm")


#```


## detailed graphs pasoh
#```{r}

setwd("F:\\porjects-wangbin\\EAA_pnas")
plot_name = "pasoh"
dat_0 = dat_0
gam_k = c(born = 5,  clust = 5, diedL = 4, diedS = 4, growth = 3)

#

p_j_list <- patchwork_detailed_values_FUN(dat_0, plot_name, gam_k)

p_all <- graph_rbind_FUN(p_j_list)

filename = sprintf(
  fmt = "pdf_save//patchwork_detailed_values_%s.pdf", plot_name)

ggsave(
  filename = filename,
  plot = p_all, device = "pdf",
  width = 8 * 5, height = 8 * 5, units = "cm")


#```

