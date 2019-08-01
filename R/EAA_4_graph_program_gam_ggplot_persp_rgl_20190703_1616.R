# ---
# title: "EAA_program_gam_ggplot_persp_rgl"
# author: "wangbinzjcc@qq.com"
# date: "2019_6_27 1543"
# output: html_document
# ---


# devtools::install_github("thomasp85/patchwork")

#```{r}
dat_1_FUN <- function(
  dat_0 = dat_0, 
  plot_name = c("bci", "nonggang")[1],
  data_type = c("clust", "growth", "born", "died", "diedS", "diedL")[1],
  census_interval = list(NA, 1:5)[[1]],
  condition = list(NA, 1:5)[[1]],
  annul_interval = list(NA, 1:5)[[1]]) {
  

  if(all(plot_name %in% unique(dat_0[, "plot_name"])) == T) {NULL} else {
    stop(sprintf(fmt = "'plot_name' must be one of: %s",
                 paste(unique(dat_0[, "plot_name"]), collapse = ", ")))}

  if(all(data_type %in% unique(dat_0[, "data_type"])) == T) {NULL} else {
    stop(sprintf(fmt = "'data_type' must be one of: %s",
                 paste(unique(dat_0[, "data_type"]), collapse = ", ")))}
  
  dat_1 <- dat_0[
    dat_0[, "plot_name"] == plot_name & 
      dat_0[, "data_type"] == data_type, ]
  
  if(all(is.na(census_interval)) == T) {NULL} else {
    index <- census_interval %in% unique(dat_1[, "census_interval"])
    if(all(index) == T) {
      dat_1 <- dat_1[dat_1[, "census_interval"] %in% census_interval, ]
    } else {
      stop(
        sprintf(fmt = "'census_interval' must be NA or a subset of: %s",
                paste(unique(dat_1[, "census_interval"]), collapse = ", ")))}
  }  
  
  
  if(all(is.na(condition)) == T) {NULL} else {
    index <- condition %in% unique(dat_1[, "condition"])
    if(all(index) == T) {
      dat_1 <- dat_1[dat_1[, "condition"] %in% condition, ]
    } else {
      stop(
        sprintf(fmt = "'condition' must be NA or a subset of: %s",
                paste(unique(dat_1[, "condition"]), collapse = ", ")))}
    }  
  
  if(all(is.na(annul_interval)) == T) {NULL} else {
    index <- annul_interval %in% unique(dat_1[, "annul_interval"])
    if(all(index) == T) {
        dat_1 <- dat_1[dat_1[, "annul_interval"] %in% annul_interval, ]
      } else {
        stop(
          sprintf(fmt = "'annul_interval' must be NA or a subset of: %s",
                  paste(unique(dat_1[, "annul_interval"]), collapse = ", ")))}
    }
  
  if(inherits(try(dat_1[, "sd_shuffled"], silent = T), "try-error") == F) {
    dat_1[, "graph_value"] <- round(x = dat_1[, "relative_value"] / dat_1[, "sd_shuffled"], digits = 3)
    } else {
      if(inherits(try(dat_1[, "z_score"], silent = T), "try-error") == F){ 
        dat_1[, "graph_value"] <- round(x = (dat_1[, "value"] / dat_1[, "z_score"]) * 1.96, digits = 3)
        } else {
        warning("Have not found dat_1[, 'sd_shuffled'] or dat_1[, 'z_score'].")
        }
    }
  
  dat_1[is.na(dat_1[, "graph_value"]) == T, "graph_value"] <- 0
  
  dat_1[, "annul_dist"] <- round(x = dat_1[, "annul_dist"], digits = 2)
  
  dat_1[, "phyl_dist"] <- round(x = dat_1[, "phyl_dist"] / 2)
  
  dat_1[, "plot_name"] <- as.factor(as.character(dat_1[, "plot_name"]))
  
  dat_1[, "condition"] <- as.factor(dat_1[, "condition"])
  
  # return
  dat_1
}

#```





#```{r}

ggplot_graph_FUN <- function(
  dat_1 = dat_1_growth,
  gam_k = c(NA, 7)[2],
  graph_name = "graph_value") {
    
    require(ggplot2)
    
    if(is.na(gam_k) == T) {
      formula <- formula("y ~ s(x)")
    } else {
        if(is.numeric(gam_k) == T & gam_k > 0) {
          formula <- formula("y ~ s(x, k = gam_k)")
        } else {
          stop("'gam_k' must be NA or a positive integer.")
        }
      }
    
    data_type <- as.character(unique(dat_1[, "data_type"]))
    plot_name <- unique(dat_1[, "plot_name"])
    annulus <- unique(dat_1[, "annul_interval"])
    dat_1[, "graph_value"] <- dat_1[, graph_name]

    y_int <- switch(
      EXPR = graph_name,
      graph_value = c(-1.96, 0, 1.96),  0)
    
    type_txt <- switch(
      EXPR = data_type,
      growth = "focal growth", born = "recruitment",
      clust = "clustering", died = "mortality",
      diedS = "mortality_S", diedL = "mortality_L")
    
    main_title <- sprintf(fmt = "%s, %s", type_txt, plot_name)
    
    sub_title <- sprintf(fmt = "annulus = %s, k = %s", annulus, gam_k)
    
    # return
    ggplot(data = dat_1,
                mapping = aes(phyl_dist, graph_value,
                              colour = condition))  +
      geom_hline(yintercept = y_int,
                 size = 0.8, color = "darkgoldenrod4") +
      
      geom_jitter(size = 1, width = 0, alpha = 2/10) +
      
      geom_smooth(method = 'gam',
                  formula = formula,
                  se = TRUE, level = 0.95, size = 2) +
      
      scale_colour_brewer(palette = "RdYlBu", direction = -1) +
      
      labs(title = main_title, subtitle = sub_title)  +
      
      theme(axis.title = element_blank(),
            axis.text = element_text(size = 10),
            title = element_text(size = 12.5),
            legend.position = "none")
    
}

# ```







## gam_FUN
#```{r }
gam_FUN <- function(dat_1){
  
  gam_0 <- try(
    expr = mgcv::gam(
      formula = graph_value ~ s(annul_dist) + 
        s(phyl_dist),
      family = gaussian(link = "identity"),
      data = dat_1),
    silent = T)
  
  if(inherits(gam_0, "try-error") == F) {
    cat("\n"); print(gam_0[["formula"]]) 
    #
    return(gam_0)
  } else {NULL}
  
  for(gam_k in 20:1) {
    
    gam_0 <- try(
      expr = mgcv::gam(
        formula = graph_value ~ s(annul_dist, k = gam_k) + 
          s(phyl_dist, k = gam_k),
        family = gaussian(link = "identity"),
        data = dat_1),
      silent = T)
    
    if(inherits(gam_0, "try-error") == T) {NULL} else {
      cat("\n")
      warning(sprintf(fmt = "gam_k was changed to %s.", gam_k))
      print(
        gsub(pattern = "gam_k", replacement = gam_k, 
             x = "graph_value ~ s(annul_dist, k = gam_k) + s(phyl_dist, k = gam_k)")
      )
      #
      return(gam_0)}
  }
  
}

#```



#```{r}
predict_gam_FUN <- function(gam_0 = gam_0, ngrid = 30) {
  
  xy_range <- gam_0[["model"]]
  
  annul_dist <- xy_range[["annul_dist"]]
  

  annul_dist <- as.numeric(as.character(annul_dist))
  annul_levels <- seq(from = min(annul_dist), to = max(annul_dist),
                      length.out = ngrid)
    
  xy_grid <- expand.grid(
    annul_dist = annul_levels,
    phyl_dist = seq(from = min(xy_range[["phyl_dist"]]),
                    to = max(xy_range[["phyl_dist"]]),
                    length.out = ngrid))
  xy_grid <- as.data.frame(x = xy_grid)
  
  fv <- mgcv::predict.gam(
    object = gam_0, 
    newdata = xy_grid,
    se.fit = T,
    type = "response")
  
  # return
  data.frame(
    annul_dist = xy_grid[, "annul_dist"],
    phyl_dist = xy_grid[, "phyl_dist"],
    fit = fv[["fit"]],
    fit_se = fv[["se.fit"]],
    fit_upper = fv[["fit"]] + 1.96 * fv[["se.fit"]],
    fit_lower = fv[["fit"]] - 1.96 * fv[["se.fit"]],
    num = 1:nrow(xy_grid)
  )
  
}

#```



#```{r}
persp_gam_graph_FUN <- function(
  predict_values = predict_values, 
  theta = 45, phi = 45,
  main = "bci, clustered, smallest focal trees",
  xlab = "annular distance (m)",
  ylab = "phylogenetic distance (Ma)", 
  zlab = "z-values") {
  
  
  z_mat <- tapply(
    X = predict_values[, "fit"], 
    INDEX = predict_values[, c("annul_dist", "phyl_dist")],
    FUN = mean)
  
  col_FUN <- function(z_mat){
    ncz <- ncol(z_mat)
    nrz <- nrow(z_mat)
    zfacet <- z_mat[-1, -1] + z_mat[-1, -ncz] + 
      z_mat[-nrz, -1] + z_mat[-nrz, -ncz]
    color <- heat.colors(n = prod(dim(zfacet)), alpha = 0.9)
    col <- color[as.numeric(as.factor(zfacet))]
    col[zfacet <= (1.96 * 4) & zfacet >= (-1.96 * 4)] <- "#CCCCCCE6"
    # 
    col
  }
  
  persp_FUN <- function(){
    col <- col_FUN(z_mat)
    persp(
      x = as.numeric(rownames(z_mat)), 
      y = as.numeric(colnames(z_mat)),
      z = z_mat, theta = theta, phi = phi,
      ticktype = "detailed",
      main = main, 
      xlab = paste("\n", xlab, sep = ""), 
      ylab = paste("\n", ylab, sep = ""), 
      zlab = zlab, 
      col = col,
      cex.lab = 1.8, cex.axis = 1.3,
      cex.main = 2, lwd = 1, 
      border = ggplot2::alpha(colour = "grey", alpha = 0.6))
  }
  
  
  #return
  persp_FUN()
}

#```




#```{r}

z_se_data_FUN <- function(predict_values){
  
  z_se_data <- rbind(predict_values, predict_values)
  
  z_se_data[1:nrow(predict_values), "fit"] <- z_se_data[1:nrow(predict_values), "fit_upper"]
  
  z_se_data[
    nrow(predict_values) + 1:nrow(predict_values), "fit"] <-
    z_se_data[
      nrow(predict_values) + 1:nrow(predict_values), "fit_lower"]
  
  z_se_data[, "annul_interval"] <- as.integer(as.factor(z_se_data[, "annul_dist"]))
  
  z_se_data[, "phyl_interval"] <- as.integer(as.factor(z_se_data[, "phyl_dist"])) 
  
  z_se_data <- z_se_data[order(z_se_data[, "num"]), ]
  
  # return
  z_se_data
}

#```




#```{r}
rgl_gam_graph_FUN <- function(
  predict_values = predict_values,
  main = "bci, clustered, smallest focal trees",
  xlab = "annular distance (m)",
  ylab = "phylogenetic distance (Ma)", 
  zlab = "z-values",
  axes_type = c(1, 2)[1]) {
  
  z_mat <- tapply(
    X = predict_values[, "fit"],
    INDEX = predict_values[, c("annul_dist", "phyl_dist")],
    FUN = mean)
  
  col_0 <- heat.colors(n = 100)[20:70]
  col <- rep(x = col_0,
             each = (prod(dim(z_mat))/length(col_0)) + 1)
  col <- col[as.numeric(as.factor(z_mat))]
  
  col[z_mat <= 1.96 & z_mat >= -1.96] <- "#4A708BE6"
  
  z_se_data <- z_se_data_FUN(predict_values)
  
  xi <- z_se_data[, "annul_interval"]
  t_x <- (max(xi) - min(xi)) / 10
  
  yi <- z_se_data[, "phyl_interval"]
  t_y <- (max(yi) - min(yi)) / 10
  
  t_z <- (max(z_mat) - min(z_mat)) / 10
  
  require(rgl)
  
  open3d()
  
  bg3d(color="gray")
  # 
  persp3d(x = 1:nrow(z_mat) / t_x,
          y = 1:ncol(z_mat) / t_y,
          z = z_mat / t_z,
          line_antialias = T,
          lwd = 4, 
          color= col,
          alpha = 0.8,
          front = "lines", back = "lines",
          lit = F, add = TRUE)
  
  col_1 <- rep(x = col, each = 2)
  col_2 <- rep(x = c("red", "blue"), times = length(col))
  col_2[which(col_1 == "#4A708BE6")] <- "#4A708BE6"
  
  segments3d(
    x = z_se_data[, "annul_interval"] / t_x, 
    y = z_se_data[, "phyl_interval"]/ t_y,
    z = z_se_data[, "fit"] / t_z,
    lwd = 3,
    color = col_2,
    alpha = 0.6,
    line_antialias = T)
  
  box3d()
  
  at_x <- round(seq(
    from = min(xi),
    to = max(xi),
    length.out = 10), 1)
  
  xj <- as.numeric(as.character(unique(z_se_data[, "annul_dist"])))
  
  labels_x <- round(seq(
    from = min(xj),
    to = max(xj),
    length.out = 10), 1)
  
  
  at_y <- round(seq(
    from = min(yi),
    to = max(yi),
    length.out = 10))
  
  yj <- as.numeric(as.character(unique(z_se_data[, "phyl_dist"])))
  
  labels_y <- round(seq(
    from = min(yj),
    to = max(yj),
    length.out = 10))
  
  at_z <- round(seq(
    from = min(z_se_data[, "fit"]),
    to = max(z_se_data[, "fit"]),
    length.out = 10))
  labels_z <- at_z
  
  axes_1_FUN <- function(){
    axes3d(
      edges = 'x--',
      at = at_x / t_x,
      labels = labels_x)
    
    axes3d(
      edges = 'y+-',
      at = at_y / t_y, 
      labels = labels_y)
    
    axes3d(
      edges = 'z--',
      at = at_z / t_z,
      labels = labels_z)
    
    title3d(
      main = main,  xlab = xlab,  ylab = NULL, zlab = zlab) 
    
    mtext3d(text = ylab, edge = 'y+-', line = 2, at = NULL) 
  }
  
  axes_2_FUN <- function(){
    
    axes3d(
      edges = 'x++',
      at = at_x / t_x,
      labels = labels_x)
    
    axes3d(
      edges = 'y-+',
      at = at_y / t_y, 
      labels = labels_y)
    
    axes3d(
      edges = 'z++',
      at = at_z / t_z,
      labels = labels_z)
    
    title3d(
      sub = main,  xlab = NULL,  ylab = NULL, zlab = NULL) 
    
    mtext3d(text = xlab, edge = 'x++', line = 2, at = NULL) 
    mtext3d(text = ylab, edge = 'y-+', line = 2, at = NULL)
    mtext3d(text = zlab, edge = 'z++', line = 2, at = NULL) 
    
  }
  
  
  if(axes_type == 1) {
    axes_1_FUN()
  } else {
    if(axes_type == 2){
      axes_2_FUN()
    } else {
      warning("\'axes_type\' must be 1 or 2.")
    }
  }
  
  userMatrix <- rotate3d(
    obj = diag(4), angle = pi/4, x = -1, y = 0, z = -1)
  
  rgl.viewpoint(userMatrix = userMatrix)
  
  
}

#``` 







