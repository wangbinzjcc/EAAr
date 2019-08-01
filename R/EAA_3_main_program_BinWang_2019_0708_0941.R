# ---
# title: "The EAA main program"
# author: "wangbinzjcc@qq.com"
# date: "2019-07-08 0952"
# output: html_document
# editor_options:
#  chunk_output_type: console
# ---


### ---
# single_census_interval_data_add_z_growth_FUN <- 
#   switch(EXPR = z_growth_model,
#          gls = gls_add_z_growth_FUN,
#          scale = scale_add_z_growth_FUN)
### !!!


#```{r}
# install.packages("bigtabulate")
# install.packages("nlme")
# require(bigtabulate)
# require(nlme)
#```

#--------------------------------------------------
# "some functions, begin ~ "

#```{r}

create_directory_FUN <- function(){
  
  gc()
  
  dir_name <- paste("big_mat", 
                    format(Sys.time(), "%Y_%m%d_%H%M_%OS"),
                    paste(sample(letters, 8), collapse = ""),
                    sep = "_")
  
  backingpath <- sprintf(fmt = "%s/%s", getwd(), dir_name)
  backingfile <- sprintf(fmt = "%s.bin", dir_name)
  descriptorfile <- sprintf(fmt = "%s.desc", dir_name)
  
  dir.create(path = backingpath)
  
  # return
  list(backingpath = backingpath,
       backingfile = backingfile,
       descriptorfile = descriptorfile)
}

#---

as_big_matrix_FUN <- function(x_matrix) {
  #
  dir_description <- create_directory_FUN()
  
  # return
  bigmemory::as.big.matrix(
    x = as.matrix(x_matrix), 
    type = "double", 
    separated = TRUE, 
    backingfile = dir_description[["backingfile"]],
    backingpath = dir_description[["backingpath"]], 
    descriptorfile = dir_description[["descriptorfile"]], 
    binarydescriptor = FALSE, 
    shared = TRUE)
  
}

#---

create_big_matrix_FUN <- function(nrow_0, colnames_0) {
  
  dir_description <- create_directory_FUN()
  
  # return
  bigmemory::filebacked.big.matrix(
    nrow = nrow_0,
    ncol = length(colnames_0), 
    type = "double",
    init = NA, 
    dimnames = list(NULL, colnames_0), 
    separated = TRUE, 
    backingfile = dir_description[["backingfile"]],
    backingpath = dir_description[["backingpath"]],
    descriptorfile = dir_description[["descriptorfile"]],
    binarydescriptor = FALSE)
}

#---

deepcopy_big_matrix_FUN <- function(big_matrix, col_0, row_0) {
  
  dir_description <- create_directory_FUN()
  
  big_matrix <- bigmemory::deepcopy(
    x = big_matrix,
    cols = col_0,
    rows = row_0,
    separated = TRUE,
    backingfile = dir_description[["backingfile"]],
    backingpath = dir_description[["backingpath"]],
    descriptorfile = dir_description[["descriptorfile"]],
    binarydescriptor = FALSE,
    shared = TRUE)
  
  options(bigmemory.allow.dimnames = TRUE)
  
  colnames(big_matrix) <- col_0
  
  # return
  big_matrix
  
}

#---

parallel_lapply_FUN <- function(
  x, fun, varlist, text_to_parse = NA) {
  
  num_cpu <- parallel::detectCores() - 1
  
  cl <- parallel::makeCluster(num_cpu)
  
  parallel::clusterExport(
    cl = cl, varlist = varlist,
    envir = sys.frame(sys.nframe() - 1))
  
  if(is.na(text_to_parse) == T) {NULL} else {
    
    parallel::clusterExport(
      cl = cl, varlist = "text_to_parse",
      envir = sys.frame(sys.nframe()))
    
    parallel::clusterEvalQ(
      cl = cl,
      expr = eval(parse(text = text_to_parse)))
  }
  
  parallel_results <- parallel::parLapply(
    cl = cl, X = x, fun = compiler::cmpfun(fun))
  
  names(parallel_results) <- x
  
  parallel::stopCluster(cl)
  
  # return
  parallel_results
}

#```




#------------------------------------------------------

# "The EAA main program, part 0, begin~"


# ---
# title: "EAA_part_0_single_census_interval_data_FUN"

# author: "wangbinzjcc@qq.com"
# date: "2019-05-10"
# ---


## single_census_interval_data_FUN 
#```{r}
single_census_interval_data_FUN <- function (big_array_data, census_interval) {
  
  big_array_data <- as.data.frame(big_array_data)
  
  for (i in 1 : ncol(big_array_data)) {
    big_array_data[, i] <- as.numeric(as.character(big_array_data[, i]))}
  
  index_col_names <- colnames(big_array_data)[
    c(1, 2, census_interval + 2, census_interval + 3, 9, 10,
      census_interval + 10, census_interval + 15, census_interval + 20)]
  
  single_census_interval_data <- big_array_data[, index_col_names]
  
  colnames(single_census_interval_data) <- c("id_name", "sp_name", "dbh_1", "dbh_2",
                                             "gx", "gy", "growth_rate", "z_growth", "biomass_1")
  
  ### ---
  single_census_interval_data[, "z_growth"] <- NA
  ### !!!
  
  
  dbh_1_test <- single_census_interval_data[single_census_interval_data[, "dbh_1"] > 0, "dbh_1"]
  if(quantile(x = dbh_1_test, probs = 0.1, na.rm = T) < 10) {
    single_census_interval_data[, "dbh_1"] <- 10 * single_census_interval_data[, "dbh_1"]
    single_census_interval_data[, "dbh_2"] <- 10 * single_census_interval_data[, "dbh_2"]
  } else {NULL}
  
  ### ---
  single_census_interval_data[, "biomass_1"] <- 
    pi * (single_census_interval_data[, "dbh_1"] / 2) ^ 2
  ### !!!
  
  suppressWarnings(
    
    single_census_interval_data[, "growth_rate"] <-
      (log(single_census_interval_data[, "dbh_2"]) -
         log(single_census_interval_data[, "dbh_1"])) / 5
    
  )
  
  single_census_interval_data[, "delete"] <- 0L
  
  single_census_interval_data[
    is.infinite(single_census_interval_data[, "gx"]) | is.infinite(single_census_interval_data[, "gy"]) |
      is.na(single_census_interval_data[, "gx"]) | is.na(single_census_interval_data[, "gy"]),
    "delete"] <- 1L
  
  single_census_interval_data[
    is.na(single_census_interval_data[, "dbh_1"]) | 
      (single_census_interval_data[, "dbh_1"] < 1), "dbh_1"] <- -2
  
  single_census_interval_data[
    is.na(single_census_interval_data[, "dbh_2"]) |
      (single_census_interval_data[, "dbh_2"] < 1), "dbh_2"] <- -2
  
  single_census_interval_data[
    single_census_interval_data[, "dbh_1"] <= 0 & single_census_interval_data[, "dbh_2"] <= 0,
    "delete"] <- 1L

  single_census_interval_data <- single_census_interval_data[
    single_census_interval_data[, "delete"] != 1L, ]
  
  single_census_interval_data[single_census_interval_data[, "gx"] == 0, "gx"] <- 0.01
  
  single_census_interval_data[single_census_interval_data[, "gy"] == 0, "gy"] <- 0.01
  
  single_census_interval_data[
    single_census_interval_data[, "dbh_1"] > 0 & single_census_interval_data[, "dbh_2"] > 0,
    "survival_born_diedS_diedL"] <- 1L
  
  single_census_interval_data[
    single_census_interval_data[, "dbh_1"] <= 0 & single_census_interval_data[, "dbh_2"] > 0,
    "survival_born_diedS_diedL"] <- 2L
  
  dbh_died <- single_census_interval_data[
    single_census_interval_data[, "dbh_1"] > 0 & single_census_interval_data[, "dbh_2"] <= 0, "dbh_1"]
  
  dbh_died_point <- quantile(x = dbh_died, probs = 0.5, na.rm = T)
  
  single_census_interval_data[
    single_census_interval_data[, "dbh_1"] > 0 &
      single_census_interval_data[, "dbh_1"] < dbh_died_point &
      single_census_interval_data[, "dbh_2"] <= 0,
    "survival_born_diedS_diedL"] <- 3L
  
  single_census_interval_data[
    single_census_interval_data[, "dbh_1"] >= dbh_died_point &
      single_census_interval_data[, "dbh_2"] <= 0,
    "survival_born_diedS_diedL"] <- 4L
  
  colnames_add <- c("is_edge", "gx_square_num", "gy_square_num", 
                    "gxy_square_name", paste("square_name", 1:8, sep = "_"))
  
  single_census_interval_data[, colnames_add] <- 0
  
  single_census_interval_data <- single_census_interval_data[
    order(single_census_interval_data[, "survival_born_diedS_diedL"]), ]
  
  single_census_interval_data[, "id_name"] <- 1:nrow(single_census_interval_data)
  
  rownames(single_census_interval_data) <- NULL
  
  # return
  as_big_matrix_FUN(x_matrix = single_census_interval_data)
} 

#```





## single_census_interval_data_add_square_FUN
#```{r}
single_census_interval_data_add_square_FUN <- function(single_census_interval_data, annulus_number, annulus_size){
  #
  max_r <- (annulus_number * annulus_size / pi) ^ 0.5
  
  #-------
  
  single_census_interval_data[, "gx_square_num"] <- as.integer(factor(ceiling(
    single_census_interval_data[, "gx"] / max_r) * max_r)); gc()
  
  single_census_interval_data[, "gy_square_num"] <- as.integer(factor(ceiling(
    single_census_interval_data[, "gy"] / max_r) * max_r)); gc()
  
  single_census_interval_data[, "gxy_square_name"] <- single_census_interval_data[, "gx_square_num"] * 10000L + 
    single_census_interval_data[, "gy_square_num"]; gc()
  
  single_census_interval_data[, "square_name_1"] <- single_census_interval_data[, "gx_square_num"] * 10000L + 
    (single_census_interval_data[, "gy_square_num"] + 1L); gc()
  
  single_census_interval_data[, "square_name_2"] <- single_census_interval_data[, "gx_square_num"] * 10000L + 
    (single_census_interval_data[, "gy_square_num"] - 1L); gc()
  
  single_census_interval_data[, "square_name_3"] <- (single_census_interval_data[, "gx_square_num"] + 1L) *
    10000L + single_census_interval_data[, "gy_square_num"]; gc()
  
  single_census_interval_data[, "square_name_4"] <- (single_census_interval_data[, "gx_square_num"] + 1L) *
    10000L + (single_census_interval_data[, "gy_square_num"] + 1L); gc()
  
  single_census_interval_data[, "square_name_5"] <- (single_census_interval_data[, "gx_square_num"] + 1L) *
    10000L + (single_census_interval_data[, "gy_square_num"] - 1L); gc()
  
  single_census_interval_data[, "square_name_6"] <- (single_census_interval_data[, "gx_square_num"] - 1L) *
    10000L + single_census_interval_data[, "gy_square_num"]; gc()
  
  single_census_interval_data[, "square_name_7"] <- (single_census_interval_data[, "gx_square_num"] - 1L) *
    10000L + (single_census_interval_data[, "gy_square_num"] + 1L); gc()
  
  single_census_interval_data[, "square_name_8"] <- (single_census_interval_data[, "gx_square_num"] - 1L) *
    10000L + (single_census_interval_data[, "gy_square_num"] - 1L); gc()
  
  #------
  
  edge_gx <- max(single_census_interval_data[, "gx"]) - max_r
  edge_gy <- max(single_census_interval_data[, "gy"]) - max_r
  
  index_is_edge <- bigmemory::mwhich(
    x = single_census_interval_data, 
    cols = c("gx", "gx", "gy", "gy"),
    vals = list(max_r, edge_gx, max_r, edge_gy),
    comps = list("lt", "gt", "lt", "gt"),
    op = "OR")
  
  single_census_interval_data[index_is_edge, "is_edge"] <- 1L
  
  #return
  single_census_interval_data
}
#```




### gls_add_z_growth_FUN
# ```{r}

gls_add_z_growth_FUN <- function(single_census_interval_data) {
  #
  gls_fun <- function(sp_dat_1) {
    
    gls0 <- nlme::gls(
      model = growth_rate ~ dbh_1 + I(dbh_1^2),
      data = sp_dat_1,
      weights = nlme::varExp(form = ~ dbh_1))
    
    res_sd <- attr(resid(gls0), "std")
    res_value <- as.numeric(resid(gls0))
    
    #
    res_value / res_sd
  }
  
  #
  fun_0 <- function(sp_data) {
    
    index <- is.na(sp_data[, "growth_rate"]) == F
    
    sp_dat_1 <- sp_data[index, ]
    
    z_growth <- rep(x = NA, times = nrow(sp_data))
    
    if (nrow(sp_dat_1) >= 5) {
      
      ### --- 
      z_try <- try(expr = gls_fun(sp_dat_1), silent = T)
      ### !!!
      
      if (inherits(z_try, "try-error") == F) {
        z_growth[index] <- z_try} else {NULL}
      
    } else {NULL}
    
    #
    z_growth
  }
  
  dat_split <- split(
    x = as.data.frame(single_census_interval_data[, c("dbh_1", "growth_rate")]),
    f = single_census_interval_data[, "sp_name"])
  
  value_unsplit <- lapply(X = dat_split, FUN = fun_0)
  
  
  z_growth <- unsplit(value = value_unsplit, f = single_census_interval_data[, "sp_name"])
  
  single_census_interval_data[, "z_growth"] <- z_growth
  
  
  #return
  single_census_interval_data
}

# ```






## scale_add_z_growth_FUN
# ```{r}

scale_add_z_growth_FUN <- function(single_census_interval_data) {
  
  dbh_0 <- single_census_interval_data[
    single_census_interval_data[, "survival_born_diedS_diedL"] == 1L, "dbh_1"]
  
  size_breakpoints <- quantile(x = dbh_0, probs = (0:11)/11, na.rm = T)
  
  
  ### ---
  scale_growth_fun <- function(sp_dat_1) {
    #
    dbh_1 <- sp_dat_1[, "dbh_1"]
    
    group_size <- cut(x = dbh_1, breaks = size_breakpoints, include.lowest = T, right = T)
    
    group_size <- as.factor(group_size)
    
    numbers <- tapply(X = dbh_1, INDEX = group_size, FUN =  length)
    #
    
    growth_rate <- sp_dat_1[, "growth_rate"]
    
    group_means <- tapply(X = growth_rate, INDEX = group_size, FUN = mean)
    
    group_sds <- tapply(X = growth_rate, INDEX = group_size, FUN = sd) 
    
    z_growth <- (growth_rate - group_means[group_size]) / group_sds[group_size]
    
    # return
    z_growth
  }
  ### !!!
  
  
  #
  fun_0 <- function(sp_data) {
    
    index <- is.na(sp_data[, "growth_rate"]) == F
    
    sp_dat_1 <- sp_data[index, ]
    
    z_growth <- rep(x = NA, times = nrow(sp_data))
    
    if (nrow(sp_dat_1) >= 5) {
      
      ### ---
      z_try <- try(expr = scale_growth_fun(sp_dat_1), silent = T)
      ### !!!
      
      if (inherits(z_try, "try-error") == F) {
        z_growth[index] <- z_try} else {NULL}
      
    } else {NULL}
    
    #
    z_growth
  }
  
  
  dat_split <- split(
    x = as.data.frame(single_census_interval_data[, c("dbh_1", "growth_rate")]),
    f = single_census_interval_data[, "sp_name"])
  
  value_unsplit <- lapply(X = dat_split, FUN = fun_0)
  
  ### --- 
  z_growth <- unsplit(value = value_unsplit, f = single_census_interval_data[, "sp_name"])
  
  single_census_interval_data[, "z_growth"] <- z_growth
  ### !!!
  
  #return
  single_census_interval_data
}

# ```







## EAA_part_0_single_census_interval_data_FUN
#```{r}  

EAA_part_0_single_census_interval_data_FUN <- function(
  big_array_data, census_interval, annulus_number,
  annulus_size, z_growth_model) {
  
  single_census_interval_data <- single_census_interval_data_FUN(big_array_data, census_interval)
  
  single_census_interval_data <- single_census_interval_data_add_square_FUN(single_census_interval_data, annulus_number, annulus_size)
  
  ### ---
  
  single_census_interval_data_add_z_growth_FUN <- 
    switch(EXPR = z_growth_model,
           gls = gls_add_z_growth_FUN,
           scale = scale_add_z_growth_FUN)
  ### !!!
  
  single_census_interval_data <- single_census_interval_data_add_z_growth_FUN(single_census_interval_data)

  # return
  single_census_interval_data
}

#```



#---------------------------------------

# "The EAA main program, part 1, begin~"

# ---
# title: "EAA_part_1_focal_and_annular_id_pair_data_FUN"
# author: "wangbinzjcc@qq.com"
# date: "2019-05-10"
# ---


## annular_id_names_of_focal_ids_FUN
#```{r }
annular_id_names_of_focal_ids_FUN <- function (single_census_interval_data, annulus_number, annulus_size) { 
  #
  annular_ids_in_gxy_squares <- split(
    x = single_census_interval_data[, "id_name"],
    f = single_census_interval_data[, "gxy_square_name"])
  
  gxy_square_names <- as.character(names(annular_ids_in_gxy_squares))
  
  #
  is_survival <- single_census_interval_data[, "survival_born_diedS_diedL"] == 1L
  
  gxy_squares_of_focal_ids <- split(
    x = single_census_interval_data[
      is_survival, grep(pattern = "square_name", x = colnames(single_census_interval_data))],
    f = single_census_interval_data[is_survival, "id_name"])
  
  index_focal_ids <- as.integer(as.character(names(gxy_squares_of_focal_ids)))
  
  #
  gx_0 <- single_census_interval_data[, "gx"]
  gy_0 <- single_census_interval_data[, "gy"]
  max_r <- (annulus_number * annulus_size / pi) ^ 0.5
  #
  
  fun <- function (index_focal_i) {
    
    index_gxy_squares <- gxy_square_names %in% gxy_squares_of_focal_ids[[index_focal_i]]
    
    annular_ids_of_focal_id <- unlist(x = annular_ids_in_gxy_squares[index_gxy_squares],
                                      use.names = FALSE)
    
    r <- ((gx_0[annular_ids_of_focal_id] - gx_0[index_focal_i]) ^ 2 + 
            (gy_0[annular_ids_of_focal_id] - gy_0[index_focal_i]) ^ 2) ^ 0.5
    # 
    as.integer(annular_ids_of_focal_id[r <= max_r])
  }
  
  x <- index_focal_ids
  
  varlist <- c("annular_ids_in_gxy_squares", "gxy_square_names", "gxy_squares_of_focal_ids",  "gx_0", "gy_0", "max_r")
  
  annular_id_names_of_focal_ids <- 
    parallel_lapply_FUN(x = x, fun = fun, varlist = varlist)
  
  names(annular_id_names_of_focal_ids) <- index_focal_ids
  
  # return    
  annular_id_names_of_focal_ids
}

#```


## focal_and_annular_id_pairs_FUN 
#```{r}
focal_and_annular_id_pairs_FUN <- function (
  annular_id_names_of_focal_ids, single_census_interval_data, annulus_number, annulus_size) 
{
  nrow_0 <- length(unlist(x = annular_id_names_of_focal_ids, recursive = TRUE))
  
  colnames_0 <- c("focal_id", "annular_id", "focal_sp", "annular_sp", "annular_biomass", "annul_dist", "annul_interval", "f_gx", "f_gy", "a_gx", "a_gy", "phyl_dist", "phyl_interval", "phyl_dist_integer", "survival_born_diedS_diedL", "delete")
  focal_and_annular_id_pairs <- create_big_matrix_FUN(nrow_0, colnames_0)
  
  # 
  focal_and_annular_id_pairs[, "focal_id"] <- as.integer(
    rep(x = as.integer(names(annular_id_names_of_focal_ids)),
        times = sapply(X = annular_id_names_of_focal_ids, FUN = length))); gc()
  
  focal_and_annular_id_pairs[, "annular_id"] <- as.integer(
    unlist(x = annular_id_names_of_focal_ids, use.names = FALSE)); gc()
  
  focal_and_annular_id_pairs[, "focal_sp"] <-
    single_census_interval_data[focal_and_annular_id_pairs[, "focal_id"], "sp_name"]; gc()
  
  focal_and_annular_id_pairs[, "annular_sp"] <-
    single_census_interval_data[focal_and_annular_id_pairs[, "annular_id"], "sp_name"]; gc()
  
  focal_and_annular_id_pairs[, "f_gx"] <-
    single_census_interval_data[focal_and_annular_id_pairs[, "focal_id"], "gx"]; gc()
  
  focal_and_annular_id_pairs[, "f_gy"] <-
    single_census_interval_data[focal_and_annular_id_pairs[, "focal_id"], "gy"]; gc()
  
  focal_and_annular_id_pairs[, "a_gx"] <-
    single_census_interval_data[focal_and_annular_id_pairs[, "annular_id"], "gx"]; gc()
  
  focal_and_annular_id_pairs[, "a_gy"] <-
    single_census_interval_data[focal_and_annular_id_pairs[, "annular_id"], "gy"]; gc()
  
  focal_and_annular_id_pairs[, "annul_dist"] <-
    ((focal_and_annular_id_pairs[, "f_gx"] - focal_and_annular_id_pairs[, "a_gx"]) ^ 2 +
       (focal_and_annular_id_pairs[, "f_gy"] - focal_and_annular_id_pairs[, "a_gy"]) ^ 2) ^ 0.5; gc()
  
  focal_and_annular_id_pairs[, "annular_biomass"] <- single_census_interval_data[
    focal_and_annular_id_pairs[, "annular_id"], "biomass_1"]; gc()
  
  focal_and_annular_id_pairs[, "annul_interval"] <- as.integer(as.character(
    cut(x = focal_and_annular_id_pairs[, "annul_dist"], 
        breaks = ((0 : annulus_number) * annulus_size / pi) ^ 0.5, 
        labels = 1 : annulus_number, include.lowest = T, right = T))); gc()
  
  focal_and_annular_id_pairs[, "delete"] <- 0
  
  # return
  focal_and_annular_id_pairs
}

#```


## phyl_dists_of_paired_species_FUN
#```{r}
phyl_dists_of_paired_species_FUN <- function (
  pairwise_table_scenario_data, min_sample_phyl_interval)
{
  phyl_dists_of_paired_species <- as.data.frame(pairwise_table_scenario_data)
  
  names(phyl_dists_of_paired_species) <- c("focal_sp", "annular_sp", "phyl_dist")
  
  phyl_dists_of_paired_species[
    phyl_dists_of_paired_species[, "focal_sp"] ==
      phyl_dists_of_paired_species[, "annular_sp"], "phyl_dist"] <- 0L
  
  phyl_dists_of_paired_species[, "phyl_dist_integer"] <- as.integer(
    phyl_dists_of_paired_species[, "phyl_dist"] / min_sample_phyl_interval) * min_sample_phyl_interval
  
  phyl_dists_of_paired_species <- phyl_dists_of_paired_species[
    sample(x = 1:nrow(phyl_dists_of_paired_species)), ]
  
  phyl_dists_of_paired_species <- phyl_dists_of_paired_species[
    order(phyl_dists_of_paired_species[, "phyl_dist_integer"]), ]
  
  # return
  phyl_dists_of_paired_species
  
}

#```


## id_pair_data_add_phyl_dist_FUN
#```{r}
id_pair_data_add_phyl_dist_FUN <- function (
  focal_and_annular_id_pairs, single_census_interval_data, phyl_dists_of_paired_species)
{
  index_match <- match(
    x = paste(
      as.integer(focal_and_annular_id_pairs[, "focal_sp"]),
      as.integer(focal_and_annular_id_pairs[, "annular_sp"])),
    table = paste(
      as.integer(phyl_dists_of_paired_species[, "focal_sp"]),
      as.integer(phyl_dists_of_paired_species[, "annular_sp"])))
  
  focal_and_annular_id_pairs[, "phyl_dist"] <- 
    phyl_dists_of_paired_species[index_match, "phyl_dist"]
  
  focal_and_annular_id_pairs[, "phyl_dist_integer"] <- 
    phyl_dists_of_paired_species[index_match, "phyl_dist_integer"]
  
  focal_and_annular_id_pairs[
    is.na(focal_and_annular_id_pairs[, "phyl_dist"]), "delete"] <- 1L
  
  # return
  focal_and_annular_id_pairs
  
} 

#```


## id_pair_data_add_phyl_interval_FUN
#```{r}
id_pair_data_add_phyl_interval_FUN <- function (
  id_pair_data_add_phyl_dist, quantile_number)
{
  index_delete <- which(id_pair_data_add_phyl_dist[, "delete"] != 1L)
  
  phyl_dist_temp <- id_pair_data_add_phyl_dist[index_delete, "phyl_dist_integer"] 
  
  phyl_dist_temp <- phyl_dist_temp + 
    round(x = rnorm(n = length(phyl_dist_temp), mean = 0, sd = 0.0001), digits = 10)
  
  phyl_dist_break_points <- sort(phyl_dist_temp)[
    round(seq(from = 1, to = length(phyl_dist_temp), length.out = (quantile_number + 1)))] 
  
  id_pair_data_add_phyl_dist[, "phyl_interval"]  <- NA
  
  id_pair_data_add_phyl_dist[index_delete, "phyl_interval"]  <- as.integer(as.character(
    cut(x = phyl_dist_temp, breaks = phyl_dist_break_points,
        labels = c(1:quantile_number - 1), include.lowest = T, right = T)))
  
  # return
  id_pair_data_add_phyl_dist
} 

#```


## id_pair_data_add_survival_state_FUN
#```{r}
id_pair_data_add_survival_state_FUN <- function (
  id_pair_data_add_phyl_interval, single_census_interval_data)
{
  id_pairs <- id_pair_data_add_phyl_interval
  
  survival_annular_ids <- single_census_interval_data[
    single_census_interval_data[, "survival_born_diedS_diedL"] == 1L, "id_name"]
  
  id_pairs[id_pairs[, "annular_id"] %in% survival_annular_ids, "survival_born_diedS_diedL"] <- 1L
  
  #
  born_annular_ids <- single_census_interval_data[single_census_interval_data[, "survival_born_diedS_diedL"] == 2L, "id_name"]
  
  id_pairs[id_pairs[, "annular_id"] %in% born_annular_ids, "survival_born_diedS_diedL"] <- 2L
  
  #
  diedS_annular_ids <- single_census_interval_data[single_census_interval_data[, "survival_born_diedS_diedL"] == 3L, "id_name"]
  
  id_pairs[id_pairs[, "annular_id"] %in% diedS_annular_ids, "survival_born_diedS_diedL"] <- 3L
  
  #
  diedL_annular_ids <- single_census_interval_data[single_census_interval_data[, "survival_born_diedS_diedL"] == 4L, "id_name"]
  
  id_pairs[id_pairs[, "annular_id"] %in% diedL_annular_ids, "survival_born_diedS_diedL"] <- 4L
  
  #
  id_pairs[! (id_pairs[, "focal_id"] %in% survival_annular_ids), "delete"] <- 1L
  
  id_pairs[! (id_pairs[, "annular_id"] %in% 
                c(diedS_annular_ids, diedL_annular_ids, born_annular_ids, survival_annular_ids)),
           "delete"] <- 1L
  
  id_pairs[is.na(id_pairs[, "annul_interval"]), "delete"] <- 1L
  
  id_pairs[is.na(id_pairs[, "phyl_interval"]), "delete"] <- 1L
  
  # return
  deepcopy_big_matrix_FUN(big_matrix = id_pairs, 
                          col_0 = colnames(id_pairs),
                          row_0 = id_pairs[, "delete"] != 1L)
  
} 

#```



## unlink_dir_FUN
#```{r}

unlink_dir_FUN <- function(big_matirx_except) {
  
  big_mat_paths <- grep(pattern = "big_mat_[0-9]{4}_.*", x = dir(getwd()), value = T)
  
  path_except <- describe(big_matirx_except)@ description[["dirname"]]
  
  path_except <- gsub(pattern = ".*/([^/]*)/$",
                      replacement = "\\1",
                      x = path_except)
  
  big_mat_paths <- big_mat_paths[!big_mat_paths %in% path_except]
  
  gc()
  
  lapply(X = big_mat_paths, FUN = unlink, recursive = TRUE, force = TRUE)
}

#```



## EAA_part_1_focal_and_annular_id_pair_data_FUN
#```{r}

EAA_part_1_focal_and_annular_id_pair_data_FUN <- function(
  single_census_interval_data, annulus_number, annulus_size,
  pairwise_table_scenario_data, quantile_number,
  min_sample_phyl_interval) {
  
  # part 1
  annular_id_names_of_focal_ids <- annular_id_names_of_focal_ids_FUN(
    single_census_interval_data, annulus_number, annulus_size)
  
  focal_and_annular_id_pairs <- focal_and_annular_id_pairs_FUN(
    annular_id_names_of_focal_ids, single_census_interval_data, annulus_number, annulus_size)
  
  phyl_dists_of_paired_species <- phyl_dists_of_paired_species_FUN(
    pairwise_table_scenario_data, min_sample_phyl_interval)
  
  id_pair_data_add_phyl_dist <- id_pair_data_add_phyl_dist_FUN(
    focal_and_annular_id_pairs, single_census_interval_data, phyl_dists_of_paired_species)
  
  id_pair_data_add_phyl_interval <- id_pair_data_add_phyl_interval_FUN(
    id_pair_data_add_phyl_dist, quantile_number)
  
  id_pair_data <- id_pair_data_add_survival_state_FUN(
    id_pair_data_add_phyl_interval, single_census_interval_data)
  
  #
  unlink_dir_FUN(big_matirx_except = id_pair_data)
  
  # return
  id_pair_data
}

#```



#------------------------------------------------------

# "The new EAA program, part 2, begin~"

# ---
# title: "EAA_part_2_all_quantiles_statistics_data_FUN"

# author: "wangbinzjcc@qq.com"
# date: "2019-05-08"
# ---



## full_big_matrix_FUN
#```{r}
full_big_matrix_FUN <- function (id_pair_data)
{
  
  levels_expand_grid <- expand.grid(
    focal_id = sort(unique(id_pair_data[, "focal_id"])), 
    annul_interval = sort(unique(id_pair_data[, "annul_interval"])), 
    phyl_interval = sort(unique(id_pair_data[, "phyl_interval"])))
  
  col_names <- c(
    "focal_id", "focal_sp", "focal_z_growth", "focal_diameter",
    "f_gx", "f_gy", "is_edge", "d_1", "d_2", "edge_coef",
    "is_small_diam", "dbh_interval", "biom_interval",
    "clust_number", "born_number", "diedS_number", 
    "diedL_number", "annular_biomass", "annul_interval",
    "annul_dist", "phyl_interval", "phyl_dist")
  
  full_big_matrix <- create_big_matrix_FUN(
    nrow_0 = nrow(levels_expand_grid), colnames_0 = col_names) 
  
  full_big_matrix[
    , c("focal_id", "annul_interval", "phyl_interval")] <- 
    as.matrix(levels_expand_grid[
      , c("focal_id", "annul_interval", "phyl_interval")])

  # return
  full_big_matrix
} 
#```


## biomass_clust_statistic_FUN 
#```{r}
biomass_clust_statistic_FUN <- function (
  id_pair_data = id_pair_data)
{
  which_survival <- bigmemory::mwhich(
    x = id_pair_data, cols = "survival_born_diedS_diedL",
    vals = 1L, comps = "eq")
  
  survival_id_pairs <- deepcopy_big_matrix_FUN(
    big_matrix = id_pair_data,
    col_0 = c("focal_id", "annul_interval", "phyl_interval",
              "annular_biomass"),
    row_0 = which_survival)
  
  X = bigtabulate::bigsplit(
    x = survival_id_pairs,
    ccols = c("focal_id", "annul_interval", "phyl_interval"),
    splitcol="annular_biomass")
  gc()
  
  annular_biomass_statistic <- sapply(X = X, FUN = sum)
  gc()  
  
  annular_clust_statistic <- bigtabulate::bigtable(
    x = survival_id_pairs, 
    ccols = c("focal_id", "annul_interval", "phyl_interval"))
  gc() 
  
  names_split_df <- do.call(
    what = rbind, args = strsplit(x = names(annular_biomass_statistic), split = ":"))
  
  biomass_clust_statistic <- data.frame(
    annular_biomass = as.numeric(annular_biomass_statistic),
    clust_number = as.integer(annular_clust_statistic),
    focal_id = as.integer(names_split_df[, 1]),
    annul_interval = as.integer(names_split_df[, 2]),
    phyl_interval = as.integer(names_split_df[, 3]),
    id_INDEX = -2, 
    annul_INDEX = -2,
    phyl_INDEX = -2)
  
  # return
  as_big_matrix_FUN(x_matrix = biomass_clust_statistic)
  
} 
#```



## born_statistic_FUN (hide)
#```{r}
born_statistic_FUN <- function (
  id_pair_data = id_pair_data)
{
  which_born <- bigmemory::mwhich(
    x = id_pair_data, cols = "survival_born_diedS_diedL",
    vals = 2L, comps = "eq")
  
  born_id_pairs <- deepcopy_big_matrix_FUN(
    big_matrix = id_pair_data,
    col_0 = c("focal_id", "annul_interval", "phyl_interval"),
    row_0 = which_born)
  
  annular_born_statistic <- bigtabulate::bigtable(
    x = born_id_pairs, 
    ccols = c("focal_id", "annul_interval", "phyl_interval"))
  
  names(dimnames(annular_born_statistic)) <- 
    c("focal_id", "annul_interval", "phyl_interval")
  gc()
  
  focal_id <- as.integer(
    dimnames(annular_born_statistic)[["focal_id"]])[
      apply(X = annular_born_statistic, MARGIN = 3, FUN = row)]
  
  annul_interval <- as.integer(
    dimnames(annular_born_statistic)[["annul_interval"]])[
      apply(X = annular_born_statistic, MARGIN = 3, FUN = col)]
  
  phyl_interval <- as.integer(
    dimnames(annular_born_statistic)[["phyl_interval"]])[
      t(apply(X = annular_born_statistic, MARGIN = 1, FUN = col))]
  
  born_statistic <- data.frame(
    born_number = as.integer(annular_born_statistic),
    focal_id = focal_id,
    annul_interval = annul_interval,
    phyl_interval = phyl_interval,
    id_INDEX = -2, 
    annul_INDEX = -2,
    phyl_INDEX = -2)
  
  # return
  as_big_matrix_FUN(x_matrix = born_statistic)
  
} 
#```


## died_statistic_FUN (hide)
#```{r}  
died_statistic_FUN <- function (id_pair_data, val = c(3, 4)[1])
{
  which_died <- bigmemory::mwhich(
    x = id_pair_data, cols = "survival_born_diedS_diedL",
    vals = val, comps = "eq")
  
  died_id_pairs <- deepcopy_big_matrix_FUN(
    big_matrix = id_pair_data,
    col_0 = c("focal_id", "annul_interval", "phyl_interval"),
    row_0 = which_died) 
  
  annular_died_statistic <- bigtabulate::bigtable(
    x = died_id_pairs, 
    ccols = c("focal_id", "annul_interval", "phyl_interval"))
  
  names(dimnames(annular_died_statistic)) <- 
    c("focal_id", "annul_interval", "phyl_interval")
  gc()
  
  focal_id <- as.integer(dimnames(annular_died_statistic)[["focal_id"]])[
    apply(X = annular_died_statistic, MARGIN = 3, FUN = row)]
  
  annul_interval <- as.integer(dimnames(annular_died_statistic)[["annul_interval"]])[
    apply(X = annular_died_statistic, MARGIN = 3, FUN = col)]
  
  phyl_interval <- as.integer(dimnames(annular_died_statistic)[["phyl_interval"]])[
    t(apply(X = annular_died_statistic, MARGIN = 1, FUN = col))]
  
  died_statistic <- data.frame(
    died_number = as.integer(annular_died_statistic),
    focal_id = focal_id,
    annul_interval = annul_interval,
    phyl_interval = phyl_interval,
    id_INDEX = -2, 
    annul_INDEX = -2,
    phyl_INDEX = -2)
  
  if(val == 3) {
    colnames(died_statistic)[colnames(died_statistic) == "died_number"] <- "diedS_number"
  } else {
    if(val == 4) {
      colnames(died_statistic)[colnames(died_statistic) == "died_number"] <- "diedL_number"
    } else {
      print("'val' must be 3 or 4")
    }
  }
  
  # return
  as_big_matrix_FUN(x_matrix = died_statistic)
  
} 
#```


## assign_value_FUN
#```{r}

assign_value_FUN <- function(
  full_big_matrix = full_big_matrix, 
  sub_matrix = biomass_clust_statistic,
  value_names = c("annular_biomass", "clust_number")) {
  
  mpermute(x = full_big_matrix, cols = c("phyl_interval", "annul_interval", "focal_id"))
  
  full_big_matrix[, value_names] <- 0L
  
  grand_id_levels <- as.integer(sort(unique(full_big_matrix[, "focal_id"])))
  grand_annul_levels <- as.integer(sort(unique(full_big_matrix[, "annul_interval"])))
  grand_phyl_levels <- as.integer(sort(unique(full_big_matrix[, "phyl_interval"])))
  
  len_id <- length(grand_id_levels)
  len_annul <- length(grand_annul_levels)
  len_phyl <- length(grand_phyl_levels)
  gc()
  
  sub_matrix[, "id_INDEX"] <- match(
    x = as.integer(sub_matrix[, "focal_id"]),
    table = grand_id_levels)
  gc()
  
  sub_matrix[, "annul_INDEX"] <- match(
    x = as.integer(sub_matrix[, "annul_interval"]),
    table = grand_annul_levels)
  gc()
  
  sub_matrix[, "phyl_INDEX"] <- match(
    x = as.integer(sub_matrix[, "phyl_interval"]),
    table = grand_phyl_levels)
  gc()
  
  INDEX_matrix <- 
    (sub_matrix[, "phyl_INDEX"] - 1) * (len_annul * len_id)  +
    (sub_matrix[, "annul_INDEX"] - 1) * len_id +
    sub_matrix[, "id_INDEX"]
  gc()
  
  
  full_big_matrix[INDEX_matrix, value_names] <- 
    sub_matrix[, value_names]
  gc()
  
  # return
  full_big_matrix
}
#```

## add_biomass_interval_FUN
#```{r}
add_biomass_interval_FUN <- function(full_big_matrix,
                                     small_diameter_point) {
  #
  annular_biomasses_temp <- 
    full_big_matrix[, "annular_biomass"] + 
    rnorm(n = nrow(full_big_matrix),
          mean = 0.001, sd = 0.001)
  
  biomass_quantiles <- quantile(
    x = annular_biomasses_temp[
      annular_biomasses_temp >= pi * (small_diameter_point / 2) ^ 2],
    probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  
  biom_interval <- as.character(
    cut(x = annular_biomasses_temp, 
        breaks = biomass_quantiles, labels = 1:5, 
        include.lowest = T, right = T))
  
  ### ---
  biom_interval[
    annular_biomasses_temp < pi * (small_diameter_point / 2) ^ 2
    ] <- 0  
  ### !!!
  
  full_big_matrix[, "biom_interval"] <- biom_interval
  
  
  # return
  full_big_matrix
}

#```

## add_dbh_interval_FUN
#```{r}
add_dbh_interval_FUN <- function(full_big_matrix) { 
  
  id_diameters <- full_big_matrix[, "focal_diameter"] + 
    rnorm(n = nrow(full_big_matrix), mean = 0.001, sd = 0.001)
  
  diameter_quantiles_2 <- quantile(x = id_diameters,
                                   probs = c(0, 0.25, 0.5, 0.75, 1))
  
  dbh_interval <- as.character(
    cut(x = id_diameters, breaks = diameter_quantiles_2,
        labels = 1:4, include.lowest = T, right = T))
  
  full_big_matrix[, "dbh_interval"] <- dbh_interval
  
  # return
  full_big_matrix
}
#```





##  calculate_edge_correction_coefficient_FUN
#```{r}
calculate_edge_correction_coefficient_FUN <- function(
  full_big_matrix = full_big_matrix,
  id_pair_data = id_pair_data, 
  single_census_interval_data = single_census_interval_data){
  
  
  mean_annul_dist <- tapply(
    X = id_pair_data[, "annul_dist"], 
    INDEX = id_pair_data[, "annul_interval"],
    FUN = mean, na.rm = T)
  
  full_big_matrix[, "annul_dist"] <- mean_annul_dist[
    as.integer(as.factor(
      full_big_matrix[, "annul_interval"]))] 
  gc()
  
  
  full_big_matrix[, "f_gx"] <- single_census_interval_data[
    match(x = full_big_matrix[, "focal_id"], 
          table = single_census_interval_data[, "id_name"]), "gx"]
  gc()
  
  full_big_matrix[, "f_gy"] <- single_census_interval_data[
    match(x = full_big_matrix[, "focal_id"], 
          table = single_census_interval_data[, "id_name"]), "gy"]
  
  gc()
  
  full_big_matrix[, "is_edge"] <- single_census_interval_data[
    match(x = full_big_matrix[, "focal_id"], 
          table = single_census_interval_data[, "id_name"]), "is_edge"]
  
  gc()
  
  #---
  
  full_big_matrix[, "edge_coef"] <- 1
  
  
  index_1 <- full_big_matrix[, "is_edge"] == 1
  
  gx_max <- max(full_big_matrix[, "f_gx"], na.rm = T)
  
  index_2 <- full_big_matrix[index_1, "f_gx"] > c(gx_max / 2)
  
  full_big_matrix[index_1, "d_1"][index_2] <- 
    gx_max - full_big_matrix[index_1, "f_gx"][index_2]
  
  full_big_matrix[index_1, "d_1"][! index_2] <- 
    full_big_matrix[index_1, "f_gx"][! index_2]
  
  gc()
  #
  gy_max <- max(full_big_matrix[, "f_gy"], na.rm = T)
  
  index_2 <- full_big_matrix[index_1, "f_gy"] > c(gy_max / 2)
  
  full_big_matrix[index_1, "d_2"][index_2] <- 
    gy_max - full_big_matrix[index_1, "f_gy"][index_2]
  
  full_big_matrix[index_1, "d_2"][! index_2] <- 
    full_big_matrix[index_1, "f_gy"][! index_2] 
  
  gc()
  
  #---
  index_3 <- (full_big_matrix[index_1, "d_2"] > full_big_matrix[index_1, "annul_dist"]) &
    (full_big_matrix[index_1, "d_1"] > full_big_matrix[index_1, "annul_dist"])
  
  full_big_matrix[index_1, "is_edge"][index_3] <- 0
  
  index_4 <- (full_big_matrix[index_1, "d_2"] < full_big_matrix[index_1, "annul_dist"]) &
    (full_big_matrix[index_1, "d_1"] < full_big_matrix[index_1, "annul_dist"])
  
  full_big_matrix[index_1, "is_edge"][index_4] <- 2
  
  gc()
  #---
  
  index_1 <- full_big_matrix[, "is_edge"] == 1
  
  index_2 <- full_big_matrix[index_1, "d_1"] > full_big_matrix[index_1, "d_2"]
  
  full_big_matrix[index_1, "d_1"][index_2] <- full_big_matrix[index_1, "d_2"][index_2]
  
  #
  x  <- full_big_matrix[index_1, "d_1"] / full_big_matrix[index_1, "annul_dist"]
  
  alpha <- 2 * pi - 2 * acos(x)
  
  arc_length <- alpha * full_big_matrix[index_1, "annul_dist"]
  
  circumference <- pi * 2 * full_big_matrix[index_1, "annul_dist"]
  
  full_big_matrix[index_1, "edge_coef"] <- arc_length / circumference
  
  #
  index_3 <- full_big_matrix[, "is_edge"] == 2
  
  # In order to avoid over-correction, the smallest correction coefficient is limited to 0.5.
  
  full_big_matrix[index_3, "edge_coef"] <- 0.5
  
  gc()
  #
  
  # return
  full_big_matrix
}

#```





## all_quantiles_statistics_data_FUN
#```{r}
all_quantiles_statistics_data_FUN <- function(
  full_big_matrix = full_big_matrix, 
  biomass_clust_statistic = biomass_clust_statistic, 
  born_statistic = born_statistic, 
  diedS_statistic = diedS_statistic,
  diedL_statistic = diedL_statistic,
  single_census_interval_data = single_census_interval_data,
  id_pair_data = id_pair_data)
{
  
  
  full_big_matrix <- calculate_edge_correction_coefficient_FUN(
    full_big_matrix = full_big_matrix,
    id_pair_data = id_pair_data, 
    single_census_interval_data = single_census_interval_data)
  
  
  full_big_matrix <- assign_value_FUN(
    full_big_matrix = full_big_matrix, 
    sub_matrix = biomass_clust_statistic,
    value_names = c("annular_biomass", "clust_number"))
  
  
  full_big_matrix <- assign_value_FUN(
    full_big_matrix = full_big_matrix, 
    sub_matrix = born_statistic,
    value_names = c("born_number"))
  
  
  full_big_matrix <- assign_value_FUN(
    full_big_matrix = full_big_matrix, 
    sub_matrix = diedS_statistic,
    value_names = c("diedS_number"))
  
  
  
  full_big_matrix <- assign_value_FUN(
    full_big_matrix = full_big_matrix, 
    sub_matrix = diedL_statistic,
    value_names = c("diedL_number"))
  
  
   for(i in c("annular_biomass", "clust_number", "born_number",
              "diedS_number", "diedL_number")) {
     full_big_matrix[, i] <- full_big_matrix[, i] / full_big_matrix[, "edge_coef"]
   }
   
   gc()

  
  full_big_matrix[, "focal_sp"] <- single_census_interval_data[
    match(x = full_big_matrix[, "focal_id"],
          table = single_census_interval_data[, "id_name"]), "sp_name"]
  
  
  full_big_matrix[, "focal_diameter"] <- single_census_interval_data[
    match(x = full_big_matrix[, "focal_id"],
          table = single_census_interval_data[, "id_name"]), "dbh_1"]
  
  
  full_big_matrix[, "focal_z_growth"] <- single_census_interval_data[
    match(x = full_big_matrix[, "focal_id"], 
          table = single_census_interval_data[, "id_name"]), "z_growth"]
  gc()
  
  ### ---
  small_diameter_point <- quantile(
    x = full_big_matrix[, "focal_diameter"],
    probs = 0.4)
  ### !!!
  
  full_big_matrix[, "is_small_diam"] <- as.integer(
    full_big_matrix[, "focal_diameter"] <= small_diameter_point)
  
  full_big_matrix <- add_dbh_interval_FUN(
    full_big_matrix)
  
  mean_phyl_dist <- tapply(
    X = id_pair_data[, "phyl_dist"], 
    INDEX = id_pair_data[, "phyl_interval"],
    FUN = mean, na.rm = T)
  
  full_big_matrix[, "phyl_dist"] <- mean_phyl_dist[
    as.integer(as.factor(full_big_matrix[, "phyl_interval"]))]
  
  
  gc()
  
  full_big_matrix <- add_biomass_interval_FUN(
    full_big_matrix, small_diameter_point)
  
  
  gc()
  
  # return
  full_big_matrix
} 

#```


## EAA_part_2_all_quantiles_statistics_data_FUN_0
#```{r}
EAA_part_2_all_quantiles_statistics_data_FUN_0 <- 
  function(id_pair_data_0, single_census_interval_data) {
    
    # part 2
    full_big_matrix <-
      full_big_matrix_FUN(id_pair_data_0)
    
    
    biomass_clust_statistic <-
      biomass_clust_statistic_FUN(id_pair_data_0)
    
    
    born_statistic <-
      born_statistic_FUN(id_pair_data_0)
    
    
    diedS_statistic <-
      died_statistic_FUN(id_pair_data_0, val = 3)
    
    
    
    diedL_statistic <-
      died_statistic_FUN(id_pair_data_0, val = 4)
    
    
    statistic_data <- all_quantiles_statistics_data_FUN(
      full_big_matrix, biomass_clust_statistic,
      born_statistic, diedS_statistic, diedL_statistic, single_census_interval_data,
      id_pair_data_0)
    
    # return
    statistic_data
  }

#```


# reduce_memory_FUN_0
#```{r }
reduce_memory_FUN_0 <- function(
  id_pair_data, single_census_interval_data, n_break = (1:5)[1]){
  
  #
  focal_ids <- sort(unique(id_pair_data[, "focal_id"]))
  
  len <- length(focal_ids)
  n_break <- round(n_break) 
  
  focal_ids_split <- split(
    x = focal_ids,
    f = rep(
      x = 1: n_break,
      each = ceiling(length(focal_ids)/n_break), 
      length.out = length(focal_ids)))
  
  results_list <- lapply(
    X = focal_ids_split, FUN = function(i){
      
      row_0 <- bigmemory::mwhich(
        x = id_pair_data,
        cols = "focal_id",
        vals = c(min(i), max(i)),
        comps = c("ge", "le"),
        op = "AND")
      
      id_pair_data_0 <- deepcopy_big_matrix_FUN(
        big_matrix = id_pair_data,
        col_0 = colnames(id_pair_data),
        row_0 = row_0)
      
      results <- EAA_part_2_all_quantiles_statistics_data_FUN_0(
        id_pair_data_0, single_census_interval_data)
      
      #
      bigmemory::describe(results)
    })
  
  # return
  results_list
}
#```




## EAA_part_2_all_quantiles_statistics_data_FUN
#```{r}

EAA_part_2_all_quantiles_statistics_data_FUN <- function(
  id_pair_data, single_census_interval_data, max_matrix_row_limit){
  
  if(max_matrix_row_limit < 1e7) {
    
    warning( "'max_matrix_row_limit' should be >= 1e7, ",  "which was converted to 1e7.")
    max_matrix_row_limit = 1e7
    
  } else {NULL}
  
  nrow_id_data <- nrow(id_pair_data)
  n_break <- ceiling(nrow_id_data / max_matrix_row_limit)
  if(n_break > 5){n_break = 5}else{NULL}
  
  
  results_list <- reduce_memory_FUN_0(
    id_pair_data, single_census_interval_data, n_break)
  
  totalRows <- lapply(X = results_list, FUN = function(list_i){
    list_i@description[["totalRows"]]})
  
  nrow_0 <- sum(unlist(totalRows), na.rm = T)
  
  colnames_0 <- results_list[[1]]@description[["colNames"]]
  
  statistic_data <- create_big_matrix_FUN(nrow_0, colnames_0)
  
  #
  row_cumsum <- c(0, cumsum(totalRows)) 
  
  for(i in 1:length(totalRows)) {
    
    list_i <- results_list[[i]]
    sub_matrix <- attach.big.matrix(list_i)
    index_row <- (row_cumsum[i] + 1): row_cumsum[i+1]
    
    for(j in colnames(sub_matrix)){
      statistic_data[index_row, j] <- sub_matrix[, j]
      gc()
    }
    
  }
  
  unlink_dir_FUN(big_matirx_except = statistic_data)
  
  # return
  statistic_data
}
#```




# -----------------------------------------



# "EAA main program, part 3, begin~"

# ---
# title: "EAA_part_3_growth_results_FUN"
# author: "wangbinzjcc@qq.com"
# date: "2019-05-10"
# ---

## as_data_frame_FUN
#```{r}
as_data_frame_FUN <- function(real_growths_of_all_quant_pairs) {
  
  name_length <- sapply(X = real_growths_of_all_quant_pairs,
                        FUN = length)
  
  names <- rep(x = names(name_length), times = as.numeric(name_length))
  
  biom_interval <- unlist(lapply(
    X = real_growths_of_all_quant_pairs, FUN = names), use.names = F)
  
  annul_interval <- gsub(pattern = "([0-9]{1,2}):[0-9]{1,2}", replacement = "\\1", x = names)
  
  phyl_interval <- gsub(pattern = "[0-9]{1,2}:([0-9]{1,2})", replacement = "\\1", x = names)
  
  growth <- unlist(x = real_growths_of_all_quant_pairs, use.names = F)
  
  results <- data.frame(
    annul_interval = as.integer(annul_interval),
    phyl_interval = as.integer(phyl_interval),
    biom_interval = as.integer(biom_interval),
    growth = as.numeric(growth))
  
  expand_levels_FUN <- function(results) {
    
    expand_data <- expand.grid(
      annul_interval = unique(results[, "annul_interval"]),
      phyl_interval = unique(results[, "phyl_interval"]),
      biom_interval = unique(results[, "biom_interval"]))
    
    if(nrow(results) == nrow(expand_data)) {
      
      #
      results
      
    } else {
      
      index <- match(
        x = paste(results[, "annul_interval"],
                  results[, "phyl_interval"],
                  results[, "biom_interval"]),
        table = paste(expand_data[, "annul_interval"],
                      expand_data[, "phyl_interval"],
                      expand_data[, "biom_interval"])) 
      
      expand_data[index, "growth"] <- results[, "growth"]
      
      #
      expand_data
    }
  }
  
  # return   
  expand_levels_FUN(results)
}

#```


## calculate_real_growth_FUN
#```{r}
calculate_real_growth_FUN <-
  function(col_diam_small = "is_small_diam") {
    
    new_data_s <-
      bigmemory::attach.big.matrix(describe_data)
    
    row_0 <- bigmemory::mwhich(
      x = new_data_s,
      cols = c("focal_z_growth", col_diam_small),
      vals = list(NA, 1),
      comps = list("neq", "eq"),
      op = "AND")
    
    data_temp <- deepcopy_big_matrix_FUN(
      big_matrix = new_data_s,
      col_0 = c("focal_z_growth", col_diam_small,
                "biom_interval", "annul_interval",
                "phyl_interval"),
      row_0 = row_0)
    
    focal_growth_split <- bigtabulate::bigsplit(
      x = data_temp,
      ccols = c("annul_interval", "phyl_interval"),
      splitcol = c("focal_z_growth"))
    
    biom_interval_split <- bigtabulate::bigsplit(
      x = data_temp,
      ccols = c("annul_interval", "phyl_interval"),
      splitcol = c("biom_interval"))
    
    x <- names(focal_growth_split) 
    
    fun <- function(name_i) {
      tapply(X = focal_growth_split[[name_i]], 
             INDEX = biom_interval_split[[name_i]], 
             FUN = mean, na.rm = T)
    }
    
    real_growths_of_all_quant_pairs <- lapply(X = x, FUN = fun)
    
    names(real_growths_of_all_quant_pairs) <- x
    
    real_growth_data <-
      as_data_frame_FUN(real_growths_of_all_quant_pairs)
    
    gc()
    
    # return
    real_growth_data
  }

#```


## sample_within_sp_FUN (hide)
#```{r}

sample_within_sp_FUN <- function(focal_sp){
  
  index_split <- split(x = 1:length(focal_sp),
                       f = focal_sp)
  
  set.seed(seed = NULL)
  
  value_split <- lapply(
    X = index_split, FUN = function(x){
      if (length(x) == 1) {x} else {sample(x)}})
  
  index_sample <- unsplit(value = value_split,
                          f = focal_sp)
  # return
  index_sample
}

#```


## statistic_data_add_shuffled_diams_FUN
#```{r}
statistic_data_add_shuffled_diams_FUN <- function (
  statistic_data, shuffled_times,
  shuffled_col = "is_small_diam")
{
  
  big_matrix_add_shuffled <- create_big_matrix_FUN(
    nrow_0 = nrow(statistic_data),
    colnames_0 = c(colnames(statistic_data),
                   paste("shuffled", 1:shuffled_times,
                         sep = "_")))
  
  big_matrix_add_shuffled[, colnames(statistic_data)] <-
    statistic_data[, colnames(statistic_data)]
  
  
  focal_sp <- big_matrix_add_shuffled[, "focal_sp"]
  
  gc()
  
  #
  x <- 1:shuffled_times
  
  fun <- function(i) {
    
    big_matrix_add_shuffled <- bigmemory::attach.big.matrix(describe_data)
    
    big_matrix_add_shuffled[, paste("shuffled", i, sep = "_")] <-
      big_matrix_add_shuffled[sample_within_sp_FUN(focal_sp), shuffled_col]
    gc()
    
    # return
    NULL
  }
  
  describe_data <- bigmemory::describe(big_matrix_add_shuffled) 
  
  varlist <- c("describe_data", "sample_within_sp_FUN", "focal_sp", "shuffled_col")
  
  text_to_parse <- "require(bigmemory)"
  
  
  parallel_lapply_FUN(
    x = x, fun = fun, varlist = varlist, text_to_parse = text_to_parse)
  
  # return
  big_matrix_add_shuffled
} 

#```





## relative_growth_FUN
#```{r}
relative_growth_FUN <- function(real_growth_data) {
  
  dat_0 <- real_growth_data[
    real_growth_data[, "biom_interval"] == 0, ]
  
  dat_1 <- real_growth_data
  
  INDEX <- match(
    x = paste(dat_1[, "annul_interval"],
              dat_1[, "phyl_interval"],
              sep = "_"),
    table = paste(dat_0[, "annul_interval"],
                  dat_0[, "phyl_interval"],
                  sep = "_"))
  
  dat_1[, "growth_0"] <- dat_0[INDEX, "growth"]
  
  dat_1[, "relative_value"] <- 
    dat_1[, "growth"] - dat_1[, "growth_0"]
  
  # return
  dat_1[, c("annul_interval", "phyl_interval",
            "biom_interval", "relative_value")]
}
#```




## real_results_add_shuffled_sd_FUN
#```{r}
real_results_add_shuffled_sd_FUN_1 <-  function(
  results_list) {
  
  real_results <- results_list[[1]]
  
  growth_results <- real_results
  
  #
  relative_results_list <- lapply(
    X = results_list, FUN = relative_growth_FUN)
  
  shuffled_data <- do.call(
    what = cbind, args = relative_results_list[-1])
  
  shuffled_data_1 <- shuffled_data[
    grep(pattern = "relative_value", x = colnames(shuffled_data))]
  
  #
  growth_results[, "sd_shuffled"] <- apply(
    X = shuffled_data_1, MARGIN = 1, FUN = sd, na.rm = T)
  
  growth_results[, "mean_shuffled"] <- apply(
    X = shuffled_data_1, MARGIN = 1, FUN = mean, na.rm = T)
  
  growth_results[, "relative_value"] <- 
    relative_results_list[[1]][, "relative_value"]
  
  colnames(growth_results)[
    colnames(growth_results) == "growth"] <- "real_value"
  
  # return
  growth_results
}

#```



## EAA_part_3_growth_results_FUN_0
#```{r}
EAA_part_3_growth_results_FUN_0 <- function(
  statistic_data_0, shuffled_times) {
  
  new_data_s <-
    statistic_data_add_shuffled_diams_FUN(
      statistic_data_0, shuffled_times, shuffled_col = "is_small_diam")
  
  x <- c("is_small_diam", 
         grep(pattern = "shuffled",
              x = colnames(new_data_s), value = T))
  
  fun <- calculate_real_growth_FUN
  
  describe_data <- bigmemory::describe(new_data_s) 
  
  varlist <- c("describe_data", "as_data_frame_FUN", "deepcopy_big_matrix_FUN", "create_directory_FUN")
  
  text_to_parse <- "require(bigmemory)"
  
  results_list <- parallel_lapply_FUN(
    x = x, fun = fun, varlist = varlist, text_to_parse = text_to_parse)
  
  
  results <- real_results_add_shuffled_sd_FUN_1(results_list)
  
  results[, "data_type"] <- "growth"
  
  unlink_dir_FUN(big_matirx_except = statistic_data_0)
  
  
  # return
  results
}

#```




## reduce_memory_consumption_FUN
#```{r}
reduce_memory_consumption_FUN <- function(
  FUN_0, statistic_data, shuffled_times,  n_running_speed) {
  
  annul_levels <- unique(statistic_data[, "annul_interval"])
  
  len <- length(annul_levels)
  
  n_running_speed <- round(n_running_speed)
  
  warning_txt <- "'n_running_speed' should be >= 1 and <= 'annulus_number', "
  
  if(n_running_speed > length(annul_levels)) {
    
    warning(warning_txt, 
            sprintf(fmt = "and which has been converted to %s.", len))
    n_running_speed <- len
    
  } else {
    
    if(n_running_speed < 1){
      
      warning(warning_txt, "and which has been converted to 1.")
      n_running_speed <- 1
      
    } else {NULL}
  }
  
  
  annul_levels_split <- split(
    x = annul_levels,
    f = rep(
      x = 1:ceiling(length(annul_levels) / n_running_speed),
      each = n_running_speed, 
      length.out = length(annul_levels)))
  
  
  results_list <- lapply(
    X = annul_levels_split, FUN = function(i){
      
      row_0 <- bigmemory::mwhich(
        x = statistic_data,
        cols = "annul_interval",
        vals = c(min(i), max(i)),
        comps = c("ge", "le"),
        op = "AND")
      
      
      statistic_data_0 <- deepcopy_big_matrix_FUN(big_matrix = statistic_data,
                                                  col_0 = colnames(statistic_data),
                                                  row_0 = row_0)
      
      
      results <- FUN_0(statistic_data_0 = statistic_data_0,
                       shuffled_times = shuffled_times)
      
      
      unlink_dir_FUN(big_matirx_except = statistic_data)
      
      #
      results
    })
  
  results <- do.call(what = rbind, args = results_list)
  
  unlink_dir_FUN(big_matirx_except = statistic_data)
  
  # return
  results
}
#```



## EAA_part_3_growth_results_FUN
#```{r}
EAA_part_3_growth_results_FUN <- function(
  statistic_data, shuffled_times,
  n_running_speed) {
  
  growth_results <- reduce_memory_consumption_FUN(
    FUN_0 = EAA_part_3_growth_results_FUN_0,
    statistic_data, shuffled_times, n_running_speed)
  
  colnames(growth_results)[
    grep(pattern = "biom_interval",
         x = colnames(growth_results))] <- "condition"
  
  # return
  growth_results
}

#```


#-----------------------------------

# "EAA main program, part 4, begin~"

# ---
# title: "EAA main program, part 4"
# subtitle: "EAA_part_4_born_clust_died_results_FUN"
# author: "wangbinzjcc@qq.com"
# date: "2019-05-10"
# ---


## split_sum_FUN
#```{r}

list_to_data_frame_FUN <- function(x_split_sum) {
  
  names_strsplit <- strsplit(x = names(x_split_sum), split = ":")
  
  results <- as.data.frame(
    do.call(what = rbind, args = names_strsplit))
  
  colnames(results) <- c("annul_interval", "phyl_interval",
                         "dbh_interval")
  
  results[, "value"] <- as.numeric(as.character(x_split_sum))
  
  # return
  results
}


#----------------------


split_stat_FUN <- function(
  new_data_s, col_dbh_interval = "dbh_interval",
  splitcol = "born_number", fun = mean) {
  
  x_split <- bigtabulate::bigsplit(
    x = new_data_s,
    ccols = c("annul_interval", "phyl_interval",
              col_dbh_interval),
    splitcol = splitcol)
  
  x_split_stat <- lapply(X = x_split, FUN = fun, na.rm = T)
  
  x_split_stat <- list_to_data_frame_FUN(x_split_stat) 
  
  x_split_stat[, "data_type"] <- strsplit(x = splitcol, split = "_")[[1]][1]
  
  # return
  x_split_stat
}


#```  



## born_clust_died_rates_FUN 
#```{r}
born_clust_died_rates_FUN <-
  function(col_dbh_interval = "dbh_interval") {
    
    new_data_s <-
      bigmemory::attach.big.matrix(describe_data)
    
    
    mean_born_abun <- split_stat_FUN(
      new_data_s, col_dbh_interval = col_dbh_interval, splitcol = "born_number", fun = mean)
    
    
    mean_clust_abun <- split_stat_FUN(
      new_data_s, col_dbh_interval = col_dbh_interval, splitcol = "clust_number", fun = mean) 
    
    
    mean_diedS_abun <- split_stat_FUN(
      new_data_s, col_dbh_interval = col_dbh_interval, splitcol = "diedS_number", fun = mean)
    
    
    mean_diedL_abun <- split_stat_FUN(
      new_data_s, col_dbh_interval = col_dbh_interval, splitcol = "diedL_number", fun = mean)
    
    
    mean_diedS_abun[, "value"] <- 
      mean_diedS_abun[, "value"] / (mean_diedS_abun[, "value"] + mean_clust_abun[, "value"])
    
    mean_diedL_abun[, "value"] <- 
      mean_diedL_abun[, "value"] / (mean_diedL_abun[, "value"] + mean_clust_abun[, "value"])
    
    
    results <- rbind(mean_born_abun, mean_clust_abun, mean_diedS_abun, mean_diedL_abun)
    
    # return
    results
  }

#```




## real_results_add_shuffled_sd_FUN
#```{r}
real_results_add_shuffled_sd_FUN_2 <-  function(
  results_list) {
  
  real_results <- results_list[[1]]
  
  shuffled_data <- do.call(what = cbind, args = results_list[-1])
  
  shuffled_data_1 <- shuffled_data[
    grep(pattern = "value", x = colnames(shuffled_data))]
  
  real_results[, "sd_shuffled"] <- apply(
    X = shuffled_data_1, MARGIN = 1, FUN = sd, na.rm = T)
  
  real_results[, "mean_shuffled"] <- apply(
    X = shuffled_data_1, MARGIN = 1, FUN = mean, na.rm = T)
  
  real_results[, "relative_value"] <- 
    real_results[, "value"] - real_results[, "mean_shuffled"]
  
  colnames(real_results)[
    colnames(real_results) == "value"] <- "real_value"
  
  # return
  real_results
}

#```



## EAA_part_4_born_clust_died_results_FUN_0
#```{r}
EAA_part_4_born_clust_died_results_FUN_0 <- function(
  statistic_data_0, shuffled_times) {
  
  
  new_data_s <-
    statistic_data_add_shuffled_diams_FUN(
      statistic_data_0, shuffled_times, shuffled_col = "dbh_interval")
  
  
  x <- c("dbh_interval", 
         grep(pattern = "shuffled",
              x = colnames(new_data_s), value = T))
  
  fun <- born_clust_died_rates_FUN
  
  describe_data <- bigmemory::describe(new_data_s) 
  
  varlist <- c(
    "describe_data", "split_stat_FUN",
    "list_to_data_frame_FUN")
  
  text_to_parse <- "require(bigmemory)"
  
  
  results_list <- parallel_lapply_FUN(
    x = x, fun = fun, varlist = varlist,
    text_to_parse = text_to_parse)
  
  results <- real_results_add_shuffled_sd_FUN_2(results_list)
  
  unlink_dir_FUN(big_matirx_except = statistic_data_0)
  
  # return
  results
}
#```


## EAA_part_4_born_clust_died_results_FUN
#```{r}
EAA_part_4_born_clust_died_results_FUN <- function(
  statistic_data, shuffled_times, n_running_speed) {
  
  born_clust_died_results <- reduce_memory_consumption_FUN(
    FUN_0 = EAA_part_4_born_clust_died_results_FUN_0,
    statistic_data, shuffled_times, n_running_speed)
  
  colnames(born_clust_died_results)[
    grep(pattern = "dbh_interval",
         x = colnames(born_clust_died_results))] <- "condition"
  
  # return
  born_clust_died_results
  
}

#```




#----------------------------

# "EAA main program, part 5, begin~"

# ---
# title: "summary_sp_pairs_FUN"

# author: "wangbinzjcc@qq.com"
# date: "2019-05-10"
# ---


#```{r }

table_sp_pairs_annul_1_FUN <- function(
  id_pair_data, data_type = c("survival",  "born", "diedS", "diedL")[1]) {
  
  mpermute(x = id_pair_data, 
           cols= c("survival_born_diedS_diedL", "annul_interval"))
  
  val <- switch(
    EXPR = data_type,
    survival = 1, born = 2, diedS = 3, diedL = 4,
    stop("'data_type' must be one of 'survival',  'born', 'diedS', 'diedL'."))
  
  which_survival <- mwhich(
    x = id_pair_data,
    cols = c("survival_born_diedS_diedL", "annul_interval"),
    vals = c(val, 1), comps = "eq", op = "AND")
  
  survival_id_pairs <- sub.big.matrix(
    x = id_pair_data, 
    firstRow = min(which_survival), 
    lastRow = max(which_survival))
  
  gc()
  
  # return
  bigtabulate::bigtable(
    x = survival_id_pairs,
    ccols = c("focal_sp", "annular_sp", "phyl_interval"))
}


###

data_frame_top_10_FUN <- function(
  sp_pairs = sp_pairs){
  
  sp_pairs <- as.array(sp_pairs)
  
  index_1_2_row <- apply(X = sp_pairs, MARGIN = 3, FUN = row)
  
  index_1_2_col <- apply(X = sp_pairs, MARGIN = 3, FUN = col)
  
  index_3 <- col(index_1_2_row)
  
  dat_new <- data.frame(
    focal_sp = dimnames(sp_pairs)[[1]][index_1_2_row],
    annular_sp = dimnames(sp_pairs)[[2]][index_1_2_col],
    phyl_interval = dimnames(sp_pairs)[[3]][index_3],
    abundance = as.integer(sp_pairs),
    stringsAsFactors = F)
  
  dat_split <- split(x = dat_new[, "abundance"],
                     f = dat_new[, "phyl_interval"])
  
  value_unsplit <- lapply(X = dat_split, FUN = function(list_i) {
    
    value_limit <- ifelse(
      test = length(list_i) >= 10, 
      yes = sort(x = list_i, decreasing = T)[10],
      no = 0)
    
    is_top_10 <- list_i > value_limit
    
    # 
    is_top_10
  })
  
  index_is_top_10 <- unsplit(value = value_unsplit,
                             f = dat_new[, "phyl_interval"])
  
  # return
  dat_new[index_is_top_10, ]
}



sp_pairs_df_FUN <- function(
  id_pair_data, data_type = "survival") {
  
  sp_pairs <- table_sp_pairs_annul_1_FUN(
    id_pair_data, data_type = data_type)
  
  sp_pairs_df <- data_frame_top_10_FUN(sp_pairs)
  
  sp_pairs_df[, "data_type"] <- data_type
  
  # return
  sp_pairs_df
}


add_phyl_dist_FUN <- function(
  summary_sp_pairs, pairwise_table_scenario_data, min_sample_phyl_interval) {
  
  phyl_dist_of_sp_pairs_data <- phyl_dists_of_paired_species_FUN(
    pairwise_table_scenario_data, min_sample_phyl_interval)
  
  index_between_phyl_pairs <- match(
    x = paste(
      summary_sp_pairs[, "focal_sp"], 
      summary_sp_pairs[, "annular_sp"]),
    table = paste(
      phyl_dist_of_sp_pairs_data[, "focal_sp"],
      phyl_dist_of_sp_pairs_data[, "annular_sp"]))
  
  summary_sp_pairs[, "phyl_dist"] <- phyl_dist_of_sp_pairs_data[
    index_between_phyl_pairs, "phyl_dist"]
  
  rownames(summary_sp_pairs) <- NULL
  
  # return
  summary_sp_pairs[
    order(summary_sp_pairs[, "data_type"],
          summary_sp_pairs[, "phyl_interval"],
          - summary_sp_pairs[, "abundance"]), ]
}

###
#```








# summary_sp_pairs_FUN
#```{r}
#
summary_sp_pairs_FUN <- function(
  id_pair_data, pairwise_table_scenario_data,
  min_sample_phyl_interval) {
  
  sp_pairs_survival <- sp_pairs_df_FUN(
    id_pair_data, data_type = "survival")
  
  sp_pairs_born <- sp_pairs_df_FUN(
    id_pair_data, data_type = "born")
  
  sp_pairs_diedS <- sp_pairs_df_FUN(
    id_pair_data, data_type = "diedS")
  
  sp_pairs_diedL <- sp_pairs_df_FUN(
    id_pair_data, data_type = "diedL")
  
  #
  
  summary_sp_pairs  <- rbind(sp_pairs_survival, sp_pairs_born, sp_pairs_diedS, sp_pairs_diedL)
  
  summary_sp_pairs  <- add_phyl_dist_FUN(
    summary_sp_pairs, pairwise_table_scenario_data,
    min_sample_phyl_interval)
  
  summary_sp_pairs[, "annul_interval"] <- 1
  
  summary_sp_pairs[, "census_interval"] <- NA
  
  # return
  summary_sp_pairs
}

#```




#----------------------------

# "EAA main program, part 6, begin~"

# ---
# title: "all_EAA_analysis_results_data_FUN"

# author: "wangbinzjcc@qq.com"
# date: "2019-05-10"
# ---



# EAA_results_add_dists_FUN
#```{r}
EAA_results_add_dists_FUN <- function(
  EAA_results, id_pair_data) {
  
  annul_dist_mean <- tapply(
    X = id_pair_data[, "annul_dist"],
    INDEX = id_pair_data[, "annul_interval"],
    FUN = mean)
  
  phyl_dist_mean <- tapply(
    X = id_pair_data[, "phyl_dist"],
    INDEX = id_pair_data[, "phyl_interval"],
    FUN = mean)
  
  index_annul <- match(
    x = as.character(EAA_results[, "annul_interval"]),
    table = names(annul_dist_mean))
  
  EAA_results[, "annul_dist"] <- round(
    annul_dist_mean[index_annul], 2)
  
  index_phyl <- match(
    x = as.character(EAA_results[, "phyl_interval"]),
    table = names(phyl_dist_mean))
  
  EAA_results[, "phyl_dist"] <- round(
    phyl_dist_mean[index_phyl], 2)
  
  # return
  EAA_results
}


###
#```


## EAA_analysis_results_of_single_census_FUN
#```{r}

EAA_analysis_results_of_single_census_FUN <- function(
  big_array_data, census_interval, annulus_number, annulus_size,
  pairwise_table_scenario_data, quantile_number,
  min_sample_phyl_interval, shuffled_times, max_matrix_row_limit, n_running_speed,
  z_growth_model) {
  #
  
  single_census_interval_data <- EAA_part_0_single_census_interval_data_FUN(
    big_array_data, census_interval, annulus_number, 
    annulus_size, z_growth_model)
  
  print("( part 0 / part 4 ) ~ ~ EAA analysis begins ~ ~", quote = F)
  st_0 = Sys.time()
  max_mem_0 = round(sum(gc()[, ncol(gc())]))
  dim_single_data = dim(single_census_interval_data)
  print(paste("Maximum memory allocation", max_mem_0, "Mb"), quote = F) 
  print(paste("Current memory allocation", round(sum(gc()[, 2])), "Mb"), quote = F)
  print(paste("dim(single_census_interval_data):",
              paste(dim_single_data, collapse = ", ")), quote = F)
  print(st_0, quote = F)
  cat("\n")
  
  id_pair_data <- EAA_part_1_focal_and_annular_id_pair_data_FUN(
    single_census_interval_data, annulus_number, annulus_size,
    pairwise_table_scenario_data, quantile_number,
    min_sample_phyl_interval)
  
  summary_sp_pairs <- summary_sp_pairs_FUN(
    id_pair_data, pairwise_table_scenario_data,
    min_sample_phyl_interval)
  
  summary_sp_pairs[, "census_interval"] <- census_interval
  
  
  st_1 = Sys.time()
  max_mem_1 = round(sum(gc()[, ncol(gc())]))
  dim_id_pairs = dim(id_pair_data)
  print("( part 1 / part 4 ) is finished", quote = F)
  print(paste("Maximum memory allocation", max_mem_1, "Mb"), quote = F) 
  print(paste("Current memory allocation", round(sum(gc()[, 2])), "Mb"), quote = F)
  print(paste("dim(focal_and_annular_id_pairs):",
              paste(dim_id_pairs, collapse = ", ")), quote = F)
  print(st_1, quote = F)
  cat("\n")
  
  
  statistic_data <- EAA_part_2_all_quantiles_statistics_data_FUN(
    id_pair_data, single_census_interval_data, max_matrix_row_limit)
  
  st_2 = Sys.time()
  max_mem_2 = round(sum(gc()[, ncol(gc())]))
  dim_stat_data = dim(statistic_data)
  print("( part 2 / part 4 ) is finished", quote = F)
  print(paste("Maximum memory allocation", max_mem_2, "Mb"), quote = F) 
  print(paste("Current memory allocation", round(sum(gc()[, 2])), "Mb"), quote = F)
  print(paste("dim(all_quantiles_statistic_data):",
              paste(dim_stat_data, collapse = ", ")), quote = F)
  print(st_2, quote = F)
  cat("\n")
  # 
  growth_results <- EAA_part_3_growth_results_FUN(
    statistic_data, shuffled_times, n_running_speed)
  # 
  
  st_3 = Sys.time()
  max_mem_3 = round(sum(gc()[, ncol(gc())]))
  dim_growth_results = dim(growth_results)
  
  print("( part 3 / part 4 ) is finished", quote = F)
  print(paste("Maximum memory allocation", max_mem_3, "Mb"), quote = F) 
  print(paste("Current memory allocation", round(sum(gc()[, 2])), "Mb"), quote = F)
  print(paste("dim(growth_results):",
              paste(dim(growth_results), collapse = ", ")), quote = F)
  print(st_3, quote = F)
  cat("\n")
  
  born_clust_died_results <- EAA_part_4_born_clust_died_results_FUN(
    statistic_data, shuffled_times, n_running_speed)
  
  EAA_results <- rbind(growth_results, born_clust_died_results)
  
  EAA_results[, "census_interval"] <- census_interval
  
  EAA_results <- EAA_results_add_dists_FUN(EAA_results, id_pair_data)
  
  
  st_4 = Sys.time()
  max_mem_4 = round(sum(gc()[, ncol(gc())]))
  dim_born_clust_died_results = dim(born_clust_died_results)
  print("( part 4 / part 4 ) is finished", quote = F)
  print(paste("Maximum memory allocation", max_mem_4, "Mb"), quote = F) 
  print(paste("Current memory allocation", round(sum(gc()[, 2])), "Mb"), quote = F)
  print(paste("dim(born_clust_died_results):",
              paste(dim_born_clust_died_results, collapse = ", ")), quote = F)
  print(st_4, quote = F)
  cat("\n")
  
  print("Results list:", quote = F)
  print(paste("dim(EAA_results):", 
              paste(dim(EAA_results), collapse = ", ")), quote = F)
  print(paste("dim(summary_sp_pairs):", 
              paste(dim(summary_sp_pairs), collapse = ", ")), quote = F)
  cat("\n")
  cat("\n")
  #
  
  detailed_information <- data.frame(
    code_location = c("part 0", "part 1", "part 2", "part 3", "part 4"),
    time_current = c(st_0, st_1, st_2, st_3, st_4),
    time_used = round(c(st_0, st_1, st_2, st_3, st_4) - c(st_0, st_0, st_1, st_2, st_3)),
    memory_max_allocation = c(max_mem_0, max_mem_1, max_mem_2, max_mem_3, max_mem_4),
    data_name = c("single_census_interval_data", "id_pair_data", "statistic_data",
                  "growth_results", "born_clust_died_results"),
    data_nrow = c(dim_single_data[1], dim_id_pairs[1], dim_stat_data[1], dim_growth_results[1],
                  dim_born_clust_died_results[1]),
    census_interval = census_interval
  )
  
  rm(list = c("single_census_interval_data", "id_pair_data", "statistic_data",
              "growth_results", "born_clust_died_results"))
  gc()
  
  # return
  list(EAA_results = EAA_results,
       summary_sp_pairs = summary_sp_pairs,
       detailed_information = detailed_information)
}

#```



## EAA_analysis_results_of_all_censuses_FUN
#```{r}
EAA_analysis_results_of_all_censuses_FUN <- function(
  big_array_data, census_interval_number, pairwise_table_scenario_data, annulus_size, annulus_number,
  quantile_number, shuffled_times, min_sample_phyl_interval, max_matrix_row_limit, n_running_speed,
  z_growth_model)
{
  
  results_list <- lapply(
    X = 1: round(census_interval_number), FUN = function(i){
      
      print(paste("    census interval =", i, "   "))
      print(Sys.time(), quote = F)
      cat("\n")
      
      EAA_analysis_results_of_single_census_FUN(
        big_array_data, census_interval = i, 
        annulus_number, annulus_size,
        pairwise_table_scenario_data, quantile_number,
        min_sample_phyl_interval, shuffled_times, max_matrix_row_limit, n_running_speed,
        z_growth_model)
    })
  
  # return
  results_list
}
#```


## results_list_to_data_frame_FUN
#```{r}
results_list_to_data_frame_FUN <- function(
  replicated_results_list, data_names = "EAA_results") {
  
  results_list <- lapply(
    X = replicated_results_list, FUN = function(list_i) {
      
      args_rbind <- lapply(X = list_i, FUN = function(list_j){
        list_j[[data_names]]})
      # 
      do.call(what = rbind, args = args_rbind)
    })
  
  results <- do.call(what = rbind, args = results_list)
  
  results[, "replicate_time"] <- rep(
    x = 1:length(results_list), 
    times = sapply(X = results_list, FUN = nrow))
  
  results[, "plot_name"] <- plot_name
  
  #return
  results
}
#```


## EAA_analysis_results_FUN
#```{r}
EAA_analysis_results_FUN <- function(
  replicate_EAA_times = 3, 
  big_array_data = nonggang_bigarray,  census_interval_number = 1,  
  pairwise_table_scenario_data = nonggang_pairwise_table_scenario_2,
  annulus_size = 5, annulus_number = 3,
  quantile_number = 3, shuffled_times = 10, 
  min_sample_phyl_interval = 2,  plot_name = plot_name,
  working_directory = getwd(),
  max_matrix_row_limit = 2e7,
  n_running_speed = 5,
  z_growth_model = c("gls", "scale")[1]){ 
  
  time_0 <- Sys.time()
  temp_file_name <- sprintf(fmt = "%s/%s_%s", working_directory, "temp_file", plot_name)
  
  if (! dir.exists(paths = temp_file_name)) {
    dir.create(path = temp_file_name)} else {NULL}
  setwd(dir = temp_file_name)
  
  if (! requireNamespace("bigtabulate", quietly = T)){
    install.packages("bigtabulate")
  } else {NULL}
  
  if (! requireNamespace("nlme", quietly = T)){
    install.packages("nlme")} else {NULL}
  
  require(bigtabulate); require(nlme); require(parallel);  require(compiler);
  
  replicated_results_list <- lapply(
    X = 1:replicate_EAA_times, 
    FUN = function(i) {
      
      cat("\n")
      print(x = sprintf(fmt = "Replicated EAA analysis time = %s", i));
      cat("\n")
      
      EAA_analysis_results_of_all_censuses_FUN(
        big_array_data, census_interval_number, 
        pairwise_table_scenario_data, annulus_size, annulus_number,
        quantile_number, shuffled_times, min_sample_phyl_interval, 
        max_matrix_row_limit, n_running_speed,
        z_growth_model)
    })
  
  EAA_results <- results_list_to_data_frame_FUN(
    replicated_results_list, data_names = "EAA_results")
  
  EAA_results <- EAA_results[
    order(EAA_results[, "data_type"], EAA_results[, "annul_interval"],
          EAA_results[, "phyl_interval"], EAA_results[, "condition"]), ]
  
  summary_sp_pairs <- results_list_to_data_frame_FUN(
    replicated_results_list, data_names = "summary_sp_pairs")
  
  detailed_information <- results_list_to_data_frame_FUN(
    replicated_results_list, data_names = "detailed_information")
  
  time_1 <- Sys.time()
  
  #
  running_information <- list(
    time_begin = time_0,
    time_end = time_1,
    time_running = time_1 - time_0,
    plot_name = plot_name,
    data_dim = dim(big_array_data),
    census_interval_number = census_interval_number,
    replicate_EAA_times = replicate_EAA_times,
    annulus_size = annulus_size,
    annulus_number = annulus_number,
    quantile_number = quantile_number,
    shuffled_times = shuffled_times,
    min_sample_phyl_interval = min_sample_phyl_interval,
    r_version = sessionInfo()[["R.version"]][["version.string"]],
    os_Version = sessionInfo()[["running"]],
    num_cpu = parallel::detectCores(), 
    memory_max_allocation = memory.size(T),
    memory_current_allocation = memory.size(F),
    memory_limit = memory.size(NA),
    gc = gc(),
    max_matrix_row_limit = max_matrix_row_limit,
    n_running_speed = n_running_speed,
    detailed_information = detailed_information
  )
  
  #
  rm(list = ls()[! ls() %in% c("EAA_results", "summary_sp_pairs", 
                               "running_information",
                               "working_directory", "temp_file_name")])
  gc()
  
  setwd(dir = working_directory)
  unlink(x = temp_file_name, recursive = T, force = T)
  
  # return
  list(EAA_results = EAA_results,
       summary_sp_pairs = summary_sp_pairs,
       running_information = running_information)
  
}

#```





