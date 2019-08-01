# ---
# title: "test_EAA_3"
# author: "wangbinzjcc@qq.com"
# date: "2019/06/03"
# output: html_document
# editor_options: 
#  chunk_output_type: console
# ---



## Package
#```{r}

require(bigtabulate)
require(nlme)

#```

## Data
#```{r}
 setwd("D:\\wangbinzjcc\\EAA_pnas")
 
 load("data\\bci.bigarray")
# 
#  bci.bigarray <- bci.bigarray[
#    bci.bigarray[, "gx"] < 100 & bci.bigarray[, "gy"] < 100, ]
#   
 load("data\\bci.pairwise.table.scenario.2")

 # bci.bigarray[1:20, 1:8]
 
#```


## Arguments
#```{r}

 replicate_EAA_times = 10;
 big_array_data = bci.bigarray; 
 census_interval_number = (6-1);
 pairwise_table_scenario_data =
  bci.pairwise.table.scenario.2;
 annulus_size = 25; annulus_number = 20;
 quantile_number = 20;
 shuffled_times = 100;
 min_sample_phyl_interval = 5;
 plot_name = "bci";
 working_directory = getwd();
 
 # census_interval = 1

#```

## File_save
#```{r }

file_save <- sprintf(
  fmt = "EAA_results\\dat_0_%s_a_%s_size_%s_q_%s_EAA_%s.save",
  plot_name,annulus_number,annulus_size,quantile_number,
  format(Sys.time(), "%Y%m%d_%H%M%OS"))

file_save
#```

### Load main program
#```{r}

 source("EAA_3_main_program_BinWang_2019_0627_1542.R")

#```


### Run main program
#```{r}

EAA_analysis_results <- EAA_analysis_results_FUN(
  replicate_EAA_times,
  big_array_data, census_interval_number,
  pairwise_table_scenario_data,
  annulus_size, annulus_number,
  quantile_number, shuffled_times,
  min_sample_phyl_interval, plot_name,
  working_directory)
 


save(EAA_analysis_results, file = file_save)

#```

 

### Read dat_0 and summary_sp_pairs
#```{r}

load(file = file_save)

dat_0 <- EAA_analysis_results[["EAA_results"]]

summary_sp_pairs <- EAA_analysis_results[["summary_sp_pairs"]]

#```





