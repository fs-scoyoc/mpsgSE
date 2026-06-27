## code to prepare `sp_list_ex` dataset goes here
library(tidyverse)
library(readr)

sp_list_ex <- readr::read_csv(file.path("data-raw/data", "spp_list.csv"),
                              show_col_types = FALSE)
usethis::use_data(sp_list_ex, overwrite = TRUE)
