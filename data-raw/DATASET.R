## code to prepare `DATASET` dataset goes here

devtools::load_all(".")
library(purrr)
library(dplyr)

1:7 %>% 
  map_dfr(
    ~ dpr_search(paste0("&page=", .x))
  ) -> lists
lists %>% 
  slice(1:62) -> lists

dpr_list <- lists

usethis::use_data(dpr_list, overwrite = T)
