# remove_HokkaidoOkinawa
setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
source("R_code/1_prefectural_data.R")

# 本章の成果物
regional_division <- regional_division[-c(1,47),]
pref_list <- pref_list[-c(1,47),]
metapredictor_table <- metapredictor_table[-c(1,47),]
pref_vec <- pref_vec[-c(1,47)]
region_vec_level <- region_vec_level[-c(1,11)]

source("R_code/2_prefectural_BLUP.R")

first_result_list
nationwide_2stage_BLUP
nationwide_2stage_BLUP$data[[1]]

# 以下、source使わない方がいいような気がする

# -------------------------------------------------------------------------
AF_usingWBGT_45prefectures_90th <- 
  AF_usingWBGT_90th %>% 
  mutate(group = "45prefectures")
save(AF_usingWBGT_45prefectures_90th,
     file="R_code/usingWBGT_data/AF_usingWBGT_45prefectures_90th.R")

# -------------------------------------------------------------------------
AF_usingWBGT_45prefectures_95th <- 
  AF_usingWBGT_95th %>% 
  mutate(group = "45prefectures")
save(AF_usingWBGT_45prefectures_95th,
     file="R_code/usingWBGT_data/AF_usingWBGT_45prefectures_95th.R")

# -------------------------------------------------------------------------
RR_usingWBGT_45prefectures <- 
  RR_usingWBGT %>% 
  mutate(group = "45prefectures")
save(RR_usingWBGT_45prefectures,
     file="R_code/usingWBGT_data/RR_usingWBGT_45prefectures.R")




