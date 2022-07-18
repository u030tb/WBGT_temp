# remove_HokkaidoOkinawa
source("R_code/1_prefectural_data.R")

regional_division <- regional_division[-c(1,47),]
pref_list <- pref_list[-c(1,47),]
metapredictor_table <- metapredictor_table[-c(1,47),]
pref_vec <- pref_vec[-c(1,47)]
region_vec_level <- region_vec_level[-c(1,11)]

source("R_code/2_prefectural_BLUP.R")

# 45 prefs?
first_result_list
nationwide_2stage_BLUP
nationwide_2stage_BLUP$data[[1]]

# -------------------------------------------------------------------------
# QAIC 
# -------------------------------------------------------------------------

bind_rows(

  # pref-level qAICs were aggregated with weight (pref-level sum of total deaths)
  first_result_list %>%
    select(prefname, qAIC_temp, qAIC_WBGT) %>%
    mutate(weight_total_all = map_dbl(1:45,function(x){
      first_result_list$data[[x]]$all %>% sum
    })) %>%
    mutate(weight_sum = sum(weight_total_all)) %>%
    mutate(weight_frac = weight_total_all / weight_sum) %>%
    mutate(qAIC_temp_weight = qAIC_temp * weight_frac,
           qAIC_WBGT_weight = qAIC_WBGT * weight_frac) %>%
    summarize(qAIC_temp = sum(qAIC_temp_weight),
              qAIC_WBGT = sum(qAIC_WBGT_weight)) %>%
    mutate(weight = "case_weight"),

  # simple mean
  first_result_list %>%
    select(prefname, qAIC_temp, qAIC_WBGT) %>%
    summarize(qAIC_temp = mean(qAIC_temp),
              qAIC_WBGT = mean(qAIC_WBGT)) %>%
    mutate(weight = "simple")
) %>%

  write.csv("R_code/qAIC/qAIC_remove_HokkaidoOkinawa.csv",
            row.names = F)


# -------------------------------------------------------------------------
# AF_usingWBGT_45prefectures_90th <- 
#   AF_usingWBGT_90th %>% 
#   mutate(group = "45prefectures")
# save(AF_usingWBGT_45prefectures_90th,
#      file="R_code/usingWBGT_data/AF_usingWBGT_45prefectures_90th.R")

# -------------------------------------------------------------------------
# AF_usingWBGT_45prefectures_95th <- 
#   AF_usingWBGT_95th %>% 
#   mutate(group = "45prefectures")
# save(AF_usingWBGT_45prefectures_95th,
#      file="R_code/usingWBGT_data/AF_usingWBGT_45prefectures_95th.R")

# -------------------------------------------------------------------------
RR_usingWBGT_45prefectures <- 
  RR_usingWBGT %>% 
  mutate(group = "45prefectures")
save(RR_usingWBGT_45prefectures,
     file="R_code/usingWBGT_data/RR_usingWBGT_45prefectures.R")
