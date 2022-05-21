# summary stat
# (1) region-level summary-stat
# (2) prefecture-level summary-stat

# font

# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
source("R_code/1_prefectural_data.R")

# R object
regional_division
pref_list
metapredictor_table
pref_vec
region_vec_level

pacman::p_load(
  gt
)


##############################################################################
# 
# summary-stat for region 
# 
##############################################################################

regional_summary_stat <- 
  pref_list %>% 
  select(prefcode,prefname,data,region) %>% 
  unnest(data) %>% 
  group_by(region,date) %>% 
  summarize(all = sum(all),
            tmax = mean(tmax),
            maxWBGT = mean(maxWBGT)) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  summarize(across(.cols= c("all","tmax","maxWBGT"),
                   .fns = list(mean = mean,
                               sd = sd,
                               q90 = ~{quantile(.x,0.9)},
                               q95 = ~{quantile(.x,0.95)}),
                   .names = "{col}_{fn}")) %>% 
  ungroup() %>% 
  mutate(across(.cols = where(is.numeric),
                .fns = ~round(.x,1))) %>% 
  left_join(regional_division %>% 
              group_by(region) %>% 
              summarize(N = n()),
            by="region") %>%
  # decimal number 
  mutate(across(.cols=contains("_"),
                .fns=~{formatC(.x, digits = 1, format="f")})) %>% 
  mutate(all_mean_sd = glue::glue("{all_mean} ({all_sd})"),
         tmax_mean_sd = glue::glue("{tmax_mean} ({tmax_sd})"),
         tmax_q90_q95 = glue::glue("({tmax_q90}, {tmax_q95})"),
         maxWBGT_mean_sd = glue::glue("{maxWBGT_mean} ({maxWBGT_sd})"),
         maxWBGT_q90_q95 = glue::glue("({maxWBGT_q90}, {maxWBGT_q95})")) %>% 
  select(region,N,
         all_mean_sd,
         tmax_mean_sd,tmax_q90_q95,
         maxWBGT_mean_sd,maxWBGT_q90_q95) %>% 
  mutate(region = region %>% as.factor %>% 
           fct_relevel(region_vec_level)) %>% 
  arrange(region)

### gt作成 ###
gt_summary_stat_region <- 
  regional_summary_stat %>% 
  gt(rowname_col = "region") %>% 
  tab_header(
    title = md("Table 1"),
    subtitle = md("Region-level summary statistics of all-cause mortality, temperature and WBGT")
  ) %>% 
  tab_options(
    table.font.names = "Arial"
  )  %>% 
  opt_align_table_header(align = "left") %>% 
  cols_width(
    "region" ~ px(140),
    "N" ~ px(60),
    everything() ~ px(110)
  ) %>% 
  cols_align(
    align = "center", 
    columns = TRUE
  ) %>% 
  tab_spanner(
    label = md("Mortality"),
    columns = vars("all_mean_sd")
  ) %>% 
  tab_spanner(
    label = md("Temperature"),
    columns = vars("tmax_mean_sd","tmax_q90_q95")
  ) %>% 
  tab_spanner(
    label = md("WBGT"),
    columns = vars("maxWBGT_mean_sd","maxWBGT_q90_q95")
  ) %>% 
  cols_label(
    all_mean_sd = md("Mean (SD)"),
    tmax_mean_sd = md("Mean (SD)"),
    tmax_q90_q95 = md("90th & 95th"),
    maxWBGT_mean_sd = md("Mean (SD)"),
    maxWBGT_q90_q95 = md("90th & 95th")
  ) %>% 
  tab_footnote(
    footnote = html("The number of prefectures within each region"),
    locations = cells_column_labels(
      column = vars("N")
    )) %>% 
  tab_footnote(
    footnote = html("90th and 95th perentile of temperature and WBGT"),
    locations = cells_column_labels(
      column = c("tmax_q90_q95","maxWBGT_q90_q95")
    )) %>% 
  tab_footnote(
    footnote = md("Daily counts of all-cause mortailty"),
    locations = cells_column_spanners(
      spanners = c("Mortality")
    )
  ) %>% 
  tab_footnote(
    footnote = md("Daily maximum temperature or WBGT during warm season (May-October)"),
    locations = cells_column_spanners(
      spanners = c("Temperature","WBGT")
    )
  )
  
gt_summary_stat_region

# save --------------------------------------------------------------------
# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# gtsave(gt_summary_stat_region,
#        "figure/Table1_summary_stat_region.pdf",
#        zoom=0.7)

##############################################################################
# 
# summary-stat for prefecture
# 
##############################################################################

prefectural_summary_stat <- 
  pref_list %>% 
  select(prefname,data,region) %>% 
  unnest(data) %>% 
  group_by(region,prefname) %>% 
  summarize(across(.cols= c("all","tmax","maxWBGT"),
                   .fns = list(mean = mean,
                               sd = sd,
                               q90 = ~{quantile(.x,0.9)},
                               q95 = ~{quantile(.x,0.95)}),
                   .names = "{col}_{fn}")) %>% 
  ungroup() %>% 
  mutate(across(.cols = where(is.numeric),
                .fns = ~round(.x,1))) %>% 
  # decimal number 
  mutate(across(.cols=contains("_"),
                .fns=~{formatC(.x, digits = 1, format="f")})) %>% 
  mutate(all_mean_sd = glue::glue("{all_mean} ({all_sd})"),
         tmax_mean_sd = glue::glue("{tmax_mean} ({tmax_sd})"),
         tmax_q90_q95 = glue::glue("({tmax_q90}, {tmax_q95})"),
         maxWBGT_mean_sd = glue::glue("{maxWBGT_mean} ({maxWBGT_sd})"),
         maxWBGT_q90_q95 = glue::glue("({maxWBGT_q90}, {maxWBGT_q95})")) %>% 
  select(region,prefname,
         all_mean_sd,
         tmax_mean_sd,tmax_q90_q95,
         maxWBGT_mean_sd,maxWBGT_q90_q95) %>% 
  mutate(prefname = prefname %>% as.factor %>% 
           fct_relevel(pref_vec),
         region = region %>% as.factor %>% 
           fct_relevel(region_vec_level)) %>% 
  arrange(region,prefname)


### gt作成 ###
gt_summary_stat_pref <- 
  prefectural_summary_stat %>% 
  gt(rowname_col = "prefname",
     groupname_col = "region") %>% 
  tab_header(
    title = md("Table S2"),
    subtitle = md("Prefecture-level summary statistics of all-cause mortality, Temperature and WBGT")
  ) %>% 
  tab_options(
    table.font.names = "Arial"
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  cols_width(
    "prefname" ~ px(120),
    everything() ~ px(110)
  ) %>% 
  cols_align(
    align = "center", 
    columns = TRUE
  ) %>% 
  tab_spanner(
    label = md("Mortality"),
    columns = vars("all_mean_sd")
  ) %>% 
  tab_spanner(
    label = md("Temperature"),
    columns = vars("tmax_mean_sd","tmax_q90_q95")
  ) %>% 
  tab_spanner(
    label = md("WBGT"),
    columns = vars("maxWBGT_mean_sd","maxWBGT_q90_q95")
  ) %>% 
  cols_label(
    all_mean_sd = md("Mean (SD)"),
    tmax_mean_sd = md("Mean (SD)"),
    tmax_q90_q95 = md("90th & 95th"),
    maxWBGT_mean_sd = md("Mean (SD)"),
    maxWBGT_q90_q95 = md("90th & 95th")
  ) %>% 
  tab_footnote(
    footnote = html("90th and 95th perentile of temperature and WBGT"),
    locations = cells_column_labels(
      column = c("tmax_q90_q95","maxWBGT_q90_q95")
    )) %>% 
  tab_footnote(
    footnote = md("Daily counts of all-cause mortailty"),
    locations = cells_column_spanners(
      spanners = c("Mortality")
    )
  ) %>% 
  tab_footnote(
    footnote = md("Daily maximum temperature or WBGT during warm season (May-October)"),
    locations = cells_column_spanners(
      spanners = c("Temperature","WBGT")
    )
  )
  

gt_summary_stat_pref

# save --------------------------------------------------------------------
# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# gtsave(gt_summary_stat_pref,
#        "figure/TableS2_summary_stat_pref.pdf",
#        zoom=0.7)
