# Table_RR_AF

# これなしで


# source("R_code/1_prefectural_data.R")
# source("R_code/2_prefectural_BLUP.R")
# source("R_code/3_AF_90th.R")
# source("R_code/3_AF_95th.R")
# source("R_code/3_RR.R")

gt_pref_AF_90th
gt_region_AF_90th
gt_pref_AF_95th
gt_region_AF_95th

gt_pref_RR_90th
gt_region_RR_90th
gt_pref_RR_95th
gt_region_RR_95th


# -------------------------------------------------------------------------
# TABLE版 prefecture-level attributable fraction ----------------------
# -------------------------------------------------------------------------

library(gt)

gt_pref_AF_90th <- 
  
  table_temp_AF_90th %>% 
  mutate(across(.cols = c(AF_point,AF_L95,AF_U95),
                .fns = ~{paste0(round(.x*100,2),"%")},
                .names = "{col}")) %>% 
  mutate(CI_temp = glue::glue("({AF_L95}, {AF_U95})")) %>% 
  select(region,prefname,point_temp=AF_point,CI_temp) %>% 
  
  left_join(table_WBGT_AF_90th %>% 
              mutate(across(.cols = c(AF_point,AF_L95,AF_U95),
                            .fns = ~{paste0(round(.x*100,1),"%")},
                            .names = "{col}")) %>% 
              mutate(CI_WBGT = glue::glue("({AF_L95}, {AF_U95})")) %>% 
              select(region,prefname,point_WBGT=AF_point,CI_WBGT),
            
            by=c("region","prefname")) %>% 
  mutate(region = region %>% as.factor %>% 
           fct_relevel(region_vec_level),
         prefname = prefname %>% as.factor %>% 
           fct_relevel(pref_vec)) %>% 
  arrange(region,prefname) %>% 
  ### gt ###
  gt(groupname_col = "region",
     rowname_col = "prefname") %>% 
  tab_header(
    title = md("prefecture-level attributable fraction"),
    subtitle = md(paste0(cutoff_per*100,"th cutoff"))
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  tab_spanner(
    label = md("Temperature"),
    columns = vars("point_temp","CI_temp")
  ) %>% 
  tab_spanner(
    label = md("WBGT"),
    columns = vars("point_WBGT","CI_WBGT")
  ) %>% 
  cols_width(
    "prefname" ~ px(130),
    everything() ~ px(150)
  ) %>% 
  cols_align(
    align = "center", 
    columns = TRUE
  ) %>% 
  cols_label(
    point_temp = "point",
    point_WBGT = "point",
    CI_temp = "95% CI",
    CI_WBGT = "95% CI"
  ) %>% 
  tab_footnote(
    footnote = md("North and South Kyushu region"),
    locations = cells_row_groups(
      groups=c("Kyushu_N","Kyushu_S")
    ))

gt_pref_AF_90th

# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# gtsave(gt_pref_AF_90th,
#        "figure/Table_pref_AF_90th.pdf",
#        zoom=0.7)


# -------------------------------------------------------------------------
# TABLE版 region-level attribution fraction ----------------------
# -------------------------------------------------------------------------

library(gt)

gt_region_AF_90th <-
  
  pooled_AF_90th %>%
  select(region,heat,Estimate,L95,U95) %>%
  mutate(across(.cols=where(is.numeric),
                .fns=~{paste0(round(.x*100,2),"%")},
                .names = "{col}")) %>%
  pivot_wider(names_from = heat,
              values_from = c(Estimate,L95,U95)) %>%
  mutate(CI_temperature = glue::glue("({L95_temperature}, {U95_temperature})"),
         CI_WBGT = glue::glue("({L95_WBGT}, {U95_WBGT})")) %>%
  mutate(group = ifelse(region == "nationwide","nationwide","region")) %>%
  mutate(region = region %>% as.factor %>%
           fct_relevel(c("nationwide",region_vec_level))) %>%
  select(group,region,contains(c("Estimate","CI"))) %>%
  arrange(region) %>%
  
  ### gt ###
  gt(groupname_col = "group",
     rowname_col = "region") %>%
  tab_header(
    title = md("region-level attributable fraction"),
    subtitle = md(paste0(cutoff_per*100,"th cutoff"))
  ) %>%
  opt_align_table_header(align = "left") %>%
  tab_spanner(
    label = md("temperature"),
    columns = vars("Estimate_temperature","CI_temperature")
  ) %>%
  tab_spanner(
    label = md("WBGT"),
    columns = vars("Estimate_WBGT","CI_WBGT")
  ) %>%
  cols_width(
    "region" ~ px(130),
    everything() ~ px(150)
  ) %>%
  cols_align(
    align = "center",
    columns = TRUE
  ) %>%
  cols_label(
    Estimate_temperature = "point",
    Estimate_WBGT = "point",
    CI_temperature = "95% CI",
    CI_WBGT = "95% CI"
  ) %>%
  tab_footnote(
    footnote = md("North and South Kyushu region"),
    locations = cells_stub(
      rows=c("Kyushu_N","Kyushu_S")
    ))

gt_region_AF_90th


# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# gtsave(gt_region_AF_90th,
#        "figure/Table_region_AF_90th.pdf",
#        zoom=0.7)



# -------------------------------------------------------------------------
# TABLE版 prefecture-level attributable fraction ----------------------
# -------------------------------------------------------------------------

library(gt)

gt_pref_AF_95th <- 
  
  table_temp_AF_95th %>% 
  mutate(across(.cols = c(AF_point,AF_L95,AF_U95),
                .fns = ~{paste0(round(.x*100,1),"%")},
                .names = "{col}")) %>% 
  mutate(CI_temp = glue::glue("({AF_L95}, {AF_U95})")) %>% 
  select(region,prefname,point_temp=AF_point,CI_temp) %>% 
  
  left_join(table_WBGT_AF_95th %>% 
              mutate(across(.cols = c(AF_point,AF_L95,AF_U95),
                            .fns = ~{paste0(round(.x*100,2),"%")},
                            .names = "{col}")) %>% 
              mutate(CI_WBGT = glue::glue("({AF_L95}, {AF_U95})")) %>% 
              select(region,prefname,point_WBGT=AF_point,CI_WBGT),
            
            by=c("region","prefname")) %>% 
  mutate(region = region %>% as.factor %>% 
           fct_relevel(region_vec_level),
         prefname = prefname %>% as.factor %>% 
           fct_relevel(pref_vec)) %>% 
  arrange(region,prefname) %>% 
  ### gt ###
  gt(groupname_col = "region",
     rowname_col = "prefname") %>% 
  tab_header(
    title = md("prefecture-level attributable fraction"),
    subtitle = md(paste0(cutoff_per*100,"th cutoff"))
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  tab_spanner(
    label = md("Temperature"),
    columns = vars("point_temp","CI_temp")
  ) %>% 
  tab_spanner(
    label = md("WBGT"),
    columns = vars("point_WBGT","CI_WBGT")
  ) %>% 
  cols_width(
    "prefname" ~ px(130),
    everything() ~ px(150)
  ) %>% 
  cols_align(
    align = "center", 
    columns = TRUE
  ) %>% 
  cols_label(
    point_temp = "point",
    point_WBGT = "point",
    CI_temp = "95% CI",
    CI_WBGT = "95% CI"
  ) %>% 
  tab_footnote(
    footnote = md("North and South Kyushu region"),
    locations = cells_row_groups(
      groups=c("Kyushu_N","Kyushu_S")
    ))

gt_pref_AF_95th

# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# gtsave(gt_pref_AF_95th,
#        "figure/Table_pref_AF_95th.pdf",
#        zoom=0.7)



# -------------------------------------------------------------------------
# TABLE版 region-level attribution fraction ----------------------
# -------------------------------------------------------------------------

library(gt)

gt_region_AF_95th <-
  
  pooled_AF_95th %>%
  select(region,heat,Estimate,L95,U95) %>%
  mutate(across(.cols=where(is.numeric),
                .fns=~{paste0(round(.x*100,2),"%")},
                .names = "{col}")) %>%
  pivot_wider(names_from = heat,
              values_from = c(Estimate,L95,U95)) %>%
  mutate(CI_temperature = glue::glue("({L95_temperature}, {U95_temperature})"),
         CI_WBGT = glue::glue("({L95_WBGT}, {U95_WBGT})")) %>%
  mutate(group = ifelse(region == "nationwide","nationwide","region")) %>%
  mutate(region = region %>% as.factor %>%
           fct_relevel(c("nationwide",region_vec_level))) %>%
  select(group,region,contains(c("Estimate","CI"))) %>%
  arrange(region) %>%
  
  ### gt ###
  gt(groupname_col = "group",
     rowname_col = "region") %>%
  tab_header(
    title = md("region-level attributable fraction"),
    subtitle = md(paste0(cutoff_per*100,"th cutoff"))
  ) %>%
  opt_align_table_header(align = "left") %>%
  tab_spanner(
    label = md("temperature"),
    columns = vars("Estimate_temperature","CI_temperature")
  ) %>%
  tab_spanner(
    label = md("WBGT"),
    columns = vars("Estimate_WBGT","CI_WBGT")
  ) %>%
  cols_width(
    "region" ~ px(130),
    everything() ~ px(150)
  ) %>%
  cols_align(
    align = "center",
    columns = TRUE
  ) %>%
  cols_label(
    Estimate_temperature = "point",
    Estimate_WBGT = "point",
    CI_temperature = "95% CI",
    CI_WBGT = "95% CI"
  ) %>%
  tab_footnote(
    footnote = md("North and South Kyushu region"),
    locations = cells_stub(
      rows=c("Kyushu_N","Kyushu_S")
    ))

gt_region_AF_95th

# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# gtsave(gt_region_AF_95th,
#        "figure/Table_region_AF_95th.pdf",
#        zoom=0.7)



# table -------------------------------------------------------------------

library(gt)

gt_pref_RR <- 
  map(1:4,
      function(pers){
        
        pers_data <- 
          RR_highper %>% 
          select(prefcode,prefname,region,RR_temp_highper) %>% 
          unnest(RR_temp_highper) %>% 
          mutate(heat = "temperature") %>% 
          bind_rows(RR_highper %>% 
                      select(prefcode,prefname,region,RR_WBGT_highper) %>% 
                      unnest(RR_WBGT_highper) %>% 
                      mutate(heat = "WBGT")) %>% 
          mutate(prefname = prefname %>% as.factor %>% 
                   fct_relevel(rev(pref_vec))) %>% 
          filter(high_per == paste0("per",seq(90,97.5,2.5))[pers]) %>% 
          select(-predvar) %>% 
          pivot_wider(names_from = heat,
                      values_from = c(allRRlow, allRRfit, allRRhigh)) %>% 
          mutate(across(.cols = contains("allRR"),
                        .fns = ~{round(.x,4)},
                        .names = "{col}")) %>% 
          mutate(CI_temperature = glue::glue("({allRRlow_temperature}, {allRRhigh_temperature})"),
                 CI_WBGT = glue::glue("({allRRlow_WBGT}, {allRRhigh_WBGT})")) %>% 
          select(region,prefname,
                 Estimate_temperature = allRRfit_temperature,
                 CI_temperature,
                 Estimate_WBGT = allRRfit_WBGT,
                 CI_WBGT) %>% 
          mutate(region = region %>% as.factor %>% 
                   fct_relevel(region_vec_level),
                 prefname = prefname %>% as.factor %>% 
                   fct_relevel(pref_vec)) %>% 
          arrange(region,prefname)
        
        pers_data %>% 
          gt(groupname_col = "region",
             rowname_col = "prefname") %>% 
          tab_header(
            title = md(glue::glue("relative risk at {seq(90,97.5,2.5)[pers]} th percentile"))
          ) %>% 
          opt_align_table_header(align = "left") %>% 
          cols_width(
            "prefname" ~ px(130),
            vars("Estimate_temperature","Estimate_WBGT") ~ px(100),
            vars("CI_temperature","CI_WBGT") ~ px(150)
          ) %>% 
          cols_align(
            align = "center", 
            columns = TRUE
          ) %>% 
          tab_spanner(
            label = md("temperature"),
            columns = vars("Estimate_temperature","CI_temperature")
          ) %>% 
          tab_spanner(
            label = md("WBGT"),
            columns = vars("Estimate_WBGT","CI_WBGT")
          ) %>% 
          cols_label(
            Estimate_temperature = "point",
            Estimate_WBGT = "point",
            CI_temperature = "95% CI",
            CI_WBGT = "95% CI"
          ) %>% 
          tab_footnote(
            footnote = md("North and South Kyushu region"),
            locations = cells_row_groups(
              groups=c("Kyushu_N","Kyushu_S")
            ))
      })

gt_pref_RR[[1]]
gt_pref_RR[[3]]

# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# gtsave(gt_pref_RR[[1]],
#        "figure/Table_pref_RR_90th.pdf",
#        zoom=0.7)
# gtsave(gt_pref_RR[[3]],
#        "figure/Table_pref_RR_95th.pdf",
#        zoom=0.7)

# -------------------------------------------------------------------------
# region table ------------------------------------------------------------
# -------------------------------------------------------------------------

library(gt)
gt_region_RR <- 
  map(1:4,
      function(pers){
        
        pers_data <- 
          
          pooled_AFtot %>% 
          filter(high_per == paste0("per",seq(90,97.5,2.5))[pers]) %>% 
          mutate(heat = ifelse(heat == 1,"WBGT","temperature")) %>% 
          pivot_wider(names_from = heat,
                      values_from = c(Estimate, L95, U95,I2)) %>% 
          mutate(across(.cols = contains(c("Estimate", "L95", "U95")),
                        .fns = ~{round(.x,4)},
                        .names = "{col}")) %>% 
          mutate(CI_temperature = glue::glue("({L95_temperature}, {U95_temperature})"),
                 CI_WBGT = glue::glue("({L95_WBGT}, {U95_WBGT})")) %>% 
          select(group,region,
                 Estimate_temperature = Estimate_temperature,
                 CI_temperature,
                 I2_temperature,
                 Estimate_WBGT = Estimate_WBGT,
                 CI_WBGT,
                 I2_WBGT)
        
        pers_data %>% 
          gt(groupname_col = "group",
             rowname_col = "region") %>% 
          tab_header(
            title = md(glue::glue("relative risk at {seq(90,97.5,2.5)[pers]} th percentile"))
          ) %>% 
          opt_align_table_header(align = "left") %>% 
          cols_width(
            "region" ~ px(130),
            vars("Estimate_temperature","Estimate_WBGT") ~ px(100),
            vars("CI_temperature","CI_WBGT") ~ px(150),
            vars("I2_temperature","I2_WBGT") ~ px(70)
          ) %>% 
          cols_align(
            align = "center", 
            columns = TRUE
          ) %>% 
          tab_spanner(
            label = md("temperature"),
            columns = vars("Estimate_temperature","CI_temperature","I2_temperature")
          ) %>% 
          tab_spanner(
            label = md("WBGT"),
            columns = vars("Estimate_WBGT","CI_WBGT","I2_WBGT")
          ) %>% 
          cols_label(
            Estimate_temperature = "point",
            Estimate_WBGT = "point",
            CI_temperature = "95% CI",
            CI_WBGT = "95% CI",
            I2_temperature = md("I<sup>2"),
            I2_WBGT = md("I<sup>2")
          )
      })

gt_region_RR[[1]]
gt_region_RR[[4]]

# setwd("C:/Users/u030t/OneDrive/デスクトップ/research proposal/temp_WBGT")
# gtsave(gt_region_RR[[1]],
#        "figure/Table_region_RR_90th.pdf",
#        zoom=0.7)
# gtsave(gt_region_RR[[3]],
#        "figure/Table_region_RR_95th.pdf",
#        zoom=0.7)



