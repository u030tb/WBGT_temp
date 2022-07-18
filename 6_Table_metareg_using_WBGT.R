# Table_metareg_usingWBGT.pdf

load("R_code/usingWBGT_data/AF_usingWBGT_90th.R")
load("R_code/usingWBGT_data/AF_usingWBGT_95th.R")
load("R_code/usingWBGT_data/AF_usingWBGT_45prefectures_90th.R")
load("R_code/usingWBGT_data/AF_usingWBGT_45prefectures_95th.R")
load("R_code/usingWBGT_data/AF_usingWBGT_first_90th.R")
load("R_code/usingWBGT_data/AF_usingWBGT_first_95th.R")
load("R_code/usingWBGT_data/RR_usingWBGT.R")
load("R_code/usingWBGT_data/RR_usingWBGT_45prefectures.R")
load("R_code/usingWBGT_data/RR_usingWBGT_first.R")

pacman::p_load(
  tidyverse,
  gt
)
ls()
AF_usingWBGT_90th
AF_usingWBGT_95th
AF_usingWBGT_45prefectures_90th
AF_usingWBGT_45prefectures_95th
AF_usingWBGT_first_90th
AF_usingWBGT_first_95th
RR_usingWBGT
RR_usingWBGT_45prefectures
RR_usingWBGT_first





# -------------------------------------------------------------------------
# gt

gt_metareg_usingWBGT <- 
  bind_rows(
    # RR
    bind_rows(
      RR_usingWBGT_45prefectures %>% select(group,vars,Estimate,L95,U95,I2,I2_pre),
      RR_usingWBGT_first %>% select(group,vars,Estimate,L95,U95,I2,I2_pre)
    ) %>% 
      mutate(across(.cols=all_of(c("Estimate","L95","U95")),
                    .fns=~{round(.x*100,2)})) %>% 
      mutate(vars = gsub("per","",vars)) %>% 
      mutate(vars = paste0(vars,"th")) %>% 
      filter(vars %in% c("90th","95th")) %>% 
      select(group,vars,Estimate,L95,U95,I2,I2_pre) %>% 
      mutate(indi = "RR"),
    
    # AF
    bind_rows(
      AF_usingWBGT_90th,
      AF_usingWBGT_95th,
      AF_usingWBGT_45prefectures_90th,
      AF_usingWBGT_45prefectures_95th,
      AF_usingWBGT_first_90th,
      AF_usingWBGT_first_95th
    ) %>% 
      as_tibble() %>% 
      mutate(across(.cols=where(is.numeric),
                    .fns = ~{round(.x,2)})) %>% 
      select(group,vars,Estimate,L95,U95,I2,I2_pre) %>% 
      mutate(vars = gsub(" cutoff","",vars)) %>% 
      mutate(indi = "AF")
    
  ) %>% 
  # decimal number
  mutate(across(.cols=c("Estimate","L95","U95","I2","I2_pre"),
                .fns=~{formatC(.x, digits = 2, format="f")})) %>% 
  
  mutate(CI_WBGT = glue::glue("({L95}%, {U95}%)"),
         beta = glue::glue("{Estimate}%")) %>%  
  select(group,vars,beta,CI_WBGT,I2,I2_pre,indi) %>% 
  pivot_wider(id_cols = c("group","vars"),
              names_from = "indi",
              values_from = c("beta","CI_WBGT","I2","I2_pre")) %>% 
  gt(groupname_col = "group",
     rowname_col = "vars") %>% 
  tab_options(
    table.font.names = "Arial"
  ) %>% 
  tab_header(
    title = md("Table 2"),
    subtitle = md("The association between prefecture-level Relative Risk or Attributable Fraction, and indicator variable of WBGT from third-stage meta-regression analysis")
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  cols_width(
    "vars" ~ px(80),
    c("beta_RR","beta_AF") ~ px(80),
    c("CI_WBGT_RR","CI_WBGT_AF") ~ px(130),
    c("I2_RR","I2_AF","I2_pre_RR","I2_pre_AF") ~ px(70)
  ) %>% 
  cols_align(
    align = "center", 
    columns = TRUE
  ) %>% 
  tab_spanner(
    label = md("Relative Risk"),
    columns = contains("RR")
  ) %>% 
  tab_spanner(
    label = md("Attributable Fraction"),
    columns = contains("AF")
  ) %>% 
  cols_label(
    beta_RR = md("*&beta;*"),
    beta_AF = md("*&beta;*"),
    CI_WBGT_RR = md("(95% CI)"),
    CI_WBGT_AF = md("(95% CI)"),
    I2_RR = md("*I<sup>2*"),
    I2_AF = md("*I<sup>2*"),
    I2_pre_RR = md("*I<sup>2*_pre"),
    I2_pre_AF = md("*I<sup>2*_pre")
  ) %>%
  tab_source_note(
    source_note = md("*Note that these results show the percentage change between the results from WBGT and those from temperature. Null association is zero, and positive values indicate the results from WBGT are higher than those for temperature.*")
  ) %>% 
  tab_footnote(
    footnote = md("the residual heterogeneity"),
    locations = cells_column_labels(
      columns = c("I2_RR","I2_AF")
    )
  ) %>% 
  tab_footnote(
    footnote = md("the estimated coefficient of indicator variable for WBGT"),
    locations = cells_column_labels(
      columns = c("beta_RR","beta_AF")
    )
  ) %>% 
  tab_footnote(
    footnote = md("sensitivity analysis for removing Hokkaido and Okinawa"),
    locations = cells_row_groups(
      groups = "45prefectures"
    )
  )%>% 
  tab_footnote(
    footnote = md("sensitivity analysis for using first-stage heat-mortality association"),
    locations = cells_row_groups(
      groups = "first"
    )
  ) %>% 
  opt_footnote_marks(marks = "standard")

gt_metareg_usingWBGT
# save --------------------------------------------------------------------
gtsave(gt_metareg_usingWBGT,
       "figure/Table2_metareg_usingWBGT.pdf",
       zoom=0.7)