# Table_metareg_usingWBGT_sa_RR

load("R_code/usingWBGT_data/RR_usingWBGT_45prefectures.R")
load("R_code/usingWBGT_data/RR_usingWBGT_first.R")
load("R_code/usingWBGT_data/RR_usingWBGT_2011.R")
load("R_code/usingWBGT_data/RR_usingWBGT_June_sep.R")
load("R_code/usingWBGT_data/RR_usingWBGT_nsyear.R")
load("R_code/usingWBGT_data/RR_usingWBGT_mean.R")

RR_usingWBGT_45prefectures
RR_usingWBGT_first <- RR_usingWBGT_first %>% mutate(group = "First")
RR_usingWBGT <- RR_usingWBGT %>% mutate(group = "Main")
RR_usingWBGT_2011 <- RR_usingWBGT_2011 %>% mutate(group = "2011-2019")
RR_usingWBGT_June_Sep <- RR_usingWBGT_June_Sep %>% mutate(group = "June-Sep")
RR_usingWBGT_nsyear <- RR_usingWBGT_nsyear %>% mutate(group = "Spline of year")
RR_usingWBGT_mean <- RR_usingWBGT_mean %>% mutate(group = "Daily mean")

pacman::p_load(
  tidyverse,
  gt
)
ls()

# -------------------------------------------------------------------------
# gt
vec_vec <- c("group","vars","Estimate","L95","U95","I2","I2_pre")

gt_metareg_usingWBGT_sa <- 
  
  bind_rows(
    RR_usingWBGT_45prefectures %>% select(all_of(vec_vec)),
    RR_usingWBGT_first %>% select(all_of(vec_vec)),
    RR_usingWBGT_2011 %>% select(all_of(vec_vec)),
    RR_usingWBGT_June_Sep %>% select(all_of(vec_vec)),
    RR_usingWBGT_nsyear %>% select(all_of(vec_vec)),
    RR_usingWBGT_mean %>% select(all_of(vec_vec))
  ) %>% 
  mutate(vars = gsub("per","",vars)) %>% 
  mutate(vars = paste0(vars,"th")) %>% 
  filter(vars %in% c("90th","95th")) %>% 
  
  # decimal number
  mutate(across(.cols=all_of(c("Estimate","L95","U95")),
                .fns=~{round(.x,2)})) %>% 
  mutate(across(.cols=c("Estimate","L95","U95","I2","I2_pre"),
                .fns=~{formatC(.x, digits = 2, format="f")})) %>% 

  mutate(CI_WBGT = glue::glue("({L95}%, {U95}%)"),
         beta = glue::glue("{Estimate}%")) %>%  
  select(group,vars,beta,CI_WBGT,I2,I2_pre) %>% 
  gt(groupname_col = "group",
     rowname_col = "vars") %>% 
  tab_options(
    table.font.names = "Arial"
  ) %>% 
  tab_header(
    title = md("Table SSS"),
    subtitle = md("The association between prefecture-level Relative Risk and indicator variable of WBGT from third-stage meta-regression analysis")
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  cols_width(
    c("vars","beta","I2","I2_pre") ~ px(100),
    c("CI_WBGT") ~ px(170),
  ) %>% 
  cols_align(
    align = "center", 
    columns = TRUE
  ) %>% 
  tab_spanner(
    label = md("Relative Risk"),
    columns = contains("RR")
  ) %>% 
  cols_label(
    beta = md("*&beta;*"),
    CI_WBGT = md("(95% CI)"),
    I2 = md("*I<sup>2*"),
    I2_pre = md("*I<sup>2*_pre"),
  ) %>%
  tab_source_note(
    source_note = md("*Note that these results show the percentage change between the results from WBGT and those from temperature. Null association is zero, and positive values indicate the results from WBGT are higher than those for temperature.*")
  ) %>% 
  tab_footnote(
    footnote = md("the residual heterogeneity"),
    locations = cells_column_labels(
      columns = c("I2")
    )
  ) %>% 
  tab_footnote(
    footnote = md("the estimated coefficient of indicator variable for WBGT"),
    locations = cells_column_labels(
      columns = c("beta")
    )
  ) %>% 
  tab_footnote(
    footnote = md("sensitivity analysis limited to data from 2011 to 2019, the period after the new HHWS was enacted"),
    locations = cells_row_groups(
      groups = "2011-2019"
    )
  )%>% 
  tab_footnote(
    footnote = md("sensitivity analysis limited to data from June to September"),
    locations = cells_row_groups(
      groups = "June-Sep"
    )
  )%>% 
  tab_footnote(
    footnote = md("sensitivity analysis with year variables modeled by natural cubic splines with only one knot"),
    locations = cells_row_groups(
      groups = "Spline of year"
    )
  )%>% 
  tab_footnote(
    footnote = md("sensitivity analysis with daily mean temperature and WBGT"),
    locations = cells_row_groups(
      groups = "Daily mean"
    )
  )%>% 
  opt_footnote_marks(marks = "standard")

gt_metareg_usingWBGT_sa
# save --------------------------------------------------------------------
gtsave(gt_metareg_usingWBGT_sa,
       "figure/Table2_metareg_usingWBGT_sa_RR.pdf",
       zoom=0.7)
