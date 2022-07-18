# 6_table_qAIC
# source("R_code/1_prefectural_data.R")
pacman::p_load(
  tidyverse,
  gt
)


qaIC_data <- 
  bind_rows(
    read.csv("R_code/qAIC/qAIC_main.csv") %>% 
      mutate(group = "Main"),
    read.csv("R_code/qAIC/qAIC_remove_HokkaidoOkinawa.csv") %>% 
      mutate(group = "45 Prefectures"),
    read.csv("R_code/qAIC/qAIC_first.csv") %>% 
      mutate(group = "First"),
    read.csv("R_code/qAIC/qAIC_y2011.csv") %>% 
      mutate(group = "2011-2019"),
    read.csv("R_code/qAIC/qAIC_Jun_Sep.csv") %>% 
      mutate(group = "June-Sep"),
    read.csv("R_code/qAIC/qAIC_nsyear.csv") %>% 
      mutate(group = "Spline of year"),
    read.csv("R_code/qAIC/qAIC_mean.csv") %>% 
      mutate(group = "Daily mean")
  )


# -------------------------------------------------------------------------
# gt
# -------------------------------------------------------------------------

gt_table_qAIC <- 
  qaIC_data %>% 
  # mutate(low_qAIC_WBGT = ifelse(qAIC_WBGT<qAIC_temp,T,F)) %>% 
  mutate(weight = gsub("case_weight","cases",weight)) %>% 
  # decimal number
  mutate(across(.cols=all_of(c("qAIC_temp","qAIC_WBGT")),
                .fns=~{round(.x,1)})) %>% 
  gt(groupname_col = "group",
     rowname_col = "weight") %>% 
  tab_options(
    table.font.names = "Arial"
  ) %>% 
  tab_header(
    title = md("Table SSS"),
    subtitle = md("qAIC table for all versions of analysis")
  ) %>% 
  opt_align_table_header(align = "left") %>% 
  cols_width(
    c("qAIC_temp","qAIC_WBGT") ~ px(150),
    c("weight") ~ px(100)
  ) %>% 
  cols_align(
    align = "center", 
    columns = everything()
  ) %>% 
  cols_label(
    qAIC_temp = md("Temperature"),
    qAIC_WBGT = md("WBGT")
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

gt_table_qAIC
# save --------------------------------------------------------------------
gtsave(gt_table_qAIC,
       "figure/TableS3_table_qAIC.pdf",
       zoom=0.7)

