# pref-level correlation coefficients
# (1) daily maximum ~ (May-Oct)
# (2) daily maximum ~ (May-Oct)
# (3) daily mean ~ (May-Oct)
# (4) daily mean ~ (May-Oct)

# -------------------------------------------------------------------------
# heat_metrics
# -------------------------------------------------------------------------

# temperature
load("data/temperature_data/all_temp.rda")

# WBGT
load("data/WBGT_data/capitals.rda")

heat_metrics <- 
  # WBGT
  capitals %>%
  left_join(regional_division,
            by="prefJap") %>% 
  mutate(month = month(date)) %>% 
  
  left_join(
    # temp
    all_temp %>% 
      map_df(~{bind_rows(.x)}) %>% 
      mutate(date = ymd(paste(year,month,day,sep="-"))) %>% 
      select(capEng=place,date,tmean,tmax),
    
    by = c("capEng","date")
  ) %>% 
  select(prefname, date, month, tmax, tmean, maxWBGT, meanWBGT)


heat_metrix
heat_metrix %>% summary


heat_metrix_May_October <- heat_metrix %>% filter(month %in% 5:10)
heat_metrix_June_September <- heat_metrix %>% filter(month %in% 6:9)

source("R_code/1_prefectural_data.R")
pref_vec
regional_division

# -------------------------------------------------------------------------
# (1) daily maximum ~ (May-Oct)
# -------------------------------------------------------------------------

daily_maximum_May_Oct <- 
  map_dfr(pref_vec,function(x){
    
    fdata = heat_metrix_May_October %>% filter(prefname == x)
    
    f_cor = cor(
      fdata$tmax,
      fdata$maxWBGT
    )
    
    data.frame(
      prefname = x,
      corr_max510 = f_cor
    )
    
  })

# -------------------------------------------------------------------------
# (2) daily maximum ~ (Jun-Sep)
# -------------------------------------------------------------------------

daily_maximum_Jun_Sep <- 
  map_dfr(pref_vec,function(x){
    
    fdata = heat_metrix_June_September %>% filter(prefname == x)
    
    f_cor = cor(
      fdata$tmax,
      fdata$maxWBGT
    )
    
    data.frame(
      prefname = x,
      corr_max69 = f_cor
    )
    
  })

# -------------------------------------------------------------------------
# (3) daily mean ~ (May-Oct)
# -------------------------------------------------------------------------

daily_mean_May_Oct <- 
  map_dfr(pref_vec,function(x){
    
    fdata = heat_metrix_May_October %>% filter(prefname == x)
    
    f_cor = cor(
      fdata$tmean,
      fdata$meanWBGT
    )
    
    data.frame(
      prefname = x,
      corr_mean510 = f_cor
    )
    
  })

# -------------------------------------------------------------------------
# (4) daily mean ~ (Jun-Sep)
# -------------------------------------------------------------------------

daily_mean_Jun_Sep <- 
  map_dfr(pref_vec,function(x){
    
    fdata = heat_metrix_June_September %>% filter(prefname == x)
    
    f_cor = cor(
      fdata$tmean,
      fdata$meanWBGT
    )
    
    data.frame(
      prefname = x,
      corr_mean69 = f_cor
    )
    
  })


# -------------------------------------------------------------------------
# integration
# -------------------------------------------------------------------------

table_corr <- 
  daily_maximum_May_Oct %>% 
  left_join(daily_maximum_Jun_Sep,by="prefname") %>% 
  left_join(daily_mean_May_Oct,by="prefname") %>% 
  left_join(daily_mean_Jun_Sep,by="prefname") %>% 
  left_join(regional_division %>% select(region,prefname),
            by = "prefname") %>% 
  mutate(across(.cols=contains("corr_"),
                .fns=~{round(.x,3)}))


# median
table_corr %>% 
  summarise(across(.cols=contains("corr_"),
                   .fns=~{median(.x)}))

# supplemental material 
pacman::p_load(gt)
table_corr %>% 
  gt(groupname_col = "region",
     rowname_col = "prefname")

