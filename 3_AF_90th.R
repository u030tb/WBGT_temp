# AF_90th
source("R_code/1_prefectural_data.R")
source("R_code/2_prefectural_BLUP.R")


regional_division
pref_list
metapredictor_table %>% head(2)
pref_vec
region_vec_level
first_result_list
nationwide_2stage_BLUP

nationwide_2stage_BLUP$data[[1]]



###############################################################################
### outline ###
# 1. analysis
## 1-0. parametric bootstrap
## 1-1. pref analysis
## 1-2. region analysis
## 1-3. usingWBGT
###############################################################################

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# analysis ----------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

###############################################################################
# 
# 1-0. parametric bootstrap
# 
###############################################################################
cutoff_per <- 0.9

nsim=1000

pref_temp_AF_90th <-
  nationwide_2stage_BLUP %>% 
  select(data,MMT,bvar_temp,nationwide_temp_BLUP) %>% 
  pmap(function(data,MMT,bvar_temp,nationwide_temp_BLUP){
    
    # ?ϐ??ǉ? --------------------------------------------------------------------
    p_data <- 
      data %>% 
      mutate(over_90th = data$tmax >= quantile(data$tmax,cutoff_per))
    
    # basis?쐬 -----------------------------------------------------------------
    # ?ߋ??쐬crosspred?̓p?[?Z???g???Ƃ?basis???????��ĂȂ??B
    # ?????Ŏ??n???f?[?^?ɑ΂??Ă?basis???ēx?\?z?????K?v???????B
    p_argvar_temp <- 
      list(fun = varfun,
           intercept = FALSE,
           knots = attr(bvar_temp,"knots"),
           Boundary.knots = attr(bvar_temp,"Boundary.knots"))
    
    p_bvar_temp <- 
      do.call(onebasis, c(list(x = p_data$tmax),p_argvar_temp))
    p_cen_temp <- 
      do.call(onebasis, c(list(x = MMT), p_argvar_temp))
    p_bvarcen_temp <- 
      scale(p_bvar_temp, center = p_cen_temp, scale = F)
    
    
    # ?_???? ---------------------------------------------------------------------
    # BLUP???ʂ̂ς??߂??擾
    p_coef = nationwide_temp_BLUP$blup
    p_vcov = nationwide_temp_BLUP$vcov
    
    # daily AN vector(f-AN?ł?b-AN?ł??Ȃ?????)
    p_an_heatcold <- (1 - exp(-p_bvarcen_temp %*% p_coef)) * p_data$all
    
    # MMT?ȏ??݂̂? AN
    point_AN <- sum(p_an_heatcold[p_data$over_90th])
    death_denominator <- sum(p_data$all[p_data$over_90th])
    point_AF <- point_AN / death_denominator
    
    
    # ???Ԑ??? parametric bootstrap -----------------------------------------------
    
    # ????????
    set.seed(19941004)
    p_coefsim <- MASS::mvrnorm(nsim, p_coef, p_vcov)
    
    random_AN <- 
      map_dbl(1:nsim,
              function(y){
                y_an <- (1 - exp(-p_bvarcen_temp %*% p_coefsim[y, ])) * p_data$all
                y_random_AN <- sum(y_an[p_data$over_90th])
                return(y_random_AN)
              })
    
    # summary table --------------------------------------------------
    final_list <- 
      list(point_AN = point_AN,
           death_denominator = death_denominator,
           point_AF = point_AF,
           random_AN = random_AN)
    
    return(final_list)
    
  })


pref_WBGT_AF_90th <- 
  
  nationwide_2stage_BLUP %>% 
  select(data,MMW,bvar_WBGT,nationwide_WBGT_BLUP) %>% 
  pmap(function(data,MMW,bvar_WBGT,nationwide_WBGT_BLUP){
    
    
    # ?ϐ??ǉ? --------------------------------------------------------------------
    p_data <- 
      data %>% 
      mutate(over_90th = data$maxWBGT >= quantile(data$maxWBGT,cutoff_per))
    
    
    # basis?쐬 -----------------------------------------------------------------
    # ?ߋ??쐬crosspred?̓p?[?Z???g???Ƃ?basis???????��ĂȂ??B
    # ?????Ŏ??n???f?[?^?ɑ΂??Ă?basis???ēx?\?z?????K?v???????B
    p_argvar_WBGT <- 
      list(fun = varfun,
           intercept = FALSE,
           knots = attr(bvar_WBGT,"knots"),
           Boundary.knots = attr(bvar_WBGT,"Boundary.knots"))
    
    p_bvar_WBGT <- 
      do.call(onebasis, c(list(x = p_data$maxWBGT),p_argvar_WBGT))
    p_cen_WBGT <- 
      do.call(onebasis, c(list(x = MMW), p_argvar_WBGT))
    p_bvarcen_WBGT <- 
      scale(p_bvar_WBGT, center = p_cen_WBGT, scale = F)
    
    
    # ?_???? ---------------------------------------------------------------------
    # BLUP???ʂ̂ς??߂??擾
    p_coef = nationwide_WBGT_BLUP$blup
    p_vcov = nationwide_WBGT_BLUP$vcov
    
    
    # daily AN vector(f-AN?ł?b-AN?ł??Ȃ?????)
    p_an_heatcold <- (1 - exp(-p_bvarcen_WBGT %*% p_coef)) * p_data$all
    
    # MMT?ȏ??݂̂? AN
    point_AN <- sum(p_an_heatcold[p_data$over_90th])
    death_denominator <- sum(p_data$all[p_data$over_90th])
    point_AF <- point_AN / death_denominator
    
    # ???Ԑ??? parametric bootstrap -----------------------------------
    
    # ????????
    set.seed(19941004)
    p_coefsim <- MASS::mvrnorm(nsim, p_coef, p_vcov)
    
    random_AN <- 
      map_dbl(1:nsim,
              function(y){
                y_an <- (1 - exp(-p_bvarcen_WBGT %*% p_coefsim[y, ])) * p_data$all
                y_random_AN <- sum(y_an[p_data$over_90th])
                return(y_random_AN)
              })
    
    # summary table --------------------------------------------------
    final_list <- 
      list(point_AN = point_AN,
           death_denominator = death_denominator,
           point_AF = point_AF,
           random_AN = random_AN)
    
    return(final_list)
  })

# ???ʕ?
pref_temp_AF_90th[[1]]
pref_WBGT_AF_90th[[1]]


###############################################################################
# 
# 1-1. pref analysis
# 
###############################################################################

# empirical distribution?̏W?v -----------------------------------------------
table_temp_AF_90th <- 
  map_df(1:length(pref_temp_AF_90th), function(p){
    
    random_AF <- 
      pref_temp_AF_90th[[p]]$random_AN / pref_temp_AF_90th[[p]]$death_denominator
    
    quantile(random_AF,probs=c(0.025,0.975)) %>% 
      t() %>% 
      as.data.frame() %>% 
      mutate(point = pref_temp_AF_90th[[p]]$point_AF,
             prefname = pref_vec[p],
             AF_var = random_AF %>% var) %>% 
      mutate(across(.cols=where(is.numeric),
                    .fns=~{round(.x,4)})) %>% 
      select(prefname,
             AF_point = point,
             AF_var,
             AF_L95=`2.5%`,
             AF_U95=`97.5%`) %>% 
      return()
  }) %>% 
  left_join(nationwide_2stage_BLUP %>% 
              select(prefcode,prefname,region),
            by="prefname")

table_WBGT_AF_90th <- 
  map_df(1:length(pref_WBGT_AF_90th), function(p){
    
    random_AF <- 
      pref_WBGT_AF_90th[[p]]$random_AN / pref_WBGT_AF_90th[[p]]$death_denominator
    
    quantile(random_AF,probs=c(0.025,0.975)) %>% 
      t() %>% 
      as.data.frame() %>% 
      mutate(point = pref_WBGT_AF_90th[[p]]$point_AF,
             prefname = pref_vec[p],
             AF_var = random_AF %>% var) %>% 
      mutate(across(.cols=where(is.numeric),
                    .fns=~{round(.x,4)})) %>% 
      select(prefname,
             AF_point = point,
             AF_var,
             AF_L95=`2.5%`,
             AF_U95=`97.5%`) %>% 
      return()
  }) %>% 
  left_join(nationwide_2stage_BLUP %>% 
              select(prefcode,prefname,region),
            by="prefname")

table_temp_AF_90th
table_WBGT_AF_90th

###############################################################################
# 
# 1-2. region analysis
# 
###############################################################################

pooled_AF_90th <-
  map_df(c("temperature","WBGT"),function(fheat){
    
    # temperature or WBGT -----------------------------------------------------
    f_data <-
      bind_rows(table_temp_AF_90th %>% mutate(heat = "temperature"),
                table_WBGT_AF_90th %>% mutate(heat = "WBGT")) %>%
      filter(heat == fheat)
    
    # nationwide --------------------------------------------------------------
    # national result??two-level random-effect?K?{?ł??傤
    mixmeta_nationwide <-
      mixmeta(AF_point,
              AF_var,
              random= ~ 1|region/prefname,
              data = f_data,
              method = "reml")
    
    nationwide_result <-
      mixmeta_nationwide %>%
      ci.lin() %>%
      as_tibble() %>%
      select(Estimate,L95=`2.5%`,U95=`97.5%`) %>%
      mutate(region = "Nationwide",
             I2 = mixmeta_nationwide %>% summary() %>% .$i2stat %>% round(1))
    
    # region result -----------------------------------------------------------
    region_result <-
      map_df(1:length(region_vec_level),~{
        
        if (.x %in% c(1,11)) {
          f_data %>%
            filter(region == region_vec_level[.x]) %>%
            select(Estimate = AF_point,
                   L95 = AF_L95,
                   U95 = AF_U95) %>%
            mutate(region = region_vec_level[.x],
                   I2 = 0)
        } else {
          f_mixmeta <-
            mixmeta(AF_point,
                    AF_var,
                    data = f_data %>% filter(region == region_vec_level[.x]),
                    method = "reml")
          
          f_mixmeta %>%
            ci.lin() %>%
            as_tibble() %>%
            select(Estimate,L95=`2.5%`,U95=`97.5%`) %>%
            mutate(region = region_vec_level[.x],
                   I2 = f_mixmeta %>% summary() %>% .$i2stat %>% round(1))
        }
      })
    
    bind_rows(nationwide_result,
              region_result) %>%
      mutate(heat = fheat) %>%
      mutate(group = ifelse(region == "Nationwide","Nationwide","region")) %>%
      return()
    
  }) %>%
  mutate(region = region %>% as.factor %>%
           fct_relevel(rev(c(region_vec_level,"Nationwide"))))

# nationwide results????Results?????ŕ񍐂??????B
pooled_AF_90th %>% 
  filter(region == "Nationwide")

###############################################################################
# 
# 1-3. usingWBGT
# 
###############################################################################

AF_table_90th <- 
  bind_rows(
    table_temp_AF_90th %>% mutate(heat = 0),
    table_WBGT_AF_90th %>% mutate(heat = 1)
  )

# meta-regression without fixed-effect
mixmeta_pre_90th <- 
  mixmeta(AF_point ~ 1,
          AF_var,
          random= ~ 1|region/prefname, # 
          data = AF_table_90th,
          method = "reml")

# meta-regression with fixed-effect
mixmeta_90th <- 
  mixmeta(AF_point ~ heat,
          AF_var,
          random= ~ 1|region/prefname, # 
          data = AF_table_90th,
          method = "reml")

AF_usingWBGT_90th <- 
  mixmeta_90th %>% 
  ci.lin() %>% 
  as.data.frame() %>% 
  mutate(rowlabel = rownames(.)) %>% 
  filter(rowlabel == "heat") %>% 
  select(Estimate,L95=`2.5%`,U95=`97.5%`) %>% 
  mutate(across(.cols=all_of(c("Estimate","L95","U95")),
                .fns=~{.x*100})) %>% 
  mutate(group = "main",
         vars = "90th cutoff",
         I2 = mixmeta_90th %>% summary() %>% .$i2stat %>% round(2),
         I2_pre = mixmeta_pre_90th %>% summary() %>% .$i2stat %>% round(2))

# ???ʕ?
AF_usingWBGT_90th

# save(AF_usingWBGT_90th,
#      file="R_code/usingWBGT_data/AF_usingWBGT_90th.R")